/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.categorize;


import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.Serializable;
import java.lang.reflect.Method;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import innowake.mining.server.discovery.categorize.csharp.CSharpFileTypeDetection;
import org.eclipse.core.internal.resources.Project;
import org.ff4j.FF4j;
import org.springframework.beans.factory.annotation.Autowired;

import com.google.common.collect.Sets;

import brave.Span;
import brave.Tracer;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.lib.job.api.management.JobMonitor;
import innowake.lib.job.api.task.ResultConsumer;
import innowake.lib.job.api.task.ResultOrderTaskProcessor;
import innowake.lib.job.api.task.TaskProcessor;
import innowake.lib.job.api.task.TaskSource;
import innowake.mining.server.discovery.cache.DiscoveryJobCache;
import innowake.mining.server.discovery.categorize.assembler.AssemblerFileTypeDetection;
import innowake.mining.server.discovery.categorize.assembler.VAXMacroFileTypeDetection;
import innowake.mining.server.discovery.categorize.basic.BasicFileTypeDetection;
import innowake.mining.server.discovery.categorize.binary.BinaryFileTypeDetection;
import innowake.mining.server.discovery.categorize.c.CFileTypeDetection;
import innowake.mining.server.discovery.categorize.cobol.CobolFileTypeDetection;
import innowake.mining.server.discovery.categorize.cobol.CobolModuleSplitter;
import innowake.mining.server.discovery.categorize.cpp.CppFileTypeDetection;
import innowake.mining.server.discovery.categorize.csd.CSDFileTypeDetection;
import innowake.mining.server.discovery.categorize.dcl.DclFileTypeDetection;
import innowake.mining.server.discovery.categorize.easytrieve.EasytrieveFileTypeDetection;
import innowake.mining.server.discovery.categorize.ecl.EclFileTypeDetection;
import innowake.mining.server.discovery.categorize.fms.FmsFileTypeDetection;
import innowake.mining.server.discovery.categorize.ims.ImsFileTypeDetectionByPattern;
import innowake.mining.server.discovery.categorize.java.JavaFileTypeDetection;
import innowake.mining.server.discovery.categorize.jcl.JclFileTypeDetection;
import innowake.mining.server.discovery.categorize.natural.NaturalFileTypeDetection;
import innowake.mining.server.discovery.categorize.oracle.OracleFileTypeDetection;
import innowake.mining.server.discovery.categorize.pl1.Pl1FileTypeDetection;
import innowake.mining.server.discovery.categorize.sql.SQLScriptCategorizer;
import innowake.mining.server.discovery.categorize.vb.VbFileTypeDetection;
import innowake.mining.server.discovery.categorize.xml.XmlFileTypeDetection;
import innowake.mining.server.discovery.metrics.TaskHandler;
import innowake.mining.server.discovery.monitor.ProgressMonitorText;
import innowake.mining.server.job.MiningJob;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.core.IdentificationMapper;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.discovery.config.utility.UtilityList;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.discovery.ResolveTarget;

/**
 * This class is the entry point for categorizing the Source code which is executed as a Job.
 * It has various detection process. See {@link DiscoverCodeJob#performDetectionProcess}.
 * Once the categorizing is done, it updates the database.
 * In addition to source code categorization, it also does some modification to the source code as part of postCategroization.
 * See {@link DiscoverCodeJob#postCategorize}
 */
public class DiscoverCodeJob extends MiningJob<Serializable> implements TaskHandler {

	private static final Logger LOG = LoggerFactory.getLogger(innowake.mining.server.discovery.Logging.CATEGORIZE);

	private static final Set<DetectionType> PRIMARY_DETECTIONS = Sets.newHashSet(
			DetectionType.COBOL,
			DetectionType.JCL,
			DetectionType.NATURAL,
			DetectionType.JAVA);

	private static final Set<DetectionType> SECONDARY_DETECTIONS = Sets.newHashSet(
			DetectionType.CSD,
			DetectionType.ASSEMBLER,
			DetectionType.VAX_MACRO,
			DetectionType.XML,
			DetectionType.PL1,
			DetectionType.EASYTRIEVE,
			DetectionType.C,
			DetectionType.BINARY,
			DetectionType.IMS_PATTERN,
			DetectionType.DCL,
			DetectionType.FMS,
			DetectionType.ORACLE,
			DetectionType.BASIC,
			DetectionType.SQL_SCRIPT,
			DetectionType.ECL,
			DetectionType.VB,
			DetectionType.CPP,
			DetectionType.CSHARP);

	@Autowired
	private transient SourceCachingService sourceService;
	@Autowired
	private transient ProjectService projectService;
	@Autowired
	private transient DiscoveryJobCache discoveryCache;
	@Autowired
	private transient Tracer tracer;
	@Autowired
	private transient GenericConfigProperties configProperties;
	@Autowired
	private transient FF4j ff4j;

	@Nullable
	protected transient TaskProcessor<?> taskProcessor;

	/**
	 * Constructor takes project id and the required dependencies.
	 *
	 * @param projectId id of the Project.
	 */
	public DiscoverCodeJob(final EntityId projectId) {
		super(projectId);
	}

	@Override
	public Result<Serializable> run(final ProgressMonitor progressMonitor) {
		taskProcessor = new ResultOrderTaskProcessor<>(jobManager, assertNotNull(jobMonitor));
		final Map<FeatureId, Boolean> featureMap = new EnumMap<>(FeatureId.class);
		Stream.of(FeatureId.values()).forEach(feature -> featureMap.put(feature, Boolean.valueOf(ff4j.getFeature(feature.getId()).isEnable())));
		try {
			/* log a message to ensure a log file that can be stored by log framework */
			LOG.info(() -> "Starting Discover Code for project ID " + projectId);
			progressMonitor.setJobDescription(ProgressMonitorText.DISCOVERY_CODE_TASK_DISCOVERCODE);
			final List<SourcePojo> allSourceObject = sourceService.find(q -> q.ofProject(projectId));
			final Map<Long, SourcePojo> sourceObjects = allSourceObject.parallelStream()
					.filter(sourceObject -> ! sourceObject.getPath().split("/")[0].equals(IdentificationMapper.SRC_ROOT))
					.collect(Collectors.toConcurrentMap(SourcePojo::getId, so -> so));

			final List<SourcePojo> existingSourceObjects = allSourceObject.stream()
					.filter(sourceObject -> sourceObject.getPath().split("/")[0].equals(IdentificationMapper.SRC_ROOT))
					.collect(Collectors.toList());

			if (sourceObjects.isEmpty()) {
				LOG.warn(() -> "No resources exist in the project.");
				return new Result<>(Status.OK);
			}

			final ProjectPojo project = projectService.get(projectId);
			final SearchOrders searchOrders = new SearchOrders(project.getSearchOrders());

			final Config config;
			try {
				config = Config.loadConfig(projectService, projectId);
			} catch (final DiscoveryException e) {
				LOG.error(() -> "Failed to load configuration for project ID " + projectId, e);
				return new Result<>(new Status(e));
			}

			if (config.getUtilityExclusionEnabled()) {
				try {
					config.setUtilityList(UtilityList.loadUtilityList(projectService, projectId));
				} catch (final DiscoveryException e) {
					return new Result<>(new Status(e));
				}
				LOG.info(() -> "Utility exclusion is enabled. Artifacts matching entries in utilities.xml will be skipped for analysis.");
			} else {
				LOG.info(() -> "Utility exclusion is disabled.");
			}
			progressMonitor
					.setJobDescription(ProgressMonitorText.DISCOVERY_CODE_TASK_DISCOVERCODE + ": " + ProgressMonitorText.DISCOVERY_CODE_TASK_PRECATEGORIZE);
			progressMonitor.setStepDescription(ProgressMonitorText.DISCOVERY_CODE_TASK_PRECATEGORIZE);

			preCategorize(projectId);

			/* Perform most common type detections */
			final IdentificationMapper identificationMapper = new IdentificationMapper(config);
			final Categorizer categorizer = new Categorizer(projectId, identificationMapper, sourceObjects, sourceService, this, discoveryCache,
					tracer, configProperties, existingSourceObjects, searchOrders, featureMap);
			final Set<DetectionType> primaryDetections = getPrimaryDetections(config);
			final Set<DetectionType> secondaryDetections = getSecondaryDetections(config);
			final Statistic statistic = categorizer.getStatistic();
			progressMonitor.setJobDescription(ProgressMonitorText.DISCOVERY_CODE_TASK_DISCOVERCODE + ": " + ProgressMonitorText.DISCOVERY_CODE_TASK_CATEGORIZE);
			progressMonitor.setStepDescription(ProgressMonitorText.DISCOVERY_CODE_TASK_CATEGORIZE);

			performDetectionProcess(config, progressMonitor.subMonitor(96), categorizer, primaryDetections, secondaryDetections);

			progressMonitor
					.setJobDescription(ProgressMonitorText.DISCOVERY_CODE_TASK_DISCOVERCODE + ": " + ProgressMonitorText.DISCOVERY_CODE_TASK_POSTCATEGORIZE);
			progressMonitor.setStepDescription(ProgressMonitorText.DISCOVERY_CODE_TASK_POSTCATEGORIZE);
			
			postCategorize(statistic);
			statistic.printSummary(getJobMonitor());
			
			/* Clear the step description as we are done now. */
			progressMonitor.setJobDescription(ProgressMonitorText.DISCOVERY_CODE_TASK_DISCOVERCODE);
			progressMonitor.setStepDescription("");

			return new Result<>(Status.OK);
		} finally {
			discoveryCache.clearDiscoveryJobCache(getJobId());
		}
	}

	@Override
	public String getJobName() {
		return "Discover Code";
	}

	@Override
	@SuppressWarnings("unchecked") /* we can use the taskProcessor to submit any kind of task */
	public final <R extends Serializable> void forkJobTasks(final TaskSource<R> taskSource, final ResultConsumer<R> resultConsumer) {
		super.forkTasks((TaskProcessor<R>) assertNotNull(taskProcessor), taskSource, resultConsumer);
	}

	@Override
	public JobMonitor getJobMonitor() {
		return assertNotNull(jobMonitor);
	}

	@Override
	public Span getSpan() {
		return assertNotNull(jobSpan);
	}

	/**
	 * Performs the detection process for categorization.
	 * Performs detection based on the priority set in {@link Config}.
	 *
	 * The default detection process :
	 * 1. Prepare for detection
	 * 2. Perform detection by main types (primary detections then secondary)
	 * 3. Perform detection by token types (primary detections then secondary)
	 * 4. Perform detection by dependencies (primary detections then secondary)
	 * 5. Perform detection by extension (primary detections then secondary)
	 * 6. Wrap up the detection process.
	 *
	 * @param config The discovery configuration.
	 * @param mon The progress monitor for the detection process.
	 * @param detectMonitor The task monitor for the detection process.
	 * @param categorizer The categorizer to drive the detection process.
	 * @param primaryDetections The list of primary detections to consider, will be given priority over secondary detections.
	 * @param secondaryDetections The list of secondary detections to consider.
	 */
	private void performDetectionProcess(final Config config, final ProgressMonitor detectMonitor,
			final Categorizer categorizer, final Set<DetectionType> primaryDetections, final Set<DetectionType> secondaryDetections) {
		try {
			/* The actual amount of work is not important for the ProgressMonoitor as long as it is divisible by 2 */
			final int detectionWorkPerCategory = 100;
			categorizer.prepareBeforeDetection();
			config.getFileDetectionTypes().stream().sorted((f1, f2) -> f1.getPriority().compareTo(f2.getPriority())).forEach(fileType -> {
				switch (fileType.getType()) {
					case "MAIN":
						categorizer.detectByMainType(detectMonitor.subMonitor(detectionWorkPerCategory / 2), primaryDetections);
						categorizer.detectByMainType(detectMonitor.subMonitor(detectionWorkPerCategory / 2), secondaryDetections);
						break;
					case "TOKEN":
						categorizer.detectByTokens(detectMonitor.subMonitor(detectionWorkPerCategory / 2), primaryDetections);
						categorizer.detectByTokens(detectMonitor.subMonitor(detectionWorkPerCategory / 2), secondaryDetections);
						break;
					case "DEPENDENCY":
						categorizer.detectByDependencies(detectMonitor.subMonitor(detectionWorkPerCategory / 2), primaryDetections);
						categorizer.detectByDependencies(detectMonitor.subMonitor(detectionWorkPerCategory / 2), secondaryDetections);
						break;
					case "EXTENSION":
						categorizer.detectByExtension(detectMonitor.subMonitor(detectionWorkPerCategory / 2), primaryDetections);
						categorizer.detectByExtension(detectMonitor.subMonitor(detectionWorkPerCategory / 2), secondaryDetections);
						break;
					default:
						if (LOG.isErrorEnabled()) {
							LOG.error(String.format("Detection category doesn't exist for the file type : %s.", fileType.getType()));
						}
						break;
				}
			});

			categorizer.detectRemainingUnidentified(detectMonitor.subMonitor(detectionWorkPerCategory));
		} finally {
			categorizer.wrapUpDetection(getJobMonitor());
		}
	}

	/**
	 * Retrieves the list of primary detections we recognize (JCL, COBOL, Natural).
	 *
	 * @param config The discovery configuration.
	 * @param worker The Timed worker.
	 * @return List of primary FileTypeDetections (JCL, COBOL, Natural).
	 */
	private Set<DetectionType> getPrimaryDetections(final Config config) {
		return PRIMARY_DETECTIONS.parallelStream()
				.filter(filterConfigLanguage(config))
				.collect(Collectors.toSet());
	}

	/**
	 * Retrieves the list of secondary detections we recognize (everything except JCL, COBOL, Natural).
	 *
	 * @param config The discovery configuration.
	 * @param worker The Timed worker.
	 * @return List of secondary FileTypeDetections (everything except JCL, COBOL, Natural).
	 */
	private Set<DetectionType> getSecondaryDetections(final Config config) {
		return SECONDARY_DETECTIONS.parallelStream()
				.filter(filterConfigLanguage(config))
				.collect(Collectors.toSet());
	}

	/**
	 * Functional interface filter for filtering out the languages that are disabled in the discovery configuration file.
	 *
	 * @param config The discovery configuration to use to determine what languages are disabled.
	 * @return true if the detection's language is enabled, false otherwise.
	 */
	private Predicate<DetectionType> filterConfigLanguage(final Config config) {
		return type -> {
			final boolean result = config.enableLanguage(type.language);
			if ( ! result) {
				LOG.warn(() -> type.name() + " is disabled.");
			}
			return result;
		};
	}

	private static ResolveTarget getDetectionLanguage(final Class<? extends FileTypeDetection> detectionClass) {
		try {
			final Method method = detectionClass.getMethod("getLanguage");
			return ((ResolveTarget) method.invoke(null));
		} catch (final ReflectiveOperationException e) {
			throw new IllegalStateException("Every FileTypeDetection requires a public static method 'getLanguage' returning the ResolveTarget", e);
		}
	}

	/**
	 * Perform operations that are required before Discover code process.
	 *
	 * Currently it updates the {@link Project#getSourceCodeRevision()} by incrementing value 1.
	 *
	 * @param project the project for which Discover code runs.
	 */
	private void preCategorize(final EntityId projectId) {
		projectService.incrementSourceCodeRevision(projectId);
	}

	/**
	 * Performs operations that are to be processed after Discover code process.
	 *
	 * Currently it does the Cobol file split operation on the identified {@link Technology#COBOL} artifacts.
	 * 
	 * @param statistic the statistic summary
	 */
	private void postCategorize(final Statistic statistic) {
		sourceService.find(q -> q.ofProject(projectId).withTechnology(Technology.COBOL)).parallelStream()
			.forEach(cobolObject -> CobolModuleSplitter.execute(cobolObject, sourceService, statistic));
	}

	/**
	 * The supported language detection types for discover code.
	 */
	public enum DetectionType {

		COBOL(CobolFileTypeDetection.class),
		JCL(JclFileTypeDetection.class),
		NATURAL(NaturalFileTypeDetection.class),
		CSD(CSDFileTypeDetection.class),
		ASSEMBLER(AssemblerFileTypeDetection.class),
		VAX_MACRO(VAXMacroFileTypeDetection.class),
		XML(XmlFileTypeDetection.class),
		PL1(Pl1FileTypeDetection.class),
		EASYTRIEVE(EasytrieveFileTypeDetection.class),
		C(CFileTypeDetection.class),
		CPP(CppFileTypeDetection.class),
		BINARY(BinaryFileTypeDetection.class),
		IMS_PATTERN(ImsFileTypeDetectionByPattern.class),
		DCL(DclFileTypeDetection.class),
		FMS(FmsFileTypeDetection.class),
		ORACLE(OracleFileTypeDetection.class),
		BASIC(BasicFileTypeDetection.class),
		SQL_SCRIPT(SQLScriptCategorizer.class),
		JAVA(JavaFileTypeDetection.class),
		ECL(EclFileTypeDetection.class),
		VB(VbFileTypeDetection.class),
		CSHARP(CSharpFileTypeDetection.class);

		public final Class<? extends FileTypeDetection> detectionClass;
		public final ResolveTarget language;

		private DetectionType(final Class<? extends FileTypeDetection> detectionClass) {
			this.detectionClass = detectionClass;
			this.language = getDetectionLanguage(detectionClass);
		}
	}

}

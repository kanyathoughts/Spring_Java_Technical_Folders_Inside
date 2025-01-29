/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.categorize;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.annotation.PostConstruct;

import innowake.mining.server.discovery.categorize.csharp.CSharpFileTypeDetection;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.OperationCanceledException;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.task.Task;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.discovery.metrics.TimedWorkerImpl;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.PersistingSourceObjectResolver;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.server.discovery.cache.DiscoveryJobCache;
import innowake.mining.server.discovery.categorize.FileTypeDetection.DetectionPhase;
import innowake.mining.server.discovery.categorize.assembler.AssemblerFileTypeDetection;
import innowake.mining.server.discovery.categorize.assembler.VAXMacroFileTypeDetection;
import innowake.mining.server.discovery.categorize.basic.BasicFileTypeDetection;
import innowake.mining.server.discovery.categorize.binary.BinaryFileTypeDetection;
import innowake.mining.server.discovery.categorize.c.CFileTypeDetection;
import innowake.mining.server.discovery.categorize.cobol.CobolFileTypeDetection;
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
import innowake.mining.server.discovery.config.DiscoveryConfigAccessor;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.server.discovery.parser.assembler.AssemblerHelper;
import innowake.mining.server.discovery.parser.batch.JclParseResultProvider;
import innowake.mining.server.service.ParseResultCacheService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.discovery.categorize.Identification.ID;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.discovery.ResolveTarget;

/**
 * {@link Task} implementation for discover metrics. This will be forked by {@link Categorizer}.
 */
public class CategorizerTask extends Task<Identification[]> {
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.CATEGORIZE_CATEGORIZER);
	
	@Autowired
	private transient SourceService sourceService;
	
	@Autowired
	private transient DiscoveryJobCache discoveryCache;
	
	@Autowired
	private transient DiscoveryConfigAccessor configAccessor;
	
	private final Parameters parameters;
	
	@Nullable
	private transient Config config;
	
	@Autowired
	private transient ParseResultCacheService parseResultCacheService;
	
	@Autowired
	private transient ParserProviderService parserProviderService;

	/**
	 * Constructor.
	 * 
	 * @param progressMonitor the {@link ProgressMonitor} for the task
	 * @param jobId the Id of the {@link Job} that forked this task
	 * @param parameters the {@link Parameters} for this task
	 */
	public CategorizerTask(final ProgressMonitor progressMonitor, final String jobId, final Parameters parameters) {
		super(progressMonitor, jobId);
		this.parameters = parameters;
	}
	
	@PostConstruct
	private void postContruct() {
		config = configAccessor.getConfig(parameters.projectId, getJobId());
	}

	@Override
	protected Result<Identification[]> run(final ProgressMonitor progressMonitor) {
		progressMonitor.checkCanceled();
		
		final TimedWorker timedWorker = getTimedWorker(parameters.detectionLanguages, progressMonitor);
		try {
			final Set<FileTypeDetection> detections = getDetectionInstances(timedWorker);
			final List<Identification> identifications = new ArrayList<>(detections.size());
			
			final SourcePojo sourceObject = sourceService.get(q -> q.byId(EntityId.of(parameters.sourceObjectId)));
			
			boolean stopDetection = false;
			for (final FileTypeDetection detection : detections) {
				progressMonitor.checkCanceled();
				if (stopDetection) {
					break;
				}

				try {
					switch (parameters.mode) {
						case MAIN:
							identifyByMainType(identifications, detection, sourceObject);
							break;
						case MAIN_MULTI_PHASE:
							identifyByMainTypeMultiPhase(identifications, detection, sourceObject);
							break;
						case TOKEN:
							stopDetection = identifyByTokens(identifications, detection, sourceObject);
							break;
						case EXTENSION:
							stopDetection = identifyByExtension(identifications, detection, sourceObject);
							break;
						case DEPENDENCY:
							stopDetection = identifyDependencies(detection, sourceObject);
							break;
						default:
							throw new IllegalArgumentException("Unsupported categorization mode: " + parameters.mode.name());
					}
				} catch (final OperationCanceledException e) {
					throw e;
				} catch (final Exception e) {
					handleError(e, sourceObject);
				}
			}

			return new Result<>(identifications.toArray(new Identification[] {}));
		} finally {
			if (timedWorker != null) {
				timedWorker.shutdown();
			}
		}
	}

	/**
	 * Writes discovery code errors to log file and job view.
	 *
	 * @param exception the {@linkplain Throwable exception}}
	 * @param sourceObject the source object
	 */
	protected void handleError(final Throwable exception, final SourcePojo sourceObject) {
		LOG.error(() -> "Error while identifying " + sourceObject.getPath(), exception);
	}

	private void identifyByMainType(final List<Identification> identifications, final FileTypeDetection detection, final SourcePojo sourceObject) {
		try {
			final Identification identification = detection.identifyMainObject(sourceObject);
			if (identification != null) {
				identifications.add(identification);
			}
		} catch (final Exception e) {
			LOG.error(() -> String.format("Error occurred while identifying main object type %s with %s", sourceObject.getName(), detection.getClass().getName()), e);
		}
	}
	
	private void identifyByMainTypeMultiPhase(final List<Identification> identifications, final FileTypeDetection detection, final SourcePojo sourceObject) {
		try {
			final Identification identification = detection.identifyMainObjectMultiPhase(sourceObject, parameters.phase, getProgressMonitor());
			if (identification != null) {
				identifications.add(identification);
			}
		} catch (final Exception e) {
			LOG.error(() -> String.format("Error occurred while identifying main object type %s with %s", sourceObject.getName(), detection.getClass().getName()), e);
		}
	}
	
	private boolean identifyByTokens(final List<Identification> identifications, final FileTypeDetection detection, final SourcePojo sourceObject) {
		try {
			final Identification identification = detection.identifyByContent(sourceObject);
			if (identification != null) {
				identifications.add(identification);
				
				/* Token identification stops as soon as the first YES is encountered */
				if (identification.getId() == ID.YES) {
					return true;
				}
			}
		} catch (final Exception e) {
			LOG.error(() -> String.format("Error occurred while identifying %s by token with %s.", sourceObject.getName(), detection.getClass().getName()), e);
		}
		return false;
	}
	
	private boolean identifyByExtension(final List<Identification> identifications, final FileTypeDetection detection, final SourcePojo sourceObject) {
		try {
			final Identification identification = detection.identifyByExtension(sourceObject);
			if (identification != null) {
				identifications.add(identification);
				
				/* Extension identification stops as soon as the first YES is encountered */
				if (identification.getId() == ID.YES) {
					return true;
				}
			}
		} catch (final Exception e) {
			LOG.error(() -> String.format("Error occurred while identifying %s by extension with %s", sourceObject.getName(), detection.getClass().getName()), e);
		}
		return false;
	}
	
	private boolean identifyDependencies(final FileTypeDetection detection, final SourcePojo sourceObject) {
		try {
			if (isEmpty(sourceObject)) {
				LOG.warn(() -> "Found empty file during dependency resolving: " + sourceObject.getName());
			}
			return detection.identifyDependencies(sourceObject);
		} catch (final Exception e) {
			LOG.error(() -> String.format("Error occurred while identifying dependencies of %s with %s", sourceObject.getName(), detection.getClass().getName()), e);
		}
		return false;
	}
	
	@Nullable
	private TimedWorker getTimedWorker(final Set<ResolveTarget> detections, final ProgressMonitor progressMonitor) {
		final boolean requiresWorker = detections.stream().anyMatch(contributor -> {
			switch (contributor) {
				case JCL:
				case PL1:
				case EASYTRIEVE:
				case C:
					return true;
				default:
					return false;
			}
		});
		return requiresWorker ? new TimedWorkerImpl(progressMonitor, tracer, taskSpan) : null;
	}
	
	private Set<FileTypeDetection> getDetectionInstances(@Nullable final TimedWorker timedWorker) {
		final Set<FileTypeDetection> detectionInstances = new HashSet<>();
		
		final Config configSafe = assertNotNull(config);
		parameters.detectionLanguages.stream().forEach(detection -> {
			switch (detection) {
				case COBOL:
					detectionInstances.add(new CobolFileTypeDetection(configSafe, discoveryCache, getJobId()));
					break;
				case JCL:
					final JclParseResultProvider parser = parserProviderService
						.createJclParser(configSafe, assertNotNull(timedWorker), getJobId(), parameters.projectId);
					detectionInstances.add(new JclFileTypeDetection(configSafe, parser, discoveryCache, getJobId()));
					break;
				case NATURAL:
					detectionInstances.add(new NaturalFileTypeDetection());
					break;
				case CSD:
					detectionInstances.add(new CSDFileTypeDetection());
					break;
				case ASSEMBLER:
					final SourceObjectResolver sourceObjectResolver = new PersistingSourceObjectResolver(sourceService, assertNotNull(parameters.searchOrders));
					detectionInstances.add(new AssemblerFileTypeDetection(configSafe, new AssemblerHelper(sourceObjectResolver)));
					break;
				case VMS:
					detectionInstances.add(new VAXMacroFileTypeDetection());
					detectionInstances.add(new DclFileTypeDetection(configSafe));
					detectionInstances.add(new FmsFileTypeDetection(configSafe));
					break;
				case XML:
					detectionInstances.add(new XmlFileTypeDetection());
					break;
				case PL1:
					detectionInstances.add(
							new Pl1FileTypeDetection(
									configSafe, 
									assertNotNull(timedWorker), 
									discoveryCache, 
									getJobId(), 
									parseResultCacheService, 
									parameters.featureMap, 
									new PersistingSourceObjectResolver(sourceService, assertNotNull(parameters.searchOrders))
									)
							);
					break;
				case EASYTRIEVE:
					detectionInstances.add(new EasytrieveFileTypeDetection(configSafe, assertNotNull(timedWorker), sourceService, discoveryCache,
							getJobId()));
					break;
				case C:
					detectionInstances.add(new CFileTypeDetection(configSafe, assertNotNull(timedWorker), discoveryCache, getJobId()));
					break;
				case CPP:
					detectionInstances.add(new CppFileTypeDetection());
					break;
				case BINARY:
					detectionInstances.add(new BinaryFileTypeDetection(configSafe));
					break;
				case IMS:
					detectionInstances.add(new ImsFileTypeDetectionByPattern());
					break;
				case ORACLE:
					detectionInstances.add(new OracleFileTypeDetection(configSafe));
					break;
				case BASIC:
					detectionInstances.add(new BasicFileTypeDetection());
					break;
				case SQL:
					detectionInstances.add(new SQLScriptCategorizer());
					break;
				case JAVA:
					detectionInstances.add(new JavaFileTypeDetection());
					break;
				case ECL:
					detectionInstances.add(new EclFileTypeDetection(configSafe));
					break;
				case VB:
					detectionInstances.add(new VbFileTypeDetection());
					break;
				case CSHARP:
					detectionInstances.add(new CSharpFileTypeDetection());
					break;
				default:
					break;
			}
		});
		return detectionInstances;
	}
	
	private boolean isEmpty(final SourcePojo sourceObject) {
		return StringUtils.isEmpty(sourceObject.getContent().toString());
	}
	
	/**
	 * The parameters for the {@link CategorizerTask}.
	 */
	public static class Parameters implements Serializable {
		
		/**
		 * Supported modes for the source object categorization.
		 */
		public enum CategorizingMode {
			/** Identifies any main objects like job, Cobol/Natural/C/ASM/... program. */
			MAIN,
			MAIN_MULTI_PHASE,
			/** Identifies by specific occurrences in the content. */
			TOKEN,
			/** Identifies by file extension. */
			EXTENSION,
			/** Identifies the dependencies found in a specific object like JCL procedure or Cobol copybook. */
			DEPENDENCY
		}
		
		private final EntityId projectId;
		private final Set<ResolveTarget> detectionLanguages;
		private final Long sourceObjectId;
		private final CategorizingMode mode;
		private final DetectionPhase phase;
		private final SearchOrders searchOrders;
		private final Map<FeatureId, Boolean> featureMap;
		
		/**
		 * Constructor.
		 * 
		 * @param projectId the project id
		 * @param detectionLanguages the {@link ResolveTarget} with the language to detect
		 * @param sourceObjectId the Id of the source object to process
		 * @param mode the {@link CategorizingMode} to run in
		 * @param phase the {@link DetectionPhase} as specific modes support multiple phases
		 * @param searchOrders the project searchOrders
	 * @param featureMap the map containing feature settings.
		 */
		public Parameters(final EntityId projectId, final Set<ResolveTarget> detectionLanguages, final Long sourceObjectId, final CategorizingMode mode,
				final DetectionPhase phase, final SearchOrders searchOrders, final Map<FeatureId, Boolean> featureMap) {
			this.projectId = projectId;
			this.detectionLanguages = detectionLanguages;
			this.sourceObjectId = sourceObjectId;
			this.mode = mode;
			this.phase = phase;
			this.searchOrders = searchOrders;
			this.featureMap = featureMap;
		}
	}
	
}

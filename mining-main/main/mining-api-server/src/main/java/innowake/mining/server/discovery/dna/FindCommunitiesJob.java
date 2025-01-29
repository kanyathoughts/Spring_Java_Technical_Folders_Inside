/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.dna;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.Serializable;
import java.time.Instant;
import java.util.Collections;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.ff4j.FF4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;

import brave.Span;
import brave.Tracer;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Severity;
import innowake.lib.job.api.Status;
import innowake.lib.job.api.management.JobMonitor;
import innowake.lib.job.api.task.DefaultTaskProcessor;
import innowake.lib.job.api.task.ResultConsumer;
import innowake.lib.job.api.task.TaskSource;
import innowake.mining.data.discovery.metrics.DiscoveryCache;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.discovery.metrics.TimedWorkerImpl;
import innowake.mining.data.model.discovery.dna.DnaConfig;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.PersistingSourceObjectResolver;
import innowake.mining.server.discovery.dna.community.CommunityDetector;
import innowake.mining.server.discovery.dna.sequencer.Sequencer;
import innowake.mining.server.discovery.dna.sequencer.SequencerProvider;
import innowake.mining.server.discovery.dna.sequencer.jcl.JclSequencer;
import innowake.mining.server.discovery.dna.similarity.SimilarityProcessor;
import innowake.mining.server.discovery.metrics.TaskHandler;
import innowake.mining.server.discovery.monitor.ProgressMonitorText;
import innowake.mining.server.event.ModelDnaModifiedEvent;
import innowake.mining.server.job.MiningJob;
import innowake.mining.server.service.ParseResultCacheService;
import innowake.mining.server.util.ProgressMonitorThrottle;
import innowake.mining.shared.FutureUtil;
import innowake.mining.shared.access.DnaDataService;
import innowake.mining.shared.access.DnaDataService.DnaStringInquiryBuilder;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.configuration.GenericConfiguration;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.dna.DnaSequencer;
import innowake.mining.shared.entities.dna.DnaSimilarityAlgorithm;
import innowake.mining.shared.entities.dna.DnaSimilarityPojo;
import innowake.mining.shared.entities.dna.DnaSnapshotPojo;
import innowake.mining.shared.entities.dna.DnaSnapshotPojoPrototype;
import innowake.mining.shared.entities.dna.DnaStringElementPojoPrototype;
import innowake.mining.shared.entities.dna.DnaStringPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.job.Message;

/**
 * Expert script to find communities based on dna extract.
 * <br>This script does the following tasks
 * <ol>
 * <li> For all suitable modules produce a dna string with all available rules. Every rule create its own dna string.
 * <li> For every dna string find the similarity to every other dna string of the same rule
 * <li> For every rule build a graph where the edges are connected with the attributes of similarity. Find communities based on this graph.
 * </ol>
 *
 * <p>These intermediate steps are used to provide insights how the communities are created.
 * The final communities are stored in the DB, to visualize the results.
 *
 * <br>After execution the latest DNA cache gets evicted.
 *
 * <p>Some of the work is done in parallel tasks.
 * All tasks respect the monitor and are able to cancel (Maybe require some work to be done until the task noticed the cancel request).
 *
 */
public class FindCommunitiesJob extends MiningJob<Serializable> implements TaskHandler {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.DNA);

	/**
	 * List of technology-type pairs that are supported by DNA analysis.
	 */
	private static final Set<Tuple2<Technology, Type>> SUPPORTED_TYPES = Set.of(
					Tuple2.of(Technology.COBOL, Type.PROGRAM),
					Tuple2.of(Technology.PL1, Type.PROGRAM),
					Tuple2.of(Technology.PL1, Type.MAINPROGRAM),
					Tuple2.of(Technology.NATURAL, Type.PROGRAM),
					Tuple2.of(Technology.NATURAL, Type.SUBPROGRAM),
					Tuple2.of(Technology.NATURAL, Type.FUNCTION),
					Tuple2.of(Technology.NATURAL, Type.SUBROUTINE),
					Tuple2.of(Technology.C, Type.PROGRAM),
					Tuple2.of(Technology.C, Type.HEADER),
					Tuple2.of(Technology.JCL, Type.JOB));

	private final Instant discoveryDnaTimestamp;
	@Autowired
	private transient SourceCachingService sourceService;
	@Autowired
	private transient ProjectService projectService;
	@Autowired
	private transient Tracer tracer;
	@Autowired
	private transient ParseResultCacheService parseResultCacheService;
	@Autowired
	private transient DiscoveryCache discoveryCache;

	@Autowired
	private transient FF4j ff4j;

	@Autowired
	private transient DnaDataService dnaService;

	@Autowired
	private transient ModuleService moduleService;

	@Autowired
	private transient ApplicationEventPublisher eventPublisher;

	private Integer minDnaLength = Integer.valueOf(20);

	@Nullable
	private UUID dnaSnapshotId = null;

	@Nullable
	private transient ProjectPojo project;

	@Autowired
	private transient GenericConfiguration configProperties;
	
	@Nullable
	private transient DnaConfig dnaConfig;
	@Autowired
	private transient JclSequencer jclSequencer;

	/**
	 * Public constructor.
	 *
	 * @param projectId the id of the project to execute Discover DNA
	 */
	public FindCommunitiesJob(final EntityId projectId) {
		super(projectId);
		this.discoveryDnaTimestamp = Instant.now();
	}

	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		/* log a message to ensure a log file that can be stored by log framework */
		LOG.info(() -> "Starting Find Communities for project ID " + projectId);
		progressMonitor.setJobDescription(ProgressMonitorText.DISCOVERY_DNA_TASK_FIND_COMMUNITIES);
		project = projectService.get(projectId);
		dnaConfig = DnaConfig.loadAndSetDnaConfigs(cfg -> projectService.getXmlConfig(projectId, cfg));
		final TimedWorker timedWorker = new TimedWorkerImpl(progressMonitor, tracer, tracer.currentSpan());
		final int totalCountOfSourceObjects = (int) moduleService.countModules(b -> b.ofProject(projectId).withTechnologiesAndTypes(SUPPORTED_TYPES));
		final Instant metricsDateOfProject = Objects.requireNonNull(project).getMetricsDate();
		final Optional<Instant> latestSnapshotUpdatedOn = dnaService.latestSnapshot(projectId).map(DnaSnapshotPojo::getUpdated);
		final Optional<DnaSnapshotPojo> dnaSnapshotWithSameConfig;
		final DiscoveryDnaRunType discoveryDnaRunType;
		if (configProperties.isDiscoveryDnaOptimalPersist()) {
			discoveryDnaRunType = DiscoveryDnaRunType.OPTIMAL_PERSIST_ENABLED;
			dnaSnapshotWithSameConfig = Optional.empty();
		} else if (latestSnapshotUpdatedOn.isEmpty()) {
			discoveryDnaRunType = DiscoveryDnaRunType.NEW;
			dnaSnapshotWithSameConfig = Optional.empty();
		} else {
			if (metricsDateOfProject != null && metricsDateOfProject.isAfter(latestSnapshotUpdatedOn.get())) {
				discoveryDnaRunType = DiscoveryDnaRunType.RERUN_WITH_METRICS_CHANGE;
				dnaSnapshotWithSameConfig = Optional.empty();
			} else {
				dnaSnapshotWithSameConfig = checkForConfigChange(metricsDateOfProject);
				if (dnaSnapshotWithSameConfig.isPresent()) {
					discoveryDnaRunType = DiscoveryDnaRunType.RERUN_WITH_NO_CHANGE;
				} else {
					discoveryDnaRunType = DiscoveryDnaRunType.RERUN_WITH_CONFIG_CHANGE;
				}
			}
		}

		if ((discoveryDnaRunType == DiscoveryDnaRunType.NEW || discoveryDnaRunType == DiscoveryDnaRunType.OPTIMAL_PERSIST_ENABLED) 
				&& totalCountOfSourceObjects == 0) {
			LOG.warn("No supported modules in project available. Please check whether the Discover Metrics on Project performed or not.");
			return new Result<>(Status.OK);
		}

		if ( ! DiscoveryDnaRunType.RERUN_WITH_NO_CHANGE.equals(discoveryDnaRunType)) {
			/*
			 * If the current run is rerun with no metrics or no configuration change then we will just update the updatedOn date in existing DnaSnapshot,
			 * other wise we will create a new DnaSnapshot.
			 */
			dnaSnapshotId = createOrUpdateDnaSnapshot(Optional.empty(), totalCountOfSourceObjects);
		}

		try {
			performDiscoverDna(progressMonitor, latestSnapshotUpdatedOn, totalCountOfSourceObjects, dnaSnapshotWithSameConfig.map(DnaSnapshotPojo::getId), discoveryDnaRunType);
		} catch (final DiscoveryException e) {
			LOG.error("Failed to finish Find Communities job with exception", e);
			/*
			 * If there is any error occurred during the processing of Discover DNA, we need to delete all the DNA related records for that particular run.
			 */
			deleteAllDnaEntitiesForCurrentRun();
			return new Result<>(new Status(Severity.ERROR));
		} finally {
			timedWorker.shutdown();
			progressMonitor.setJobDescription(ProgressMonitorText.DISCOVERY_DNA_TASK_FIND_COMMUNITIES);
			progressMonitor.setStepDescription("");
		}
		eventPublisher.publishEvent(new ModelDnaModifiedEvent(projectId));
		return new Result<>(new Status(Severity.OK));
	}

	@Override
	public String getJobName() {
		return "Discover DNA";
	}

	private void performDiscoverDna(final ProgressMonitor progressMonitor, final Optional<Instant> latestSnapshotUpdatedOn,
			final int totalCountOfSourceObjects, final Optional<UUID> snapshotWithSameConfigs, final DiscoveryDnaRunType discoveryDnaRunType) throws DiscoveryException {
		progressMonitor.setJobDescription(ProgressMonitorText.DISCOVERY_DNA_TASK_FIND_COMMUNITIES + ": " + ProgressMonitorText.DISCOVERY_DNA_TASK_INITIAL);
		progressMonitor.setStepDescription(ProgressMonitorText.DISCOVERY_SUBTASK_INITIAL);
		switch (discoveryDnaRunType) {
			case NEW:
				LOG.info("Performing discovery DNA for the first time");
				/*$FALL-THROUGH$*/
			case OPTIMAL_PERSIST_ENABLED:
				collectDiscoverDna(progressMonitor, Optional.empty(), discoveryDnaRunType);
				break;
			case RERUN_WITH_METRICS_CHANGE:
				LOG.info("Performing discovery DNA rerun with metrics change");
				if (latestSnapshotUpdatedOn.isEmpty()) {
					throw new IllegalArgumentException("Time of last update required.");
				}
				collectDiscoverDna(progressMonitor, latestSnapshotUpdatedOn, discoveryDnaRunType);
				break;
			case RERUN_WITH_CONFIG_CHANGE:
				LOG.info("Performing discovery DNA rerun with configuration change");
				/*
				 * If the current run is rerun with configuration change, then we don't do anything with DnaString and DnaSimilarities,
				 * we will just create DnaCommunities with latest configurations.
				 */
				findCommunities(progressMonitor, totalCountOfSourceObjects);
				break;
			case RERUN_WITH_NO_CHANGE:
				LOG.info("Performing discovery DNA rerun with no configuration and no metrics change");
				/*
				 * If the current run is rerun with no metrics or configuration change, we do nothing with the DnaString, DnaSimilarity, DnaCommunity.
				 *  we will just update the updatedOn date in existing DnaSnapshot.
				 */
				if (snapshotWithSameConfigs.isPresent()) {
					createOrUpdateDnaSnapshot(snapshotWithSameConfigs, totalCountOfSourceObjects);
				}
				break;
			default:
				break;
		}
	}

	private void collectDiscoverDna(final ProgressMonitor progressMonitor, final Optional<Instant> latestDnaSnapshotUpdatedOn, 
			final DiscoveryDnaRunType discoveryDnaRunType) throws DiscoveryException {
		progressMonitor.setStepDescription("Collecting source objects");	/* collecting sourceObjects */
		final Map<String, UUID> modulePathAndUids = getModulesForDna(latestDnaSnapshotUpdatedOn);
		final List<SourcePojo> sourceObjects = modulePathAndUids.keySet().stream()
				.map(path -> sourceService.cachingByProjectPath(projectId.getNid(), path)).collect(Collectors.toList());
		progressMonitor.setStepDescription(String.format("Collected %d source objects", Integer.valueOf(sourceObjects.size())));
		progressMonitor.setJobDescription(ProgressMonitorText.DISCOVERY_DNA_TASK_FIND_COMMUNITIES + ": (1/4) " + ProgressMonitorText.DISCOVERY_DNA_TASK_COLLECT);
		final int workUnits = sourceObjects.size() * 3;
		progressMonitor.begin(workUnits);
		/* Processing DnaStrings and DnaSimilarities */
		processDnaStringAndDnaSimilarities(progressMonitor, sourceObjects, modulePathAndUids, discoveryDnaRunType);

		progressMonitor.setJobDescription(
				ProgressMonitorText.DISCOVERY_DNA_TASK_FIND_COMMUNITIES + ": (3/4) " + ProgressMonitorText.DISCOVERY_DNA_TASK_FIND_COMMUNITIES);

		/* Processing the DnaCommunities */
		findCommunities(progressMonitor.subMonitor(workUnits / 3), sourceObjects.size());
	}

	private Map<String, UUID> getModulesForDna(final Optional<Instant> latestDnaSnapshotUpdatedOn) {
		return moduleService.findModulesLightweight(q -> {
														q.ofProject(projectId).withTechnologiesAndTypes(SUPPORTED_TYPES)
														 .withRepresentation(Representation.PHYSICAL);
														if (latestDnaSnapshotUpdatedOn.isPresent()) {
															q.withMetricsDateAfter(latestDnaSnapshotUpdatedOn.get());
														}
													}).stream()
				.collect(Collectors.toMap(ModuleLightweightPojo::getPath, ModuleLightweightPojo::getUid));
	}

	private void processDnaStringAndDnaSimilarities(final ProgressMonitor progressMonitor, final List<SourcePojo> sourceObjects,
			final Map<String, UUID> modulePathAndUids, final DiscoveryDnaRunType discoveryDnaRunType) throws DiscoveryException {
		final int sourceObjectsCount = sourceObjects.size();
		collectDnaStringsAndPersist(sourceObjects, progressMonitor.subMonitor(sourceObjectsCount), modulePathAndUids);
		progressMonitor.setJobDescription(ProgressMonitorText.DISCOVERY_DNA_TASK_FIND_COMMUNITIES + ": (2/4) " + ProgressMonitorText.DISCOVERY_DNA_TASK_CALCULATE_DISTANCES);
		/*
		 * this will get the total DnaStrings count and DnaStrings count by minimum DNA length from DNA_Sequencer_Config.xml file.
		 * if the above calculated DnaStrings count values are not met with minimum values that we will use to analyze DNA,
		 * it will advise the user to adjust the DNA configuration values especially minimum DNA length from DNA_Sequencer_Config.xml file.
		 * If there is no valid number of DnaStrings found for any of the sequencerIds then, we will delete all the DnaStrings, DnaStringElements, DnaSnapshots
		 * for the current run.
		 */
		if ( ! giveAlertToAdjustDnaConfigs()) {
			throw new DiscoveryException("No valid DnaStrings found for processing discovery DNA.");
		}
		calculateDnaSimilaritiesAndPersist(progressMonitor.subMonitor(sourceObjectsCount), discoveryDnaRunType);
	}

	private void collectDnaStringsAndPersist(final List<SourcePojo> sourceObjects, final ProgressMonitor monitor,
			final Map<String, UUID> modulePathAndUids) throws DiscoveryException {
		final Map<FeatureId, Boolean> featureMap = new EnumMap<>(FeatureId.class);
		Stream.of(FeatureId.values()).forEach(feature -> featureMap.put(feature, Boolean.valueOf(ff4j.getFeature(feature.getId()).isEnable())));
		final TimedWorker timedWorker = new TimedWorkerImpl(monitor, tracer, tracer.currentSpan());
		monitor.begin(sourceObjects.size());
		final var searchOrder = new SearchOrders(assertNotNull(project, "Project must not be null").getSearchOrders());
		final var sourceObjectResolver = new PersistingSourceObjectResolver(sourceService, searchOrder);
		final var discoveryConfig = Config.loadConfig(projectService, projectId);

		final var sequencerProvider = new SequencerProvider(jobId, cfg -> projectService.getXmlConfig(projectId, cfg), timedWorker, sourceObjectResolver,
				parseResultCacheService, sourceService, searchOrder, discoveryCache, featureMap, discoveryConfig, jclSequencer);

		LOG.info("Collecting DnaStrings");
		final MonitorExecutorService pool = new MonitorExecutorService();
		try {
			final var counter = new AtomicInteger(1);
			final var size = Integer.valueOf(sourceObjects.size());

			/* Delete existing DnaStrings to avoid duplicate key value violates unique constraint errors */
			final int cnt = dnaService.deleteDnaStrings(modulePathAndUids.values());
			LOG.debug(() -> String.format("Deleted %d DnaStrings", Integer.valueOf(cnt)));

			final List<Future<?>> futures = sourceObjects.stream().map(sourceObject -> pool.submit(() -> {
				final var moduleUid = modulePathAndUids.get(sourceObject.getPath());
				if (moduleUid != null) {
					/* Create module if required */
					collectDnaStrings(sourceObject, sequencerProvider).forEach(dnaTuple -> {
						dnaTuple.a.setModule(EntityId.of(moduleUid));
						dnaTuple.a.setGenerated(discoveryDnaTimestamp);
						dnaService.createDnaString(dnaTuple.a, dnaTuple.b);
					});
					LOG.info("DnaStrings collected and persisted for source object: {}", sourceObject.getPath());
				}

				ProgressMonitorThrottle.throttleStepDescription(String.format("Collecting DNA on source objects (%d/%d)",
						Integer.valueOf(counter.getAndIncrement()), size), monitor);
				monitor.worked(1);
			})).collect(Collectors.toList());
			FutureUtil.awaitAll(futures);
			LOG.info("Collection of DnaStrings completed");
		} catch (final Exception e) {
			Thread.currentThread().interrupt();
			throw new DiscoveryException(e.getMessage(), e);
		} finally {
			pool.shutdown();
			timedWorker.shutdown();
		}
	}

	private List<Tuple2<DnaStringPojoPrototype, List<DnaStringElementPojoPrototype>>> collectDnaStrings(final SourcePojo sourceObject, final SequencerProvider sequencerProvider) {
		final Sequencer sequencer = sequencerProvider.provideSequencer(sourceObject.getTechnology(), sourceObject.getType());
		try {
			return sequencer.apply(sourceObject);
		} catch (final Exception e) {
			LOG.error(() -> "Error while applying sequencer to " + sourceObject.getName() + " in path " + sourceObject.getPath(), e);
			return Collections.emptyList();
		}
	}

	private void calculateDnaSimilaritiesAndPersist(final ProgressMonitor monitor, final DiscoveryDnaRunType discoveryDnaRunType) throws DiscoveryException {
		LOG.info("Collecting DnaSimilarities");
		try {
			/*
			 * Here we are loading all the available DnaStrings from DB at once to process  DNA similarities which will be heavy on large projects.
			 * We will handle this in future tasks, we are keeping the current approach for now.
			 */
			final Optional<Instant> generatedOn = DiscoveryDnaRunType.RERUN_WITH_METRICS_CHANGE.equals(discoveryDnaRunType)
					? Optional.of(discoveryDnaTimestamp)
					: Optional.empty();

			monitor.begin(DnaSequencer.values().length);
			for (final DnaSequencer sequencerId : DnaSequencer.values()) {
				final BuildingConsumer<DnaStringInquiryBuilder> queryBuilder = builder -> {
					builder.bySequencer(sequencerId)
							.ofProject(projectId);
					generatedOn.ifPresent(builder::withGenerated);
				};

				final List<UUID> dnaStringUuids = dnaService.findAllDnaStringUuids(queryBuilder);
				if ( ! dnaStringUuids.isEmpty()) {
					final SimilarityProcessor proc = new SimilarityProcessor(configProperties, this, monitor.subMonitor(1), 
							assertNotNull(dnaConfig, "DnaConfig must not be null in calculateDnaSimilaritiesAndPersist()"));
					proc.process(sequencerId, dnaStringUuids);
					LOG.info("DnaSimilarities processed for sequencer: {}", sequencerId);
				} else {
					monitor.worked(1);
					LOG.info("No DnaSimilarities found for sequencer: {}", sequencerId);
				}
			}
			LOG.info("Collection of DnaSimilarities completed");
		} catch (final Exception e) {
			throw new DiscoveryException(e.getMessage(), e);
		}
	}

	private void findCommunities(final ProgressMonitor monitor, final int sourceObjectCount) throws DiscoveryException {
		LOG.info("Collecting DnaCommunities");
		final var dnaSnapshotIdNonNull = assertNotNull(this.dnaSnapshotId, "Snapshot id must not be null");
		final var dnaSnapshot = dnaService.getSnapshot(dnaSnapshotIdNonNull);
		final var snapshotDnaConfig = DnaConfig.fromMap(dnaSnapshot.getDnaConfig());
		try {
			monitor.begin(DnaSimilarityAlgorithm.values().length * DnaSequencer.values().length);
			for (final DnaSimilarityAlgorithm similarityId : DnaSimilarityAlgorithm.values()) {
				for (final DnaSequencer sequencerId : DnaSequencer.values()) {
					final var similarityThreshold = snapshotDnaConfig.getSimilarityThreshold();
					final var minimumDnaLength = snapshotDnaConfig.getMinDnaLength();
					LOG.info("Executing DNA Similarities Query - Project: {}, Algorithm: {}, Sequencer: {}, Threshold: {}, Min Dna Length: {}",
							projectId, similarityId, sequencerId, similarityThreshold, minimumDnaLength);

					final List<DnaSimilarityPojo> dnaSimilarities = dnaService
							.findDnaSimilarities(builder -> builder.ofProject(projectId)
								.withAlgorithm(similarityId)
								.bySequencer(sequencerId)
								.withThreshold(similarityThreshold)
								.withMinimumAmount(minimumDnaLength));
					LOG.info("Number of DNA Similarities found for {} and {}: {}", similarityId, sequencerId, dnaSimilarities.size());
					if ( ! dnaSimilarities.isEmpty()) {
						final ProgressMonitor subMonitor = monitor.subMonitor(1);
						subMonitor.begin(sourceObjectCount);
						new CommunityDetector(cfg -> projectService.getXmlConfig(projectId, cfg), projectService.getNid(projectId),
								dnaService, subMonitor).detect(dnaSimilarities, dnaSnapshotIdNonNull);
					} else {
						LOG.info("No Dna Similarities found for {}", sequencerId);
						monitor.worked(1);
					}
				}
			}
			LOG.info("Collection of DnaCommunities completed");
		} catch (final Exception e) {
			throw new DiscoveryException(e.getMessage(), e);
		}
	}

	private UUID createOrUpdateDnaSnapshot(Optional<UUID> snapshotId, final int totalModuleCount) {
		if (snapshotId.isPresent()) {
			dnaService.updateSnapshot(new DnaSnapshotPojoPrototype()
												.setId(snapshotId.get())
												.setTotalModuleCount(Integer.valueOf(totalModuleCount))
												.setUpdated(discoveryDnaTimestamp));
			return snapshotId.get();
		}
		minDnaLength = Integer.valueOf(assertNotNull(dnaConfig, "DnaConfig must not be null in createOrUpdateDnaSnapshot(1)").getMinDnaLength());
		return dnaService.createSnapshot(new DnaSnapshotPojoPrototype()
							.setProjectId(projectId)
							.setTotalModuleCount(Integer.valueOf(totalModuleCount))
							.setDnaConfig(assertNotNull(dnaConfig, "DnaConfig must not be null createOrUpdateDnaSnapshot(2)").toMap()));
	}

	private boolean giveAlertToAdjustDnaConfigs() {
		boolean result = false;
		final List<DnaSequencer> sequencerIds = dnaService.getSequencerIdsFromDnaString(projectId);
		for (final DnaSequencer sequencerId : sequencerIds) {
			final long totalDnaStringCount = dnaService.getDnaStringCount(b -> b.bySequencer(sequencerId).ofProject(projectId));

			if (totalDnaStringCount == 1) {
				writeMessage(Message.Severity.INFO,
						String.format("Unable to perform DNA analysis using %s: at least 2 Modules for the language are required", sequencerId));
			} else if (totalDnaStringCount > 1 && 
						dnaService.getDnaStringCount(b -> b.bySequencer(sequencerId).ofProject(projectId).withMinimumAmount(minDnaLength)) <= 1) {
				writeMessage(Message.Severity.INFO, String.format("Unable to perform DNA analysis using %s: not enough DNA strings were collected. "
						+ "Try adjusting the \"minDNALength\" setting in DNA_Sequencer_Config.xml", sequencerId));
			} else {
				result = true;
			}
		}
		return result;
	}

	private Optional<DnaSnapshotPojo> checkForConfigChange(@Nullable final Instant metricsDateOfProject) {
		return dnaService.findSnapshots(q -> {
				q.ofProject(projectId);
				if (metricsDateOfProject != null) {
					q.withUpdateAfter(metricsDateOfProject);
				}
				q.sortUpdated(SortDirection.DESCENDING);
			}).stream().filter(snapshot -> snapshot.getDnaConfig().equals(assertNotNull(dnaConfig, "DnaConfig must not be null in checkForConfigChange()").toMap())).findAny();
	}

	private void deleteAllDnaEntitiesForCurrentRun() {
		LOG.info("Deleting DNA Entities");
		dnaService.deleteDnaStrings(projectId, discoveryDnaTimestamp);

		if (dnaSnapshotId != null) {
			dnaService.deleteSnapshot(dnaSnapshotId);
		}

		LOG.info("Deletion of DNA Entities for the current run completed.");
	}

	/**
	 * Different types of Discover DNA runs.
	 */
	private enum DiscoveryDnaRunType {
		NEW, RERUN_WITH_METRICS_CHANGE, RERUN_WITH_CONFIG_CHANGE, RERUN_WITH_NO_CHANGE, OPTIMAL_PERSIST_ENABLED
	}

	@Override
	public <R extends Serializable> void forkJobTasks(final TaskSource<R> taskSource, final ResultConsumer<R> resultConsumer) {
		super.forkTasks(new DefaultTaskProcessor<R>(jobManager, assertNotNull(jobMonitor, "Job monitor must not be null in forkJobTasks()")), taskSource, resultConsumer);
	}

	@Override
	public JobMonitor getJobMonitor() {
		return assertNotNull(jobMonitor, "Job monitor must not be null getJobMonitor()");
	}

	@Override
	public Span getSpan() {
		Span currentSpan = tracer.currentSpan();
		if (currentSpan == null) {
			currentSpan = tracer.newTrace();
		}
		return currentSpan;
	}
}

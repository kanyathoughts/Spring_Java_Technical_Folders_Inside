/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.StopWatch;
import org.apache.commons.lang3.tuple.Pair;
import org.ff4j.FF4j;
import org.springframework.boot.info.BuildProperties;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.config.properties.JobConfigurationProperties;
import innowake.lib.job.api.management.JobMonitor;
import innowake.lib.job.api.task.ReportMessageExceptionHandler;
import innowake.lib.job.api.task.ResultConsumer;
import innowake.lib.job.api.task.Task;
import innowake.lib.job.api.task.TaskSource;
import innowake.mining.data.discovery.metrics.MetricsContributor.Phase;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.dawn.metrics.api.service.DiscoveryService;
import innowake.mining.server.discovery.dawn.metrics.api.service.OngoingDiscovery;
import innowake.mining.server.discovery.dawn.metrics.impl.service.DiscoveryResultConsumer;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.server.util.ProgressMonitorThrottle;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleUndiscoveredPojoPrototype;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.ModuleParameters;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.job.Message;

/**
 * Collects all metrics contributed by language contributors for each existing source object.
 */
public class MetricsCollector {

	private static final String STATUS_MESSAGE_GENERIC_METRICS_TASK_RESULT = "Receiving generic metrics task result";
	private static final String STATUS_MESSAGE_ADDITIONAL_GENERIC_METRICS_TASK_RESULT = "Receiving additional generic metrics task result for legacy"
			+ " contributor extension";

	private static final String FAILED_TASK_MESSAGE_FORMAT = "%s: %d tasks failed";

	private static final Technology[] SUPPORTED_TECHNOLOGIES_EXCEPT_JAVA = {
			Technology.COBOL, Technology.JCL, Technology.SQL, Technology.IMS,
			Technology.BINARY, Technology.ASSEMBLER, Technology.CSD, Technology.EASYTRIEVE,
			Technology.BASIC, Technology.RESOURCE, Technology.C, Technology.VMS, Technology.ORACLE,
			Technology.ECL, Technology.NATURAL, Technology.PL1, Technology.XML,
			Technology.CICS, Technology.VB, Technology.CPP
	};

	private static final Technology[] SUPPORTED_TECHNOLOGIES = ArrayUtils.add(SUPPORTED_TECHNOLOGIES_EXCEPT_JAVA, Technology.JAVA);

	private static final Logger LOG = LoggerFactory.getLogger(Logging.METRICS_COLLECTOR);

	private final SourceCachingService sourceService;
	private final ModuleService moduleService;
	private final MetricsContributorProvider contributorProvider;
	private final TaskHandler taskHandler;
	private final PersistMetricsModelService persistMetricsModelService;
	private final BuildProperties buildProperties;
	private final ModuleParameters moduleParameters;
	private final Map<FeatureId, Boolean> featureMap;
	private final DiscoveryService dawnDiscoveryService;

	/** All source Objects Ids for which metrics will be determined. */
	private List<EntityId> allSourceObjectIds = Collections.emptyList();
	private final IncrementalScanUtility incrementalScanUtility;

	private static final Set<RelatedTypes> RELATED_TYPES = Set.of(new RelatedTypes(Technology.RESOURCE, Type.VSAM_FILE, Type.FILE),
			new RelatedTypes(Technology.RESOURCE, Type.LIB, Type.FILE),
			new RelatedTypes(Technology.RESOURCE, Type.GDG_FILE, Type.FILE));

	/**
	 * Constructor.
	 *
	 * @param sourceService the {@link SourceService}
	 * @param moduleService the {@link ModuleService}
	 * @param contributorProvider the {@link MetricsContributorProvider}
	 * @param persistMetricsModelService the {@link PersistMetricsModelService}
	 * @param taskHandler the {@link TaskHandler} to fork worker tasks
	 * @param buildProperties the {@link BuildProperties} for build-related information, like the version number
	 * @param moduleParameters the additional module parameters
	 * @param ff4j the {@link FF4j}
	 * @param jobConfig the {@link JobConfigurationProperties}
	 * @param restartEnabled if the restart file discoverMetricsRestartFile is used
	 * @param genericConfigProperties the {@link GenericConfigProperties}
     * @param dawnDiscoveryService the dawn discovery service used to initiate the dawn services
	 */
	public MetricsCollector(final SourceCachingService sourceService,
							final ModuleService moduleService,
							final MetricsContributorProvider contributorProvider,
							final PersistMetricsModelService persistMetricsModelService,
							final TaskHandler taskHandler,
							final BuildProperties buildProperties,
							final ModuleParameters moduleParameters,
							final FF4j ff4j,
							final JobConfigurationProperties jobConfig,
							final boolean restartEnabled,
							final GenericConfigProperties genericConfigProperties,
							final DiscoveryService dawnDiscoveryService) {

		this.sourceService = sourceService;
		this.moduleService = moduleService;
		this.contributorProvider = contributorProvider;
		this.persistMetricsModelService = persistMetricsModelService;
		this.taskHandler = taskHandler;
		this.buildProperties = buildProperties;
		this.moduleParameters = moduleParameters;
		featureMap = new EnumMap<>(FeatureId.class);
		Stream.of(FeatureId.values()).forEach(feature -> featureMap.put(feature, Boolean.valueOf(ff4j.getFeature(feature.getId()).isEnable())));
		incrementalScanUtility = new IncrementalScanUtility(moduleService, sourceService, genericConfigProperties);
		this.dawnDiscoveryService = dawnDiscoveryService;
	}

	/**
	 * Determines the incremental changes and the changed modules to backup before collecting Metrics
	 *
	 * @param project the Project
	 * @param progressMonitor the {@link ProgressMonitor}
	 * @param isIncrementalScanEnabled flag specifying if incrementalScan feature is enabled or not
	 * @return moduleIds of the modules for metaData backup
	 */
	public Optional<Set<Long>> prepare(final ProjectPojo project, final ProgressMonitor progressMonitor, final boolean isIncrementalScanEnabled) {
		LOG.info(() -> "prepare phase for metrics collection to find the changed modules to be deleted and backup ");
		progressMonitor.setJobDescription(DiscoverMetricsJob.JOB_PREFIX + ": (1/7) Prepare");

		progressMonitor.setStepDescription("Determining source objects");

		final boolean changedJava;
		if (isIncrementalScanEnabled && isServerAndProjectVersionMatching(project)) {
			allSourceObjectIds = sourceService.findIDs(q -> q
					.ofProject(project.identity())
					.withModuleHashDiffers(true, false)
					.withTechnology(Arrays.asList(SUPPORTED_TECHNOLOGIES_EXCEPT_JAVA)));
			final List<EntityId> allJavaSourceObjectIds = sourceService.findIDs(q -> q
					.ofProject(project.identity())
					.withTechnology(Technology.JAVA)
					.withModuleHashDiffers(true, false));
			/* All Java modules must be recollected, when existing java sources have changed or when new java sources were added */
			changedJava = ! allJavaSourceObjectIds.isEmpty();
			allSourceObjectIds.addAll(allJavaSourceObjectIds);
			allSourceObjectIds.addAll(sourceService.findIDs(q -> q.ofProject(project.identity()).withModuleExists(false)));
		} else {
			allSourceObjectIds = sourceService.findIDs(q -> q.ofProject(project.identity())
															 .withTechnology(Arrays.asList(SUPPORTED_TECHNOLOGIES)));
			changedJava = false;
		}

		final Set<Long> moduleIdsToBackUp;
		if (isIncrementalScanEnabled) {
			moduleIdsToBackUp = new TreeSet<>();
			progressMonitor.setStepDescription("Determining incremental changes");
			moduleIdsToBackUp.addAll(incrementalScanUtility.handleIncrementalChanges(project.identity(), EntityId.allUids(allSourceObjectIds), progressMonitor, changedJava));
		} else {
			moduleIdsToBackUp = Collections.emptySet();
		}

		return isIncrementalScanEnabled ? Optional.of(moduleIdsToBackUp) : Optional.empty();
	}

	/**
	 * Starts collecting of the metrics for the provided {@code project}.
	 *
	 * @param project the Project to collect the metrics for
	 * @param progressMonitor the {@link ProgressMonitor}
	 * @param isIncrementalScanEnabled flag specifying if incrementalScan feature is enabled or not
	 */
	public void collect(final ProjectPojo project, final ProgressMonitor progressMonitor, final boolean isIncrementalScanEnabled) {
		final StopWatch watch = new StopWatch();
		if (LOG.isDebugEnabled()) {
			watch.start();
		}

		/* Delete modules to be processed */
		progressMonitor.setJobDescription(DiscoverMetricsJob.JOB_PREFIX + ": (2/7) Deleting Modules");
		if (isIncrementalScanEnabled) {
			progressMonitor.setStepDescription("Deleting modules and metrics data for incremental scan.");
			allSourceObjectIds.addAll(incrementalScanUtility.deleteModulesForIncrementalScan(project.identity(), progressMonitor,
					EntityId.allNids(allSourceObjectIds)).stream().map(EntityId::of).collect(Collectors.toList()));
		} else {
			progressMonitor.setStepDescription("Deleting modules and metrics data from previous scan.");
			moduleService.deleteModules(project.identity(), false, true);
		}

		/* undiscovered files are always re-processed, even when running incremental scan, so we can always delete all entries */
		moduleService.deleteUndiscovered(q -> q.ofProject(project.identity()));

		final int totalSourceObjects = allSourceObjectIds.size();
		progressMonitor.setStepDescription(String.format("Calculating Metrics on %d source objects", Integer.valueOf(totalSourceObjects)));

		progressMonitor.setJobDescription(DiscoverMetricsJob.JOB_PREFIX + ": (3/7) Executing Dawn Contributors");
		final OngoingDiscovery ongoingDiscovery = dawnDiscoveryService.discoverMetrics(taskHandler, project.identity(), moduleParameters, allSourceObjectIds);
		/* execute DAWN contributors */
		ongoingDiscovery.executeNextCycle();

		/* We first calculate the generic metrics without any dependency information between the modules. This will create all modules in the database. */
		progressMonitor.setJobDescription(DiscoverMetricsJob.JOB_PREFIX + ": (4/7) Generic Metrics");
		final Set<UUID> processedModuleRids = forkTasksForGenericMetrics(project, allSourceObjectIds, progressMonitor.subMonitor(totalSourceObjects),
				isIncrementalScanEnabled);

		final Set<UUID> transModuleUids;
		/* Based on having all modules in the database, we can now calculate the dependent metrics by resolving dependency information and call chains. */
		progressMonitor.setJobDescription(DiscoverMetricsJob.JOB_PREFIX + ": (5/7) Dependent Metrics");
		transModuleUids = forkTasksForDependentOrTransitiveMetrics(project, processedModuleRids, Phase.DEPENDENT_METRICS, progressMonitor.subMonitor(totalSourceObjects));

		if (ongoingDiscovery.hasNextCycle()) {
			/* execute DAWN dependency resolution and anchoring tasks */
			ongoingDiscovery.executeNextCycle();
		}

		/* Based on having all modules and dependencies in the database, we can now calculate transitive metrics. */
		progressMonitor.setJobDescription(DiscoverMetricsJob.JOB_PREFIX + ": (6/7) Transitive Metrics");
		while (ongoingDiscovery.hasNextCycle()) {
			/* execute DAWN deferred actions */
			ongoingDiscovery.executeNextCycle();
		}

		forkTasksForDependentOrTransitiveMetrics(project, transModuleUids, Phase.TRANSITIVE_METRICS, progressMonitor.subMonitor(totalSourceObjects));

		progressMonitor.setJobDescription(DiscoverMetricsJob.JOB_PREFIX + ": (7/7) Undiscovered Entities");
		collectUndiscoveredEntities(project.identity(), progressMonitor.subMonitor(0));

		if (LOG.isDebugEnabled()) {
			watch.stop();
			LOG.debug(() -> String.format("Metrics collection took %s (H:mm:ss.SSS)", watch.toString()));
		}
		progressMonitor.setJobDescription(DiscoverMetricsJob.JOB_PREFIX);
	}

	private boolean isServerAndProjectVersionMatching(final ProjectPojo project) {
		return buildProperties.getVersion().equalsIgnoreCase(project.getMetricsVersion());
	}

	private Set<UUID> forkTasksForGenericMetrics(final ProjectPojo project, final List<EntityId> sourceObjectIds, final ProgressMonitor progressMonitor,
			final boolean isIncrementalScanEnabled) {
		final Set<UUID> moduleUids = new HashSet<>();
		final int totalTasks = sourceObjectIds.size();
		final SearchOrders searchOrders = new SearchOrders(project.getSearchOrders());

		/* collect source objects here for which no ModelArtifacts were created - maybe these can be handled by Dawn and MetricsContributorExtensions */
		final Map<String, EntityId> taskIdToSourceObjectId = new HashMap<>();

		final TaskSource<UUID[]> taskSource = new TaskSource<UUID[]>() {
			private int taskCount = 0;

			@Override
			public boolean hasNextTask() {
				return taskCount < sourceObjectIds.size();
			}

			@Override
			public Task<UUID[]> nextTask() {
				LOG.trace(() -> String.format("Submitting generic metrics task (%d/%d)", Integer.valueOf(taskCount + 1), Integer.valueOf(totalTasks)));
				final EntityId sourceObjectId = sourceObjectIds.get(taskCount++);
				final MetricsCollectorTask.Parameters parameters = new MetricsCollectorTask.Parameters.Builder()
						.setProjectId(project.identity())
						.setSourceObjectId(sourceObjectId)
						.setPhase(Phase.GENERIC_METRICS)
						.setSearchOrders(searchOrders)
						.setFeatureMap(featureMap)
						.build();

				final MetricsCollectorTask task = new MetricsCollectorTask(progressMonitor.subMonitor(1), taskHandler.getJobId(), parameters, moduleParameters);
				taskIdToSourceObjectId.put(task.getTaskId(), sourceObjectId);
				return task;
			}
		};

		final JobMonitor jobMonitor = taskHandler.getJobMonitor();
		final DiscoveryResultConsumer<UUID[]> resultConsumer = new DiscoveryResultConsumer<>(jobMonitor, STATUS_MESSAGE_GENERIC_METRICS_TASK_RESULT) {
			private int resultCount = 1;

			@Override
			protected void handleResult(final String taskId, final Result<UUID[]> result) {
				final var message = String.format(" %s (%d/%d) (%d%%)", statusMessage, Integer.valueOf(resultCount++),
						Integer.valueOf(totalTaskCount), Integer.valueOf(resultCount * 100 / totalTaskCount));
				LOG.trace(() -> message);
				ProgressMonitorThrottle.throttleStepDescription(message, progressMonitor);
				/* we collect the RIDs that have been created by the submitted tasks so that we can use them to calculate the dependent metrics later on. */
				final UUID[] uids = result.value;
				if (uids != null && uids.length != 0) {
					moduleUids.addAll(Arrays.asList(uids));
					taskIdToSourceObjectId.remove(taskId);
				}
			}
		};

		resultConsumer.setTotalTaskCount(totalTasks);
		taskHandler.forkJobTasks(taskSource, resultConsumer);

		if (contributorProvider.hasCustomContributors()) {
			final int totalTaskCount = totalTasks + taskIdToSourceObjectId.size();
			resultConsumer.setTotalTaskCount(totalTaskCount);
			resultConsumer.setStatusMessage(STATUS_MESSAGE_ADDITIONAL_GENERIC_METRICS_TASK_RESULT);
			final Iterator<EntityId> unhandledSourceObjectsIterator = taskIdToSourceObjectId.values().iterator();
			final TaskSource<UUID[]> taskSourceForUnhandled = new TaskSource<UUID[]>() {

				int taskCount = 0;

				@Override
				public boolean hasNextTask() {
					return unhandledSourceObjectsIterator.hasNext();
				}

				@Override
				public Task<UUID[]> nextTask() {
					taskCount++;
					LOG.trace(() -> String.format("Submitting additional generic metrics task for legacy contributor extension (%d/%d)", taskCount,
							taskIdToSourceObjectId.size()));
					final EntityId sourceObjectId = unhandledSourceObjectsIterator.next();
					final MetricsCollectorTask.Parameters parameters = new MetricsCollectorTask.Parameters.Builder()
							.setProjectId(project.identity())
							.setSourceObjectId(sourceObjectId)
							.setPhase(Phase.GENERIC_METRICS)
							.setSearchOrders(searchOrders)
							.setFeatureMap(featureMap)
							.setWithExistingModelArtifacts(true)
							.build();

					return new MetricsCollectorTask(progressMonitor.subMonitor(1), taskHandler.getJobId(), parameters, moduleParameters);
				}
			};

			taskHandler.forkJobTasks(taskSourceForUnhandled, resultConsumer);
		}

		if (resultConsumer.getFailedTaskCount() > 0) {
			jobMonitor.addMessage(new Message(Message.Severity.ERROR, String.format(FAILED_TASK_MESSAGE_FORMAT, STATUS_MESSAGE_GENERIC_METRICS_TASK_RESULT,
					resultConsumer.getFailedTaskCount())));
		}

		/* We now need to remove the duplicate modules that are of related types. Technically these are not duplicate modules.
		 * Example., In a system, there cannot be two modules with same name that are RESOURCE_FILE and RESOURCE_VSAM_FILE.
		 * In the resource collector stage it is not always possible to correctly identify whether it's a RESOURCE_FILE or RESOURCE_VSAM_FILE because the usage
		 * of such artifacts in the code would be similar and only in certain usages we could recognize it as a RESOURCE_VSAM_FILE. Let's say a
		 * RESOURCE_VSAM_FILE is used in two places of the client code and say in one of the usage/place we could not classify it as a VSAM then we create a
		 * RESOURCE_FILE artifact and let's say in the second usage we could identify it as VSAM, we wont alter the existing one as VSAM rather we create a new
		 * artifact as RESOURCE_VSAM_FILE.
		 * Thus causing duplicate: one as RESOURCE_FILE and another as RESOURCE_VSAM_FILE.
		 * In the below logic, we will do 2 things,
		 * 1. For normal discovery metrics scan:
		 *    we would remove the modules with RESOURCE_FILE for normal scan.
		 * 2. For incremental scan:
		 * 	  We will not remove the modules with RESOURCE_FILE directly for incremental scan instead we will collect all the duplicated modules by name and
		 *    types in ascending order based on metrics date and then we will update type of the module which created first to RESOURCE_VSAM_FILE and remove
		 *    other modules.This will preserve the dependencies that got created for the existing module.
		 */
		moduleUids.removeAll(mergeRelatedTypes(project, isIncrementalScanEnabled));

		return moduleUids;
	}

	private Set<UUID> mergeRelatedTypes(final ProjectPojo project, final boolean isIncrementalScanEnabled) {
		final var idsToDelete = RELATED_TYPES.stream()
				.map(relatedType -> isIncrementalScanEnabled ?
						mergeRelatedModulesForIncrementalScan(project.identity(), relatedType) :
						mergeRelatedModulesForNormalScan(project.identity(), relatedType))
				.flatMap(Collection::stream)
				.collect(Collectors.toSet());

		if ( ! idsToDelete.isEmpty()) {
			moduleService.deleteModules(b -> b.byUids(idsToDelete));
		}

		return idsToDelete;
	}

	private List<UUID> mergeRelatedModulesForNormalScan(final EntityId project, final RelatedTypes relatedType) {
		return moduleService.findDuplicateModuleIds(project, relatedType.technology, relatedType.supersedingType, relatedType.subsidingType)
							.stream().map(EntityId::getUid)
							.collect(Collectors.toList());
	}

	/**
	 * In incremental scan, we need to get the duplicate modules with same name, technology and given types.
	 * Once we got duplicate modules, we should delete modules that are created in current run and update the type of module which is created in previous run
	 * with given type to update i.e {@code relatedType.supersedingType}.
	 *
	 * @param projectId id of the project
	 * @param relatedType {@link RelatedTypes}
	 * @return Uids of the modules to delete
	 */
	private List<UUID> mergeRelatedModulesForIncrementalScan(final EntityId projectId, final RelatedTypes relatedType) {
		final List<Type> typesToCheck = Arrays.asList(relatedType.supersedingType, relatedType.subsidingType);
		/* Here we are getting names of duplicate modules with given technology and types. */
		final List<String> names = moduleService.findDuplicateModuleNames(projectId, relatedType.technology, typesToCheck);
		final List<UUID> idsToUpdate = new ArrayList<>(names.size());
		final List<UUID> idsToDelete = new ArrayList<>(names.size());
		names.forEach(name -> {
			/* Here we are getting duplicate modules with same name, technology and given types, 
			 * In order to identify which module to update and which module to delete, we are getting the modules with the dependency_hash.
			 * If a module has dependency hash means , its has some dependencies so , this module will be updated and others will be removed.
			 * If no module has dependencies, Since we are getting duplicate modules in ascending order by metricsDate, so the record at 0 index will be
			 * removed and others will be updated. */
			final var moduleIdAndDependencyHashesPairs = moduleService.findModuleUUIDsAndDependencyHashes(b -> b.ofProject(projectId)
																	.withName(name)
																	.withTechnology(relatedType.technology)
																	.withTypes(typesToCheck)
																	.sortMetricsDate(SortDirection.ASCENDING));
			
			final var moduleWithDependencies = getFirstModuleWithDependencies(moduleIdAndDependencyHashesPairs);
			final var moduleIds = moduleIdAndDependencyHashesPairs.stream().map(Pair::getLeft).collect(Collectors.toList());
			if (moduleWithDependencies.isPresent()) {
				final UUID moduleUid = moduleWithDependencies.get();
				idsToUpdate.add(moduleUid);
				moduleIds.remove(moduleUid);
			} else {
				idsToUpdate.add(moduleIds.remove(0));	
			}
			idsToDelete.addAll(moduleIds);			
		});

		if ( ! idsToUpdate.isEmpty()) {
			moduleService.updateModules(q -> q.byUids(idsToUpdate), new ModulePojoPrototype()
																		.setTechnology(relatedType.technology)
																		.setType(relatedType.supersedingType));
		}

		return idsToDelete;
	}
	
	private Optional<UUID> getFirstModuleWithDependencies(final List<Pair<UUID, String>> moduleIdAndDependencyHashesPairs) {
	    for (final Pair<UUID, String> pair : moduleIdAndDependencyHashesPairs) {
	        if (StringUtils.isNotBlank(pair.getRight())) {
	            return Optional.of(pair.getLeft());
	        }
	    }
	    return Optional.empty();
	}

	private Set<UUID> forkTasksForDependentOrTransitiveMetrics(final ProjectPojo project, final Set<UUID> moduleRids,
																 final Phase phase, final ProgressMonitor progressMonitor) {
		final SearchOrders searchOrders = new SearchOrders(project.getSearchOrders());
		final Iterator<UUID> ridIterator = moduleRids.iterator();

		final TaskSource<UUID[]> taskSource = new TaskSource<UUID[]>() {
			private int taskCount = 0;

			@Override
			public boolean hasNextTask() {
				return ridIterator.hasNext();
			}

			@Override
			public Task<UUID[]> nextTask() {
				LOG.trace(() -> String.format("Submitting dependent metrics task (%d/%d)", Integer.valueOf(++taskCount), Integer.valueOf(moduleRids.size())));
				final MetricsCollectorTask.Parameters parameters = new MetricsCollectorTask.Parameters.Builder()
						.setProjectId(project.identity())
						.setModule(ridIterator.next())
						.setPhase(phase)
						.setFeatureMap(featureMap)
						.setSearchOrders(searchOrders)
						.build();
				return new MetricsCollectorTask(progressMonitor.subMonitor(1), taskHandler.getJobId(), parameters, moduleParameters);
			}
		};

		final Set<UUID> returnedUids = phase == Phase.DEPENDENT_METRICS ? new HashSet<>() : Collections.emptySet();
		final JobMonitor jobMonitor = taskHandler.getJobMonitor();
		final ResultConsumer<UUID[]> resultConsumer = new ResultConsumer<UUID[]>(new ReportMessageExceptionHandler<>(jobMonitor)) {
			private int resultCount = 1;

			@Override
			protected void handleResult(final String taskId, final Result<UUID[]> result) {
				final String message = String.format("Receiving %s metrics task (%d/%d) (%d%%)",
						(phase == Phase.DEPENDENT_METRICS ? "dependent" : "transitive"), Integer.valueOf(resultCount++),
						Integer.valueOf(moduleRids.size()), Integer.valueOf(resultCount * 100 / moduleRids.size()));
				LOG.trace(() -> message);
				ProgressMonitorThrottle.throttleStepDescription(message, progressMonitor);
				/* we collect the RIDs that have been returned by the submitted tasks for dependent metrics so that we can use them for the transitive metrics phase. */
				if (phase == Phase.DEPENDENT_METRICS) {
					final UUID[] uids = result.value;
					if (uids != null && uids.length != 0) {
						returnedUids.addAll(Arrays.asList(uids));
					}
				}
			}
		};

		taskHandler.forkJobTasks(taskSource, resultConsumer);

		return returnedUids;
	}

	private void collectUndiscoveredEntities(final EntityId projectId, final ProgressMonitor monitor) {
		final var undiscoveredEntities = sourceService.find(q -> q.ofProject(projectId).withModuleExists(false)).stream()
				.map(uso -> new ModuleUndiscoveredPojoPrototype()
									.setProject(projectId)
									.setName(FilenameUtils.getName(uso.getPath()))
									.setPath(uso.getPath()))
				.collect(Collectors.toList());

		persistMetricsModelService.importUndiscoveredEntities(undiscoveredEntities, monitor.subMonitor(0));
	}

	private static class RelatedTypes {
		private final Technology technology;
		private final Type supersedingType;
		private final Type subsidingType;

		private RelatedTypes(final Technology technology, final Type supersedingType, final Type subsidingType) {
			this.technology = technology;
			this.supersedingType = supersedingType;
			this.subsidingType = subsidingType;
		}
	}
}

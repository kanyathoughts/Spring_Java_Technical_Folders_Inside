/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.metrics;

import static innowake.mining.shared.ProfilingHelper.executeWithProfiling;
import static innowake.mining.shared.entities.ModulePojo.Representation.PHYSICAL;
import static innowake.mining.shared.model.Creator.DISCOVERY;
import static innowake.mining.shared.model.Technology.CSD;
import static innowake.mining.shared.model.Technology.JAVA;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.commons.collections4.ListUtils;
import org.apache.commons.collections4.MultiValuedMap;
import org.apache.commons.collections4.multimap.HashSetValuedHashMap;
import org.apache.commons.lang3.time.StopWatch;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.io.Files;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.api.profiling.Profiler;
import innowake.lib.core.api.profiling.ProfilingFactory;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.server.util.ProgressMonitorThrottle;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.access.Table;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Creator;

/**
 * Utility class to handle Incremental scan changes for metrics collector 
 */
public class IncrementalScanUtility {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.INCREMENTAL_SCAN);

	private static final int DELETION_BATCH_SIZE = 10_000;
	private static final int FETCH_BATCH_SIZE = 1_000;
	private static final String PROFILING_CATEGORY = "discovery.incremental";
	
	private final ModuleService moduleService;
	private final SourceCachingService sourceService;
	
	/** Changed module Rids which needs to be deleted in case of incremental scan. */
	private final Set<UUID> toBeDeletedUids = new HashSet<>();
	private final LoadingCache<UUID, ModuleLightweightPojo> moduleCache;
	/** Map containing all source objects, with paths as keys */
	private final Map<String, SourcePojo> existingSourceObjects = new HashMap<>();
	private final MultiValuedMap<String, SourcePojo> sourcesPerName = new HashSetValuedHashMap<>();

	private final Profiler profiler = ProfilingFactory.getProfilingSession().getProfiler(PROFILING_CATEGORY);
	
	/**
	 * Constructor.
	 * 
	 * @param moduleService the {@link ModuleService}
	 * @param sourceService Source Object Service
	 * @param genericConfigProperties the {@link GenericConfigProperties}
	 */
	public IncrementalScanUtility(final ModuleService moduleService,
								  final SourceCachingService sourceService,
								  final GenericConfigProperties genericConfigProperties) {
		this.moduleService = moduleService;
		this.sourceService = sourceService;
		this.moduleCache = CacheBuilder.newBuilder().maximumSize(genericConfigProperties.getDiscoveryModuleRepositoryCacheSize())
				.build(new CacheLoader<UUID, ModuleLightweightPojo>() {
					@Override
					@Nullable
					public ModuleLightweightPojo load(@Nullable final UUID key) {
						return moduleService.findAnyModuleLightweight(b -> b.byUid(Objects.requireNonNull(key)))
													.orElseThrow(() -> new MiningEntityNotFoundException("Module doesn't exists for UID: " + key));
					}
				});
	}
	
	/**
	 * Find and mark the changed (and deleted) modules and its' virtual dependent modules for Deletion 
	 * and returns the changed modules having metadata. 
	 * 
	 * @param projectId the project id
	 * @param changedSourceIds the source {@link UUID UUIDs} of the changed modules
	 * @param progressMonitor the {@link ProgressMonitor}
	 * @param collectJava {@code true} if all Java modules must be recollected. Otherwise {@code false}
	 * @return moduleIdsToBackUp The changed moduleIds having metadata
	 */
	public List<Long> handleIncrementalChanges(final EntityId projectId, final Collection<UUID> changedSourceIds, final ProgressMonitor progressMonitor,
			final boolean collectJava) {
		profiler.start("handleIncrementalChanges");
		try {
			final StopWatch stopWatch = new StopWatch();
			stopWatch.start();

			final List<UUID> toBeCheckedUids = new ArrayList<>();
			final List<Long> moduleIdsToBackUp = new ArrayList<>();
			final Set<Long> moduleIdsWithMetaData = moduleService.findModulesWithMetaData(projectId).stream().map(EntityId::getNid).collect(Collectors.toSet());

			/* create a cache with all existing SourceObjects */
			sourceService.find(q -> q.ofProject(projectId)).forEach(so -> {
				existingSourceObjects.put(so.getPath(), so);
				sourcesPerName.put(so.getName(), so);
			});

			/* Find modules that have changed and mark their contained modules for deletion. */
			executeWithProfiling(() -> markChangedModulesToDelete(projectId, changedSourceIds, progressMonitor, moduleIdsToBackUp, moduleIdsWithMetaData,
					collectJava), profiler, "markChangedModulesToDelete");

			/* Mark all modules that contain changed or deleted modules. */
			executeWithProfiling(() -> collectContainingAndReferencingModules(progressMonitor, toBeCheckedUids),
					profiler, "collectContainingAndReferencingModules");

			/* Recursively mark all virtual modules that depend on changed or deleted modules as deleted.
			 * Mark all physical modules as changed whose referenced modules have been marked as deleted. */
			executeWithProfiling(() -> markVirtualModulesOfChangedModulesToDelete(progressMonitor, toBeCheckedUids, moduleIdsToBackUp, moduleIdsWithMetaData),
					profiler, "markVirtualModulesOfChangedModulesToDelete");

			stopWatch.stop();
			LOG.info(() -> String.format("Handling of incremental changes took %s (H:mm:ss.SSS)", stopWatch.toString()));
			return moduleIdsToBackUp;
		} finally {
			profiler.stop();
		}
	}
	
	/**
	 * Mark the changed modules for deletion. 
	 * 
	 * @param projectId the project id
	 * @param changedSourceIds The Id's of changed  {@link SourcePojo}s
	 * @param progressMonitor the {@link ProgressMonitor}
	 * @param moduleIdsWithMetaData changed moduleIds having metaData for backup modules 
	 * @param moduleIdsToBackUp The changed moduleIds having metadata
	 * @param collectJava {@code true} if all Java modules must be recollected. Otherwise {@code false}
	 */
	private void markChangedModulesToDelete(final EntityId projectId, final Collection<UUID> changedSourceIds, final ProgressMonitor progressMonitor,
			final List<Long> moduleIdsToBackUp, final Set<Long> moduleIdsWithMetaData, final boolean collectJava) {
		final StopWatch stopWatch = new StopWatch();
		stopWatch.start();

		if (collectJava) {
			setStepDescription(progressMonitor, "Searching for Java modules...");
			markChangedModulesToDelete(moduleIdsToBackUp, moduleIdsWithMetaData, moduleService.findModulesLightweight(q -> q.ofProject(projectId)
																															.withCreator(DISCOVERY)
																															.withTechnology(JAVA)));
		}

		setStepDescription(progressMonitor, "Searching for modules having no matching source file...");
		markChangedModulesToDelete(moduleIdsToBackUp, moduleIdsWithMetaData, moduleService.findModulesLightweight(q -> q.ofProject(projectId)
																														.withCreator(DISCOVERY)
																														.filterHasPathNoSource()));

		setStepDescription(progressMonitor, "Searching for modules having no source...");
		markChangedModulesToDelete(moduleIdsToBackUp, moduleIdsWithMetaData, moduleService.findModuleLightweightMissingOrChangedSource(projectId.getUid(), DISCOVERY));

		setStepDescription(progressMonitor, "Searching for modules (type=MISSING) having source file...");
		/* ioDao.findMissingModulesIfSourceObjectAvailable can return false positives so we check again for exact SourcePojo matches */
		markChangedModulesToDelete(moduleIdsToBackUp, moduleIdsWithMetaData, moduleService.findModulesLightweight(q -> q.ofProject(projectId)
																														.withCreator(DISCOVERY)
																														.withIdentified(false)
																														.withSourceMatch()));

		setStepDescription(progressMonitor, "Searching for modules marked as to be deleted...");
		markChangedModulesToDelete(moduleIdsToBackUp, moduleIdsWithMetaData, moduleService.findModulesLightweight(q -> q.ofProject(projectId)
																														.withCreator(DISCOVERY)
																														.ofSources(changedSourceIds)));

		/*
		 * we will do a re scan of modules which are having errors with the key UNDISCOVERED_DEPENDENCY.
		 */
		for (final EntityId moduleId : moduleService.findModulesWithUnresolvedDependencies(projectId)) {
			toBeDeletedUids.add(moduleId.getUid());
			addModuleIdsToBackup(moduleId.getNid(), moduleIdsWithMetaData, moduleIdsToBackUp);
		}

		stopWatch.stop();
		LOG.info(() -> String.format("Marking changed modules for deletion took %s (H:mm:ss.SSS)", stopWatch.toString()));
	}

	private void markChangedModulesToDelete(final List<Long> moduleIdsToBackUp, final Set<Long> moduleIdsWithMetaData, final List<ModuleLightweightPojo> modules) {
		for (final var module : modules) {
			moduleCache.put(module.getUid(), module);
			toBeDeletedUids.add(module.getUid());
			addModuleIdsToBackup(module.getId(), moduleIdsWithMetaData, moduleIdsToBackUp);
		}
	}

	private void collectContainingAndReferencingModules(final ProgressMonitor progressMonitor, final List<UUID> toBeCheckedUids) {
		final StopWatch stopWatch = new StopWatch();
		stopWatch.start();

		final Integer size = Integer.valueOf(toBeDeletedUids.size());
		setStepDescription(progressMonitor, String.format("Determining containing and referencing modules (%d)", size));

		final List<UUID> moduleIds = new ArrayList<>(toBeDeletedUids.size());
		for (final var uid : toBeDeletedUids) {
			moduleIds.add(uid);
			toBeCheckedUids.add(uid);
		}

		collectContainingContainedAndreferencingModules(moduleIds, false, toBeCheckedUids);

		stopWatch.stop();
		LOG.info(() -> String.format("Collecting containing and referencing modules took %s (H:mm:ss.SSS)", stopWatch.toString()));
	}
	
	private void markVirtualModulesOfChangedModulesToDelete(final ProgressMonitor progressMonitor, final List<UUID> toBeCheckedUids,
			final List<Long> moduleIdsToBackUp, final Set<Long> moduleIdsWithMetaData) {
		final StopWatch stopWatch = new StopWatch();
		stopWatch.start();

		setStepDescription(progressMonitor, "Determining virtual modules");

		final List<UUID> moduleUids = new ArrayList<>(toBeCheckedUids.size());
		while ( ! toBeCheckedUids.isEmpty()) {
			progressMonitor.checkCanceled();

			for (final var uid : toBeCheckedUids) {
				if ( ! toBeDeletedUids.contains(uid)) {
					toBeDeletedUids.add(uid);

					final ModuleLightweightPojo module = Objects.requireNonNull(moduleCache.getUnchecked(uid));
					final Long moduleId = module.getId();
					moduleUids.add(uid);
					addModuleIdsToBackup(moduleId, moduleIdsWithMetaData, moduleIdsToBackUp);
				}
			}

			toBeCheckedUids.clear();
			collectContainingContainedAndreferencingModules(moduleUids, true, toBeCheckedUids);
			moduleUids.clear();
		}

		stopWatch.stop();
		LOG.info(() -> String.format("Marking virtual modules for deletion took %s (H:mm:ss.SSS)", stopWatch.toString()));
	}
	
	/**
	 * Delete the changed modules during Incremental Scan.
	 *
	 * @param projectId the project id
	 * @param progressMonitor the {@link ProgressMonitor}
	 * @param allSourceObjectIds the changed source ids
	 * @return additionalChangedSources The changed {@link SourcePojo}s
	 */
	public List<Long> deleteModulesForIncrementalScan(final EntityId projectId, final ProgressMonitor progressMonitor, final Collection<Long> allSourceObjectIds) {
		profiler.start("deleteModulesForIncrementalScan");
		try {
			final StopWatch stopWatch = new StopWatch();
			stopWatch.start();

			final List<Long> additionalChangedSources = new ArrayList<>();
			final Set<Long> changedSourceIds = new HashSet<>(allSourceObjectIds);

			/*  Delete all modules marked for deletion but keep their source objects. */
			executeWithProfiling(() -> deleteAllModules(progressMonitor, additionalChangedSources, changedSourceIds, projectId), profiler, "deleteAllModules");

			/* If there were no deletions then we don't need to check all modules again. */
			if ( ! toBeDeletedUids.isEmpty()) {
				executeWithProfiling(() -> deleteRemainingModules(projectId, progressMonitor, additionalChangedSources, changedSourceIds), 
						profiler, "deleteRemainingModules");
			}
			
			stopWatch.stop();
			LOG.info(() -> String.format("Deletion of modules took %s (H:mm:ss.SSS)", stopWatch.toString()));
			return additionalChangedSources;
		} finally {
			profiler.stop();
		}
	}
	
	private void deleteAllModules(final ProgressMonitor progressMonitor, final List<Long> additionalChangedSources, final Set<Long> changedSourceIds, 
			final EntityId projectId) {
		final StopWatch stopWatch = new StopWatch();
		stopWatch.start();

		final List<ModuleLightweightPojo> toBeDeletedModules = toBeDeletedUids.stream()
																.map(moduleCache::getUnchecked)
																.filter(Objects::nonNull)
																.collect(Collectors.toList());
		progressMonitor.checkCanceled();
		deleteModules(projectId, toBeDeletedModules, additionalChangedSources, changedSourceIds, progressMonitor, "Deleting modules");

		stopWatch.stop();
		LOG.info(() -> String.format("Deletion of %d modules in phase I took %s (H:mm:ss.SSS)", Integer.valueOf(toBeDeletedModules.size()), stopWatch.toString()));
	}
	
	private void deleteRemainingModules(final EntityId projectId, final ProgressMonitor progressMonitor, final List<Long> additionalChangedSources,
			final Set<Long> changedSourceIds) {
		boolean checkForUnused;

		final StopWatch stopWatch = new StopWatch();
		stopWatch.start();

		setStepDescription(progressMonitor, "Deleting remaining modules");
		int counter = 0;
		do {
			progressMonitor.checkCanceled();
			final List<ModuleLightweightPojo> toBeDeletedModules = new ArrayList<>();
			toBeDeletedModules.addAll(moduleService.findModuleLightweightOrphaned(projectId, Creator.DISCOVERY));
			if ( ! toBeDeletedModules.isEmpty()) {
				counter += toBeDeletedModules.size();
				deleteModules(projectId, toBeDeletedModules, additionalChangedSources, changedSourceIds, progressMonitor, "Deleting remaining modules");
				checkForUnused = true;
			} else {
				checkForUnused = false;
			}
		} while (checkForUnused);

		stopWatch.stop();
		final Integer cnt = Integer.valueOf(counter);
		LOG.info(() -> String.format("Deletion of %d modules in phase II took %s (H:mm:ss.SSS)", cnt, stopWatch.toString()));
	}

	/** Consider the changed module having metaData for backup modules. */
	private static void addModuleIdsToBackup(final Long moduleId, final Set<Long> moduleIdsWithMetaData, final List<Long> moduleIdsToBackup) {
		if ( ! moduleIdsWithMetaData.isEmpty() && moduleIdsWithMetaData.contains(moduleId)) {
			moduleIdsToBackup.add(moduleId);
		}
	}
	
	private void deleteModules(final EntityId projectId, final List<ModuleLightweightPojo> modules, final List<Long> additionalChangedSources, 
			final Set<Long> changedSourceIds, final ProgressMonitor progressMonitor, final String msg) {

		if ( ! modules.isEmpty()) {
			final List<UUID> moduleIds = new ArrayList<>(modules.size());
			for (final var module : modules) {
				moduleIds.add(module.getUid());

				final String path = module.getPath();
				if (path != null) {
					final SourcePojo source = existingSourceObjects.get(path);
					if (source != null && ! changedSourceIds.contains(source.getId())) {
						additionalChangedSources.add(source.getId());
					}
				}

				/* CSDResourceCollector also creates xml files for any CSD module if "csd model xml dump " is enabled by config */
				if (PHYSICAL == module.getRepresentation() && module.getTechnology() == CSD) {
					if (path == null) {
						throw new IllegalStateException("Module must have a path: " + module);
					}
					final String directory = path.substring(0, path.lastIndexOf("/") + 1);
					final SourcePojo xml = existingSourceObjects.remove(directory + Files.getNameWithoutExtension(path) + ".xml");
					if (xml != null) {
						moduleService.deleteUndiscovered(q -> q.ofProject(xml.getProject()).withName(xml.getName()).withPath(xml.getPath()));
						sourceService.remove(xml.identity(), null);
					}
				}
			}

			final Integer size = Integer.valueOf(moduleIds.size());
			int counter = 0;
			final List<List<UUID>> partitions = ListUtils.partition(moduleIds, DELETION_BATCH_SIZE);
			for (final List<UUID> partition : partitions) {
				counter+= partition.size();
				setStepDescription(progressMonitor, String.format("%s (%d/%d))", msg, Integer.valueOf(counter), size));
				moduleService.deleteModules(b -> b.ofProject(projectId).byUids(partition));
			}
		}
	}

	private void collectContainingContainedAndreferencingModules(final List<UUID> moduleUids, final boolean collectContaining, final List<UUID> toBeCheckedUids) {
		if ( ! moduleUids.isEmpty()) {
			for (final List<UUID> idPartition : ListUtils.partition(moduleUids, FETCH_BATCH_SIZE)) {
				final Optional<Table> table = moduleService.findContainingAndRelatedModules(idPartition, collectContaining);
				if (table.isPresent()) {
					validate(table.get(), collectContaining);
					
					final var iterator = table.get().iterator();
					while (iterator.hasNext()) {
						final var row = iterator.next();
						if (collectContaining) {
							final UUID containing = (UUID) row.get("containing");
							if (containing != null && row.get("path") == null) {
								toBeCheckedUids.add(containing);
							}
						}
						
						row.<List<UUID>>getOptional("relations").ifPresent(toBeCheckedUids::addAll);
					}
				}
			}
		}
	}

	private static void validate(final Table table, final boolean collectContaining) {
		final var columns = table.columns();
		if (collectContaining) {
			if ( ! columns.contains("containing")) {
				throw new UnsupportedOperationException("Containing module UUID must be present");
			}
			if ( ! columns.contains("path")) {
				throw new UnsupportedOperationException("Path must be present");
			}
		}
		if ( ! columns.contains("relations")) {
			throw new UnsupportedOperationException("Contained and referencing module UUIDs must be present");
		}
	}

	private static void setStepDescription(final ProgressMonitor progressMonitor, final String message) {
		progressMonitor.checkCanceled();
		ProgressMonitorThrottle.throttleStepDescription(message, progressMonitor);
	}
}

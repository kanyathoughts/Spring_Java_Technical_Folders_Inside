/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.shared.ProfilingHelper.executeWithProfiling;
import static innowake.mining.shared.model.RelationshipType.CONTAINS;
import static java.util.Collections.emptyMap;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.annotation.PostConstruct;

import org.apache.commons.lang.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.api.profiling.Profiler;
import innowake.lib.core.api.profiling.ProfilingFactory;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.preset.AbstractBuilder;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.task.Task;
import innowake.mining.data.access.ModelArtifactService;
import innowake.mining.data.discovery.metrics.ContributorParameters;
import innowake.mining.data.discovery.metrics.DiscoveryCache;
import innowake.mining.data.discovery.metrics.IModuleRepository;
import innowake.mining.data.discovery.metrics.MetricsContributor;
import innowake.mining.data.discovery.metrics.MetricsContributor.Phase;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.data.io.DiscoveryReferenceImporter;
import innowake.mining.data.io.LazyModelArtifact;
import innowake.mining.data.model.discovery.ErrorMarker;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.cache.DiscoveryJobCache;
import innowake.mining.server.discovery.config.DiscoveryConfigAccessor;
import innowake.mining.server.util.ModuleFilterUtil;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleParameters;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.mining.shared.model.discovery.ResolveTargetHelper;

/**
 * Depending on the {@link Phase} this {@link Task} implementation is either collecting the generic metrics of one single {@link SourcePojo} or the dependent
 * metrics of one single {@link ModelArtifact}. It will be forked by the {@link MetricsCollector} as part of the {@link DiscoverMetricsJob}.
 */
public class MetricsCollectorTask extends Task<UUID[]> {

	private static final String PROFILING_CATEGORY = "discovery.metricscollectortask";
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.METRICS_COLLECTOR);
	
	private final Parameters parameters;
	
	private final ModuleParameters moduleParameters;

	@Autowired
	private transient SourceCachingService sourceService;
	@Autowired
	private transient SourceObjectResourceCollector resourceCollector;
	@Autowired
	private transient MetricsContributorProvider contributorProvider;
	@Autowired
	private transient ModuleService moduleService;
	@Autowired
	private transient ModelArtifactService modelArtifactService;
	@Autowired
	private transient PersistMetricsModelService persistMetricsModelService;
	@Autowired
	private transient DiscoveryJobCache discoveryCache;
	@Autowired
	private transient DiscoveryConfigAccessor configAccessor;
	@Autowired
	private transient ArtifactCacheService artifactCacheService;
	@Autowired
	private transient SourceExportService sourceExportService;
	@Autowired
	private transient ModuleFilterUtil moduleFilterUtil;
	
	@Nullable
	private transient DbModuleRepository repo;
	@Nullable
	private transient Config config;
	@Nullable
	private transient Profiler profiler;
	
	/**
	 * Constructor.
	 *
	 * @param progressMonitor the {@link ProgressMonitor}
	 * @param jobId the Id of the {@link Job} this task belongs to
	 * @param parameters the {@link Parameters} for this task
	 * @param moduleParameters the additional module parameters
	 */
	public MetricsCollectorTask(final ProgressMonitor progressMonitor, final String jobId, final Parameters parameters, 
			final ModuleParameters moduleParameters) {
		super(progressMonitor, jobId);
		this.parameters = parameters;
		this.moduleParameters = moduleParameters;
	}
	
	@PostConstruct
	private void postContruct() {
		repo = new DbModuleRepository(parameters.projectId, modelArtifactService, getJobId(), artifactCacheService, parameters.searchOrders, moduleFilterUtil);
		config = configAccessor.getConfig(parameters.projectId, getJobId());
		profiler = ProfilingFactory.getProfilingSession().getProfiler(PROFILING_CATEGORY);
	}

	@Override
	protected Result<UUID[]> run(final ProgressMonitor progressMonitor) {
		progressMonitor.checkCanceled();
		
		final DbModuleRepository repoLocal = this.repo;
		if (repoLocal == null) {
			throw new IllegalStateException("The module repository is not available.");
		}
		
		final List<UUID> moduleUids = new ArrayList<>();
		try {
			switch (parameters.phase) {
				case GENERIC_METRICS:
					/* Handles the generic metrics of one source object and returns the RIDs of all modules that have been updated or added to the database.
					 * No other modules are known at this time of execution! */
					repoLocal.setQueryLock(true);
					moduleUids.addAll(executeWithProfiling(() -> handleGenericMetrics(progressMonitor), assertNotNull(profiler), "handleGenericMetrics"));
					break;
				case DEPENDENT_METRICS:
					/* Handles the dependent metrics (i.e. dependency information) of one single module, based on the RID of the generic metrics step. */
					executeWithProfiling(() -> handleDependentMetrics(progressMonitor, parameters.phase).ifPresent(moduleUids::add),
							assertNotNull(profiler), "handleDependentMetrics");
					break;
				case TRANSITIVE_METRICS:
					executeWithProfiling(() -> handleDependentMetrics(progressMonitor, parameters.phase).ifPresent(moduleUids::add),
							assertNotNull(profiler), "handleTransitiveMetrics");
					break;
			}
		} finally {
			repoLocal.setQueryLock(false);
		}

		return new Result<>(moduleUids.toArray(new UUID[0]));
	}
	
	/*
	 * Handles the generic metrics of one source object and returns the RIDs of all modules that have been updated or added to the database.
	 * Don't use the IModuleRepository in the scope of this method!
	 */
	private List<UUID> handleGenericMetrics(final ProgressMonitor progressMonitor) {
		final List<ModelArtifact> modules = new ArrayList<>();
		final SourcePojo sourceObject = sourceService.findAny(q -> q.byId(assertNotNull(parameters.sourceObjectId))).orElse(null);
		
		if (sourceObject != null) {
			if (sourceObject.getTechnology() == Technology.JAVA) {
				sourceExportService.exportSources(getJobId(), parameters.projectId);
			}
			List<ModelArtifact> modelArtifacts;
			if (parameters.withExistingModelArtifacts) {
				modelArtifacts = loadExistingModelArtifacts();
			} else {
				modelArtifacts = executeWithProfiling(() -> collectResources(sourceObject, progressMonitor), assertNotNull(profiler), "collectResources");
			}
			final DiscoveryReferenceImporter referenceImporter = new DiscoveryReferenceImporter(moduleService, emptyMap(), getJobId(), discoveryCache, moduleParameters);
	
			final List<ModelArtifact> processedArtifacts = new ArrayList<>();
			final Map<ModelArtifact, ModelArtifact> processedCache = new HashMap<>();

			for (final ModelArtifact modelArtifact : modelArtifacts) {
				if ( ! processedArtifacts.contains(modelArtifact)) {
					processedArtifacts.add(modelArtifact);
					final Set<ModelArtifact> storedVirtualModules = new HashSet<>();
					for (final ModelArtifact virtualModule : modelArtifact.getVirtualModules()) {
						if ( ! processedArtifacts.contains(virtualModule)) {
							processedArtifacts.add(virtualModule);
							final ModelArtifact vModelArtifact = handleGenericMetricsForModelArtifact(virtualModule, referenceImporter, progressMonitor);
							if (vModelArtifact != null) {
								modules.add(vModelArtifact);
								storedVirtualModules.add(vModelArtifact);
							}
						} else {
							/* fixes an issue where some or all virtual modules of a physical ModelArtifact get lost if the
							 * virtual modules are processed before the physical */
							final ModelArtifact processedArtifact = processedCache.get(virtualModule);
							if (processedArtifact != null) {
								storedVirtualModules.add(processedArtifact);
							} else {
								final ModelArtifact vModelArtifact = handleGenericMetricsForModelArtifact(virtualModule, referenceImporter, progressMonitor);
								if (vModelArtifact != null) {
									modules.add(vModelArtifact);
									storedVirtualModules.add(vModelArtifact);
								}
								LOG.trace(() -> "Virtual ModuleArtifact was already processed but missing in processed cache " + virtualModule.getName());
							}
						}
					}

					modelArtifact.setVirtualModules(storedVirtualModules);
					final ModelArtifact processedArtifact = handleGenericMetricsForModelArtifact(modelArtifact, referenceImporter, progressMonitor);
					if (processedArtifact != null) {
						processedCache.put(modelArtifact, processedArtifact);
						modules.add(processedArtifact);
					}
				}
			}
		}

		return modules.stream()
						.filter(artifact -> contributorProvider.hasApplicableContributors(artifact, Phase.DEPENDENT_METRICS))
						.map(m -> assertNotNull(m.getModuleId()).getUid())
						.collect(Collectors.toList());
	}

	private List<ModelArtifact> collectResources(final SourcePojo sourceObject, final ProgressMonitor progressMonitor) {
		final Technology technology = sourceObject.getTechnology();
		/* Some resources like CSD are collected multiple times with different resolve targets. */
		final List<ResolveTarget> resolveTargets = new ArrayList<>();
		switch (technology) {
			case COBOL:
				resolveTargets.add(ResolveTarget.COBOL);
				break;
			case JCL:
				resolveTargets.add(ResolveTarget.JCL);
				break;
			case JAVA:
				resolveTargets.add(ResolveTarget.JAVA);
				break;
			case SQL:
				resolveTargets.add(ResolveTarget.SQL);
				break;
			case IMS:
				resolveTargets.add(ResolveTarget.IMS);
				break;
			case BINARY:
				resolveTargets.add(ResolveTarget.BINARY);
				break;
			case ASSEMBLER:
				resolveTargets.add(ResolveTarget.ASSEMBLER);
				break;
			case CSD:
				resolveTargets.add(ResolveTarget.CSD_LIST);
				resolveTargets.add(ResolveTarget.CSD_EXTRACT);
				break;
			case EASYTRIEVE:
				resolveTargets.add(ResolveTarget.EASYTRIEVE);
				break;
			case BASIC:
				resolveTargets.add(ResolveTarget.BASIC);
				break;
			case RESOURCE:
				resolveTargets.add(ResolveTarget.RESOURCE);
				break;
			case C:
				resolveTargets.add(ResolveTarget.C);
				break;
			case VMS:
				resolveTargets.add(ResolveTarget.DCL);
				resolveTargets.add(ResolveTarget.FMS_FORM);
				resolveTargets.add(ResolveTarget.IFDL_FORM);
				resolveTargets.add(ResolveTarget.VAX_MACRO);
				break;
			case ORACLE:
				resolveTargets.add(ResolveTarget.SQLMOD);
				break;
			case ECL:
				resolveTargets.add(ResolveTarget.ECL);
				break;
			case NATURAL:
				resolveTargets.add(ResolveTarget.NATURAL);
				break;
			case PL1:
				resolveTargets.add(ResolveTarget.PL1);
				break;
			case XML:
				resolveTargets.add(ResolveTarget.XML);
				break;
			case CICS: 
				resolveTargets.add(ResolveTarget.CICS);
				break;
			case CPP: 
				resolveTargets.add(ResolveTarget.CPP);
				break;
			default:
				throw new UnsupportedOperationException("Technology not yet supported with discover metrics: " + technology.name());
		}
		
		LOG.trace(() -> String.format("Collecting resources from %s. Resolve targets: %s", sourceObject.getPath(), resolveTargets));
		
		final List<ModelArtifact> modelArtifacts = new ArrayList<>();
		for (final ResolveTarget resolveTarget : resolveTargets) {
			final SourceObjectResourceCollector.Parameters collectParameters = new SourceObjectResourceCollector.Parameters.Builder()
					.setSourceObject(sourceObject)
					.setConfig(assertNotNull(config))
					.setResolveTarget(resolveTarget)
					.setSearchOrders(parameters.searchOrders)
					.setProgressMonitor(progressMonitor)
					.setJobId(getJobId())
					.setFeatureMap(parameters.featureMap)
					.setTaskSpan(assertNotNull(taskSpan)).build();
			modelArtifacts.addAll(resourceCollector.collect(collectParameters));
		}

		LOG.trace(() -> String.format("Collected resources from %s. Found artifacts: %s", sourceObject.getPath(),
				modelArtifacts.stream()
					.map(artifact -> String.format("[%s, %s, %s]", artifact.getModuleId(), artifact.getName(), artifact.getType()))
					.collect(Collectors.joining(", "))));
		
		return modelArtifacts;
	}

	private List<ModelArtifact> loadExistingModelArtifacts() {
		final List<LazyModelArtifact> existingArtifacts = modelArtifactService.find(b -> b.ofSource(Objects.requireNonNull(parameters.sourceObjectId)));
		return Stream.concat(existingArtifacts.stream(), existingArtifacts.stream().flatMap(artifact -> artifact.getVirtualModules().stream()))
				.collect(Collectors.toList());
	}
	
	@Nullable
	private ModelArtifact handleGenericMetricsForModelArtifact(final ModelArtifact modelArtifact, final DiscoveryReferenceImporter referenceImporter,
			final ProgressMonitor progressMonitor) {
		/* This method has to process utilities in the same way as any other type of module so don't return immediately if modelArtifact is a utility */
		ModelArtifact result = null;
		
		LOG.trace(() -> String.format("Collecting generic metrics for [%s, %s, %s]", modelArtifact.getModuleId(), modelArtifact.getName(), modelArtifact.getType()));
		
		final String lockKey = DiscoveryCache.createLockKeyForArtifact(modelArtifact);
		final ModelArtifact artifact;
		try {
			/* This task is processed highly parallel. Therefore it can for example happen that two tasks process separate Cobol modules that
			 * access the same database resource. We have to avoid creating the virtual resource for the table two times, which we can only
			 * achieve by locking and a lookup if it already exists.
			 * This approach is also required for incremental processing to properly operate on existing data. */
			discoveryCache.createLocks(getJobId(), lockKey);
			
			final ResolveTarget type = modelArtifact.getType();
			final Technology technology = ResolveTargetHelper.toTechnology(type);
			final Type moduleType = ResolveTargetHelper.toType(type);
			final ModelArtifact parent = modelArtifact.getParentModule();
			final String path = modelArtifact.getPath().orElse(null);
			final List<LazyModelArtifact> existingModelArtifacts = modelArtifactService.find(query -> { 
					/* no parent or path -> external Module: we need to find any existing Module with the given name, technology and type */
					query.ofProject(parameters.projectId)
						 .withName(modelArtifact.getName())
						 .withTechnology(technology)
						 .withType(moduleType);

					/* parent or path is present -> Module definition from source file: there should be either 0 or exactly 1 matching Module */
					if (path != null) {
						query.withPath(path);
					}

					if (parent != null) {
						if (parent.getModuleId() != null) {
							query.withSourceRelationshipsFrom(Objects.requireNonNull(parent.getModuleId()), CONTAINS);
						} else if (path == null && parent.getPath().isPresent()) {
							query.withPathsSelfOrContaining(parent.getPath().get(), false);
						}
					}
				});
			if (existingModelArtifacts.isEmpty()) {
				artifact = modelArtifact;
			} else if (existingModelArtifacts.size() == 1) {
				/* If this artifact already exists in the database, then we will re-use the existing one and update it. */
				artifact = existingModelArtifacts.get(0);
			} else if (path != null || (parent != null && parent.getModuleId() != null)) {
				/* multiple Modules have been declared within the same file or the same parent that have the same name, technology and type
				 -> currently we can't handle this, so error out */
				final StringBuilder errorMsg = new StringBuilder();
				errorMsg.append("Ambiguous module definition:");
				if (path != null) {
					errorMsg.append(" file ");
					errorMsg.append(path);
				}
				if (parent != null) {
					errorMsg.append(" parent module ");
					errorMsg.append(parent);
				}
				errorMsg.append(" contains the same module multiple times:");
				errorMsg.append(" name=");
				errorMsg.append(modelArtifact.getName());
				errorMsg.append(" technology=");
				errorMsg.append(technology);
				errorMsg.append(" type=");
				errorMsg.append(moduleType);
				throw new IllegalStateException(errorMsg.toString());
			} else {
				/* external Module found (no path or parent) that has already been declared elsewhere - skip processing this Module */
				return modelArtifact;
			}

			final Optional<ModuleLocation> modelArtifactLocation = modelArtifact.getLocation();
			if (modelArtifactLocation.isPresent() && ! artifact.getLocation().isPresent()) {
				artifact.setLocation(modelArtifactLocation.get());
			}

			if (artifact != modelArtifact) {
				/* When modelArtifact contains non persisted virtual modules then they would get lost.
				 * This causes that ContainsModule edges from BMS_MAPSETs to their BMS_MAPs would be missing */
				modelArtifact.getVirtualModules().forEach(artifact::addVirtualModule);
			}

			if ((artifact.getRepresentation() == Representation.PHYSICAL
					|| ! assertNotNull(config).getUtilityList().filterUtility(artifact.getName(), LOG, () -> this.getClass().getSimpleName()))) {
				LOG.debug(() -> String.format("Analyzing %s of type %s", artifact.getName(), artifact.getType().name()));

				if ( ! parameters.withExistingModelArtifacts) {
					/* no need to check if empty, if we are executing on pre-existing ModelArtifact */
					checkIsEmpty(artifact);
				}
				executeContributor(artifact, Phase.GENERIC_METRICS, assertNotNull(repo), progressMonitor);
				
				final EntityId moduleId;
				if (artifact instanceof LazyModelArtifact) {
					/* It will be a LazyModelArtifact when processing a module that already exists in the database. */
					moduleId = persistMetricsModelService.updateModuleAll(parameters.projectId, (LazyModelArtifact) artifact, referenceImporter, moduleParameters);
				} else {
					/* New module added to the database. */
					moduleId = persistMetricsModelService.importModule(parameters.projectId, artifact, moduleParameters);
					artifact.setModuleId(moduleId);
					artifact.setIdentification(Identification.IDENTIFIED);

					persistMetricsModelService.importGenericMetrics(parameters.projectId, artifact);

					/* The resource collector may have already created dependency information which we must import here, even though no explicit dependent
					 * metrics have actually been handled here. */
					persistMetricsModelService.importDependentMetrics(parameters.projectId, artifact, referenceImporter);
				}
				if (moduleId != null || parameters.withExistingModelArtifacts) {
					/* Only return something if we actually inserted/updated anything in the database. */
					result = artifact;
				}
			}
		} finally {
			discoveryCache.releaseLocks(getJobId(), lockKey);
		}
		
		return result;
	}
	
	/**
	 * @param progressMonitor the {@link ProgressMonitor}
	 * @param phase the {@link Phase}
	 * @return module rid if the current phase is for collecting dependent metrics and if the processed {@link ModelArtifact} is applicable for the
	 * transitive metrics phase. Otherwise {@code null}
	 */
	private Optional<UUID> handleDependentMetrics(final ProgressMonitor progressMonitor, final Phase phase) {
		final var artifact = modelArtifactService.findAny(b -> b.byUid(Objects.requireNonNull(parameters.moduleId)))
							.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with id: " + Objects.requireNonNull(parameters.moduleId)));

		if (artifact.getType().getLanguage() == ResolveTarget.JAVA) {
			sourceExportService.exportSources(getJobId(), parameters.projectId);
		}

		LOG.trace(() -> String.format("Collecting dependent metrics for [%s, %s, %s]", artifact.getModuleId(), artifact.getName(), artifact.getType()));
		executeContributor(artifact, phase, assertNotNull(repo), progressMonitor);

		final DiscoveryReferenceImporter referenceImporter = new DiscoveryReferenceImporter(moduleService, emptyMap(), getJobId(), discoveryCache, moduleParameters);

		/* update the main artifact first, then all modified virtual modules and finally all of the modified dependent artifacts */
		final List<String> keys = new ArrayList<>();
		final List<LazyModelArtifact> lazyModelArtifacts = new ArrayList<>();
		if (artifact.isModified()) {
			lazyModelArtifacts.add(artifact);
			keys.add(DiscoveryCache.createLockKeyForArtifact(artifact));
		}

		/* The contributor may have resolved and modified virtual modules. Therefore we must also update those if they're modified. */
		if (artifact.isResolvedVirtualModules()) {
			artifact.getVirtualModules().stream()
							.filter(LazyModelArtifact.class::isInstance)
							.map(LazyModelArtifact.class::cast)
							.filter(LazyModelArtifact::isModified)
							.forEach(virtual -> {
								lazyModelArtifacts.add(virtual);
								keys.add(DiscoveryCache.createLockKeyForArtifact(virtual, artifact));
							});
		}

		/* We have to update the processed module and every module that has been resolved by the module repository
		 * from the database and modified, so their changes are persisted back. */
		assertNotNull(repo).getModifiedModelArtifacts().stream()
								.filter(LazyModelArtifact.class::isInstance)
								.map(LazyModelArtifact.class::cast)
								.filter(LazyModelArtifact::isModified)
								.forEach(modified -> {
									lazyModelArtifacts.add(modified);
									keys.add(DiscoveryCache.createLockKeyForArtifact(modified));
								});

		/* Consistent ordering of all lock keys including the one of the main artifact is important. When every Task acquires the keys
		 * in the same order, no two tasks can deadlock here.
		 * For performance reasons set the parent for the virtual modules by ourself so it is not fetched from DB again */
		final List<String> sortedKeys = keys.stream()
												.sorted()
												.collect(Collectors.toList());

		final String jobId = getJobId();
		try {
			/* Different tasks may resolve the same module from the database (i.e. same included procedure).
			 * Therefore we must lock here, to ensure that there are never two separate tasks trying to update the same module at the same time.
			 * Otherwise we get random results.
			 * We now acquire all locks before storing to avoid deadlocks */
			discoveryCache.createLocks(jobId, sortedKeys);

			/* update the main artifact first, then all modified virtual modules and finally all of the modified dependent artifacts */
			lazyModelArtifacts.forEach(ma -> persistMetricsModelService.updateModuleAll(parameters.projectId, ma, referenceImporter, moduleParameters));
			
		} finally {
			discoveryCache.releaseLocks(jobId, sortedKeys);
		}

		if (phase == Phase.DEPENDENT_METRICS && contributorProvider.hasApplicableContributors(artifact, Phase.TRANSITIVE_METRICS)) {
			return Optional.of(parameters.moduleId);
		}

		return Optional.empty();
	}

	private void executeContributor(final ModelArtifact artifact, final Phase phase, final IModuleRepository repo, final ProgressMonitor progressMonitor) {
		final ContributorParameters contributorParams = new ContributorParameters.Builder()
				.setJobId(getJobId())
				.setModelArtifact(artifact)
				.setProjectId(parameters.projectId)
				.setConfig(assertNotNull(config))
				.setPhase(phase)
				.setSearchOrders(parameters.searchOrders)
				.setModuleRepository(repo)
				.setProgressMonitor(progressMonitor)
				.setTaskSpan(assertNotNull(taskSpan))
				.setFeatureMap(parameters.featureMap)
				.build();
		final List<MetricsContributor> contributors = contributorProvider.provideContributors(contributorParams);
		for (final MetricsContributor contributor : contributors) {
			try {
				switch (phase) {
					case GENERIC_METRICS:
						contributor.calculateGenericMetrics(artifact);
						break;
					case DEPENDENT_METRICS:
						contributor.calculateDependentMetrics(artifact);
						break;
					case TRANSITIVE_METRICS:
						contributor.calculateTransitiveMetrics(artifact);
						break;
					default:
						throw new IllegalArgumentException("Unsupported metrics phase: " + phase.name());
				}
			} catch (final Exception e) {
				handleError(e, artifact);
			} finally {
				contributor.close();
			}
		}
	}
	
	private void checkIsEmpty(final ModelArtifact artifact) {
		final Optional<String> path = artifact.getPath();
		if (path.isPresent()) {
			try {
				final SourcePojo sourceObject = sourceService.cachingByProjectPath(parameters.projectId.getNid(), path.get());
				final String content = sourceObject.getContent().toString();
				if (content.length() == 0) {
					LOG.warn(() -> "Content was empty when creating metrics for " + sourceObject.getName());

					artifact.addError(new ErrorMarker()
							.setWarningSeverity()
							.setKey(ErrorKey.EMPTY_FILE)
							.setCause("Found empty file: " + artifact.getName())
							.validate());
				}
			} catch (final Exception e) {
				handleError(e, artifact);
			}
		}
	}

	private void handleError(final Exception e, final ModelArtifact entry) {
		final String name = "[" + entry.getName() + ":" + entry.getPath().orElse(null) + "] ";
		final String message = e.getClass().getSimpleName() + ": " + e.getMessage();
		final String cause = e.getCause() != null ? " Cause: " + e.getCause().getMessage() : "";
		final String error = e.getMessage() == null ? name + " " + ExceptionUtils.getFullStackTrace(e) : name + message + cause;

		LOG.error(() -> error, e);
		LOG.debug(() -> ExceptionUtils.getFullStackTrace(e));
		
		entry.addError(new ErrorMarker()
				.setErrorSeverity()
				.setKey(ErrorKey.MODULE_ABORT)
				.setCause(error)
				.validate());
	}
	
	/**
	 * The parameters for the {@link MetricsCollectorTask}.
	 */
	public static class Parameters implements Serializable {

		private final EntityId projectId;
		@Nullable
		private final EntityId sourceObjectId;
		@Nullable
		private final UUID moduleId;
		private final Phase phase;
		private final SearchOrders searchOrders;
		private final Map<FeatureId, Boolean> featureMap;
		private final boolean withExistingModelArtifacts;

		private Parameters(final Builder builder) {
			this.projectId = assertNotNull(builder.projectId);
			this.phase = assertNotNull(builder.phase);
			this.featureMap = assertNotNull(builder.featureMap);
			this.sourceObjectId = phase == Phase.GENERIC_METRICS ?
						assertNotNull(builder.sourceObjectId, "SourcePojo Id required for " + phase.name()) : builder.sourceObjectId;
			this.moduleId = phase == Phase.DEPENDENT_METRICS || phase == Phase.TRANSITIVE_METRICS
					? assertNotNull(builder.moduleId, "Module RID required for " + phase.name()) 
					: builder.moduleId;
			this.searchOrders = assertNotNull(builder.searchOrders);
			this.withExistingModelArtifacts = builder.withExistingModelArtifacts;
		}
		
		/**
		 * Builder for the {@link Parameters} of the {@link MetricsCollectorTask}.
		 */
		public static class Builder extends AbstractBuilder<Parameters, Builder> {
			
			@Nullable
			private EntityId projectId;
			@Nullable
			private EntityId sourceObjectId;
			@Nullable
			private UUID moduleId;
			@Nullable
			private Phase phase;
			@Nullable
			private SearchOrders searchOrders;
			@Nullable
			private Map<FeatureId, Boolean> featureMap;
			private boolean withExistingModelArtifacts;

			/**
			 * Sets the project Id.
			 * 
			 * @param projectId the project Id
			 * @return this builder instance
			 */
			public Builder setProjectId(final EntityId projectId) {
				this.projectId = projectId;
				return getThis();
			}
			
			/**
			 * Sets the Id of the {@link SourcePojo} to process when using {@link Phase#GENERIC_METRICS}.
			 *
			 * @param sourceObjectId the source object Id
			 * @return this builder instance
			 */
			public Builder setSourceObjectId(final EntityId sourceObjectId) {
				this.sourceObjectId = sourceObjectId;
				return getThis();
			}
			
			/**
			 * Sets the UUID of the {@code module} to process when using {@link Phase#DEPENDENT_METRICS} or {@link Phase#TRANSITIVE_METRICS}.
			 *
			 * @param moduleId the module id
			 * @return this builder instance
			 */
			public Builder setModule(final UUID moduleId) {
				this.moduleId = moduleId;
				return getThis();
			}
			
			/**
			 * Sets the {@link Phase} in which either the {@link SourcePojo} set with {@link #setSourceObjectId(EntityId)}
			 * or the {@link ModelArtifact} set with {@link #setModule(UUID)} is processed.
			 *
			 * @param phase the {@link Phase}
			 * @return this builder instance
			 */
			public Builder setPhase(final Phase phase) {
				this.phase = phase;
				return getThis();
			}
			
			/**
			 * Sets the {@link SearchOrders} if required.
			 * 
			 * @param searchOrders the {@link SearchOrders}
			 * @return this builder instance
			 */
			public Builder setSearchOrders(final SearchOrders searchOrders) {
				this.searchOrders = searchOrders;
				return getThis();
			}

			/**
			 * Sets the map containing feature settings.
			 * 
			 * @param featureMap the feature map
			 * @return this builder instance
			 */
			public Builder setFeatureMap(final Map<FeatureId, Boolean> featureMap) {
				this.featureMap = featureMap;
				return getThis();
			}

			/**
			 * When {@code true} does not execute resource collection on the source object
			 * and instead looks for already existing ModelArtifacts (created by DAWN contributors).
			 *
			 * @param withExistingModelArtifacts {@code true} to skip resource collection
			 * @return this builder instance
			 */
			public Builder setWithExistingModelArtifacts(final boolean withExistingModelArtifacts) {
				this.withExistingModelArtifacts = withExistingModelArtifacts;
				return getThis();
			}

			@Override
			protected Builder reset() {
				projectId = null;
				sourceObjectId = null;
				moduleId = null;
				phase = null;
				searchOrders = null;
				featureMap = null;
				withExistingModelArtifacts = false;
				return getThis();
			}

			@Override
			protected Parameters internalBuild() {
				return new Parameters(this);
			}
			
		}
	}

}

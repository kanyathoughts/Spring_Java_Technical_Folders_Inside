/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics;

import static innowake.mining.shared.PatternConverter.convertAntToRegexPattern;
import static java.util.Objects.requireNonNull;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.function.BiConsumer;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.regex.Pattern;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.io.FilenameUtils;

import com.google.common.collect.Sets;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.Logging;
import innowake.mining.data.access.ModelArtifactService;
import innowake.mining.data.discovery.metrics.IModuleRepository;
import innowake.mining.data.discovery.metrics.MetricsContributor;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.data.io.LazyModelArtifact;
import innowake.mining.data.model.discovery.ErrorMarker;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.server.util.ModuleFilterUtil;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.config.searchorder.SearchOrder;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.discovery.config.searchorder.Target;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.mining.shared.model.discovery.ResolveTargetHelper;

/**
 * Special {@link IModuleRepository} implementation that directly operates on the data contained in the database.
 * <p>
 * <b>Note:</b> Every task must have its own instance of this.
 */
class DbModuleRepository implements IModuleRepository {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.DISCOVERY_MODULE_REPO);
	public static final String SIMILAR_NAME_ENTRY = "SIMILAR_NAME_ENTRY";

	private final EntityId projectId;
	private final ModelArtifactService modelArtifactService;
	private final String jobId;
	/* Holds all ModelArtifact instances that have been resolved by this repo, so that their respective DB records can be updated if necessary. */
	private final Map<String, Object> resolvedModelArtifacts = new LinkedHashMap<>();
	private final ArtifactCacheService artifactCacheService;
	
	private final SearchOrders searchOrders;
	
	private final ModuleFilterUtil moduleFilterUtil;
	
	private boolean queryLock;
	
	/**
	 * Constructor.
	 * 
	 * @param projectId the {@link EntityId} of the project
	 * @param modelArtifactService the {@link ModelArtifactService}
	 * @param jobId the id of the job for which this instance was created
	 * @param artifactCacheService the {@link ArtifactCacheService}
	 * @param searchOrders the {@link SearchOrders} of the project
	 * @param moduleFilterUtil {@link ModuleFilterUtil}
	 */
	DbModuleRepository(
			final EntityId projectId, 
			final ModelArtifactService modelArtifactService,
			final String jobId, 
			final ArtifactCacheService artifactCacheService,
			final SearchOrders searchOrders,
			final ModuleFilterUtil moduleFilterUtil) {
		
		this.projectId = projectId;
		this.modelArtifactService = modelArtifactService;
		this.jobId = jobId;
		this.artifactCacheService = artifactCacheService;
		this.searchOrders = searchOrders;
		this.moduleFilterUtil = moduleFilterUtil;
	}
	
	/**
	 * With the query lock set, all functions returning module artifacts from the repository will throw an IllegalStateException when called.<br>
	 * As during the initial collection of modules the order in which they are collected is not guaranteed, no attempts to resolve dependencies should be made 
	 * then.<br>
	 * See also {@link MetricsContributor}
	 *
	 * @param queryLock if the lock shall be in effect
	 */
	void setQueryLock(final boolean queryLock) {
		this.queryLock = queryLock;
	}
	
	private void checkQueryLock() {
		if (queryLock) {
			throw new IllegalStateException("Querying artifacts from the repository is not allowed at this point");
		}
	}

	@Override
	public Iterator<ModelArtifact> iterator() {
		/* Explicitly not doing any caching here, when reading everything for a project. */
		return modelArtifactService.find(b -> b.ofProject(projectId)).stream().map(ModelArtifact.class::cast).iterator();
	}

	@Override
	public List<ModelArtifact> getLanguageEntries(final ResolveTarget... types) {
		checkQueryLock();
		
		final StringBuilder key = new StringBuilder(200);
		for (final ResolveTarget type : types) {
			key.append(type.name());
		}
		
		final List<ModelArtifact> artifacts = getModelArtifacts(key.toString(), () -> {
			final List<Technology> technologies = new ArrayList<>();
			for (final ResolveTarget type : types) {
				technologies.add(ResolveTargetHelper.toTechnology(type));
			}

			return modelArtifactService.find(b -> b.ofProject(projectId).withTechnologies(technologies));
		});
		
		LOG.trace(() -> String.format("getLanguageEntries by types: types=%s. %s", Arrays.toString(types), getDebugArtifactDetails(artifacts)));
		
		return artifacts;
	}
	
	@Override
	public int size() {
		throw new UnsupportedOperationException();
	}

	@Override
	public Optional<ModelArtifact> getPhysicalEntry(final String path) {
		checkQueryLock();

		final Optional<ModelArtifact> artifact = getModelArtifact(path, 
				() -> modelArtifactService.findAny(b -> b.ofProject(projectId)
														 .withPath(path, true)
														 .withRepresentation(Representation.PHYSICAL)), true);
		if (artifact.isEmpty()) {
			throw new MiningEntityNotFoundException(
					String.format("Module not found for path=%s (case insensitive), representation=PHYSICAL, project=%s", path, projectId));
		}
		LOG.trace(() -> String.format("getPhysicalEntry by path: path=%s. %s", path, getDebugArtifactDetails(artifact)));
		return artifact;
	}
	
	@Override
	public Optional<ModelArtifact> getEntry(final ModelArtifact source, final String name, final ResolveTarget... types) {
		return getEntry(source, Optional.empty(), name, types);
	}

	@Override
	public Optional<ModelArtifact> getEntry(
			final ModelArtifact source, 
			final Optional<String> searchPathOverride, 
			final String name, 
			final ResolveTarget... types) {
		
		checkQueryLock();
		
		final StringBuilder key = new StringBuilder(200);
		key.append(name);
		for (final ResolveTarget type : types) {
			key.append(type.name());
		}

		final List<ModelArtifact> resolvedArtifacts = getModelArtifacts(key.toString(), () -> 
				modelArtifactService.find(b -> b.ofProject(projectId)
												.withName(name, true)
												.withTechnologiesAndTypes(target2TechnologyAndType(types))));
		if (resolvedArtifacts.isEmpty()) {
			return Optional.empty();
		}

		if (searchPathOverride.isPresent()) {
			/* if searchPathOverride is present, look for the target at the given search path first, irrespective of configured SearchOrders */
			final Optional<ModelArtifact> resolvedArtifact = findMatchByPath(resolvedArtifacts, convertAntToRegexPattern(searchPathOverride.get()), source, name, types);
			if (resolvedArtifact.isPresent()) {
				return resolvedArtifact;
			}
		}
		
		final Optional<String> contextPath = source.getPath().isPresent() ? source.getPath() : source.getParentPath();
		
		if (contextPath.isPresent()) {
			/* when the source module has a path we perform the lookup of the requested artifact according to the configured SearchOrders */
			final List<SearchOrder> matchingSearchOrders = searchOrders.getSearchOrdersList().stream()
					.filter(searchOrder -> moduleFilterUtil.matches(moduleFilterUtil.toModuleFilter(searchOrder.getSource(), contextPath), source))
					.collect(Collectors.toList());
			
			if (matchingSearchOrders.isEmpty()) {
				LOG.debug(() -> String.format("No SearchOrder matches %s while resolving dependency name=%s, types=%s for artifact %s",
						contextPath.get(), name, Arrays.toString(types), getDebugArtifactDetails(Optional.of(source))));
				return Optional.empty();
			}
			
			for (final SearchOrder searchOrder : matchingSearchOrders) {
				final Optional<ModelArtifact> resolveArtifactBySearchOrder = resolveArtifactBySearchOrder(resolvedArtifacts, source, contextPath.get(),
						searchOrder, name, types);
				LOG.trace(() -> String.format("getEntry by name and types and source pattern: name=%s, types=%s, pattern=%s. %s", name, Arrays.toString(types),
						searchOrder.getSourcePattern(), getDebugArtifactDetails(resolveArtifactBySearchOrder)));
				if (resolveArtifactBySearchOrder.isPresent()) {
					return resolveArtifactBySearchOrder;
				}
			}
			return Optional.empty();
		} else {
			/* when the source module has no path, we fall back to searching anywhere -- if multiple modules with the same name and type exist in the project,
			 * this will fail */
			final Optional<ModelArtifact> artifact = resolvedArtifacts.size() == 1 ?
														Optional.of(resolvedArtifacts.get(0)) : 
														resolvedArtifacts.stream().collect(toOptional(source, name, types));
			LOG.trace(() -> String.format("getEntry by name and types: name=%s, types=%s. %s", name, Arrays.toString(types), getDebugArtifactDetails(artifact)));
			return artifact;
		}
	}

	private Optional<ModelArtifact> resolveArtifactBySearchOrder(
			final List<ModelArtifact> resolvedArtifacts,
			final ModelArtifact source, 
			final String contextPath,
			final SearchOrder searchOrder,
			final String name, 
			final ResolveTarget[] types) {
		
		for (final Target target : searchOrder.getTargets()) {
		    final ModuleFilter moduleFilter = moduleFilterUtil.toModuleFilter(target, Optional.of(contextPath));
		    final Optional<ModelArtifact> resolvedArtifact = resolvedArtifacts.stream()
		        .filter(artifact -> moduleFilterUtil.matches(moduleFilter, artifact))
		        .collect(toOptional(source, name, types));
		 
		    if (resolvedArtifact.isPresent()) {
		      return resolvedArtifact;
		    }
		  }
		 
		  return Optional.empty();
	}

	private Optional<ModelArtifact> findMatchByPath(final List<ModelArtifact> resolvedArtifacts, final String pathRegEx,
			final ModelArtifact source, final String name, final ResolveTarget[] types) {
		try {
			final Pattern pattern = artifactCacheService.pathPatternCache.get(pathRegEx);
			return resolvedArtifacts.stream()
					.filter(artifact -> {
						/* Before we called IoDao.findByNamePathTechnologyAndType() which searches for modules whose path OR contains module path matches */
						Optional<String> path = artifact.getPath();
						if (path.isPresent() && pattern.matcher(path.get()).matches()) {
							return true;
						}

						path = artifact.getParentPath();
						return path.isPresent() && pattern.matcher(path.get()).matches();
						
					})
					.collect(toOptional(source, name, types));
		} catch (final ExecutionException e) {
			throw new IllegalStateException(e);
		}
 	}

	private static List<Tuple2<Technology, Type>> target2TechnologyAndType(final ResolveTarget... targets) {
		final List<Tuple2<Technology, Type>> technologyTypePairs = new ArrayList<>();
		for (final ResolveTarget target : targets) {
			final Technology technology = ResolveTargetHelper.toTechnology(target);
			final Type type = ResolveTargetHelper.toType(target);
			technologyTypePairs.add(Tuple2.of(technology, type));
		}

		return technologyTypePairs;
	}

	@Override
	public List<ModelArtifact> getEntries(final String name) {
		checkQueryLock();
		final List<ModelArtifact> artifacts = getModelArtifacts(name, () -> modelArtifactService.find(b -> b.ofProject(projectId)
																											.withName(name, true)));
		LOG.trace(() -> String.format("getEntries by name: name=%s. %s", name, getDebugArtifactDetails(artifacts)));
		return artifacts;
	}

	@Override
	public List<ModelArtifact> getEntries(final String name, final ResolveTarget... types) {
		checkQueryLock();

		final StringBuilder key = new StringBuilder(200);
		key.append(name);
		for (final ResolveTarget type : types) {
			key.append(type.name());
		}

		final List<ModelArtifact> artifacts = getModelArtifacts(key.toString(), () ->
													modelArtifactService.find(b -> b.ofProject(projectId)
																					.withName(name, true)
																					.withTechnologiesAndTypes(target2TechnologyAndType(types))));

		LOG.trace(() -> String.format("getEntries by name and types: name=%s, types=%s. %s", name, Arrays.toString(types), getDebugArtifactDetails(artifacts)));

		return artifacts;
	}
	
	@Override
	public List<ModelArtifact> getEntries(final ModelArtifact artifact, final ResolveTarget... types) {
		checkQueryLock();

		final StringBuilder key = new StringBuilder(200);
		key.append("ContainsModules:");
		key.append(artifact.getModuleId());
		for (final ResolveTarget type : types) {
			key.append(type.name());
		}

		/* Get the modules for 'types' that are contained by 'artifact' */
		final List<ModelArtifact> artifacts = getModelArtifacts(key.toString(), () ->
													modelArtifactService.find(b -> b.withSourceRelationshipsFrom(requireNonNull(artifact.getModuleId()), RelationshipType.CONTAINS)
																					.withTechnologiesAndTypes(target2TechnologyAndType(types))));

		LOG.trace(() -> String.format("getEntries by artifact and types: artifact=%s, types=%s. %s", artifact, Arrays.toString(types),
				getDebugArtifactDetails(artifacts)));

		return artifacts;
	}

	@Override
	public List<ModelArtifact> getEntries(final ResolveTarget... types) {
		checkQueryLock();
		
		final StringBuilder key = new StringBuilder(types.length * 20);
		for (final ResolveTarget type : types) {
			key.append(type.name());
		}

		final List<ModelArtifact> artifacts = getModelArtifacts(key.toString(), () ->
													modelArtifactService.find(b -> b.ofProject(projectId)
																					.withTechnologiesAndTypes(target2TechnologyAndType(types))));
		LOG.trace(() -> String.format("getEntries by types: types=%s. %s", Arrays.toString(types), getDebugArtifactDetails(artifacts)));
		return artifacts;
	}

	@Override
	public List<ModelArtifact> getEntriesByFilename(final String filename) {
		checkQueryLock();
		final List<ModelArtifact> artifacts = getModelArtifacts(filename, () -> 
													modelArtifactService.find(b -> b.ofProject(projectId)
																					.withName(FilenameUtils.getBaseName(filename), true)).stream()
				.filter(artifact -> artifact.getPath().isPresent() && FilenameUtils.getName(artifact.getPath().get()).equalsIgnoreCase(filename))
				.collect(Collectors.toList()));
		LOG.trace(() -> String.format("getEntries by filename: filename=%s. %s", filename, getDebugArtifactDetails(artifacts)));
		return artifacts;
	}

	@Override
	public Optional<ModelArtifact> getEntry(final ModelArtifact sourceArtifact, final String name, final String path, final ResolveTarget... types) {
		checkQueryLock();
		
		final StringBuilder key = new StringBuilder(200);
		key.append(name).append(path);
		for (final ResolveTarget type : types) {
			key.append(type.name());
		}

		final Optional<ModelArtifact> artifact = getModelArtifact(key.toString(), () -> 
													modelArtifactService.find(b -> b.ofProject(projectId)
																					.withName(name, true)
																					.withPathsSelfOrContaining(path, true)
																					.withTechnologiesAndTypes(target2TechnologyAndType(types))).stream()
				.collect(toOptional(sourceArtifact, name, types)), true);

		LOG.trace(() -> String.format("getEntry by name, path and types: name=%s, path=%s types=%s. %s", name, path, Arrays.toString(types),
				getDebugArtifactDetails(artifact)));
		return artifact;
	}

	@Override
	public Optional<ModelArtifact> getEntry(final EntityId moduleId) {
		checkQueryLock();
		final Optional<ModelArtifact> artifact = getModelArtifact(moduleId.toString(),() -> modelArtifactService.findAny(b -> b.byId(moduleId)), true);
		if (artifact.isEmpty()) {
			throw new MiningEntityNotFoundException("Module not found for id:" + moduleId);
		}
		LOG.trace(() -> String.format("getEntry by module id: %s. %s", moduleId.toString(), getDebugArtifactDetails(artifact)));
		return artifact;
	}

	@Override
	public List<ModelArtifact> getReferences(final EntityId moduleId, final RelationshipType relationship, final RelationshipDirection direction) {
		checkQueryLock();

		final List<ModelArtifact> artifacts = getModelArtifacts(moduleId + relationship.name() + direction.toString(),
																() -> modelArtifactService.find(q -> {
																	switch (direction) {
																		case IN:
																			q.withSourceRelationshipsFrom(moduleId, relationship);
																			break;
																		case OUT:
																			q.withDestinationRelationshipsTo(moduleId, relationship);
																			break;
																		case BOTH: 
																			throw new IllegalArgumentException("Unhandled direction: " + direction);
																	}
																}));
		LOG.trace(() -> String.format("getReferences by id, relationship and direction: id=%s, relationship=%s, direction=%s. %s", 
								moduleId, relationship.name(), direction.toString(), getDebugArtifactDetails(artifacts)));
		return artifacts;
	}

	@Override
	public Optional<ModelArtifact> getSimilarNameEntry(final ModelArtifact sourceArtifact, final String namePattern, final ResolveTarget type) {
		final var artifacts = getModelArtifacts(SIMILAR_NAME_ENTRY + namePattern + type,
												() -> modelArtifactService.find(q -> q.ofProject(projectId)
																						.withNameLike(namePattern)
																						.withTechnology(ResolveTargetHelper.toTechnology(type))
																						.withType(ResolveTargetHelper.toType(type))));
		if (artifacts.isEmpty()) {
			return Optional.empty();
		}
		if (artifacts.size() > 1) {
			final String errorMessage = "Multiple possible candidates found: " + artifacts.stream().map(ModelArtifact::getModuleId).collect(Collectors.toList());
			sourceArtifact.addError(new ErrorMarker().setCause(errorMessage).setErrorSeverity().setKey(ErrorKey.UNDISCOVERED_DEPENDENCY).validate());
			return Optional.empty();
		}
		return Optional.of(artifacts.get(0));
	}

	/**
	 * Returns all {@link ModelArtifact} instances that have been resolved from the database.
	 * 
	 * @return all resolved {@link ModelArtifact} instances
	 */
	public List<ModelArtifact> getResolvedModelArtifacts() {
		return getAllArtifacts(null);
	}

	/**
	 * Returns all {@link ModelArtifact} instances that have been resolved from the database and which have been modified.
	 * <p>Only instances of {@linkplain LazyModelArtifact} know whether they have been modified or not. Therefore all {@link ModelArtifact}
	 * that are not an instance of {@linkplain LazyModelArtifact} are returned as well, even if they have not been modified.</p>
	 * 
	 * @return all resolved and modified {@link ModelArtifact} instances
	 */
	public List<ModelArtifact> getModifiedModelArtifacts() {
		return getAllArtifacts(artifact -> ! (artifact instanceof LazyModelArtifact) || ((LazyModelArtifact) artifact).isModified());
	}

	public static <T extends ModelArtifact> Collector<T, Set<T>, Optional<T>> toOptional(final ModelArtifact sourceEntry, final String name,
			final ResolveTarget... types) {
		return new Collector<T, Set<T>, Optional<T>>() {

			@Override
			public Supplier<Set<T>> supplier() {
				return HashSet::new;
			}

			@Override
			public BiConsumer<Set<T>, T> accumulator() {
				return (accumulator, element) -> accumulator.add(element);
			}

			@Override
			public BinaryOperator<Set<T>> combiner() {
				return Sets::union;
			}

			@Override
			public Function<Set<T>, Optional<T>> finisher() {
				return accumulator -> {
					if (accumulator.size() == 1) {
						return Optional.of(accumulator.iterator().next());
					} else if (accumulator.isEmpty()) {
						return Optional.empty();
					} else {
						if (types.length == 0) {
							addModelError(sourceEntry, name, accumulator, types);
							return Optional.empty();
						}

						/*
						 * When multiple artifacts found, it will be matched with the types in order and if multiple artifacts found for any type, ModelError
						 * will be added
						 */
						for (final ResolveTarget type : types) {
							final List<T> matchingArtifacts =
									accumulator.stream().filter(artifact -> artifact.getTypeUnchecked() == type).collect(Collectors.toList());
							if (matchingArtifacts.size() == 1) {
								return Optional.of(matchingArtifacts.get(0));
							} else if (matchingArtifacts.size() > 1) {
								/* Since OrientDB gets the modules names with case insensitive we need to filter the module with exact name */
								final List<T> artifactsWithExactName = matchingArtifacts.stream().filter(artifact -> artifact.getName().equals(name))
										.collect(Collectors.toList());
								if (artifactsWithExactName.size() == 1) {
									return Optional.of(artifactsWithExactName.get(0));
								}
								addModelError(sourceEntry, name, accumulator, types);
								return Optional.empty();
							}
						}
					}
					return Optional.empty();
				};
			}

			private void addModelError(final ModelArtifact sourceEntry, final String name, final Set<T> accumulator, final ResolveTarget... types) {

				/* more than one matching artifact found - we can't figure out which is the correct one, so we set an error */
				final List<String> uids = accumulator.stream().map(entry -> requireNonNull(entry.getModuleId()).getNid().toString()).collect(Collectors.toList());

				final String errorMessage = String.format(ERROR_MULTIPE_CANDIDATES, name, Arrays.toString(types), uids);
				sourceEntry.addError(new ErrorMarker().setCause(errorMessage).setErrorSeverity().setKey(ErrorKey.UNDISCOVERED_DEPENDENCY).validate());
			}

			@Override
			public Set<Characteristics> characteristics() {
				return EnumSet.of(Collector.Characteristics.UNORDERED);
			}
		};
	}
	
	@SuppressWarnings("unchecked")
	private List<ModelArtifact> getAllArtifacts(@Nullable final Predicate<ModelArtifact> filter) {
		checkQueryLock();

		final Stream<ModelArtifact> artifacStream = resolvedModelArtifacts.values().stream()
				.flatMap(value -> {
					if (value instanceof List) {
						return ((List<ModelArtifact>) value).stream();
					}
					return Stream.of(value);
				})
				.map(ModelArtifact.class::cast);
		
		return (filter == null ? artifacStream : artifacStream.filter(filter))
					.collect(Collectors.toList());
	}

	@SuppressWarnings("unchecked")
	private List<ModelArtifact> getModelArtifacts(final String key, final Supplier<List<LazyModelArtifact>> artifactsSupplier) {
		if (artifactCacheService.maxCacheSize == 0) {
			/* We cache the results to avoid querying the database again for the same key. Every task has its own instance of this repository. */
			return (List<ModelArtifact>) resolvedModelArtifacts.computeIfAbsent(key, k -> artifactsSupplier.get());
		}

		return (List<ModelArtifact>) resolvedModelArtifacts.computeIfAbsent(key, k -> {
			/* We cache the results to avoid querying the database again for the same key. Every task has its own instance of this repository. */
			final List<LazyModelArtifact> resolvedArtifacts;
			try {
				resolvedArtifacts = (List<LazyModelArtifact>) artifactCacheService.cache.get(key + "@" + jobId, artifactsSupplier::get);
			} catch (final ExecutionException e) {
				throw new IllegalStateException(e);
			}
			return resolvedArtifacts.stream()
					.map(LazyModelArtifact::new)
					.collect(Collectors.toList());
		});
	}
	
	@SuppressWarnings("unchecked")
	private Optional<ModelArtifact> getModelArtifact(final String key, final Supplier<Optional<LazyModelArtifact>> artifactSupplier, final boolean addToCacheIfAbsent) {
		if (artifactCacheService.maxCacheSize == 0) {
			/* We cache the result to avoid querying the database again for the same key. Every task has its own instance of this repository. */
			return Optional.ofNullable((ModelArtifact) resolvedModelArtifacts.computeIfAbsent(key, k -> artifactSupplier.get().orElse(null)));
		}

		return (Optional<ModelArtifact>) (Optional<?>) Optional.ofNullable(resolvedModelArtifacts.computeIfAbsent(key, k -> {
			/* We cache the result to avoid querying the database again for the same key. Every task has its own instance of this repository. */
			final Optional<LazyModelArtifact> resolvedArtifact;
			try {
				resolvedArtifact = getCachedArtifact(key, artifactSupplier, addToCacheIfAbsent);
			} catch (final ExecutionException e) {
				throw new IllegalStateException(e);
			}
			return resolvedArtifact.map(LazyModelArtifact::new).orElse(null);
		}));
	}
	
	/**
	 * Retrieves an artifact from the lookup cache. If the artifact is not in the cache, it will be looked up using the provided supplier.
	 * <p>
	 * If {@code addToCacheIfAbsent} is {@code true} then the result of the lookup is added to the cache, even if the supplier returns {@code Optional.empty()}.
	 * Thus a "negative" lookup result is cached. If this is not desired, set {@code addToCacheIfAbsent} to {@code false}.
	 *
	 * @param key the lookup key
	 * @param artifactSupplier the supplier which supplies the (optional) artifact if it is not in the cache
	 * @param addToCacheIfAbsent add the artifact from the supplier to the cache even if it is empty
	 * @return the artifact that was retrieved from cache or from the supplier
	 * @throws ExecutionException if the supplier throws an exception
	 */
	@SuppressWarnings("unchecked")
	private Optional<LazyModelArtifact> getCachedArtifact(final String key, final Supplier<Optional<LazyModelArtifact>> artifactSupplier,
			final boolean addToCacheIfAbsent) throws ExecutionException {
		if (addToCacheIfAbsent) {
			return (Optional<LazyModelArtifact>) artifactCacheService.cache.get(key + "@" + jobId, artifactSupplier::get);
		} else {
			Optional<LazyModelArtifact> artifact = (Optional<LazyModelArtifact>) artifactCacheService.cache.getIfPresent(key + "@" + jobId);
			if (artifact == null) {
				artifact = artifactSupplier.get();
				if (artifact.isPresent()) {
					artifactCacheService.cache.put(key, artifact);
				}
			}
			return artifact;
		}
	}

	private String getDebugArtifactDetails(final List<ModelArtifact> artifacts) {
		if (artifacts.isEmpty()) {
			return "no entries found";
		}
	
		final StringBuilder sb = new StringBuilder();
		artifacts.forEach(a -> sb.append(String.format("%n\tFound [%s,%s,%s]", a.getModuleId(), a.getNameUnchecked(), a.getTypeUnchecked())));
		return sb.toString();
	}
	
	private String getDebugArtifactDetails(final Optional<ModelArtifact> artifact) {
		return artifact.map(a -> String.format("Found [%s,%s,%s]", a.getModuleId(), a.getNameUnchecked(), a.getTypeUnchecked())).orElse("not found");
	}
}

/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.datapoints.registry;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import javax.persistence.EntityNotFoundException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;
import org.springframework.util.ClassUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.util.collection.Pair;
import innowake.mining.data.datapoints.MiningDataPointBuilder;
import innowake.mining.data.datapoints.MiningDataPointSource;
import innowake.mining.data.datapoints.builder.MiningDataPointBuilderImpl;
import innowake.mining.data.datapoints.builder.MiningDataPointDefinitionWithCustomFetch;
import innowake.mining.data.event.DataPointRegistryUpdatedEvent;
import innowake.mining.data.event.MiningDataPointSourceInvalidatedEvent;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinitionWithPath;
import innowake.mining.shared.datapoints.definition.MiningDataTypeDefinition;
import innowake.mining.shared.datapoints.definition.MiningEnumDefinition;
import innowake.mining.shared.entities.ProjectPojo;

/**
 * Central registry for mining data points. Data points can be contributed to the registry by implementing {@link MiningDataPointSource} on a spring bean.
 */
@Service
public class DataPointRegistry {

	private final Logger log = LoggerFactory.getLogger(this.getClass());

	private List<MiningDataPointSource> dataPointSources;
	private final AtomicReference<RegistryView> registry = new AtomicReference<>(new RegistryView());

	private final ApplicationEventPublisher eventPublisher;

	private final ProjectService projectService;

	/**
	 * Initializes the data point registry.
	 * @param eventPublisher An {@link ApplicationEventPublisher} to distribute notifications about changes in the registry.
	 * @param projectService Access to Project specific Data.
	 */
	@Autowired
	public DataPointRegistry(final ApplicationEventPublisher eventPublisher, final ProjectService projectService) {
		this.dataPointSources = Collections.emptyList();
		this.eventPublisher = eventPublisher;
		this.projectService = projectService;
	}
	
	/**
	 * Calls {@link MiningDataPointSource#provideDataPoints(MiningDataPointBuilder)} on all registered data point sources in order to collect
	 * data points during startup.
	 * @param event Event providing the ApplicationContext to scan for {@link MiningDataPointSource} Beans.
	 */
	@EventListener
	public void initializeDataPointSources(final ContextRefreshedEvent event) {
		initializeDataPointSources(event.getApplicationContext().getBeanProvider(MiningDataPointSource.class).orderedStream().collect(Collectors.toList()));
		
	}
	
	/**
	 * Initialize the registry with specific Data Point sources.
	 * This method is intended for testing purposes.
	 * Normally all {@link MiningDataPointSource}s in an ApplicationContext will be discovered automatically. 
	 * @param sources Components providing Data Points.
	 */
	public void initializeDataPointSources(final List<MiningDataPointSource> sources) {
		this.dataPointSources = sources;
		log.debug(() -> "Initialized with providers:\n" + 
				dataPointSources.stream().map(s -> ClassUtils.getUserClass(s.getClass()).getName()).collect(Collectors.joining("\n")));
		collectDataPoints(dataPointSources, registry.get(), Optional.empty());
		raiseUpdate(Optional.empty());
	}
	
	@EventListener
	public synchronized void onDataPointSourceInvalidated(final MiningDataPointSourceInvalidatedEvent event) {
		if (dataPointSources.stream().anyMatch(e -> e.toString().equals(event.getDataPointSource().toString()))) {
			final String providerName = ClassUtils.getUserClass(event.getDataPointSource().getClass()).getName();
			final var eventProjectId = event.getProjectId();
			final Optional<Long> projectId = eventProjectId.map(e -> e.hasNid() ? e.getNid() : projectService.getNid(e));
			final RegistryView tempRegistry = this.registry.get().clone(Optional.of(providerName), projectId);
			if ( ! eventProjectId.isPresent()) {
				log.debug(() -> "re-collecting data points for provider " + providerName + " on all projects");
				collectDataPoints(Collections.singletonList(event.getDataPointSource()), tempRegistry, projectId);
			} else {
				try {
					final Optional<ProjectPojo> project = projectService.find(eventProjectId.get());
					if (project.isEmpty()) {
						log.debug(() -> "Project " + projectId.get() + " not found. Not re-collecting data points for provider " + providerName + " on this project");
					} else if (project.get().isMarkedDeleted()) {
						log.debug(() -> "Project " + projectId.get() + " is marked for deletion. Not re-collecting data points for provider " + providerName + " on this project");
					} else {
						log.debug(() -> "re-collecting data points for provider " + providerName + " on project " + projectId.get());
						collectDataPoints(Collections.singletonList(event.getDataPointSource()), tempRegistry, projectId);
					}
				} catch (final EntityNotFoundException e) {
					log.debug(() -> "Project " + projectId.get() + " not found. Not re-collecting data points for provider " + providerName + " on this project");
				}
			}
			this.registry.set(tempRegistry);
			raiseUpdate(eventProjectId);
		}
	}
	
	private void raiseUpdate(final Optional<EntityId> projectId) {
		log.debug(() -> "Updated " + (projectId.isPresent() ? "for Project " + projectId.get() : "globally") + ":\n" + registry.get().toString());
		eventPublisher.publishEvent(new DataPointRegistryUpdatedEvent(this, projectId));
	}
	
	private static void collectDataPoints(final List<MiningDataPointSource> dataPointSources, final RegistryView registry, final Optional<Long> projectId) {
		dataPointSources.stream()
			.sorted(new SpringOrderComparingComparator())
			.forEach(source -> {
				final MiningDataPointBuilder builder = new MiningDataPointBuilderImpl(
						ClassUtils.getUserClass(source.getClass()).getName(), projectId, registry);
				source.provideDataPoints(builder);
				builder.getTypeDefinitions().forEach(typeDef -> {
					final long[] projectIds = typeDef.getProjectIds();
					if (projectIds == null) {
						registry.getOrCreateModel(Optional.empty()).typeDefinitions.put(typeDef.getName(), typeDef);
					} else {
						for (final long currentProjectId : projectIds) {
							registry.getOrCreateModel(Optional.of(currentProjectId)).typeDefinitions.put(typeDef.getName(), typeDef);
						}
					}
				});
				builder.getEnumDefinitions().forEach(enumDef -> {
					final long[] projectIds = enumDef.getProjectIds();
					if (projectIds == null) {
						registry.getOrCreateModel(Optional.empty()).enumDefinitions.put(enumDef.getName(), enumDef);
					} else {
						for (final long currentProjectId : projectIds) {
							registry.getOrCreateModel(Optional.of(currentProjectId)).enumDefinitions.put(enumDef.getName(), enumDef);
						}
					}
				});
				builder.getDataPointDefinitions().forEach(dataPointDef -> {
					final long[] projectIds = dataPointDef.getProjectIds();
					if (projectIds == null) {
						registry.getOrCreateModel(Optional.empty()).dataPointDefinitions
							.computeIfAbsent(dataPointDef.getParentTypeName(), k -> new HashMap<>()).put(dataPointDef.getName(), dataPointDef);
					} else {
						for (final long currentProjectId : projectIds) {
							registry.getOrCreateModel(Optional.of(currentProjectId)).dataPointDefinitions
								.computeIfAbsent(dataPointDef.getParentTypeName(), k -> new HashMap<>()).put(dataPointDef.getName(), dataPointDef);
						}
					}
				});
				builder.getQueryDefinitions().forEach(queryDef -> registry.queryDefinitions.put(queryDef.getName(), queryDef));
			});
		registry.validateDataPointTypes();
		registry.resolveAliases();
	}

	/**
	 * Gets all global type definitions as map from type name to definition.
	 *
	 * @return map of registered types
	 */
	public Map<String, MiningDataTypeDefinition> getTypeDefinitions() {
		return getTypeDefinitions(Optional.empty());
	}

	/**
	 * Gets the type definitions specific to a project, including all global types.
	 * @param projectId the ID of the project
	 *
	 * @return map a map from type name to definition
	 */
	public Map<String, MiningDataTypeDefinition> getTypeDefinitions(final Optional<Long> projectId) {
		return registry.get().getTypeDefinitions(projectId);
	}

	/**
	 * Gets all global enum definitions as a map from enum name to definition.
	 *
	 * @return map of registered enums
	 */
	public Map<String, MiningEnumDefinition> getEnumDefinitions() {
		return Collections.unmodifiableMap(getEnumDefinitions(Optional.empty()));
	}

	/**
	 * Gets the enum definitions specific to a project, including all global enums.
	 * @param projectId the ID of the project
	 *
	 * @return a map from enum name to definition
	 */
	public Map<String, MiningEnumDefinition> getEnumDefinitions(final Optional<Long> projectId) {
		return Collections.unmodifiableMap(registry.get().getEnumDefinitions(projectId));
	}

	/**
	 * Gets all registered data points as a map from type name to map of data points.
	 *
	 * @return map of type name to map of registered data points
	 */
	public Map<String, Map<String, MiningDataPointDefinitionWithCustomFetch>> getDataPointDefinitions() {
		return getDataPointDefinitions(Optional.empty());
	}

	/**
	 * Gets the data points specific to a project, including all global data points.
	 * @param projectId the ID of the project
	 *
	 * @return map of type name to map of registered data points
	 */
	public Map<String, Map<String, MiningDataPointDefinitionWithCustomFetch>> getDataPointDefinitions(final Optional<Long> projectId) {
		return Collections.unmodifiableMap(registry.get().getDataPointDefinitions(projectId));
	}

	/**
	 * Gets all registered query definitions as a map from query name to definition.
	 *
	 * @return map of registered queries
	 */
	public Map<String, MiningDataPointDefinition> getQueryDefinitions() {
		return Collections.unmodifiableMap(registry.get().queryDefinitions);
	}

	public Map<String, Map<String, MiningDataPointDefinition>> getDataPointDefinitionsWithUsage(final @Nullable String... usages) {
		return getDataPointDefinitionsWithUsage(usages == null ? Collections.emptyList() : Arrays.asList(usages));
	}

	public Map<String, Map<String, MiningDataPointDefinition>> getDataPointDefinitionsWithUsage(final Collection<String> usages) {
		return getDataPointDefinitionsWithUsage(Optional.empty(), usages);
	}

	public Map<String, Map<String, MiningDataPointDefinition>> getDataPointDefinitionsWithUsage(final Optional<Long> projectId, final Collection<String> usages) {
		return registry.get().getDataPointDefinitionsWithUsage(projectId, usages);
	}

	public List<MiningDataPointDefinitionWithPath> getDataPointsForTypeRecursively(final String typeName) {
		return getDataPointsForTypeRecursively(Optional.empty(), typeName);
	}

	public List<MiningDataPointDefinitionWithPath> getDataPointsForTypeRecursively(final Optional<Long> projectId, final String typeName) {
		return getDataPointsForTypeRecursivelyWithUsage(projectId, typeName, Collections.emptyList());
	}

	public List<MiningDataPointDefinitionWithPath> getDataPointsForTypeRecursivelyWithUsage(final String typeName, final @Nullable String... usages) {
		return getDataPointsForTypeRecursivelyWithUsage(Optional.empty(), typeName, usages == null ? Collections.emptyList() : Arrays.asList(usages));
	}

	public List<MiningDataPointDefinitionWithPath> getDataPointsForTypeRecursivelyWithUsage(final String typeName, final Collection<String> usages) {
		return getDataPointsForTypeRecursivelyWithUsage(Optional.empty(), typeName, usages);
	}

	public List<MiningDataPointDefinitionWithPath> getDataPointsForTypeRecursivelyWithUsage(final Optional<Long> projectId,
			final String typeName, final Collection<String> usages) {
		final List<MiningDataPointDefinitionWithPath> dataPoints = new ArrayList<>();
		final Set<String> visitedTypes = new HashSet<>();

		getDataPointsRecursively(projectId, dataPoints, visitedTypes, new HashSet<>(usages), typeName, "", Optional.empty());

		return dataPoints;
	}

	private void getDataPointsRecursively(final Optional<Long> projectId, final List<MiningDataPointDefinitionWithPath> dataPoints,
			final Set<String> visitedTypes, final Set<String> usages, final String typeName, final String path,
			final Optional<Map<String, Map<String, MiningDataPointDefinitionWithCustomFetch>>> cachedDataPointDefinitions) {
		
		final Queue<Pair<String>> toBeVisited = new LinkedList<>();
		toBeVisited.add(new Pair<>(typeName, path));
		
		final Map<String, Map<String, MiningDataPointDefinitionWithCustomFetch>> dataPointDefinitions = cachedDataPointDefinitions
				.orElseGet(() -> getDataPointDefinitions(projectId));
		
		while ( ! toBeVisited.isEmpty()) {
			final Pair<String> pair = toBeVisited.poll();

			final String currentType = pair.getFirst();
			final String currentPath = pair.getSecond();
			if (visitedTypes.contains(currentType) || ! dataPointDefinitions.containsKey(currentType)) {
				continue;
			}
			
			visitedTypes.add(currentType);
			
			for (final MiningDataPointDefinition dataPoint : dataPointDefinitions.get(currentType).values()) {
				/* filter out data points that don't have any of the requested usages */
				if (usages.isEmpty() || ! Collections.disjoint(usages, dataPoint.getUsages())) {
					dataPoints.add(new MiningDataPointDefinitionWithPath(dataPoint, currentPath + dataPoint.getName()));
				}
				final String referenceTypeName = dataPoint.getReferenceTypeName();
				if (referenceTypeName != null) {
					toBeVisited.add(new Pair<>(referenceTypeName, currentPath + dataPoint.getName() + "."));
				}
			}
		}
	}
	
	/**
	 * Gets the data point definition for a data point at a certain path relative to a given root type.
	 *
	 * @param projectId optionally a project id for retrieving project-specific data point definitions
	 * @param rootTypeName the name of the root type
	 * @param path the path of the target data point underneath the root type
	 * @return the data point definition or {@code Optional.empty()} if no data point exists at the given path
	 */
	public Optional<MiningDataPointDefinitionWithPath> getDataPointAtPath(final Optional<Long> projectId, final String rootTypeName, final String path) {
		final var consumedSegments = new LinkedList<String>();
		return getDataPointDefinitionAtPath(projectId, rootTypeName, path.split("\\."), consumedSegments)
				.map(dp -> new MiningDataPointDefinitionWithPath(dp, String.join(".", consumedSegments)));
	}
	
	/**
	 * Gets the data point definition for a data point at a certain path relative to a given root type.
	 *
	 * @param projectId optionally a project id for retrieving project-specific data point definitions
	 * @param rootTypeName the name of the root type
	 * @param path the path of the target data point underneath the root type
	 * @return the data point definition or {@code Optional.empty()} if no data point exists at the given path
	 */
	public Optional<MiningDataPointDefinitionWithCustomFetch> getDataPointDefinitionAtPath(final Optional<Long> projectId,
			final String rootTypeName, final String[] path) {
		return getDataPointDefinitionAtPath(projectId, rootTypeName, path, null);
	}
	
	private Optional<MiningDataPointDefinitionWithCustomFetch> getDataPointDefinitionAtPath(final Optional<Long> projectId,
			final String rootTypeName, final String[] path, @Nullable final List<String> consumedSegments) {
		final Map<String, Map<String, MiningDataPointDefinitionWithCustomFetch>> dataPoints = getDataPointDefinitions(projectId);
		final LinkedList<String> remainingSegments = new LinkedList<>(Arrays.asList(path));
		Map<String, MiningDataPointDefinitionWithCustomFetch> currentType = dataPoints.get(rootTypeName);
		MiningDataPointDefinitionWithCustomFetch currentDataPoint = null;
		while ( ! remainingSegments.isEmpty()) {
			if (currentType == null) {
				return Optional.empty();
			}
			final String currentSegment = remainingSegments.remove();
			if (consumedSegments != null) {
				consumedSegments.add(currentSegment);
			}
			currentDataPoint = currentType.get(currentSegment);
            if (currentDataPoint == null || currentDataPoint.getReferenceTypeName() == null) {
				if (currentDataPoint == null) {
					log.debug(() -> "No Datapoint found for segment: " + currentSegment);
					final Map<String, MiningDataPointDefinitionWithCustomFetch> finalCurrentType = currentType;
					log.debug(() -> "valid segments would have been: " + finalCurrentType.keySet());
				}
				currentType = null;
			} else {
				currentType = dataPoints.get(currentDataPoint.getReferenceTypeName());
			}

            if (currentDataPoint != null && currentDataPoint.isAlias()
					&& assertNotNull(currentDataPoint.getAliasFor()).getSubSelection().equals(String.join(".", remainingSegments))) {
				/* This "hack" ensures that if we have a data point alias with sub-selection, then the alias is returned regardless whether the sub-selection is
				 * included in the path. This only makes sense if we look at an example:
				 *
				 * Let's say we have a data point "foo" which is an alias for datapoint "bar" with sub-selection "baz". When querying for either the
				 * "foo" OR "foo.baz" data points, we need to use the following fragment in GraphQL in both cases:
				 * foo: bar {
				 *     baz
				 * }
				 * The thing is that querying for "foo" is equivalent to querying for "foo.baz", since the sub-selection "baz" is always appended
				 * when querying for "foo". However, when building the query we need to know that "foo.baz" is actually an alias, or we will mistakenly
				 * try to build this query instead:
				 * foo {
				 *   baz
				 * }
				 * which does not work.
				 *
				 * This "hack" ensures that this method always returns the definition for the "foo" data point (the alias) regardless of whether
				 * "foo" is requested directly or the "foo.baz" sub-selection is requested.
				 *
				 */
				break;
			}
		}
		return Optional.ofNullable(currentDataPoint);
	}
}

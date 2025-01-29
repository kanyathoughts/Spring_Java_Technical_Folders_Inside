package innowake.mining.shared.model.dependency.graph;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.util.collection.CollectionUtil;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.model.RelationshipType;
import org.apache.commons.collections4.keyvalue.MultiKey;
import org.apache.commons.collections4.map.MultiKeyMap;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

/**
 * Domain class for DependencyGraph
 */
public class DependencyGraph {
	
	private static final Logger LOGGER = LoggerFactory.getLogger(DependencyGraph.class);
	private static final UUID DUMMY_UUID = new UUID(0, 0);
	
	@JsonIgnore
	private final List<ModulePojo> rootModules;

	private final Set<Long> rootModuleIds;

	private final List<ModulePojo> modules;

	private final List<ModuleRelationshipPojo> references;

	private final List<String> moduleTypes = new ArrayList<>();

	private final List<String> relationshipTypes = new ArrayList<>();

	private final Set<Long> modulesWithMissingDependencies = new HashSet<>();

	/**
	 * Constructor for {@link DependencyGraph}
	 * 
	 * @param modulesList the list of dependent modules
	 * @param moduleLinksList the list of dependent references
	 */
	public DependencyGraph(final List<ModulePojo> modulesList, final List<ModuleRelationshipPojo> moduleLinksList) {
		this(modulesList, moduleLinksList, modulesList.isEmpty() ? Collections.emptySet() : Collections.singleton(modulesList.get(0).getId()));
	}

	/**
	 * Constructor for {@link DependencyGraph}
	 *
	 * @param modulesList the list of dependent modules
	 * @param moduleLinksList the list of dependent references
	 * @param rootModuleIds the set of root module IDs of the graph
	 */
	@JsonCreator
	public DependencyGraph(@JsonProperty("modules") final List<ModulePojo> modulesList,
			@JsonProperty("references") final List<ModuleRelationshipPojo> moduleLinksList,
			@JsonProperty("rootModuleIds") final Set<Long> rootModuleIds) {
		this(modulesList, moduleLinksList, rootModuleIds, Collections.emptyList());
	}

	/**
	 * Constructor for {@link DependencyGraph}
	 * 
	 * @param modulesList the list of dependent modules
	 * @param moduleLinksList the list of dependent references
	 * @param rootModuleIds the set of root module IDs of the graph
	 * @param modulesWithMissingDependencies the list of modules Ids having dependency modules missing
	 */
	public DependencyGraph(final List<ModulePojo> modulesList, final List<ModuleRelationshipPojo> moduleLinksList, final Set<Long> rootModuleIds,
			final List<Long> modulesWithMissingDependencies) {
		this.rootModuleIds = rootModuleIds;
		if (modulesList.isEmpty()) {
			this.rootModules = Collections.emptyList();
		} else {
			this.rootModules = modulesList.stream().filter(module -> rootModuleIds.contains(module.getId())).collect(Collectors.toList());
		}
		this.modules = modulesList;
		this.references = moduleLinksList;
		
		this.moduleTypes.addAll(modules.stream().map(this::getNodeTypeName).distinct().collect(Collectors.toList()));
		this.relationshipTypes.addAll(references.stream().map(ModuleRelationshipPojo::getRelationship)
		                                                 .map(RelationshipType::toString)
		                                                 .distinct()
		                                                 .collect(Collectors.toList()));
		this.modulesWithMissingDependencies.addAll(modulesWithMissingDependencies);
	}
	
	public DependencyGraph(final List<DependencyGraph> graphs) {
		final Set<ModuleRelationshipPojo> referencesSet = new HashSet<>();
		final Set<ModulePojo> rootModulesSet = new HashSet<>();
		rootModuleIds = new HashSet<>();
		final Set<ModulePojo> modulesSet = new HashSet<>();
		final Set<String> moduleTypesSet = new HashSet<>();
		final Set<String> relationshipTypesSet = new HashSet<>();
		for (final DependencyGraph graph : graphs) {
			if (graph.modules.isEmpty()) {
				continue;
			}
			rootModulesSet.addAll(graph.rootModules);
			rootModuleIds.addAll(graph.rootModuleIds);
			modulesSet.addAll(graph.getModules());
			referencesSet.addAll(graph.getReferences());
			moduleTypesSet.addAll(graph.getModuleTypes());
			relationshipTypesSet.addAll(graph.getRelationshipTypes());
			modulesWithMissingDependencies.addAll(graph.getModulesWithMissingDependencies());
		}
		rootModules = new ArrayList<>(rootModulesSet);
		modules = new ArrayList<>(modulesSet);
		references = new ArrayList<>(referencesSet);
		moduleTypes.addAll(moduleTypesSet);
		relationshipTypes.addAll(relationshipTypesSet);
	}

	/**
	 * Getter method to fetch the list of {@link ModulePojo ModulePojos} in the graph.
	 * The method will return the filtered list if the list has been filtered
	 * based on the predicates provided.
	 *
	 * @return list of {@link Module}s in the graph
	 */
	public List<ModulePojo> getModules() {
		return CollectionUtil.unmodifiableArrayList(modules);
	}

	/**
	 * Getter method to fetch the list of {@code references} in the graph.
	 * The method will return the filtered list if the list has been filtered
	 * based on the predicates provided.
	 *
	 * @return list of {@code references} in the graph
	 */
	public List<ModuleRelationshipPojo> getReferences() {
		return CollectionUtil.unmodifiableArrayList(references);
	}

	/**
	 * Getter method to fetch the list of distinct {@link Module} types in the graph.
	 *
	 * @return list of distinct {@link Module} types in the graph
	 */
	public List<String> getModuleTypes() {
		return CollectionUtil.unmodifiableArrayList(moduleTypes);
	}

	/**
	 * Getter method to fetch the list of distinct {@link RelationshipType}s in the graph.
	 *
	 * @return list of distinct {@link RelationshipType}s in the graph
	 */
	public List<String> getRelationshipTypes() {
		return CollectionUtil.unmodifiableArrayList(relationshipTypes);
	}
	
	/**
	 * Getter method to fetch to list of IDs of the root modules in the graph.
	 *
	 * @return list of root module IDs in the graph
	 */
	public List<Long> getRootModuleIds() {
		return CollectionUtil.unmodifiableArrayList(rootModuleIds);
	}

	/**
	 * Getter method to fetch the list of IDs of the modules in the graph having dependency modules missing.
	 *
	 * @return list of module ids having dependency modules missing
	 */
	public List<Long> getModulesWithMissingDependencies() {
		return CollectionUtil.unmodifiableArrayList(modulesWithMissingDependencies);
	}
	
	/**
	 * Method to filter the Dependency Graph.
	 * It should be noted that calling the method <i>multiple</i> times
	 * with different values for the filtering {@link Predicate}s
	 * can cause in the graph being filtered in unexpected ways.
	 *
	 * @param moduleTypeFilterPredicate types of modules to filter
	 * @param relationshipTypeFilterPredicate types of reference relationships to filter
	 */
	public void filterGraph(final Predicate<NodeType> moduleTypeFilterPredicate, final Predicate<RelationshipType> relationshipTypeFilterPredicate) {
		final long startTime = System.nanoTime();
		final List<ModulePojo> filteredModulesByPredicate = new ArrayList<>();
		final Map<UUID, ModulePojo> moduleUidToModule = new HashMap<>();
		final Set<UUID> rootModuleUids = rootModules.stream().map(ModulePojo::getUid).collect(Collectors.toSet());
		modules.stream()
				.filter(node -> rootModuleUids.contains(node.getUid()) || ! moduleTypeFilterPredicate.test(NodeType.of(node.getTechnology(), node.getType())))
				.forEach(module -> {
					filteredModulesByPredicate.add(module);
					moduleUidToModule.put(module.getUid(), module);
				});
		final List<ModuleRelationshipPojo> filteredReferencesByPredicate = references.stream()
		                                                                .filter(ref -> ! relationshipTypeFilterPredicate.test(ref.getRelationship()))
		                                                                .collect(Collectors.toList());
		final MultiKeyMap<UUID, Set<Tuple2<RelationshipType, UUID>>> adjacencyMap = getAdjacencyMap(filteredReferencesByPredicate, moduleUidToModule);
		computeFilteredModulesAndReferences(moduleUidToModule, adjacencyMap, rootModuleUids);
		LOGGER.debug(() -> "Time take to filter module " + (rootModules.isEmpty() ? "NO ROOT EXISTS" : rootModules.get(0).getName()) + ": " + (System.nanoTime() - startTime) + " nanoseconds");
	}

	private void computeFilteredModulesAndReferences(final Map<UUID, ModulePojo> moduleUidToModule, final MultiKeyMap<UUID, Set<Tuple2<RelationshipType, UUID>>> adjacencyMap, final Set<UUID> rootModuleUids) {
		final Set<UUID> nodeIdsConnectedToRoot = getModuleIdsThatCanBeTracedToRootModule(adjacencyMap);
		modules.clear();
		modules.addAll(rootModules);
		final Set<UUID> nodeIdsAlreadyAdded = new HashSet<>();
		nodeIdsAlreadyAdded.addAll(rootModuleUids);
		references.clear();
		for (final var entry : adjacencyMap.entrySet()) {
			final UUID srcModule = entry.getKey().getKey(0);
			final UUID dstModule = entry.getKey().getKey(1);
			if (nodeIdsConnectedToRoot.contains(srcModule) || nodeIdsConnectedToRoot.contains(dstModule)) {
				addModulesForThePair(moduleUidToModule, nodeIdsAlreadyAdded, srcModule, dstModule);
				addReferencesForThePair(srcModule, dstModule, entry.getValue());
			}
		}
	}

	private Set<UUID> getModuleIdsThatCanBeTracedToRootModule(final MultiKeyMap<UUID, Set<Tuple2<RelationshipType, UUID>>> adjacencyMap) {
		final Set<UUID> nodeIdsConnectedToRoot = new HashSet<>();
		final Map<UUID, List<UUID>> bidirectionalMap = new HashMap<>();
		/* changing the representation into undirected graph to find connected nodes */
		adjacencyMap.keySet().forEach(key -> {
			final UUID srcModule = key.getKey(0);
			final UUID dstModule = key.getKey(1);
			bidirectionalMap.computeIfAbsent(srcModule, id -> new ArrayList<>());
			bidirectionalMap.get(srcModule).add(dstModule);

			bidirectionalMap.computeIfAbsent(dstModule, id -> new ArrayList<>());
			bidirectionalMap.get(dstModule).add(srcModule);
		});
		
		for (final ModulePojo currentRoot : rootModules) {
			final Queue<UUID> queue = new LinkedList<>();
			queue.add(currentRoot.getUid());
			while ( ! queue.isEmpty()) {
				final UUID currentSrc = queue.poll();
				nodeIdsConnectedToRoot.add(currentSrc);
				final var dstModules = bidirectionalMap.get(currentSrc);
				if (dstModules != null) {
					dstModules.stream()
						.filter(dstModule -> ! nodeIdsConnectedToRoot.contains(dstModule))
						.forEach(queue::add);
				}
			}
		}
		
		return nodeIdsConnectedToRoot;
	}

	private void addModulesForThePair(final Map<UUID, ModulePojo> moduleUidToModule, final Set<UUID> nodeUidsAlreadyAdded, final UUID srcModule, final UUID dstModule) {
		if ( ! nodeUidsAlreadyAdded.contains(srcModule)) {
			modules.add(moduleUidToModule.get(srcModule));
			nodeUidsAlreadyAdded.add(srcModule);
		}
		if ( ! nodeUidsAlreadyAdded.contains(dstModule)) {
			modules.add(moduleUidToModule.get(dstModule));
			nodeUidsAlreadyAdded.add(dstModule);
		}
	}

	private void addReferencesForThePair(final UUID srcModule, final UUID dstModule, final Set<Tuple2<RelationshipType, UUID>> relationships) {
		for (final var relationship : relationships) {
			references.add(new ModuleRelationshipPojo(relationship.b, srcModule, null, dstModule, null, relationship.a, null,
					null, null, null, Collections.emptyList(), null, null, null));
		}
	}

	/**
	 * Method to create the adjacency map that is used for filtering the graph.
	 * For a simple graph as below:
	 * 
	 * (Module A) ----CALLS----> (Module B) ----REFERENCES----> (Module C)
	 * 
	 * The map will look like:
	 *  ____________________________________________________
	 * |key	=       |Module A   |Module B   |   Module C    |
	 * |(moduleId,  |           |           |               |
	 * | moduleId)  |           |           |               |
	 * |____________|___________|___________|_______________|
	 * |Module A    |           |  CALLS    |               |
	 * |____________|___________|___________|_______________|
	 * |Module B    |           |           |   REFERENCES  |
	 * |____________|___________|___________|_______________|
	 * |Module C    |           |           |               |
	 * |____________|___________|___________|_______________|
	 *
	 * @param filteredReferencesByPredicate references that have been filtered by the predicate
	 * @param moduleUidToModuleMap map that is used to identify {@link Module} by it's ID
	 * @return the adjacency map created for the references between modules
	 */
	private MultiKeyMap<UUID, Set<Tuple2<RelationshipType, UUID>>> getAdjacencyMap(final List<ModuleRelationshipPojo> filteredReferencesByPredicate,
	                                                             final Map<UUID, ModulePojo> moduleUidToModuleMap) {
		final MultiKeyMap<UUID, Set<Tuple2<RelationshipType, UUID>>> adjacencyMap = new MultiKeyMap<>();
		final Map<UUID, List<ModuleRelationshipPojo>> srcUidToReferencesMap = new HashMap<>();
		final Map<UUID, List<ModuleRelationshipPojo>> dstUidToReferencesMap = new HashMap<>();
		filteredReferencesByPredicate.stream().forEach(reference -> {
			final UUID src = reference.getSrcModule();
			final UUID dst = reference.getDstModule();
			srcUidToReferencesMap.computeIfAbsent(src, key -> new ArrayList<>()).add(reference);
			dstUidToReferencesMap.computeIfAbsent(dst, key -> new ArrayList<>()).add(reference);
		});
		for (final ModuleRelationshipPojo reference : filteredReferencesByPredicate) {
			final UUID src = reference.getSrcModule();
			final UUID dst = reference.getDstModule();
			if (moduleUidToModuleMap.containsKey(src) && moduleUidToModuleMap.containsKey(dst)) {
				adjacencyMap.computeIfAbsent(new MultiKey<>(src, dst), key -> new HashSet<>())
						.add(new Tuple2<>(reference.getRelationship(), reference.getId()));
			} else if ( ! moduleUidToModuleMap.containsKey(src) && moduleUidToModuleMap.containsKey(dst)) {
				final List<UUID> fromIds = getArtificiallyLinkedModuleIds(src, dstUidToReferencesMap, moduleUidToModuleMap, ModuleRelationshipPojo::getSrcModule);
				fromIds.stream().forEach(id -> adjacencyMap.computeIfAbsent(new MultiKey<>(id, dst), key -> new HashSet<>())
						.add(new Tuple2<>(RelationshipType.ARTIFICIAL, DUMMY_UUID)));
			} else if (moduleUidToModuleMap.containsKey(src) && ! moduleUidToModuleMap.containsKey(dst)) {
				final List<UUID> toIds = getArtificiallyLinkedModuleIds(dst, srcUidToReferencesMap, moduleUidToModuleMap, ModuleRelationshipPojo::getDstModule);
				toIds.stream().forEach(id -> adjacencyMap.computeIfAbsent(new MultiKey<>(src, id), key -> new HashSet<>())
						.add(new Tuple2<>(RelationshipType.ARTIFICIAL, DUMMY_UUID)));
			}
		}
		return adjacencyMap;
	}

	private List<UUID> getArtificiallyLinkedModuleIds(final UUID moduleUid, final Map<UUID, List<ModuleRelationshipPojo>> uidToReferences,
	                                                    final Map<UUID, ModulePojo> moduleUidToModules,
	                                                    final Function<? super ModuleRelationshipPojo, UUID> referenceMapper) {
		final List<UUID> distantReferencedUids = new ArrayList<>();
		final Deque<UUID> moduleUidsToCheck = new LinkedList<>();
		final Set<UUID> visitedModules = new HashSet<>();
		moduleUidsToCheck.add(moduleUid);
		while ( ! moduleUidsToCheck.isEmpty()) {
			final UUID currentModuleId = moduleUidsToCheck.pop();
			final List<ModuleRelationshipPojo> referencesWithModuleId = uidToReferences.get(currentModuleId);
			if (referencesWithModuleId != null) {
				final List<UUID> allReferencedUids = referencesWithModuleId.stream().map(referenceMapper).collect(Collectors.toList());
				distantReferencedUids.addAll(allReferencedUids.stream()
																.filter(moduleUidToModules::containsKey)
																.collect(Collectors.toList()));
 				if ( ! distantReferencedUids.isEmpty()) {
					break;
				}
				moduleUidsToCheck.addAll(allReferencedUids.stream()
															.filter(connectedId ->  ! connectedId.equals(moduleUid) && ! visitedModules.contains(connectedId))
															.collect(Collectors.toList()));
			}
			visitedModules.add(currentModuleId);
		}
		return distantReferencedUids;
	}

	private String getNodeTypeName(final ModulePojo module) {
		return module.getTechnology() + " " + module.getType().toString().replace("_", " ");
	}
}

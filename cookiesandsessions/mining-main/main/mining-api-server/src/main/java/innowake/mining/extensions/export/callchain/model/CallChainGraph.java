/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.export.callchain.model;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.UUID;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MultiValuedMap;
import org.apache.commons.collections4.multimap.HashSetValuedHashMap;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.commons.lang.builder.ToStringStyle;

import graphql.com.google.common.base.Objects;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.extensions.export.callchain.model.CallChain.CallChainDirection;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * A graph for call chains that has a root {@link ModuleLightweightPojo} and a map of edges.
 */
public class CallChainGraph {

	private final CallChainDirection direction;
	private final ModuleLightweightPojo root;
	private final MultiValuedMap<ModuleLightweightPojo, CallChainEdge> edgeMap = new HashSetValuedHashMap<>();
	private final MultiValuedMap<ModuleLightweightPojo, ModuleLightweightPojo> targetMap = new HashSetValuedHashMap<>();
	private int size = 0;
	private int filterCount = 0;

	/**
	 * Constructor.
	 * 
	 * @param direction the {@link CallChainDirection} for all call chains
	 * @param root the root {@link ModuleLightweightPojo} for which all call chains are collected
	 */
	public CallChainGraph(final CallChainDirection direction, final ModuleLightweightPojo root) {
		this.direction = direction;
		this.root = root;
	}

	/**
	 * Adds the give {@code module} together with its {@code edge} to this graphs {@code edge} map.
	 * <p>Call chains are computed concurrently. Therefore the access is synchronized.</p>
	 *
	 * @param module the source {@link ModuleLightweightPojo} of the {@code edge}
	 * @param edge the edge
	 * @return {@code true} if the edge map changed. Returns {@code false} if the map already contained a
	 * 			an entry for the same {@code module} and {@code edge}
	 */
	public boolean add(final ModuleLightweightPojo module, final CallChainEdge edge) {
		/* Call chains are computed concurrently. Therefore, we must synchronize the access here */
		synchronized (edgeMap) {
			targetMap.put(edge.getTarget(), module);
			size++;
			return edgeMap.put(module, edge);
		}
	}

	/**
	 * Returns if the given {@code module} is contained in this graphs {@code edge} map.
	 * <p>Call chains are computed concurrently. Therefore the access is synchronized.</p>
	 *
	 * @param module the {@link ModuleLightweightPojo} to check
	 * @return {@code true} if this graph already contains edges for {@code module}. Returns {@code false} if not.
	 */
	public boolean contains(final ModuleLightweightPojo module) {
		/* Call chains are computed concurrently. Therefore, we must synchronize the access here */
		synchronized (edgeMap) {
			return edgeMap.containsKey(module);
		}
	}
	
	/**
	 * Returns if a module with the given {@code moduleId} is contained in this graphs {@code edge} map.
	 * <p>Call chains are computed concurrently. Therefore the access is synchronized.</p>
	 *
	 * @param moduleId the id of the module to check
	 * @return {@code true} if this graph already contains edges for {@code moduleId}. Returns {@code false} if not.
	 */
	public boolean contains(final Long moduleId) {
		/* Since the equals and hashCode methods of ModuleLightweight only consider the ID, we can construct a dummy instance of ModuleLightweight */
		return contains(new ModuleLightweightPojo(new UUID(0, 0), moduleId, null, null, null, "", null,
				Technology.COBOL, Type.UNKNOWN, null, null, true, null, null, null, null));
	}

	/**
	 * Removes all outgoing edges for a module.
	 *
	 * @param module the {@link ModuleLightweightPojo} to check
	 */
	public void remove(final ModuleLightweightPojo module) {
		final Set<ModuleLightweightPojo> sourceModules = new HashSet<>();
		final Collection<ModuleLightweightPojo> srcModules = targetMap.remove(module);
		if (srcModules != null) {
			sourceModules.addAll(srcModules);
		}

		/* remove all outgoing edges */
		size -= CollectionUtils.size(edgeMap.remove(module));

		for (final ModuleLightweightPojo source : sourceModules) {
			final Collection<CallChainEdge> edges = edgeMap.get(source);
			if (edges != null) {
				for (final Iterator<CallChainEdge> sourceIt = edges.iterator(); sourceIt.hasNext(); ) {
					if (module.equals(sourceIt.next().getTarget())) {
						size--;
						sourceIt.remove();
					}
				}
			}
		}
	}

	/**
	 * Removes all outgoing edges where a module in modules is the target .
	 *
	 * @param modules set of possible {@link ModuleLightweightPojo}
	 */
	public void removeAll(final Collection<ModuleLightweightPojo> modules) {
		final Set<ModuleLightweightPojo> targetModules = new HashSet<>(modules.size());
		final Set<ModuleLightweightPojo> sourceModules = new HashSet<>();
		for (final ModuleLightweightPojo module : modules) {
			/* remove all outgoing edges */
			size -= CollectionUtils.size(edgeMap.remove(module));
			targetModules.add(module);
			/* get all incoming edges */
			final Collection<ModuleLightweightPojo> srcModules = targetMap.remove(module);
			if (srcModules != null) {
				sourceModules.addAll(srcModules);
			}
		}

		/* delete all outgoing edges where a module in modules is the target */
		for (final ModuleLightweightPojo source : sourceModules) {
			final Collection<CallChainEdge> edges = edgeMap.get(source);
			if (edges != null) {
				for (final Iterator<CallChainEdge> sourceIt = edges.iterator(); sourceIt.hasNext(); ) {
					if (targetModules.contains(sourceIt.next().getTarget())) {
						size--;
						sourceIt.remove();
					}
				}
			}
		}
	}

	/** 
	 * Removes any node that is filtered by the given predicate. If a filtered node has incoming and outgoing edges then new ARTIFICIAL edge is created for 
	 * every starting node of an incoming edge to every target node of every outgoing edge.
	 * <p>Please note: This method is not thread-safe!</p>
	 * 
	 * @param nodeFilter predicate modules to filter
	 */
	public void filterCallChainGraphNodes(final Predicate<ModuleLightweightPojo> nodeFilter) {
		final List<ModuleLightweightPojo> modulesToBeDeleted = new LinkedList<>();
		final Map<ModuleLightweightPojo, Collection<CallChainEdge>> edgeMapping = edgeMap.asMap();

		/* Example where modX must be filtered out:
		 * 
		 *        +--> modA --+           +--> mod1
		 * root --+           +--> modX --+
		 *        +--> modb --+           +--> mod2 */
		/* get all outgoing edges of modX to mod1 and mod2 */
		final Map<ModuleLightweightPojo, Collection<ModuleLightweightPojo>> outgoings = targetMap.asMap();
		for (final Entry<ModuleLightweightPojo, Collection<ModuleLightweightPojo>> entry : outgoings.entrySet()) {
			/* the graph root is never filtered */
			if ( ! entry.getKey().equals(root) && ! nodeFilter.test(entry.getKey())) {

				/* Found modX */
				modulesToBeDeleted.add(entry.getKey());

				final Collection<CallChainEdge> incommings = edgeMapping.get(entry.getKey());
				if (incommings != null) {
					/* add an edge from every incoming modA and modB to every outgoing mod1 and mod2 of modX */
					for (final ModuleLightweightPojo caller : entry.getValue()) {
						for (final CallChainEdge edge : incommings) {
							add(caller, new CallChainEdge(edge.getId() + "_" + (filterCount++),
									edge.getTarget(), RelationshipType.ARTIFICIAL, edge.getRelationshipAttributes()));
						}
					}
				}
			}
		}

		if ( ! modulesToBeDeleted.isEmpty()) {
			removeAll(modulesToBeDeleted);
		}

		/* Result:
		 *        +--> modA --+--> mod1
		 * root --+           |
		 *        +--> modb --+--> mod2 */
	}

	/** 
	 * Filters end nodes and decides if an end module resp. path in the graph must be deleted.
	 * 
	 * @param endNodeFilter predicate modules to filter
	 */
	public void filterCallChainGraphEndNodes(final Predicate<ModuleLightweightPojo> endNodeFilter) {
		final Map<ModuleLightweightPojo, Collection<CallChainEdge>> edges = edgeMap.asMap();
		final Set<ModuleLightweightPojo> targetModules = targetMap.keySet();
		List<ModuleLightweightPojo> modulesToBeDeleted;
		do {
			modulesToBeDeleted = new LinkedList<>();
			for (final ModuleLightweightPojo module : targetModules) {
				/* Check that module has only incoming but no outgoing edges and if it should be filtered */
				if ( ! edges.containsKey(module) && ! endNodeFilter.test(module)) {
					modulesToBeDeleted.add(module);
				}
			}

			if ( ! modulesToBeDeleted.isEmpty()) {
				removeAll(modulesToBeDeleted);
			}
		} while ( ! modulesToBeDeleted.isEmpty());

		final Set<Long> keepModuleIds = new HashSet<>();
		keepModuleIds.add(root.getId());
		final Set<Long> processedModuleIds = new HashSet<>();
		processedModuleIds.add(root.getId());
		final Map<ModuleLightweightPojo, Collection<ModuleLightweightPojo>> targetModuleMap = targetMap.asMap();
		for (final ModuleLightweightPojo entry : targetModuleMap.keySet()) {
			if ( ! processedModuleIds.contains(entry.getId()) && endNodeFilter.test(entry)) {
				traverseTargetMap(targetModuleMap, entry, keepModuleIds, processedModuleIds, endNodeFilter);
			}
		}

		modulesToBeDeleted = targetModuleMap.keySet().stream()
									.filter(module -> ! keepModuleIds.contains(module.getId()))
									.collect(Collectors.toList());
		if ( ! modulesToBeDeleted.isEmpty()) {
			removeAll(modulesToBeDeleted);
		}
	}

	private static void traverseTargetMap(final Map<ModuleLightweightPojo, Collection<ModuleLightweightPojo>> targetMap, final ModuleLightweightPojo module,
			final Set<Long> keepModuleIds, final Set<Long> processedModuleIds, final Predicate<ModuleLightweightPojo> pathFilter) {

		processedModuleIds.add(module.getId());

		if (keepModuleIds.contains(module.getId()) || pathFilter.test(module)) {
			keepModuleIds.add(module.getId());

			final Collection<ModuleLightweightPojo> callers = targetMap.get(module);
			if (CollectionUtils.isNotEmpty(callers)) {
				for (final ModuleLightweightPojo callingModule : callers) {
					if ( ! processedModuleIds.contains(callingModule.getId())) {
						keepModuleIds.add(callingModule.getId());
						traverseTargetMap(targetMap, callingModule, keepModuleIds, processedModuleIds, pathFilter);
					}
				}
			}
		}
	}

	/**
	 * @return the number of {@link CallChainEdge CallChainEdges} this graph contains
	 */
	public int getSize() {
		return size;
	}

	/**
	 * @return the {@link CallChainDirection} of this graph
	 */
	public CallChainDirection getDirection() {
		return direction;
	}

	/**
	 * @return the root {@link ModuleLightweightPojo} of this graph
	 */
	public ModuleLightweightPojo getRoot() {
		return root;
	}

	/**
	 * Returns the map of edges which maps source {@link ModuleLightweightPojo ModuleLightweightPojos} to their {@link CallChainEdge CallChainEdges}.
	 * <p>This method should be called after all call chains were computed. Call chains are computed concurrently. Therefore all accesses to
	 * the edge map should be synchronized.</p>
	 *
	 * @return the edge map
	 */
	public MultiValuedMap<ModuleLightweightPojo, CallChainEdge> getEdgeMap() {
		return edgeMap;
	}

	/**
	 * Returns the map of edges which maps target {@link ModuleLightweightPojo ModuleLightweightPojos} to their incoming {@link ModuleLightweightPojo ModuleLightweightPojos}.
	 * <p>This method should be called after all call chains were computed. Call chains are computed concurrently. Therefore all accesses to
	 * the edge map should be synchronized.</p>
	 *
	 * @return the edge map
	 */
	public MultiValuedMap<ModuleLightweightPojo, ModuleLightweightPojo> getTargetMap() {
		return targetMap;
	}

	/** 
	 * Returns all nodes that have incoming but no outgoing edges.
	 * 
	 * @param filter predicate modules to filter
	 * @return list of {@linkplain ModuleLightweightPojo}
	 */
	public List<ModuleLightweightPojo> getEndModules(final Predicate<ModuleLightweightPojo> filter) {
		final List<ModuleLightweightPojo> result = new LinkedList<>();
		
		for (final ModuleLightweightPojo module : targetMap.keySet()) {
			/* Check that module is a table or file and that it has only incoming but no outgoing edges */
			if ( ! edgeMap.containsKey(module) && filter.test(module)) {
				result.add(module);
			}
		}

		return result;
	}
	
	/** 
	 * Returns all Target module ids that are in the call chain.
	 * 
	 * @param filter predicate modules to filter
	 * @return list of {@linkplain EntityId EntityIds}
	 */
	public List<EntityId> getTargetModulesInCallChain(final Predicate<ModuleLightweightPojo> filter) {
			return targetMap.keySet().stream()
					.filter(filter::test)
					.map(ModuleLightweightPojo::identity)
					.collect(Collectors.toList());
	}
	
	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.SHORT_PREFIX_STYLE)
			.append("Direction", direction)
			.append("Root", root)
			.toString();
	}

	/**
	 * An edge of the {@link CallChainGraph} containing the target {@link ModuleLightweightPojo} of the edge and all relation ship properties of the
	 * call from the source to target.
	 */
	public static class CallChainEdge {
		
		private final String id;
		private final ModuleLightweightPojo target;
		private final RelationshipType relationship;
		private final Map<String, Object> relationshipAttributes;
		private final List<UUID> reachedFromModules;

		/**
		 * Constructor.
		 *
		 * @param id the ID of the reference
		 * @param target the target {@link ModuleLightweightPojo} of this edge
		 * @param relationship the {@link RelationshipType} of the reference
		 * @param relationshipAttributes the attributes
		 */
		public CallChainEdge(final String id, final ModuleLightweightPojo target, final RelationshipType relationship, final Map<String, Object> relationshipAttributes) {
			this(id, target, relationship, relationshipAttributes, Collections.emptyList());
		}

		/**
		 * Constructor.
		 *
		 * @param id the ID of the reference
		 * @param target the target {@link ModuleLightweightPojo} of this edge
		 * @param relationship the {@link RelationshipType} of the reference
		 * @param relationshipAttributes the attributes
		 * @param reachedFromModules list of reaching modules (conditional dependencies) of the reference. Empty if there are none
		 */
		public CallChainEdge(final String id, final ModuleLightweightPojo target, final RelationshipType relationship, final Map<String, Object> relationshipAttributes,
				final List<UUID> reachedFromModules) {
			this.id = id;
			this.target = target;
			this.relationship = relationship;
			this.relationshipAttributes = relationshipAttributes;
			this.reachedFromModules = reachedFromModules;
		}

		/**
		 * @return the target {@link ModuleLightweightPojo} of this edge
		 */
		public ModuleLightweightPojo getTarget() {
			return target;
		}
		
		/**
		 * @return the {@link RelationshipType} of this edge
		 */
		public RelationshipType getRelationship() {
			return relationship;
		}
		
		/**
		 * @return the reference attributes
		 */
		public Map<String, Object> getRelationshipAttributes() {
			return relationshipAttributes;
		}

		/**
		 * @return the id of the reference
		 */
		public String getId() {
			return id;
		}
		
		/**
		 * @return list of reaching modules (conditional dependencies) of the reference. Empty if there are none
		 */
		public List<UUID> getReachedFromModules() {
			return reachedFromModules;
		}
		
		@Override
		public int hashCode() {
			return Objects.hashCode(target, relationship, relationshipAttributes);
		}

		@Override
		public boolean equals(@Nullable final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null || getClass() != obj.getClass()) {
				return false;
			}
			
			final CallChainEdge other = (CallChainEdge) obj;
			return other.relationship == relationship &&
					Objects.equal(other.target, target) &&
					Objects.equal(other.relationshipAttributes, relationshipAttributes); 
		}
	
		@Override
		public String toString() {
			return new ToStringBuilder(this, ToStringStyle.SHORT_PREFIX_STYLE)
				.append("target", target)
				.append("relationship", relationship)
				.append("relationshipAttributes", relationshipAttributes)
				.toString();
		}
	}
}
/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.export.callchain.graphml;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import innowake.mining.extensions.export.callchain.model.CallChain.CallChainDirection;
import innowake.mining.extensions.export.callchain.model.CallChainGraph;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.lang.NotImplementedException;
import org.apache.tinkerpop.gremlin.process.computer.GraphComputer;
import org.apache.tinkerpop.gremlin.structure.Edge;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.apache.tinkerpop.gremlin.structure.Transaction;
import org.apache.tinkerpop.gremlin.structure.Vertex;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.extensions.export.callchain.model.CallChainGraph.CallChainEdge;

/**
 * A {@link Graph} implementation for exporting {@link CallChainGraph CallChainGraphs} to GraphML.
 */
public class GraphML implements Graph {

	private final CallChainGraph[] callChainGraphs;

	private final Map<Long, Vertex> verticeMap;
	private final Map<String, Edge> edgeMap;
	
	final Map<EntityId, List<Tuple2<String, String>>> taxonomies;
	
	public GraphML(final Map<EntityId, List<Tuple2<String, String>>> taxonomies, final CallChainGraph... callChainGraphs) {
		this.callChainGraphs = callChainGraphs;
		verticeMap = callChainGraphs.length == 0 ? Collections.emptyMap() : new HashMap<>();
		edgeMap = callChainGraphs.length == 0 ? Collections.emptyMap() : new HashMap<>();
		this.taxonomies = taxonomies;
	}

	@Override
	public Iterator<Vertex> vertices(@Nullable final Object... vertexIds) {
		if (callChainGraphs.length != 0 && verticeMap.isEmpty()) {
			convertGraphs();
		}

		return verticeMap.values().iterator();
	}

	@Override
	public Iterator<Edge> edges(@Nullable final Object... edgeIds) {
		if (callChainGraphs.length != 0 && verticeMap.isEmpty()) {
			convertGraphs();
		}

		return edgeMap.values().iterator();
	}

	/**
	 * Processes the calculated CallChainGraphs and converts all edges and nodes into the GraphML vertices and edges that are required
	 * for the export
	 */
	private void convertGraphs() {
		for (final CallChainGraph callChainGraph : callChainGraphs) {
			final boolean isOut = callChainGraph.getDirection() == CallChainDirection.OUT;
			for (final Entry<ModuleLightweightPojo, Collection<CallChainEdge>> entry : callChainGraph.getEdgeMap().asMap().entrySet()) {
				final ModuleLightweightPojo module = entry.getKey();
				verticeMap.computeIfAbsent(module.getId(), key -> new GraphMLVertex(this, module, getModuleTaxonomies(module)));

				for (final CallChainEdge edge : entry.getValue()) {
					verticeMap.computeIfAbsent(edge.getTarget().getId(), key -> new GraphMLVertex(this, edge.getTarget(), getModuleTaxonomies(edge.getTarget())));
					if (isOut) {
						edgeMap.computeIfAbsent(edge.getId(),
								key -> new GraphMLEdge(this, new GraphMLVertex(this, module, getModuleTaxonomies(module)),
										new GraphMLVertex(this, edge.getTarget(), getModuleTaxonomies(edge.getTarget())),
										 edge.getRelationship().toString(), edge.getRelationshipAttributes(), key));
					} else {
						edgeMap.computeIfAbsent(edge.getId(),
								key -> new GraphMLEdge(this, new GraphMLVertex(this, edge.getTarget(), getModuleTaxonomies(edge.getTarget())),
										new GraphMLVertex(this, module, getModuleTaxonomies(module)),
										 edge.getRelationship().toString(), edge.getRelationshipAttributes(), key));
					}
				}
			}
		}
	}

	private List<Tuple2<String, String>> getModuleTaxonomies(final ModuleLightweightPojo module) {
		return taxonomies.getOrDefault(module.identity(), List.of());
	}

	@Override
	public void close() throws Exception {
		/* Nothing to close */
	}

	@Override
	public Vertex addVertex(@Nullable final Object... keyValues) {
		throw new NotImplementedException("Adding vertex is not supported");
	}

	@Override
	public <C extends GraphComputer> C compute(@Nullable final Class<C> graphComputerClass) throws IllegalArgumentException {
		throw new NotImplementedException("Compute with graph compute class is not supported");
	}

	@Override
	public GraphComputer compute() throws IllegalArgumentException {
		throw new NotImplementedException("Compute is not supported");
	}

	@Override
	public Transaction tx() {
		throw new NotImplementedException("Transactions are not supported");
	}

	@Override
	public Variables variables() {
		throw new NotImplementedException("Variables are not supported");
	}

	@Override
	public Configuration configuration() {
		throw new NotImplementedException("Configuration is not supported");
	}
}

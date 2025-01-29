/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.export.callchain.graphml;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang.NotImplementedException;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Edge;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.apache.tinkerpop.gremlin.structure.VertexProperty;
import org.apache.tinkerpop.gremlin.structure.VertexProperty.Cardinality;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.extensions.export.callchain.CallChainExporterJob;
import innowake.mining.shared.entities.ModuleLightweightPojo;

/**
 * A {@link Vertex} implementation for the {@code GraphMLGraph}.
 */
class GraphMLVertex implements Vertex {

	private static final String TYPE = "type";
	private static final String TECHNOLOGY = "technology";

	private final Graph graph;
	private final ModuleLightweightPojo module;
	private final List<Tuple2<String, String>> taxonomies;

	GraphMLVertex(final Graph graph, final ModuleLightweightPojo module, final List<Tuple2<String, String>> taxonomies) {
		this.graph = graph;
		this.module = module;
		this.taxonomies = taxonomies;
	}

	@Override
	public Object id() {
		return module.getId();
	}

	@Override
	public String label() {
		return module.getName();
	}

	@Override
	public Graph graph() {
		return graph;
	}

	@SuppressWarnings("unchecked")
	@Override
	public <V> Iterator<VertexProperty<V>> properties(@Nullable final String... propertyKeys) {
		final List<VertexProperty<V>> properties = new ArrayList<>();

		if (propertyKeys == null || propertyKeys.length == 0) {
			properties.add((VertexProperty<V>) new GraphMLVertexProperty(this, TECHNOLOGY, module.getTechnology().toString()));
			properties.add((VertexProperty<V>) new GraphMLVertexProperty(this, TYPE, module.getType().toString()));

			for (final Tuple2<String, String> t : taxonomies) {
				properties.add((VertexProperty<V>) new GraphMLVertexProperty(this, t.a, t.b));
			}

		} else {
			final Set<String> expectedKeys = new HashSet<>(propertyKeys.length);
			Collections.addAll(expectedKeys, propertyKeys);
			if (expectedKeys.contains(TECHNOLOGY)) {
				properties.add((VertexProperty<V>) new GraphMLVertexProperty(this, TECHNOLOGY, module.getTechnology().toString()));
			}
			if (expectedKeys.contains(TYPE)) {
				properties.add((VertexProperty<V>) new GraphMLVertexProperty(this, TYPE, module.getType().toString()));
			}
			if (checkTaxonomyPrefix(expectedKeys)) {

				final Map<String, List<String>> uniqueKeys = taxonomies.stream()
						.filter(t-> expectedKeys.contains(t.a))
						.collect(Collectors.groupingBy(t-> t.a,
										Collectors.mapping(t-> t.b, Collectors.toList())));
				
				for (final Entry<String, List<String>> e : uniqueKeys.entrySet()) {
					properties.add((VertexProperty<V>) new GraphMLVertexProperty(this, e.getKey(), e.getValue().toString()));
				}
			}
		}
		return properties.iterator();
	}

	private boolean checkTaxonomyPrefix(final Set<String> expectedKeys) {
		for (final String s : expectedKeys) {
			if (s.startsWith(CallChainExporterJob.TAXONOMY_PREFIX)) {
				return true;
			}
		}
		return false;
	}

	@Override
	public Iterator<Edge> edges(@Nullable final Direction direction, @Nullable final String... edgeLabels) {
		throw new NotImplementedException("Edges iterator is not supported");
	}

	@Override
	public Iterator<Vertex> vertices(@Nullable final Direction direction, @Nullable final String... edgeLabels) {
		throw new NotImplementedException("Vertices iterator is not supported");
	}

	@Override
	public void remove() {
		throw new NotImplementedException("Removing vertex is not supported");
	}

	@Override
	public Edge addEdge(@Nullable final String label, @Nullable final Vertex inVertex, @Nullable final Object... keyValues) {
		throw new NotImplementedException("Adding new edges is not supported");
	}

	@Override
	public <V> VertexProperty<V> property(@Nullable final Cardinality cardinality, @Nullable final String key, @Nullable final V value, @Nullable final Object... keyValues) {
		throw new NotImplementedException("Creating new vertex property is not supported");
	}

}

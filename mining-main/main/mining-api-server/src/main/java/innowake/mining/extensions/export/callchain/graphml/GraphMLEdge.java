/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.export.callchain.graphml;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

import org.apache.commons.lang.NotImplementedException;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Edge;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.apache.tinkerpop.gremlin.structure.Property;
import org.apache.tinkerpop.gremlin.structure.Vertex;

import innowake.lib.core.api.lang.Nullable;

/**
 * A {@link Edge} implementation for the {@code GraphMLGraph}.
 */
class GraphMLEdge implements Edge {

	private final Graph graph;
	private final Vertex source;
	private final Vertex target;
	private final String id;
	private final String relationship;
	private final Map<String, Object> relationshipAttributes;
	
	GraphMLEdge(final Graph graph, final Vertex source, final Vertex target, final String relationShip, final Map<String, Object> relationShipAttributes,
			final String id) {
		this.graph = graph;
		this.source = source;
		this.target = target;
		this.relationship = relationShip;
		this.relationshipAttributes = relationShipAttributes;
		this.id = id;
	}

	@Override
	public Object id() {
		return id;
	}

	@Override
	public String label() {
		return relationship;
	}

	@Override
	public Graph graph() {
		return graph;
	}

	@Override
	public Iterator<Vertex> vertices(@Nullable final Direction direction) {
		if (direction == null) {
			return Collections.emptyIterator();
		}

		switch (direction) {
			case IN:
				return Arrays.asList(target).iterator();
			case OUT:
				return Arrays.asList(source).iterator();
			default:
				return Arrays.asList(source, target).iterator();
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public <V> Iterator<Property<V>> properties(@Nullable final String... propertyKeys) {
		if (relationshipAttributes.isEmpty()) {
			return Collections.emptyIterator();
		}
	
		final Stream<Property<V>> properties;
		if (propertyKeys == null || propertyKeys.length == 0) {
				properties = relationshipAttributes.entrySet().stream()
									.map(entry -> (Property<V>) new GraphMLProperty<>(this, entry.getKey(), entry.getValue()));
		} else {
			final Set<String> expectedKeys = new HashSet<>(propertyKeys.length);
			Collections.addAll(expectedKeys, propertyKeys);
			properties = relationshipAttributes.entrySet().stream()
									.filter(entry -> expectedKeys.contains(entry.getKey()))
									.map(entry -> (Property<V>) new GraphMLProperty<>(this, entry.getKey(), entry.getValue()));
		}

		return properties.iterator();
	}

	@Override
	public <V> Property<V> property(@Nullable final String key, @Nullable final V value) {
		throw new NotImplementedException("Creating new edge property is not supported");
	}

	@Override
	public void remove() {
		throw new NotImplementedException("Removing edges is not supported");
	}
}

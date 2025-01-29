/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.export.callchain.graphml;

import java.util.Iterator;

import org.apache.commons.lang.NotImplementedException;
import org.apache.tinkerpop.gremlin.structure.Property;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.apache.tinkerpop.gremlin.structure.VertexProperty;

import innowake.lib.core.api.lang.Nullable;

/**
 * A {@link VertexProperty} implementation for the {@code GraphMLVertex}.
 */
class GraphMLVertexProperty extends GraphMLProperty<Vertex> implements VertexProperty<String> {
	
	/**
	 * Creates a new property instance.
	 * 
	 * @param vertex the vertex of this property
	 * @param key the property key
	 * @param value the property value
	 */
	GraphMLVertexProperty(final Vertex vertex, final String key, final String value) {
		super(vertex, key, value);
	}

	@Override
	public Object id() {
		throw new NotImplementedException("Getting id is not supported");
	}

	@Override
	public Property<Object> property(@Nullable final String key, @Nullable final Object value) {
		throw new NotImplementedException("Getting property is not supported");
	}

	@Override
	public <U> Iterator<Property<U>> properties(@Nullable final String... propertyKeys) {
		throw new NotImplementedException("Getting properties is not supported");
	}

}

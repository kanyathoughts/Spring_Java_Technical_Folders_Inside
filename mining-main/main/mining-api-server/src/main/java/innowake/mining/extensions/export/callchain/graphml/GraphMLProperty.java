/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.export.callchain.graphml;

import java.util.NoSuchElementException;

import org.apache.commons.lang.NotImplementedException;
import org.apache.tinkerpop.gremlin.structure.Element;
import org.apache.tinkerpop.gremlin.structure.Property;

/**
 * A {@link Property} implementation for the {@code GraphMLVertex}.
 */
class GraphMLProperty<E extends Element> implements Property<String> {

	private final E element;
	private final String key;
	private final String value;
	
	GraphMLProperty(final E element, final String key, final Object value) {
		this.element = element;
		this.key = key;
		this.value = value.toString();
	}
	
	@Override
	public String key() {
		return key;
	}

	@Override
	public String value() throws NoSuchElementException {
		return value;
	}

	@Override
	public boolean isPresent() {
		return true;
	}

	@Override
	public E element() {
		return element;
	}

	@Override
	public void remove() {
		throw new NotImplementedException("Removing properties is not supported");
	}

}

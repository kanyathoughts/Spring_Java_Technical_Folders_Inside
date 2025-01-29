/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.taxonomy.api;

import java.util.Collections;
import java.util.Map;

import innowake.mining.shared.model.RelationshipType;

/**
 * Default implementation of {@link DependencyEdge}.
 */
public class DefaultDependencyEdge implements DependencyEdge {

	private final DependencyModule incoming;
	private final DependencyModule outgoing;
	private final RelationshipType relationship;
	private final Map<String, Object> attributes;

	/**
	 * Creates a new dependency edge.
	 * 
	 * @param incoming the incoming module
	 * @param outgoing the outgoing module
	 * @param relationship the relationship between the modules starting from incoming
	 */
	public DefaultDependencyEdge(final DependencyModule incoming, final DependencyModule outgoing, final RelationshipType relationship) {
		this(incoming, outgoing, relationship, Collections.emptyMap());
	}

	/**
	 * Creates a new dependency edge.
	 * 
	 * @param incoming the incoming module
	 * @param outgoing the outgoing module
	 * @param relationship the relationship between the modules starting from incoming
	 * @param attributes additional attributes which are part of the edge
	 */
	public DefaultDependencyEdge(final DependencyModule incoming, final DependencyModule outgoing, final RelationshipType relationship, 
			final Map<String, Object> attributes) {
		this.incoming = incoming;
		this.outgoing = outgoing;
		this.relationship = relationship;
		this.attributes = attributes;
	}

	@Override
	public DependencyModule getIncomingModule() {
		return incoming;
	}

	@Override
	public DependencyModule getOutgoingModule() {
		return outgoing;
	}

	@Override
	public RelationshipType getRelationship() {
		return relationship;
	}

	@Override
	public Map<String, Object> getProperties() {
		return attributes;
	}

}

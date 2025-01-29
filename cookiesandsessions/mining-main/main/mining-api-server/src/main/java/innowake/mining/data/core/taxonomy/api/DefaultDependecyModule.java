/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.taxonomy.api;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;


/**
 * Default implementation of {@link DependencyModule}.
 */
public class DefaultDependecyModule implements DependencyModule {

	private final Technology technology;
	private final Type type;
	private final String name;
	private final EntityId projectId;
	private final @Nullable ModuleLocation fromLocation;
	private final List<DependencyEdge> incomings = new ArrayList<>();
	private final List<DependencyEdge> outgoings = new ArrayList<>();

	/**
	 * Creates a new dependency module.
	 * 
	 * @param technology the technology of the module
	 * @param type the type of the module
	 * @param name the name of the module
	 */
	public DefaultDependecyModule(final Technology technology, final Type type, final String name) {
		this.projectId = EntityId.VOID;
		this.technology = technology;
		this.type = type;
		this.name = name;
		this.fromLocation = null;
	}

	/**
	 * Creates a new dependency module.
	 * @param project project of the module
	 * @param technology the technology of the module
	 * @param type the type of the module
	 * @param name the name of the module
	 * @param fromLocation the {@linkplain ModuleLocation}
	 */
	public DefaultDependecyModule(final EntityId project, final Technology technology, final Type type, final String name,
			@Nullable final ModuleLocation fromLocation) {
		this.projectId = project;
		this.technology = technology;
		this.type = type;
		this.name = name;
		this.fromLocation = fromLocation;
	}

	@Override
	public Technology getTechnology() {
		return technology;
	}

	@Override
	public Type getType() {
		return type;
	}
	
	@Override
	public String getName() {
		return name;
	}

	@Override
	public EntityId getProjectId() {
		return projectId;
	}

	@Nullable
	public ModuleLocation getFromLocation() {
		return fromLocation;
	}

	@Override
	public List<DependencyEdge> getIncomings() {
		return incomings;
	}

	@Override
	public List<DependencyEdge> getIncomings(final RelationshipType relationship) {
		return incomings.stream().filter(edge -> edge.getRelationship() == relationship).collect(Collectors.toList());
	}

	@Override
	public List<DependencyEdge> getOutgoings() {
		return outgoings;
	}

	@Override
	public List<DependencyEdge> getOutgoings(final RelationshipType relationship) {
		return outgoings.stream().filter(edge -> edge.getRelationship() == relationship).collect(Collectors.toList());
	}

	/**
	 * Adds the given Module as an incoming Module with the corresponding Relationship.
	 * <p>
	 * This will not automatically add this Module to the outgoing list of the given Module.
	 *
	 * @param module the Module which is added as an incoming Module
	 * @param relationship the Relationship the edge is associated with
	 */
	public void addIncoming(final DependencyModule module, final RelationshipType relationship) {
		addIncoming(module, relationship, Collections.emptyMap());
	}

	/**
	 * Adds the given Module as an incoming Module with the corresponding Relationship and attributes.
	 * <p>
	 * This will not automatically add this Module to the outgoing list of the given Module.
	 *
	 * @param module the Module which is added as an incoming Module
	 * @param relationship the Relationship the edge is associated with
	 * @param attributes the attributes that should be associated with the edge
	 */
	public void addIncoming(final DependencyModule module, final RelationshipType relationship, final Map<String, Object> attributes) {
		incomings.add(new DefaultDependencyEdge(module, this, relationship, attributes));
	}

	/**
	 * Adds the given Module as an outgoing Module with the corresponding Relationship.
	 * <p>
	 * This will not automatically add this Module to the incoming list of the given Module.
	 *
	 * @param module the Module which is added as an outgoing Module
	 * @param relationship the Relationship the edge is associated with
	 */
	public void addOutgoing(final DependencyModule module, final RelationshipType relationship) {
		addOutgoing(module, relationship, Collections.emptyMap());
	}

	/**
	 * Adds the given Module as an outgoing Module with the corresponding Relationship and attributes.
	 * <p>
	 * This will not automatically add this Module to the incoming list of the given Module.
	 *
	 * @param module the Module which is added as an outgoing Module
	 * @param relationship the Relationship the edge is associated with
	 * @param attributes the attributes that should be associated with the edge
	 */
	public void addOutgoing(final DependencyModule module, final RelationshipType relationship, final Map<String, Object> attributes) {
		outgoings.add(new DefaultDependencyEdge(this, module, relationship, attributes));
	}
}

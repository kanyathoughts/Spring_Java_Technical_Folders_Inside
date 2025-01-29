/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model.datalineage.graph;

import java.beans.ConstructorProperties;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.model.ModuleLocation;

/**
 * Node in a {@link DataFlowGraph} that represents a data field.
 */
public class FieldNode extends DataFlowGraphNode {

	/**
	 * Creates a new field node with given id and display name.
	 * @param id id of the node - must be unique within the graph
	 * @param name display name for the node
	 * @param location The location of the traced field
	 */
	@ConstructorProperties({"id", "name", "location"})
	public FieldNode(final String id, final String name, @Nullable final ModuleLocation location) {
		super(Type.FIELD, id, name, location);
	}

}

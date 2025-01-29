/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model.datalineage.graph;

import java.beans.ConstructorProperties;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.model.ModuleLocation;

/**
 * Node in a {@link DataFlowGraph} that represents a statement.
 */
public class StatementNode extends DataFlowGraphNode {

	/**
	 * Creates a new statement node with given id and display name.
	 * 
	 * @param id id of the node - must be unique within the graph
	 * @param name display name for the node
	 * @param location The location of the traced field
	 */
	@ConstructorProperties({"id", "name", "location"})
	public StatementNode(final String id, final String name, @Nullable final ModuleLocation location) {
		super(Type.STATEMENT, id, name, location);
	}

}

/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.ast;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import innowake.mining.shared.model.RelationshipDirection;

/**
 * Types of relationships between ast nodes and modules.
 */
public enum AstModuleRelationshipType {
	ENTRY(RelationshipDirection.IN),
	HALT(RelationshipDirection.OUT),
	RETURN(RelationshipDirection.OUT),
	ROOT(RelationshipDirection.BOTH);
	
	public static final List<AstModuleRelationshipType> CONTROL_FLOW_TERMINALS = Collections.unmodifiableList(Arrays.asList(
		AstModuleRelationshipType.ENTRY,
		AstModuleRelationshipType.HALT,
		AstModuleRelationshipType.RETURN
	));
	
	private final RelationshipDirection direction;

	private AstModuleRelationshipType(RelationshipDirection direction) {
		this.direction = direction;
	}

	
	public RelationshipDirection getDirection() {
		return direction;
	}
	
}

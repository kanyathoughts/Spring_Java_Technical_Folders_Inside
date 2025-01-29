/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.ast;

/**
 * Types of relationships between ast nodes.
 */
public enum AstRelationshipType {

	/** For {@code AstBinding} edges */
	BINDING,
	/** For {@code RefersTo} edges */
	REFERS,
	/** For {@code Redefines} edges */
	REDEFINES,
	/** For {@code FlowsControl} edges */
	FLOW;
}

/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.dataflow;

/**
 * Types of relationships between data flow nodes.
 */
public enum DataFlowNodeRelationshipType {

	PROXY_SHORTCUT,
	RELATED_FIELD,
	WRITE_ACCESS,
	READ_ACCESS;
}

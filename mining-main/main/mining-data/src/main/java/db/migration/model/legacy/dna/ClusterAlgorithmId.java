/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package db.migration.model.legacy.dna;

/**
 * Constants class for cluster algorithm ids.
 * 
 * DO NOT CHANGE THIS FILE !!!
 * 
 * @deprecated must be used in old java based migration scripts only. Will be removed once migration from OrientDB to Postres is done.
 */
@Deprecated
public enum ClusterAlgorithmId {
	
	/**
	 * Louvain
	 */
	LOUVAIN("Louvain");
	
	private final String id;
	
	private ClusterAlgorithmId(final String id) {
		this.id = id;
	}

	/**
	 * Get the cluster algorithm id.
	 *
	 * @return the rule id.
	 */
	public String getId() {
		return id;
	}
	
}

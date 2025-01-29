/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.dna;

/**
 * Constants class for cluster algorithm ids.
 */
public enum DnaClusterAlgorithm {
	
	/**
	 * Louvain
	 */
	LOUVAIN("Louvain");
	
	private final String id;
	
	private DnaClusterAlgorithm(final String id) {
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

/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dna.community;

import innowake.mining.shared.entities.dna.DnaClusterAlgorithm;

/**
 * Defines an algorithm for clustering objects.<p>
 */
public interface ClusteringAlgorithm {
	
	/**
	 * Get the id for this cluster algorithm instance.
	 *
	 * @return The id.
	 */
	DnaClusterAlgorithm getId();
	
}

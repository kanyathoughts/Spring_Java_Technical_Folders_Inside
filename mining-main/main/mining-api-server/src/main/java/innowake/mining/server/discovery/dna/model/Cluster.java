/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dna.model;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

/**
 * Represent a cluster.
 */
public class Cluster {

	private final List<UUID> identifiers = new ArrayList<>();
	private final int clusterNumber;

	/**
	 * Create a new instance.
	 * 
	 * @param clusterNumber The number of the cluster.
	 */
	public Cluster(final int clusterNumber) {
		this.clusterNumber = clusterNumber;
	}

	/**
	 * Get the node identifiers for nodes part of this cluster.
	 *
	 * @return Unmodifiable list of node identifier.
	 */
	public List<UUID> getIdentifiers() {
		return Collections.unmodifiableList(identifiers);
	}
	
	/**
	 * Get the cluster number, which is an identifier for this cluster and unique over other clusters.
	 *
	 * @return The cluster number.
	 */
	public int getClusterNumber() {
		return clusterNumber;
	}

	/**
	 * Add a new node identifier as part of this cluster.
	 *
	 * @param identifier The new node identifier.
	 */
	public void add(final UUID identifier) {
		identifiers.add(identifier);
	}

	/**
	 * Get the cluster title.
	 * The cluster title should describe the reason of this community within nodes.
	 * @Todo: Implement cluster title finding.
	 *
	 * @return The cluster title.
	 */
	public String getTitle() {
		return "";
	}
	
}

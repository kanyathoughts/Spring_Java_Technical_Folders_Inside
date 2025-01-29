/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.externalparsing;

import innowake.lib.job.api.config.properties.ClusterProperties;

/**
 * Configuration properties for external parsing.
 */
public class ExternalParsingProperties {

	private boolean enabled;

	private ClusterProperties cluster;

	/**
	 * Returns whether external parsing is enabled.
	 * @return {@code true} if external parsing service is enabled
	 */
	public boolean isEnabled() {
		return enabled;
	}

	/**
	 * Sets whether external parsing is enabled.
	 * @param enabled whether external parsing is enabled
	 */
	public void setEnabled(final boolean enabled) {
		this.enabled = enabled;
	}

	/**
	 * Returns properties to configure the Hazelcast cluster for external parsing.
	 * @return Hazelcast cluster properties
	 */
	public ClusterProperties getCluster() {
		return cluster;
	}

	/**
	 * Sets the properties to configure the Hazelcast cluster for external parsing.
	 * @param cluster Hazelcast cluster properties
	 */
	public void setCluster(final ClusterProperties cluster) {
		this.cluster = cluster;
	}
}

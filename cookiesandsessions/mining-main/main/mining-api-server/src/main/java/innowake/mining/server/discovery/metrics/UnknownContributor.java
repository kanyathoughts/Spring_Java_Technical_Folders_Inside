/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics;

import innowake.mining.data.discovery.metrics.MetricsContributor;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.shared.discovery.DiscoveryException;

/**
 * Contributor to add all modules with language as UNKNOWN.
 */
public class UnknownContributor implements MetricsContributor {
	
	/**
	 * Returns if this collector can process the given {@link ModelArtifact}.
	 * 
	 * @param artifact the {@link ModelArtifact}
	 * @param phase the {@link innowake.mining.data.discovery.metrics.MetricsContributor.Phase}
	 * @return {@code true} if this collector can process the given {@link ModelArtifact}; {@code false} otherwise
	 */	
	public static boolean accept(final ModelArtifact artifact, final Phase phase) {
		/* !!!
		 * Dependent metrics are collected only if accept returns true for GENERIC_METRICS.
		 * Transitive metrics are collected only if accept returns true for GENERIC_METRICS and DEPENDENT_METRICS. */
		return false;
	}
	
	@Override
	public void calculateGenericMetrics(final ModelArtifact artifact) throws DiscoveryException {
		/*
		 * Generic Metrics not supported currently
		 */
	}

	@Override
	public void calculateDependentMetrics(final ModelArtifact artifact) throws DiscoveryException {
		/*
		 * Dependent Metrics not supported currently
		 */
	}
}

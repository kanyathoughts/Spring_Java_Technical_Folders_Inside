/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.server.discovery.metrics.generic.GenericMetricsContributor;
import innowake.mining.server.discovery.metrics.generic.MetricException;
import innowake.mining.shared.entities.SourceMetricsPojoPrototype;
import innowake.mining.shared.model.SourceMetrics;

/**
 * Utility class for executing {@link GenericMetricsContributor} and converting the result to {@link SourceMetricsPojoPrototype}
 * for use by Dawn contributors.
 */
public final class GenericMetricsUtil {

	private GenericMetricsUtil() { /* static utility class */ }

	/**
	 * Execute the contributor and retrieve the results.
	 *
	 * @param contributor the contributor to execute
	 * @return the metrics results
	 * @throws MetricException if calculating the metrics fails
	 */
	public static SourceMetrics executeAndGetResults(final GenericMetricsContributor contributor) throws MetricException {
		return executeAndGetResults(contributor, new ModelArtifact(), null);
	}
	
	/**
	 * Execute the contributor and retrieve the results.
	 *
	 * @param contributor the contributor to execute
	 * @param complexity the calculated code complexity value, set it to null if the value can not be computed
	 * @return the metrics results
	 * @throws MetricException if calculating the metrics fails
	 */
	public static SourceMetrics executeAndGetResults(final GenericMetricsContributor contributor, final Integer complexity) throws MetricException {
		return executeAndGetResults(contributor, new ModelArtifact(), complexity);
	}

	/**
	 * Execute the contributor on a custom {@link ModelArtifact} and retrieve the results.
	 *
	 * @param contributor the contributor to execute
	 * @param artifact the {@link ModelArtifact}
	 * @param complexity the code complexity value, set it to null if the value can not be computed
	 * @return the metrics results
	 * @throws MetricException if calculating the metrics fails
	 */
	public static SourceMetrics executeAndGetResults(final GenericMetricsContributor contributor, final ModelArtifact artifact,
			@Nullable final Integer complexity) throws MetricException {
		contributor.executeAndApplyResults(artifact);
		final SourceMetrics metrics = artifact.getSourceMetrics();
		final SourceMetrics sourceMetrics = metrics == null ? new SourceMetrics() : metrics;
		if (complexity != null) {
			sourceMetrics.setComplexityMcCabe(complexity);
		}
		return sourceMetrics;
	}
}

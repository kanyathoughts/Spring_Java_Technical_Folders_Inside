/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.extensions;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.discovery.metrics.ContributorParameters;
import innowake.mining.data.discovery.metrics.MetricsContributor;
import innowake.mining.data.discovery.metrics.MetricsContributor.Phase;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.shared.access.SourceService;

/**
 * Interface for MetricsContributorExtension that enables one to introduce custom {@link MetricsContributor} for the Discover metrics process.
 * @deprecated Implement {@code innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource} or
 *             {@code innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributor} instead.
 */
@Deprecated(forRemoval = true)
public interface MetricsContributorExtension {
	
	/**
	 * Returns whether this contributor extension can process the given {@link ModelArtifact}
	 * in the given {@link Phase}.
	 * <p>
	 * If this method returns {@code true} then {@link #init(ContributorParameters, SourceService, TimedWorker)}
	 * will be called to produce a contributor instance for the given model artifact and the current phase.
	 *
	 * @param artifact the artifact for which to contribute
	 * @param phase the current discovery phase
	 * @return whether the extension can process this artifact in the current phase
	 */
	boolean accept(ModelArtifact artifact, Phase phase);

	/**
	 * Initializes and creates a new instance of the custom metrics contributor.
	 * Depending on the current Discovery {@link Phase} one of
	 * {@link MetricsContributor#calculateGenericMetrics(ModelArtifact)}, {@link MetricsContributor#calculateDependentMetrics(ModelArtifact)}
	 * or {@link MetricsContributor#calculateTransitiveMetrics(ModelArtifact)} will be called on the returned contributor. 
	 * 
	 * @param parameters the {@link ContributorParameters}
	 * @param sourceService to access the source code content 
	 * @param timedWorker the {@link TimedWorker}
	 * @return a metrics contributor instance initialized with the given parameters
	 */
	MetricsContributor init(final ContributorParameters parameters, final SourceService sourceService, @Nullable final TimedWorker timedWorker);
	
	/**
	 * Return {@code true} to get a {@link TimedWorker} provided in {@link this#init(Parameters, SourceObjectDao, TimedWorker)}. 
	 *
	 * @return {@code false} as default
	 */
	default boolean requiresTimedWorker() {
		return false;
	}
}

/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.generic.input;

import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.ndt.core.parsing.ITokenPartitioning;

/**
 * Interface for getting the source as {@link ITokenPartitioning} for a given {@link ModelArtifact}.
 */
public interface TokenProvider extends MetricInputProvider {
	
	/**
	 * Returns the token partitioning of the given artifact.
	 *
	 * @param artifact for which the token partitioning is required
	 * @return instance of {@link ITokenPartitioning} of the given artifact
	 */
	public ITokenPartitioning getTokens(ModelArtifact artifact);

}

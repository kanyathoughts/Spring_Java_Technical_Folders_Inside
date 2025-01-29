package innowake.mining.server.discovery.metrics.generic.input;

import innowake.mining.data.model.discovery.ModelArtifact;

/**
 * Interface for getting the source as {@link String} for a given {@link ModelArtifact}.
 */
public interface SourceProvider extends MetricInputProvider {
	
	/**
	 * Returns the source as {@link String} for the given artifact.
	 * 
	 * @param artifact the entry to get the source for
	 * @return the source of the given artifact
	 */
	public String getSource(ModelArtifact artifact);
}

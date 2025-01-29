package innowake.mining.server.discovery.metrics.generic;

import innowake.mining.data.model.discovery.ModelArtifact;

/**
 * Base interface for the result of a metric.
 *  
 * @param <T> the type the metric is working on
 */
public interface MetricResult<T> {
	
	/**
	 * Returns the Metric Type for the given Metric Result.
	 *
	 * @return the metric type for the given result
	 */
	MetricType<T> getMetricType();
	
	/**
	 * Applies the result to the given artifact; i.e. the calculated metrics are
	 * stored in the artifact.
	 * 
	 * @param artifact the artifact the result should be applied to
	 */
	void applyTo(ModelArtifact artifact);

}

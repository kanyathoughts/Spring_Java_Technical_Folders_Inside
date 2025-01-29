package innowake.mining.server.discovery.metrics.generic;

import innowake.mining.server.discovery.metrics.generic.input.InputType;

/**
 * Base interface for all metrics 
 *
 * @param <T> input type
 */
public interface Metric<T> {
	
	/**
	 * Returns the Input type of the Metric.
	 *
	 * @return input type of the metric
	 */
	InputType<T> getInputType();
	
	/**
	 * Returns the Metric type of the Metric.
	 *
	 * @return Metric type of the metric
	 */
	MetricType<T> getMetricType();
	
	/**
	 * Metric implementations should be typesafe. Since {@link GenericMetricsContributor} is dealing with metrics of different
	 * types we have to provide this special generic method which delegates to the typed {@link #execute(Object)} method.
	 * This method is not designed to be overriden.
	 * 
	 * @param input the untyped input
	 * @return typed metric result
	 */
	default MetricResult<T> executeGeneric(final Object input) {
		final T typedInput = getInputType().getTypeClass().cast(input);
		return execute(typedInput);
	}
	
	/**
	 * Executes the metric for the given input.
	 * 
	 * @param input the input for the metric
	 * @return the metric result
	 */
	MetricResult<T> execute(T input);
	
}

package innowake.mining.server.discovery.metrics.generic;

import innowake.mining.server.discovery.metrics.generic.input.InputType;

/**
 * Base implementation for all metrics. Only dealing with the {@link InputType} here. 
 *
 * @param <T> the actual input type the metric is working on
 */
public abstract class AbstractMetric<T> implements Metric<T> {
	
	private final InputType<T> inputType;
	
	/**
	 * Constructor to initialize Abstract Metric.
	 * 
	 * @param inputType for the given metric
	 */
	public AbstractMetric(final InputType<T> inputType) {
		this.inputType = inputType;
	}
	
	@Override
	public InputType<T> getInputType() {
		return inputType;
	}

}

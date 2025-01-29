package innowake.mining.server.discovery.metrics.generic;

import innowake.mining.server.discovery.metrics.generic.complexity.McCabeComplexityFlatMetric;
import innowake.mining.server.discovery.metrics.generic.complexity.McCabeComplexityMetric;
import innowake.mining.server.discovery.metrics.generic.input.InputType;
import innowake.mining.server.discovery.metrics.generic.loc.LocMetric;
import innowake.ndt.core.parsing.ITokenPartitioning;
import innowake.ndt.core.parsing.ast.AstModel;

/**
 * Used to instantiate the default implementation for each {@link MetricType}.
 */
public class MetricFactory {

	private MetricFactory() {}

	/**
	 * Returns the default metric implementation for the given {@link MetricType}.
	 * 
	 * @param <T> the type the metric is working on
	 * @param metricType the metric type an implementation should be returned for
	 * @return the metric implementation
	 */
	@SuppressWarnings("unchecked")
	public static <T> Metric<T> get(final MetricType<T> metricType) {
		if (metricType == MetricType.MCCABE_COMPLEXITY) {
			return (Metric<T>) new McCabeComplexityMetric((InputType<AstModel>) metricType.getInputType());
		} else if (metricType == MetricType.MCCABE_COMPLEXITY_FLAT) {
			return (Metric<T>) new McCabeComplexityFlatMetric((InputType<AstModel>) metricType.getInputType());
		} else if (metricType == MetricType.LOC_LOC) {
			return (Metric<T>) new LocMetric((InputType<ITokenPartitioning>) metricType.getInputType());
		} else {
			throw new IllegalStateException("unsupported metric: " + metricType);
		}
	}

}

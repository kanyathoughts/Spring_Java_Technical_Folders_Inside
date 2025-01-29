package innowake.mining.server.discovery.metrics.generic;

import innowake.mining.server.discovery.metrics.generic.input.InputType;
import innowake.ndt.core.parsing.ITokenPartitioning;
import innowake.ndt.core.parsing.ast.AstModel;

/**
 * Type of a metric. Each type is associated with a corresponding {@link InputType}. 
 *
 * @param <T> the type the metric is working on.
 */
public class MetricType<T> {

	public static final MetricType<AstModel> MCCABE_COMPLEXITY = new MetricType<>(InputType.AST_MODEL);
	public static final MetricType<AstModel> MCCABE_COMPLEXITY_FLAT = new MetricType<>(InputType.AST_MODEL);
	public static final MetricType<ITokenPartitioning> LOC_LOC = new MetricType<>(InputType.TOKEN);
	
	private InputType<T> inputType;

	private MetricType(final InputType<T> type) {
		this.inputType = type;
	}
	
	/**
	 * Returns the Input type for the Metric Type.
	 *
	 * @return input type for the metric type
	 */
	public InputType<T> getInputType() {
		return inputType;
	}
	
}

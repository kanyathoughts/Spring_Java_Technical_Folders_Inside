package innowake.mining.server.discovery.metrics.generic;

import innowake.mining.server.discovery.metrics.generic.input.InputType;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.ast.AstNode;

/**
 * Base class for all metric implementations using an {@link AstModel} for calculation.
 */
public abstract class AstBasedMetric extends AbstractMetric<AstModel> {
	
	/**
	 * Constructor to initialize AstBasedMetric.
	 * 
	 * @param inputType for the given metric
	 */
	public AstBasedMetric(final InputType<AstModel> inputType) {
		super(inputType);
	}

	/**
	 * Called for each node of the {@link AstModel}. Based on the given node the metric implementation
	 * can calculate its results.
	 * 
	 * @param node the {@link AstNode} to used for calculation 
	 * @return true if the metrics for all the child has been calculated and calculation for no more child is required. <br/>
	 * false if the metric calculation for only current child has been calculation and deep child needs calculation.
	 */
	public abstract boolean handle(AstNode node); 
	
	/**
	 * Returns the result of the metric. Since all {@link AstBasedMetric} implementations are executed in "parallel"
	 * we need this method to grab the final result after AST traversal is finished.
	 * 
	 * @return the result of the metric
	 */
	public abstract MetricResult<AstModel> getResult();
	
	/**
	 * {@link AstBasedMetric} are not designed to be executed individually. Therefore this method is overriden to raise an error.
	 */
	@Override
	public MetricResult<AstModel> execute(final AstModel input) {
		throw new IllegalStateException("override this method if you want to implement your own traversal; otherwise just implement other abstract methods!");
	}

}

/* Copyright (c) 2021 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.metrics.generic.complexity;

import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.server.discovery.metrics.generic.MetricResult;
import innowake.mining.server.discovery.metrics.generic.MetricType;
import innowake.ndt.core.parsing.ast.AstModel;

/**
 * The result of the complexity metric
 */
public class McCabeComplexityResult implements MetricResult<AstModel> {

	private final int complexity;
	
	/**
	 * Initializes the Complexity Metric Result.
	 * 
	 * @param complexity of the given result
	 */
	public McCabeComplexityResult(final int complexity) {
		this.complexity = complexity;
	}
	
	@Override
	public MetricType<AstModel> getMetricType() {
		return MetricType.MCCABE_COMPLEXITY;
	}
	
	@Override
	public void applyTo(final ModelArtifact artifact) {
		artifact.setComplexity(complexity);
	}
	
	/**
	 * Returns the complexity of the given complexity metric result.
	 *
	 * @return complexity of the given complexity metric result
	 */
	public int getComplexity() {
		return complexity;
	}

}

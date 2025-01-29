/* Copyright (c) 2021 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.metrics.generic.complexity;

import java.util.ArrayList;
import java.util.List;
import innowake.mining.server.discovery.metrics.generic.AstBasedMetric;
import innowake.mining.server.discovery.metrics.generic.MetricResult;
import innowake.mining.server.discovery.metrics.generic.MetricType;
import innowake.mining.server.discovery.metrics.generic.input.InputType;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.ast.model.McCabeComplexityNode;
import innowake.ndt.core.parsing.ast.model.McCabeComplexityUnit;
import innowake.ndt.core.parsing.ast.visitor.TopDownVisitor;

/**
 * A generic implementation of the McCabe Complexity metric. This class has handle method which will calculate
 * the complexity. The complexity count can be retrieved by using the getResult method.
 */
public class McCabeComplexityMetric extends AstBasedMetric {

	protected int complexity;
	
	/**
	 * Initializes the Complexity Metric.
	 * 
	 * @param inputType for the given Complexity Metric
	 */
	public McCabeComplexityMetric(final InputType<AstModel> inputType) {
		super(inputType);
	}

	@Override
	public boolean handle(final AstNode node) {
		complexity += (int) Math.round(getCyclomaticComplexity(node));
		return true;
	}
	
	@Override
	public MetricResult<AstModel> getResult() {
		return new McCabeComplexityResult(complexity);
	}
	
	@Override
	public MetricType<AstModel> getMetricType() {
		return MetricType.MCCABE_COMPLEXITY;
	}
	
	/**
	 * This will sum up all the complexity for the direct units and direct complexity nodes and
	 * calculate the average of the same. <br/>
	 * If there is a single direct complexity node also, then it will be considered as a different
	 * unit for calculating average.
	 *
	 * @return the Cyclomatic complexities
	 */
	private Double getCyclomaticComplexity(final AstNode astNode) {
		final List<Integer> nodeComplexities = new ArrayList<>();
		final List<Double> unitComplexities = new ArrayList<>();
		new TopDownVisitor<AstNode>(node -> {
			if (astNode != node
					&& McCabeComplexityUnit.class.isAssignableFrom(node.getClass())) {
				final McCabeComplexityUnit complexityUnit =
						node.getGenericType(McCabeComplexityUnit.class);
				if (complexityUnit != null) {
					final Double complexityUnitCount = this.getCyclomaticComplexity(node);
					if (complexityUnitCount.doubleValue() > 0) {
						unitComplexities.add(complexityUnitCount);
					}
				}
				return false;
			}
			if (McCabeComplexityNode.class.isAssignableFrom(node.getClass())) {
				final int nodeComplexityCount = ((McCabeComplexityNode) node).getConditionCount();
				if (nodeComplexityCount > 0) {
					nodeComplexities.add(Integer.valueOf(nodeComplexityCount));
				}
			}
			return true;
		}).visit(astNode);
		/* WMIN-3276: If the Unit is empty, no nested unit or no node then the complexity of the unit will be 1*/
		if (nodeComplexities.isEmpty() && unitComplexities.isEmpty()) {
			return 1.0;
		}
		/* This will sum up all the complexities under the current unit. */
		double unitTotalComplexity = nodeComplexities.stream()
				.mapToInt(Integer::intValue)
				.sum()
				+ unitComplexities.stream()
				.mapToDouble(Double::doubleValue)
				.sum();
		/* WMIN-3034:complexity <= 1 should be ignored while calculating the average.*/
		int noOfUnits = (int) unitComplexities.stream().filter(unit -> unit > 1).count();
		/* If a unit has minimum 1 condition(complexity node) then this node is considered as a part of virtual unit with complexity as n+1 */
		if ( ! nodeComplexities.isEmpty()) {
			/* Under any unit, the combination of all the direct complexity nodes together are considered as 1 virtual unit and will add up while 
			   calculating the average. */
			++noOfUnits;
			++unitTotalComplexity;
		}
		/* The complexity should be the average of all the Unit */
		return Double.valueOf(noOfUnits > 0 ? Math.round(unitTotalComplexity / noOfUnits) : unitTotalComplexity);
	}
}

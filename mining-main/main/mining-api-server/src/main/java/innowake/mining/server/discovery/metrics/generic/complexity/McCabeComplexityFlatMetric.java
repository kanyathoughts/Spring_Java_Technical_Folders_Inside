/* Copyright (c) 2021 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.metrics.generic.complexity;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.concurrent.atomic.AtomicInteger;
import innowake.mining.server.discovery.metrics.generic.input.InputType;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.ast.model.McCabeComplexityNode;
import innowake.ndt.core.parsing.ast.model.McCabeComplexityUnit;

/**
 * A generic implementation of the McCabe Complexity metric for the flat nodes. The child nodes of program are traversed and checked for instance of
 * {@link McCabeComplexityUnit}. If found then any upcoming {@link McCabeComplexityNode} is treated as a part of last {@link McCabeComplexityUnit}
 */
public class McCabeComplexityFlatMetric extends McCabeComplexityMetric {

	/**
	 * Initializes the Complexity Metric.
	 * 
	 * @param inputType for the given Complexity Metric
	 */
	public McCabeComplexityFlatMetric(final InputType<AstModel> inputType) {
		super(inputType);
	}

	@Override
	public boolean handle(final AstNode program) {
		final Deque<AtomicInteger> unitComplexity = new ArrayDeque<>();
		boolean firstUnitFound = false;
		for (final AstNode astNode : program.getChildren()) {
			final McCabeComplexityNode cabeComplexityNode = astNode.getGenericType(McCabeComplexityNode.class);
			if (astNode.getGenericType(McCabeComplexityUnit.class) != null) {
				unitComplexity.push(new AtomicInteger(1));
				firstUnitFound = true;
			}
			if (firstUnitFound && cabeComplexityNode != null) {
				unitComplexity.getFirst().addAndGet(cabeComplexityNode.getConditionCount());
			}
		}
		final double complexitySum = unitComplexity.stream().mapToInt(AtomicInteger::intValue).sum();
		final int unitCount = (int) unitComplexity.stream().filter(unit -> unit.get() > 1).count();
		complexity += (int) Math.round(unitCount > 0 ? complexitySum / unitCount : complexitySum);
		return true;
	}
}

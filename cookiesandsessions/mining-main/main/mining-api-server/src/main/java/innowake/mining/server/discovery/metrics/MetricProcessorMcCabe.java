/* Copyright (c) 2018 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.metrics;

import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.ast.model.Branch;
import innowake.ndt.core.parsing.ast.model.DefaultBranch;
import innowake.ndt.core.parsing.ast.visitor.TopDownVisitor;
import innowake.ndt.core.parsing.ast.visitor.Visitor;

/**
 * Collects the McCabe metrics for the module.
 * @param <T> type of AstNode
 */
public class MetricProcessorMcCabe<T extends AstNode> implements Visitor<T> {

	private int complexity = 1;
	private final TopDownVisitor<T> topDownVisitor;

	/**
	 * Initializes the top down Visitor.
	 */
	public MetricProcessorMcCabe() {
		topDownVisitor = new TopDownVisitor<>(this);
	}

	/**
	 * Calculate the McCabe metric. 
	 * Count every derivative of BRANCH interface excluding DEFAULTBRANCH.
	 *
	 * @param rootNode The model to calculate the complexity for.
	 * @return The complexity value (count of code branches).
	 */
	public int calculate(final T rootNode) {
		topDownVisitor.visit(rootNode);
		return complexity;
	}

	/**
	 * Visitor to calculate the complexity for BRANCH interface only, exclude DEFAULTBRANCH.
	 * 
	 * @param node refers to AstNode
	 */
	@Override
	public boolean visit(final AstNode node) {
		if (node instanceof Branch && ! (node instanceof DefaultBranch)) {
			complexity++;
			node.getChildren().forEach(this::visit);
		}
		return true;
	}

}

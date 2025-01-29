/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;

import innowake.mining.data.core.CobolAstNodeType;
import innowake.mining.data.core.api.AbstractTraverser;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * A traverser for transforming AST nodes to {@link String} for controlflow tests.
 */
public class ControlFlowAstToString extends AbstractTraverser<AstNodePojo, StringBuilder> {

	private final Function<AstNodePojo, String> toStringFunction;
	private final StringBuilder stringBuilder = new StringBuilder();
	
	private int indent = -1;
	
	/**
	 * Constructor.
	 * 
	 * @param toStringFunction the String conversion function for an AST node.
	 */
	public ControlFlowAstToString(final Function<AstNodePojo, String> toStringFunction) {
		super(node -> node.getChildren().stream().filter(n -> ! CobolAstNodeType.REFERENCE_EXPRESSION.getType().equals(n.getType())).collect(Collectors.toList()));
		this.toStringFunction = toStringFunction;
	}
	
	@Override
	protected StringBuilder visit(final AstNodePojo node) {
		indent++;
		final String nodeContent = toStringFunction.apply(node);
 		if (StringUtils.isEmpty(nodeContent)) {
 			return stringBuilder;
 		}
		final StringBuilder result = stringBuilder.append(StringUtils.repeat(" ", indent))
				.append(nodeContent)
				.append("\n");
		return result;
	}
	
	@Override
	protected StringBuilder traversed(final StringBuilder node, final List<StringBuilder> children) {
		indent--;
		return node;
	}
}

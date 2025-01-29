/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core;

import java.util.List;
import java.util.function.Function;

import org.apache.commons.lang.StringUtils;

import innowake.mining.data.core.api.AbstractTraverser;
import innowake.mining.shared.entities.ast.StoreAstPrototype;

/**
 * A traverser for transforming AST Nodes to {@link String}.
 */
public class AstToString extends AbstractTraverser<StoreAstPrototype, StringBuilder> {

	private final Function<StoreAstPrototype, String> toStringFunction;
	private final StringBuilder stringBuilder = new StringBuilder();
	
	private int indent = 0;
	
	/**
	 * Constructor.
	 * 
	 * @param toStringFunction the String conversion function for an AST Node.
	 */
	public AstToString(final Function<StoreAstPrototype, String> toStringFunction) {
		super(node -> node.children());
		this.toStringFunction = toStringFunction;
	}
	
	@Override
	protected StringBuilder visit(final StoreAstPrototype node) {
		final StringBuilder result = stringBuilder.append(StringUtils.repeat(" ", indent))
				.append(toStringFunction.apply(node))
				.append("\n");
		indent++;
		return result;
	}
	
	@Override
	protected StringBuilder traversed(final StringBuilder node, final List<StringBuilder> children) {
		indent--;
		return node;
	}

}

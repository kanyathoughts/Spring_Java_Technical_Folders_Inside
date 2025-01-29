/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.api;

import java.util.LinkedList;
import java.util.List;
import java.util.function.Predicate;

import innowake.mining.shared.entities.ast.AstNodePojo;


/**
 * A filtered traverser which collects all filtered AST nodes.
 */
public class AstNodeCollectingTraverser extends AbstractFilteredTraverser<AstNodePojo> {

	protected final List<AstNodePojo> collectionResult = new LinkedList<>();

	/**
	 * Creates a new instance with a depth of traversed children of {@code Integer#MAX_VALUE}.
	 * 
	 * @param filterPredicate the filter predicate
	 */
	public AstNodeCollectingTraverser(final Predicate<AstNodePojo> filterPredicate) {
		super(AstNodePojo::getChildren, filterPredicate);
	}

	/**
	 * Creates a new instance with a defined depth of traversed children.
	 * 
	 * @param filterPredicate the filter predicate
	 * @param depth the depth of traversed children
	 */
	public AstNodeCollectingTraverser(final Predicate<AstNodePojo> filterPredicate, final int depth) {
		super(AstNodePojo::getChildren, filterPredicate, depth);
	}

	@Override
	protected AstNodePojo visit(AstNodePojo element) {
		collectionResult.add(element);
		return element;
	}

	/**
	 * @return the collected filtered AST nodes
	 */
	public List<AstNodePojo> collect() {
		return collectionResult;
	}

}

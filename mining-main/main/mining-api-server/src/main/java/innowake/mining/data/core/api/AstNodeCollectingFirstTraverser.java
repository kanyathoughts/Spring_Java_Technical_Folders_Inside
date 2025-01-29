/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.api;

import java.util.Optional;
import java.util.function.Predicate;

import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * A filtered traverser which collects the first filtered AST node.
 */
final class AstNodeCollectingFirstTraverser extends AstNodeCollectingTraverser {

	/**
	 * Creates a new instance with a depth of traversed children of {@code Integer#MAX_VALUE}.
	 * 
	 * @param filterPredicate the filter predicate
	 */
	AstNodeCollectingFirstTraverser(final Predicate<AstNodePojo> filterPredicate) {
		super(filterPredicate);
	}

	/**
	 * Creates a new instance with a defined depth of traversed children.
	 * 
	 * @param filterPredicate the filter predicate
	 * @param depth the depth of traversed children
	 */
	AstNodeCollectingFirstTraverser(final Predicate<AstNodePojo> filterPredicate, final int depth) {
		super(filterPredicate, depth);
	}

	@Override
	public AstNodePojo traverse(final AstNodePojo node) {
		if (collectionResult.isEmpty()) {
			return super.traverse(node);
		}
		return node;
	}
	
	Optional<AstNodePojo> first() {
		return Optional.ofNullable(collectionResult.isEmpty() ? null : collectionResult.get(0));
	}	

}

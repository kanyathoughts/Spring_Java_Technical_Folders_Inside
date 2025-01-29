/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.storeast.impl;

import java.util.LinkedList;
import java.util.List;
import java.util.function.Predicate;

import innowake.mining.data.core.api.AbstractFilteredTraverser;
import innowake.mining.shared.entities.ast.StoreAstPrototype;

/**
 * Collects AST node prototypes based on a given predicate. 
 */
public class AstPrototypeCollectingTraverser extends AbstractFilteredTraverser<StoreAstPrototype> {

	protected final List<StoreAstPrototype> collectionResult = new LinkedList<>();

	/**
	 * Creates a new instance with a depth of traversed children of {@code Integer#MAX_VALUE}.
	 * 
	 * @param filterPredicate the filter predicate
	 */
	public AstPrototypeCollectingTraverser(final Predicate<StoreAstPrototype> filterPredicate) {
		super(StoreAstPrototype::children, filterPredicate);
	}

	/**
	 * Creates a new instance with a defined depth of traversed children.
	 * 
	 * @param filterPredicate the filter predicate
	 * @param depth the depth of traversed children
	 */
	public AstPrototypeCollectingTraverser(final Predicate<StoreAstPrototype> filterPredicate, final int depth) {
		super(StoreAstPrototype::children, filterPredicate, depth);
	}

	@Override
	protected StoreAstPrototype visit(StoreAstPrototype element) {
		collectionResult.add(element);
		return element;
	}
	
	/**
	 * @return the collected filtered AST nodes
	 */
	public List<StoreAstPrototype> collect() {
		return collectionResult;
	}

	/**
	 * @param element Note to traverse.
	 * @return the collected filtered AST nodes
	 */
	public List<StoreAstPrototype> collect(StoreAstPrototype element) {
		super.traverse(element);
		return collect();
	}

}

/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.api;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * A {@link AbstractTraverser} with a filter that predicts if an element is visited and a depth that defines the depth of traversed children.
 * 
 * @param <T> the concrete type of traversed elements
 */
public abstract class AbstractFilteredTraverser<T> extends AbstractTraverser<T, T> {

	private final Predicate<T> filterPredicate;
	
	private int depth;
	
	/**
	 * Creates a new instance with a depth of traversed children of {@code Integer#MAX_VALUE}.
	 * 
	 * @param reproductionFunction function to reproduce elements
	 * @param filterPredicate the filter predicate
	 */
	protected AbstractFilteredTraverser(final Function<T, List<T>> reproductionFunction, final Predicate<T> filterPredicate) {
		super(reproductionFunction);
		this.filterPredicate = filterPredicate;
		this.depth = Integer.MAX_VALUE;
	}
	
	/**
	 * Creates a new instance with a defined depth of traversed children.
	 * 
	 * @param reproductionFunction function to reproduce elements
	 * @param filterPredicate the filter predicate
	 * @param depth the depth of traversed children
	 */
	protected AbstractFilteredTraverser(final Function<T, List<T>> reproductionFunction, final Predicate<T> filterPredicate, final int depth) {
		super(reproductionFunction);
		this.filterPredicate = filterPredicate;
		this.depth = depth;
	}
	
	@Override
	public T traverse(final T element) {
		/* traverse elements from top to bottom */
		final T visitedElement = filterPredicate.test(element) ? visit(element) : element;
		final List<T> visitedReproductions = new ArrayList<>();
		if (depth > 0) {
			depth--;
			for (final T reproducedElement : reproductionFunction.apply(element)) {
				visitedReproductions.add(traverse(reproducedElement));
			}
		}
		return traversed(visitedElement, visitedReproductions);
	}
}

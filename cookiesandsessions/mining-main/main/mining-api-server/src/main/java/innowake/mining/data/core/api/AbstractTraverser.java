/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.api;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

/**
 * A traverser for elements of type {@code T} which traverses the element and its reproductions from top to bottom and returns an element of type {@code R}.
 * 
 * @param <T> the concrete type of traversed elements
 * @param <R> the concrete type of returned elements
 */
public abstract class AbstractTraverser<T, R> {

	protected final Function<T, List<T>> reproductionFunction;
	
	/**
	 * Constructor.
	 * 
	 * @param reproductionFunction function to reproduce elements
	 */
	protected AbstractTraverser(final Function<T, List<T>> reproductionFunction) {
		this.reproductionFunction = reproductionFunction;
	}
	
    /**
     * Traverses an element and its reproductions of type {@code T} and returns an element of type {@code R}.
     *
     * @param element the element to traverse
     * @return a result element
     */
	public R traverse(final T element) {
		/* traverse elements from top to bottom */
		final R visitedElement = visit(element);
		final List<R> visitedReproductions = new ArrayList<>();
		for (final T reproducedElement : reproductionFunction.apply(element)) {
			visitedReproductions.add(traverse(reproducedElement));
		}
		return traversed(visitedElement, visitedReproductions);
	}
	
	/**
	 * Visits an element of type {@code T} and returns an element of type {@code R}.
	 *
	 * @param element the element to visit
	 * @return a result element
	 */
	protected abstract R visit(final T element);
	
	/**
	 * Called when an element and its reproductions are successfully traversed by {@link #traverse(Object)}.
	 * This method is intended to manipulate elements after traverse.
	 *
	 * @param element the traversed element
	 * @param reproductions the traversed reproductions
	 * @return a result element
	 */
	protected R traversed(final R element, final List<R> reproductions) {
		/* just return element by default */
		return element;
	}
}

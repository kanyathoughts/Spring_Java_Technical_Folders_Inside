/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.lang;

import java.util.function.BiConsumer;
import java.util.function.Consumer;

import innowake.lib.core.api.lang.Nullable;

/**
 * State object to keeping track of a sequence of arguments being applied to an object.
 * @param <S> Type of object the iteration pertains to.
 * @param <T> Type of the argument to be processed in each step.
 */
public class IterativeAction<S, T> implements Consumer<T> {
	
	private final BiConsumer<IterativeAction<S, T>, T> action;
	private final S subject;
	private boolean first;
	private int count;
	
	/**
	 * Creates an iteration instance.
	 * @param subject Object to be modified iteratively.
	 * @param action Action to perform for every iteration, receiving this state object and an argument. 
	 * @param initialFirst Determines if the initial element of the iteration is the first in a sequence.
	 */
	public IterativeAction(final S subject, final BiConsumer<IterativeAction<S, T>, T> action, final boolean initialFirst) {
		this.subject = subject;
		this.action = action;
		this.first = initialFirst;
		this.count = 0;
	}
	
	/**
	 * Creates an iteration instance.
	 * @param subject Object to be modified iteratively.
	 * @param action Action to perform for every iteration, receiving this state object and an argument. 
	 */
	public IterativeAction(final S subject, final BiConsumer<IterativeAction<S, T>, T> action) {
		this(subject, action, true);
	}
	
	/**
	 * Checks if this is the first iteration.
	 * @return If this is the first iteration.
	 */
	public boolean isFirst() {
		return first;
	}
	
	/**
	 * Gets the number of iterations performed so far.
	 * @return The number of arguments this Iteration has accepted.
	 */
	public int getCount() {
		return count;
	}
	
	/**
	 * Gets the subject of this iteration.
	 * @return Object to be modified iteratively.
	 */
	public S subject() {
		return subject;
	}
	
	@Override
	public void accept(@Nullable final T arg) {
		action.accept(this, arg);
		first = false;
		count++;
	}
	
}

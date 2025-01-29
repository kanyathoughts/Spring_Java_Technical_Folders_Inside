/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api.task;

import java.io.Serializable;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * This {@link Iterable} provides instances of {@link Task} to be submitted for execution.
 * 
 * @param <S> the concrete return type of the task
 */
public interface TaskSource<S extends Serializable> extends Iterable<Task<S>> {

	/**
	 * @return {@code true} if there are more tasks to be submitted; {@code false} otherwise
	 */
	boolean hasNextTask();
	
	/**
	 * @return the next {@link Task} instance to be submitted for execution
	 */
	Task<S> nextTask();
	
	@Override
	default Iterator<Task<S>> iterator() {
		return new Iterator<Task<S>>() {

			@Override
			public boolean hasNext() {
				return hasNextTask();
			}

			@Override
			public Task<S> next() {
				if ( ! hasNext()) {
					throw new NoSuchElementException("There are no more tasks.");
				}
				return nextTask();
			}
			
			@Override
			public void remove() {
				throw new UnsupportedOperationException();
			}
		};
	}
}

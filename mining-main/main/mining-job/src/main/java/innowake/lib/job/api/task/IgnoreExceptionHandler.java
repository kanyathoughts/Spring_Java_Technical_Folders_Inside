/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api.task;

import java.io.Serializable;

import innowake.lib.job.api.Result;

/**
 * Implementation of a {@link TaskExceptionHandler} that will do no exception handling at all.
 * 
 * @param <S> the concrete return type of the task
 */
public class IgnoreExceptionHandler<S extends Serializable> implements TaskExceptionHandler<S> {
	
	@Override
	public void handle(final String taskId, final Result<S> result) {
		/* do nothing */
	}
}

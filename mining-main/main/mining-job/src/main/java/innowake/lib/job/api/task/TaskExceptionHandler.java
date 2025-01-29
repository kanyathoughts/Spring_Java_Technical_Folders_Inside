/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api.task;

import java.io.Serializable;

import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;

/**
 * This exception handler will be called by the {@link ResultConsumer}, if the consumed {@link Result}
 * provides a {@link Throwable} in case the {@link Task} terminated with an error.
 *
 * @param <S> the concrete result type of the task
 */
public interface TaskExceptionHandler<S extends Serializable> {

	/**
	 * Handles the exception occurred during the {@link Task} execution.
	 * The {@link Throwable} can be accessed indirectly through the {@link Result#status} member by invoking {@link Status#getMessage()} and
	 * {@link Status#getStackTrace()}.
	 * It will always be non-null if this method is being invoked.
	 *
	 * @param taskId the original Id of the task
	 * @param result the {@link Result} of the task execution
	 */
	void handle(String taskId, Result<S> result);
}

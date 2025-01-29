/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api.task;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.Serializable;
import java.util.function.BiConsumer;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Severity;

/**
 * Abstract base implementation of a result consumer consuming task {@linkplain Result Results}. If the {@link Result} is of
 * {@link Severity#ERROR} with a {@link Throwable}, then the {@link TaskExceptionHandler} will be invoked.
 *
 * @param <S> the concrete return type of the task
 */
public abstract class ResultConsumer<S extends Serializable> implements BiConsumer<String, Result<S>> {

	protected final TaskExceptionHandler<S> exceptionHandler;

	private Severity highestSeverity = Severity.OK;

	/**
	 * Constructor.
	 *
	 * @param exceptionHandler the {@link TaskExceptionHandler} to call in case the task terminated with an exception
	 */
	public ResultConsumer(final TaskExceptionHandler<S> exceptionHandler) {
		this.exceptionHandler = exceptionHandler;
	}

	@Override
	public void accept(@Nullable final String taskId, @Nullable final Result<S> result) {
		final String taskIdNotNull = assertNotNull(taskId);
		final Result<S> resultNotNull = assertNotNull(result);

		final Severity severity = resultNotNull.status.getSeverity();
		if (severity == Severity.ERROR && resultNotNull.status.getMessage() != null) {
			exceptionHandler.handle(taskIdNotNull, resultNotNull);
		}
		highestSeverity = Severity.max(highestSeverity, severity);

		handleResult(taskIdNotNull, resultNotNull);
	}

	/**
	 * @return the highest {@link Severity} of all {@linkplain Result Results} consumed by this consumer
	 */
	public Severity getHighestSeverity() {
		return highestSeverity;
	}

	/**
	 * This method should be implemented with the actual logic to handle the task {@link Result}.
	 *
	 * @param taskId the original Id of the {@link Task}
	 * @param result the {@link Result} of the task
	 */
	protected abstract void handleResult(final String taskId, final Result<S> result);

}

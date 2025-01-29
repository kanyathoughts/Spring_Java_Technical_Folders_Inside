/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api;

import java.io.Serializable;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.task.Task;

/**
 * The result of a {@link Job} or {@link Task} execution.
 * 
 * @param <R> the concrete type of the result value
 */
public final class Result<R extends Serializable> implements Serializable {
	
	private static final long serialVersionUID = 1L;

	public final Status status;

	@Nullable
	public final R value;
	
	/**
	 * Constructor.
	 * 
	 * @param status the {@link Status} of the job result
	 * @param value the job specific result value. This must be serializable!
	 */
	public Result(final Status status, final R value) {
		this.status = status;
		this.value = value;
	}
	
	/**
	 * Creates a {@link Result} with the provided {@code value} and {@link Status#OK}.
	 * 
	 * @param value the job specific result value. This must be serializable!
	 */
	public Result(final R value) {
		this.status = Status.OK;
		this.value = value;
	}

	/**
	 * Creates a {@link Result} with the provided {@link Status} and no value.
	 * 
	 * @param status the {@link Status} of the job result
	 */
	public Result(final Status status) {
		this.status = status;
		this.value = null;
	}

}

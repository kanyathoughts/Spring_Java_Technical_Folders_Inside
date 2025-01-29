/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.internal.executor;

import java.io.Serializable;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledFuture;

import innowake.lib.job.api.CapacityExceededException;
import innowake.lib.job.api.JobExecutionCallback;
import innowake.lib.job.api.Result;
import innowake.lib.job.internal.HeartbeatRunnable;

/**
 * Handles submitting of {@linkplain SerializableRunnable SerializableRunnables} and {@linkplain SerializableCallable SerializableCallables}.
 * It is implementation specific where the actual execution of these take place.
 */
public interface ExecutorService {
	
	/**
	 * Submits a {@link SerializableRunnable} for execution. Depending on the specific implementation the execution may
	 * not happen instantly and may also not be on the same machine.
	 *
	 * @param serializableRunnable the {@link SerializableRunnable}
	 * @throws CapacityExceededException if the cluster has not enough capacity left for execution
	 */
	void submit(SerializableRunnable serializableRunnable);

	/**
	 * Submits a {@link SerializableRunnable} for execution. Depending on the specific implementation the execution may
	 * not happen instantly and may also not be on the same machine.
	 *
	 * @param serializableRunnable the {@link SerializableRunnable}
	 * @param executionCallback the {@link JobExecutionCallback} that can be used to track the execution
	 * @throws CapacityExceededException if the cluster has not enough capacity left for execution
	 */
	void submit(SerializableRunnable serializableRunnable, JobExecutionCallback executionCallback);

	/**
	 * Submits a {@link SerializableRunnable} for execution. Depending on the specific implementation the execution may
	 * not happen instantly but will be executed on the current local machine.
	 *
	 * @param serializableRunnable the {@link SerializableRunnable}
	 * @param executionCallback the {@link JobExecutionCallback} that can be used to track the execution
	 * @throws CapacityExceededException if the cluster has not enough capacity left for execution
	 */
	void submitLocal(SerializableRunnable serializableRunnable, JobExecutionCallback executionCallback);

	/**
	 * Submits a {@link SerializableCallable} for execution. Depending on the specific implementation the execution may
	 * not happen instantly and may also not be on the same machine.
	 *
	 * @param <R> the concrete type of the execution result
	 * @param serializableCallable the {@link SerializableCallable}
	 * @return a {@link Future} that can be used to resolve the actual {@link Result} after the task execution has finished
	 * @throws CapacityExceededException if the cluster has not enough capacity left for execution
	 */
	<R extends Serializable> Future<Result<R>> submit(SerializableCallable<R> serializableCallable);
	
	/**
	 * Submits a {@link SerializableCallable} for execution. Depending on the specific implementation the execution may
	 * not happen instantly but will be executed on the current local machine.
	 *
	 * @param <R> the concrete type of the execution result
	 * @param serializableCallable the {@link SerializableCallable}
	 * @return a {@link Future} that can be used to resolve the actual {@link Result} after the task execution has finished
	 * @throws CapacityExceededException if the cluster has not enough capacity left for execution
	 */
	<R extends Serializable> Future<Result<R>> submitLocal(SerializableCallable<R> serializableCallable);

	/**
	 * Submits the {@link HeartbeatRunnable} for periodic execution until explicitly cancelled.
	 * This will always be executed on the local cluster node that submitted it.
	 *
	 * @param heartbeatRunnable the {@link HeartbeatRunnable}
	 * @return the {@link ScheduledFuture}
	 */
	ScheduledFuture<?> submitHeartbeat(HeartbeatRunnable heartbeatRunnable);
	
}

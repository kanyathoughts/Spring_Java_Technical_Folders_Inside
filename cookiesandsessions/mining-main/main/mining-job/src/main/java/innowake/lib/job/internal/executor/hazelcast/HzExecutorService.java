/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.internal.executor.hazelcast;

import java.io.Serializable;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Future;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import com.hazelcast.core.ExecutionCallback;
import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.core.IExecutorService;
import com.hazelcast.durableexecutor.DurableExecutorService;
import com.hazelcast.durableexecutor.DurableExecutorServiceFuture;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.CapacityExceededException;
import innowake.lib.job.api.JobExecutionCallback;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.management.ClusterInformation;
import innowake.lib.job.internal.HeartbeatRunnable;
import innowake.lib.job.internal.JobConfiguration;
import innowake.lib.job.internal.Logging;
import innowake.lib.job.internal.executor.ExecutorService;
import innowake.lib.job.internal.executor.SerializableCallable;
import innowake.lib.job.internal.executor.SerializableRunnable;

/**
 * Hazelcast specific implementation of the {@link ExecutorService} using a {@link IExecutorService} internally.
 */
public class HzExecutorService implements ExecutorService {
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.EXECUTOR_SERVICE);

	@Autowired
	@Qualifier(JobConfiguration.JOB_EXECUTOR_SERVICE_ID)
	private IExecutorService jobExecutorService;
	
	@Autowired
	@Qualifier(JobConfiguration.TASK_EXECUTOR_SERVICE_ID)
	private DurableExecutorService taskExecutorService;
	
	@Autowired
	@Qualifier(JobConfiguration.LOCAL_TASK_EXECUTOR_SERVICE_ID)
	private IExecutorService localTaskExecutorService;
	
	@Autowired
	@Qualifier(JobConfiguration.HEARTBEAT_EXECUTOR_SERVICE_ID)
	private ScheduledExecutorService heartbeatExecutorService;
	
	@Autowired
	@Qualifier(JobConfiguration.HAZELCAST_INSTANCE)
	private HazelcastInstance hz;
	
	@Autowired
	private ClusterInformation clusterInfo;
	
	@Override
	public final void submit(final SerializableRunnable runnable) {
		submit(runnable, new JobExecutionCallback() {
			
			@Override
			public void onCompletion() {
				/* no-op */
			}
			
			@Override
			public void onFailure(@Nullable final Throwable throwable) {
				/* no-op */
			}
		});
	}
	
	@Override
	public void submit(final SerializableRunnable runnable, final JobExecutionCallback executionCallback) {
		LOG.debug(() -> "Submitting Runnable " + runnable + " to the cluster.");
		try {
			jobExecutorService.submit(runnable, member -> ! clusterInfo.isMemberInShutdownState(member.getUuid().toString()), 
					new ExecutionCallbackProxy(runnable, executionCallback));
		} catch (final RejectedExecutionException e) {
			LOG.debug("Job submission has been rejected.", e);
			throw new CapacityExceededException("Unable to submit job as the cluster capacity has been exceeded.");
		}
	}

	@Override
	public void submitLocal(final SerializableRunnable runnable, final JobExecutionCallback executionCallback) {
		LOG.debug(() -> "Submitting Runnable " + runnable + " to current local cluster member.");
		try {
			jobExecutorService.submitToMember(runnable, hz.getCluster().getLocalMember(), new ExecutionCallbackProxy(runnable, executionCallback));
		} catch (final RejectedExecutionException e) {
			LOG.debug("Local job submission has been rejected.", e);
			throw new CapacityExceededException("Unable to submit job locally as the cluster capacity has been exceeded.");
		}
	}

	private class ExecutionCallbackProxy implements ExecutionCallback<Void> {
		
		private final SerializableRunnable runnable;
		private final JobExecutionCallback executionCallback;

		private ExecutionCallbackProxy(final SerializableRunnable runnable, final JobExecutionCallback executionCallback) {
			this.runnable = runnable;
			this.executionCallback = executionCallback;
		}
		
		@Override
		public void onResponse(@Nullable final Void response) {
			LOG.trace(() -> "Received success response from the Runnable " + runnable);
			try {
				executionCallback.onCompletion();
			} catch (final Exception e) {
				LOG.error(() -> "Uncaught Exception while handling completion of " + runnable, e);
			}
		}
		
		@Override
		public void onFailure(@Nullable final Throwable throwable) {
			LOG.trace(() -> "Received failure response from the Runnable " + runnable, throwable);
			try {
				executionCallback.onFailure(throwable);
			} catch (final Exception e) {
				LOG.error(() -> "Uncaught Exception while handling failure of " + runnable, e);
			}
		}
	}
	
	@Override
	public final <R extends Serializable> Future<Result<R>> submit(final SerializableCallable<R> callable) {
		LOG.debug(() -> "Submitting Callable " + callable + " to the cluster.");
		try {
			final DurableExecutorServiceFuture<R> durableFuture = taskExecutorService.submit(callable);
			return checkAndGetDurableTaskFuture(durableFuture);
		} catch (final RejectedExecutionException e) {
			LOG.debug("Task submission has been rejected.", e);
			throw new CapacityExceededException("Unable to submit task as the cluster capacity has been exceeded.");
		}
	}
	
	@Override
	public final <R extends Serializable> Future<Result<R>> submitLocal(final SerializableCallable<R> callable) {
		LOG.debug(() -> "Submitting Callable " + callable + " to current local cluster member.");
		try {
			/* We have to use a separate executor service for this as the job executor allows lesser active threads than a task executor,
			 * while the existing task executor cannot be used, since the durable one doesn't allow submission to a specific member. */
			@SuppressWarnings("unchecked")
			final Future<Result<R>> future = (Future<Result<R>>) localTaskExecutorService.submitToMember(callable, hz.getCluster().getLocalMember());
			return checkAndGetTaskFuture(future);
		} catch (final RejectedExecutionException e) {
			LOG.debug("Task submission has been rejected.", e);
			throw new CapacityExceededException("Unable to submit task as the capacity of this cluster member has been exceeded.");
		}
	}

	@Override
	public ScheduledFuture<?> submitHeartbeat(final HeartbeatRunnable heartbeatRunnable) {
		LOG.debug(() -> "Submitting heartbeat Runnable " + heartbeatRunnable);
		return heartbeatExecutorService.scheduleAtFixedRate(heartbeatRunnable, 0, 5, TimeUnit.SECONDS);
	}
	
	private <R extends Serializable> Future<Result<R>> checkAndGetDurableTaskFuture(final DurableExecutorServiceFuture<R> durableFuture) {
		/* Get the actual future from the executor. This is non-blocking. */
		final Future<Result<R>> future = taskExecutorService.retrieveResult(durableFuture.getTaskId());
		return checkAndGetTaskFuture(future);
	}
	
	private <R extends Serializable> Future<Result<R>> checkAndGetTaskFuture(final Future<Result<R>> future) {
		/* A RejectedExecutionException may not be directly thrown by the submit(...) call of the executor, but may also be contained
		 * in the future. Therefore we have to check the future if it was directly returned in an exception state and check the
		 * actual exception type. */
		if (future instanceof CompletableFuture && ((CompletableFuture<Result<R>>) future).isCompletedExceptionally()) {
			try {
				/* This will not block, as the future is already in an exception state. */
				future.get();
				
			} catch (final Exception e) {
				/* We explicitly only handle the RejectedExececutionException here. Every other exception must be ignored, as
				 * those are to be handled by the actual job! */
				if (e instanceof InterruptedException) {
					Thread.currentThread().interrupt();
				}
				if (e.getCause() instanceof RejectedExecutionException) {
					throw (RejectedExecutionException) e.getCause();
				}
			}
		}
		return future;
	}

}

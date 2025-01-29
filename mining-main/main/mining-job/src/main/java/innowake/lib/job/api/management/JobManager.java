/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api.management;

import java.io.Serializable;
import java.util.List;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledFuture;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.CapacityExceededException;
import innowake.lib.job.api.IllegalJobStateException;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.JobExecutionCallback;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.lib.job.api.JobInfoService.JobInfoInquiryBuilder;
import innowake.lib.job.api.task.Task;
import innowake.lib.job.internal.HeartbeatRunnable;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * The job manager manages the actual execution of jobs and sub tasks and the relevant information of those.
 */
public interface JobManager {

	/**
	 * @return a short Id for a job that is unique for the current machine this manager is running on
	 */
	long createShortJobId();

	/**
	 * @return a short Id for a task that is unique for the current machine this manager is running on
	 */
	long createShortTaskId();

	/**
	 * @return the global {@link ClusterInformation}. This also applies when only a single machine instead of a cluster is being used
	 */
	ClusterInformation getClusterInformation();

	/**
	 * Submits a {@link Job}.
	 *
	 * @param <R> the return type of the job
	 * @param job the {@link Job} to submit
	 * @return a {@link JobMonitor} for this job
	 * @throws CapacityExceededException if the cluster has not enough capacity left for execution
	 */
	<R extends Serializable> JobMonitor submit(Job<R> job);

	/**
	 * Submits a {@link Job}.
	 *
	 * @param <R> the return type of the job
	 * @param job the {@link Job} to submit
	 * @param callback the {@link JobExecutionCallback}
	 * @return a {@link JobMonitor} for this job
	 * @throws CapacityExceededException if the cluster has not enough capacity left for execution
	 */
	<R extends Serializable> JobMonitor submit(Job<R> job, JobExecutionCallback callback);

	/**
	 * Submits a {@link Job} to be executed on the current machine.
	 *
	 * @param <R> the return type of the job
	 * @param job the {@link Job} to submit
	 * @return a {@link JobMonitor} for this job
	 * @throws CapacityExceededException if the cluster has not enough capacity left for execution
	 */
	<R extends Serializable> JobMonitor submitLocal(Job<R> job);

	/**
	 * Submits a child job from a running parent job and waits for the child job's completion.
	 *
	 * @param <R> the return type of the job
	 * @param childJob the {@link Job} to submit
	 * @param parentJobMonitor {@link JobMonitor} of the running parent job that is used for authentication
	 * @param jobExecutionCallback the {@link JobExecutionCallback}
	 * @return {@link Status} showing whether the child job completed successfully
	 */
	<R extends Serializable> Status submitFromJobAndWait(
			Job<R> childJob, JobMonitor parentJobMonitor, JobExecutionCallback jobExecutionCallback);

	/**
	 * Submits a child job from a running parent job and waits for the child job's completion.
	 *
	 * @param <R> the return type of the job
	 * @param childJob the {@link Job} to submit
	 * @param parentJobMonitor {@link JobMonitor} of the running parent job that is used for authentication
	 * @return {@link Status} showing whether the child job completed successfully
	 */
	<R extends Serializable> Status submitFromJobAndWait(Job<R> childJob, JobMonitor parentJobMonitor);

	/**
	 * Submits a {@link Task} under a certain job Id. The job does not finish unless all tasks associated with it are also finished.
	 *
	 * @param <R> the return type of the task
	 * @param jobMonitor the {@link JobMonitor} of the {@link Job} submitting the task
	 * @param task the {@link Task} to submit
	 * @return a {@link Future} used to retrieve the {@link Result} of the task after its done
	 * @throws CapacityExceededException if the cluster has not enough capacity left for execution
	 */
	<R extends Serializable> Future<Result<R>> submit(JobMonitor jobMonitor, Task<R> task);

	/**
	 * Submits a {@link Task} under a certain job Id to be executed on the current machine.
	 * The job does not finish unless all tasks associated with it are also finished.
	 *
	 * @param <R> the return type of the task
	 * @param jobMonitor the {@link JobMonitor} of the {@link Job} submitting the task
	 * @param task the {@link Task} to submit
	 * @return a {@link Future} used to retrieve the {@link Result} of the task after its done
	 * @throws CapacityExceededException if the cluster has not enough capacity left for execution
	 */
	<R extends Serializable> Future<Result<R>> submitLocal(JobMonitor jobMonitor, Task<R> task);

	/**
	 * Submits the {@link HeartbeatRunnable} for periodic execution until explicitly cancelled.
	 * This will always be executed on the local cluster node that submitted it.
	 *
	 * @param heartbeatRunnable the {@link HeartbeatRunnable}
	 * @return the {@link ScheduledFuture}
	 */
	ScheduledFuture<?> submitJobHeartbeat(HeartbeatRunnable heartbeatRunnable);

	/**
	 * @param jobId the Id of the job
	 * @return the {@link JobMonitor} for the requested job or {@code null} if there is no job with the provided {@code jobId}
	 */
	@Nullable
	JobMonitor getJobMonitor(String jobId);

	/**
	 * Returns all {@link JobInformation JobInformations} that match with the filters in the given {@code builder}. The returned list can also contain jobs
	 * that are currently executed but not yet persisted.
	 *
	 * @param builder the {@linkplain JobInfoInquiryBuilder} containing the filter criteria.
	 * @return list of matching {@link JobInformation JobInformations}
	 */
	List<JobInformation> getJobs(BuildingConsumer<JobInfoInquiryBuilder> builder);

	/**
	 * Returns a {@link Paged} of {@link JobInformation JobInformations} that match with the filters in the given {@code builder}. The returned list can also
	 * contain jobs that are currently executed but not yet persisted.
	 * 
	 * @param pageable {@link Pagination} instance
	 * @param builder the {@linkplain JobInfoInquiryBuilder} containing the filter criteria.
	 * @return a page of matching {@link JobInformation JobInformations}
	 */
	Paged<JobInformation> getJobs(Pagination pageable, BuildingConsumer<JobInfoInquiryBuilder> builder);

	/**
	 * Deletes all <b>non active</b> jobs that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain JobInfoInquiryBuilder} containing the filter criteria.
	 * @return the number of deleted jobs
	 */
	int delete(BuildingConsumer<JobInfoInquiryBuilder> builder);

	/**
	 * Deletes the given job. If the job is currently active ({@code RUNNING}, {@code SCHEDULED}, {@code CANCEL_REQUESTED}), then an error is thrown.
	 *
	 * @param jobId the id of the job to be deleted
	 * @return the number of deleted jobs
	 * @throws IllegalJobStateException if the job is currently active
	 */
	int delete(String jobId);

	/**
	 * @param jobId the Id of the job to get the result for
	 * @return the job specific result or {@code null} if the job is not finished yet or if there's no job with that Id
	 */
	@Nullable
	Serializable getJobResult(String jobId);

	/**
	 * @param jobInfo the {@link JobInformation} of the job
	 * @return the maximum number of tasks that are allowed to be submitted concurrently for the job
	 */
	int getMaximumConcurrentTasksPerJob(JobInformation jobInfo);

}

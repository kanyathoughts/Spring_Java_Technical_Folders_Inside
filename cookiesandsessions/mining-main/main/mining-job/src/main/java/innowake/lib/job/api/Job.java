/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Serializable;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.Instant;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ScheduledFuture;

import javax.annotation.PostConstruct;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.commons.lang.builder.ToStringStyle;
import org.apache.logging.log4j.ThreadContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import com.hazelcast.spring.context.SpringAware;

import brave.Span;
import brave.Tracer;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.api.profiling.ProfilingFactory;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.profile.DefaultProfiler;
import innowake.lib.job.api.config.properties.JobConfigurationProperties;
import innowake.lib.job.api.management.JobInformation;
import innowake.lib.job.api.management.JobManager;
import innowake.lib.job.api.management.JobMonitor;
import innowake.lib.job.api.task.DefaultTaskProcessor;
import innowake.lib.job.api.task.ResultConsumer;
import innowake.lib.job.api.task.Task;
import innowake.lib.job.api.task.TaskProcessor;
import innowake.lib.job.api.task.TaskSource;
import innowake.lib.job.internal.HeartbeatRunnable;
import innowake.lib.job.internal.JobConfiguration;
import innowake.lib.job.internal.JobInfo;
import innowake.lib.job.internal.JobInfoUtil;
import innowake.lib.job.internal.Logging;
import innowake.lib.job.internal.executor.SerializableRunnable;
import innowake.mining.shared.entities.JobInfoPojoPrototype;
import innowake.mining.shared.model.job.JobStatus;
import innowake.mining.shared.model.job.Message;

/**
 * Generic job implementation that can be submitted to any {@link JobManager} implementation.
 *
 * @param <X> the concrete result type of the job
 */
@SpringAware /* This annotation ensures that dependencies are automatically injected upon deserialization in a cluster node. */
public abstract class Job<X extends Serializable> implements SerializableRunnable {

	private static final long serialVersionUID = 1L;
	static final Logger LOG = LoggerFactory.getLogger(Logging.JOB_API);

	@Autowired
	private transient JobInfoService jobInfoService;

	@SuppressWarnings("null") /* Somehow eclipse can't handle @Qualifier with a static variable here and treats it as @NonNull */
	@Autowired
	@Qualifier(JobConfiguration.JOB_MANAGER_ID)
	protected transient JobManager jobManager;

	@Autowired
	protected transient JobConfigurationProperties jobConfigurationProperties;

	@Nullable
	protected transient JobMonitor jobMonitor;

	/** Globally unique Id that is visible externally. */
	protected final String jobId;

	/** Short Id of the job that is unique for the machine the job is executed on. It will be populated after the task has been scheduled. */
	protected long shortJobId;

	@Autowired
	private transient Tracer tracer;
	@Autowired
	protected transient TracingSerializer tracingSerializer;
	@Nullable
	protected transient Span parentSpan;
	@Nullable
	protected transient Span jobSpan;
	@Nullable
	protected Map<String, String> tracingContext;

	/**
	 * Constructor.
	 */
	public Job() {
		this.jobId = UUID.randomUUID().toString();
	}

	@PostConstruct
	private void postConstruct() {
		/* We have to do this here as opposed to the constructor, since a job is always instantiated by its constructor without
		 * using dependency injection. Therefore injection is done internally on the final object, which then triggers this code. */
		if (shortJobId == 0) {
			shortJobId = jobManager.createShortJobId();
		}

		if (parentSpan == null && tracingContext != null) {
			/* This will be executed after deserialization on the target cluster node right before the actual execution begins.
			 * In this case we have to create the parent span instance from the previously serialized tracing context. */
			parentSpan = tracingSerializer.deserializeTracingContext(tracingContext);
		} else if (tracingContext == null) {
			/* This will be executed before the job is being submitted for execution to serialize the current tracing context.
			 * The case above needs this information after deserialization. */
			tracingContext = tracingSerializer.serializeTracingContext();
		}
	}

	/**
	 * Invoked by the executor service. In a cluster setup this may take place on a remote machine.
	 */
	@SuppressWarnings("unchecked")
	@Override
	public final void run() {
		try (final Tracer.SpanInScope parentScope = tracer.withSpanInScope(assertNotNull(parentSpan))) {
			jobSpan = tracer.nextSpan().start();
			try (final Tracer.SpanInScope jobScope = tracer.withSpanInScope(jobSpan)) {
				final JobMonitor monitor = jobMonitor = assertNotNull(jobManager.getJobMonitor(jobId));

				ThreadContext.put("job-id", this.jobId);

				jobManager.getClusterInformation().modifyActiveJobCount(1);

				Result<X> result = null;
				ScheduledFuture<?> heartbeat = null;
				try {
					if (monitor.isCanceled()) {
						LOG.debug(() -> "Not starting " + this + " as cancel was requested before job was started");
						monitor.setStatus(JobStatus.CANCELED);
					} else {
						LOG.debug(() -> "Starting " + this);
						monitor.setStatus(JobStatus.RUNNING);
						monitor.setStartTime(Instant.now());

						/* Start sending heart beats for this job. */
						heartbeat = jobManager.submitJobHeartbeat(new HeartbeatRunnable(monitor, tracer, assertNotNull(jobSpan)));
						result = run(monitor);
						monitor.setStatus(getJobStatusForSeverity(result.status.getSeverity()));
					}
				} catch (final OperationCanceledException e) {
					monitor.setStatus(JobStatus.CANCELED);
					LOG.debug(() -> "Canceled job " + this);
				} catch (final Throwable e) {
					LOG.error(() -> "Encountered error during execution of job " + this, e);
					monitor.setStatus(JobStatus.FAILURE);
					if (jobSpan != null) {
						jobSpan.error(e);
					}
					throw e;
				} finally {
					JobInfo jobInfo = null;
					try {
						if (heartbeat != null) {
							heartbeat.cancel(false);
						}

						jobManager.getClusterInformation().modifyActiveJobCount(-1);

						final JobStatus status = monitor.getStatus();
						monitor.setFinishTime(Instant.now());

						jobInfo = assertNotNull((JobInfo) monitor.getJobInformation());
						if (result != null) {
							jobInfo.setResult((Result<Serializable>) result);
						}

						final JobInfoPojoPrototype prototype = JobInfoUtil.toPrototype(jobInfo);
						final UUID jobUId = jobInfoService.upsert(prototype);

						final var messages = jobInfo.getMessages();
						if ( ! messages.isEmpty()) {
							jobInfoService.createJobMessages(jobUId, messages);
						}

						/* saving the result may fail due to record limit, but the general job state has been persisted before */
						final var jobResult = jobInfo.getResult();
						if (jobResult != null) {
							jobInfoService.createJobResult(jobUId, JobInfoUtil.serializeObject(jobResult));
						}

						if (status != JobStatus.CANCELED && status != JobStatus.FAILURE) {
							monitor.setStatus(JobStatus.SUCCESS);
							jobInfoService.update(new JobInfoPojoPrototype()
											.setId(jobInfo.getId())
											.setStatus(JobStatus.SUCCESS));
						}
					} catch (final Exception e) {
						LOG.error(() -> "Error persisting result of job " + this, e);
						if (jobInfo != null) {
							jobInfo.setStatus(JobStatus.FAILURE);
							try {
								jobInfoService.update(new JobInfoPojoPrototype()
															.setId(jobInfo.getId())
															.setStatus(JobStatus.FAILURE));
							} catch (final Exception ex) {
								LOG.error(() -> "Error saving status of job " + this, ex);
							}
						}
						if (jobSpan != null) {
							jobSpan.error(e);
						}
					} finally {
						LOG.debug(() -> "Finished " + this + " in " + assertNotNull(monitor.getJobInformation()).getDuration());
						monitor.destroy();
					}

					try {
						afterRun();
					} catch (final Exception ex) {
						LOG.error(() -> "Error in finalizer of job: " + this, ex);
					}
				}
			} finally {
				if (jobSpan != null) {
					jobSpan.finish();
				}
				if (DefaultProfiler.isProfilingEnabled()) {
					ProfilingFactory.getProfilingSession().flushCurrentThread();
				}
			}
		}
	}

	/**
	 * The actual logic of the job should be implemented here.
	 *
	 * @param progressMonitor the progress monitor; invoke {@link ProgressMonitor#begin(int)} at the beginning to define the overall work to be done
	 * @return the result of the job
	 */
	protected abstract Result<X> run(ProgressMonitor progressMonitor);

	/**
	 * @return the unique job Id
	 */
	public final String getJobId() {
		return jobId;
	}
	/**
	 * Forks {@linkplain Task Tasks} to be executed in parallel. Every forked task may be executed on a different machine in the cluster.
	 * This method blocks until all submitted tasks are finished and their results resolved.
	 *
	 * @param <R> the concrete {@link Result} type of the tasks
	 * @param taskProcessor the {@link TaskProcessor} to use
	 * @param taskSource the {@link TaskSource} providing the {@link Task} instances
	 * @param resultConsumer the {@link ResultConsumer} consuming all task {@linkplain Result Results}
	 */
	protected final <R extends Serializable> void forkTasks(final TaskProcessor<R> taskProcessor, final TaskSource<R> taskSource,
			final ResultConsumer<R> resultConsumer) {
		taskProcessor.process(taskSource, resultConsumer);
	}

	/**
	 * Forks a single {@link Task} to be executed. The task may be executed on a different machine in the cluster.
	 * This method blocks until the task is finished and its result resolved.
	 *
	 * @param <R> the concrete {@link Result} type of the task
	 * @param taskProcessor the {@link TaskProcessor} to use
	 * @param task the {@link Task} that should be forked
	 * @param resultConsumer the {@link ResultConsumer} consuming the task {@link Result}
	 */
	protected final <T extends Task<R>, R extends Serializable> void forkTask(final TaskProcessor<R> taskProcessor, final T task,
			final ResultConsumer<R> resultConsumer) {
		taskProcessor.process(new TaskSource<R>() {

			boolean hasNext = true;

			@Override
			public boolean hasNextTask() {
				return hasNext;
			}

			@Override
			public Task<R> nextTask() {
				try {
					return task;
				} finally {
					hasNext = false;
				}
			}

		}, resultConsumer);
	}

	/**
	 * Writes a message that will be added to the {@link JobInformation} of the current job.
	 *
	 * @param severity the {@code Message.Severity} of the message
	 * @param text the actual text of the message
	 */
	protected final void writeMessage(final Message.Severity severity, final String text) {
		assertNotNull(jobMonitor).addMessage(new Message(severity, text));
	}

	/**
	 * Creates a {@link TaskProcessor} that can be used to fork {@linkplain Task Tasks}.
	 *
	 * @param <R> the concrete type of the task {@link Result}
	 * @return the {@link TaskProcessor} instance
	 */
	protected <R extends Serializable> TaskProcessor<R> createTaskProcessor() {
		return new DefaultTaskProcessor<>(jobManager, assertNotNull(jobMonitor));
	}

	/**
	 * Creates a file on the server's file system and opens an output stream to it. Only one result file is supported per Job
	 * so you must only call this method once during Job execution (calling this method again will otherwise erase the previous results).
	 * When using this method to store the Job's results you must return {@link FileSystemResult} from your Job's {@code run()} method.
	 * <p>
	 * Use this to store the results of the Job if the result size is large and/or
	 * you can't keep the results in memory until the Job is finished (otherwise just return
	 * the result directly from the {@code run()} method of your Job).
	 * <p>
	 * As a rule of thumb, this method should be used to store the Job result if the result size
	 * exceeds 10MB. That's because Job results are otherwise stored in the database and storing
	 * large objects will lead to degraded performance (or fail outright once we exceed a certain size).
	 * <p>
	 * By default the job result file is not deleted automatically, e.g. when the job is done.
	 *
	 * @return a writable {@code OutputStream} where Job results can be written
	 * @throws IOException if the output file can not be opened
	 * @see FileSystemResult
	 */
	protected OutputStream createResultFile() throws IOException {
		return createFileInResultFolder(getJobId());
	}

	/**
	 * Creates a file with the given {@code name} on the server's file system and opens an output stream to it.
	 *
	 * @param name the name of the file
	 * @return a writable {@code OutputStream} where Job results can be written
	 * @throws IOException if the output file can not be opened
	 * @see FileSystemResult
	 */
	OutputStream createFileInResultFolder(final String name) throws IOException {
		/* ensure output folder exists */
		final File resultFolder = Paths.get(jobConfigurationProperties.getJobResultFolder()).toFile();
		if ( ! resultFolder.exists()) {
			resultFolder.mkdirs();
		}

		return Files.newOutputStream(Paths.get(jobConfigurationProperties.getJobResultFolder(), name));
	}

	private static JobStatus getJobStatusForSeverity(final Severity severity) {
		switch (severity) {
			case OK:
			case WARNING:
				return JobStatus.SUCCESS;
			case CANCELED:
				return JobStatus.CANCELED;
			case ERROR:
				return JobStatus.FAILURE;
			case UNDEFINED:
				return JobStatus.UNKNOWN;
			default:
				throw new IllegalArgumentException("Unmapped severity: " + severity);
		}
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.SHORT_PREFIX_STYLE).append("shortId", shortJobId).append("id", jobId).toString();
	}

	/**
	 * Always called when the Job is done, even if it failed.
	 * <p>If this method ever throws an exception then the exception is logged only but not set into this Job's {@link Span}.
	 */
	protected void afterRun() {
		/* nothing to do by default */
	}

	/**
	 * Returns the name of the job.
	 * By default, the job name is determined by using the job class name.
	 *
	 * @return name of the job
	 */
	public String getJobName() {
		final String className = this.getClass().getSimpleName();
		if (StringUtils.isBlank(className) || className.equalsIgnoreCase("Job")) {
			return "unknown";
		}
		final String[] jobNameArray = StringUtils.splitByCharacterTypeCamelCase(className);
		final String jobName;
		if (jobNameArray[jobNameArray.length - 1].equalsIgnoreCase("Job")) {
			jobName = StringUtils.join(jobNameArray, ' ', 0, jobNameArray.length - 1);
		} else {
			jobName = StringUtils.join(jobNameArray, ' ');
		}

		if (StringUtils.isBlank(jobName)) {
			return "unknown";
		}
		return jobName;
	}
}

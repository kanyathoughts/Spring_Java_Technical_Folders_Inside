/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api.task;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.Serializable;
import java.time.Duration;
import java.time.Instant;
import java.util.Map;
import java.util.UUID;

import javax.annotation.PostConstruct;

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
import innowake.lib.job.api.Job;
import innowake.lib.job.api.OperationCanceledException;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Severity;
import innowake.lib.job.api.Status;
import innowake.lib.job.api.TracingSerializer;
import innowake.lib.job.api.management.JobInformation;
import innowake.lib.job.api.management.JobManager;
import innowake.lib.job.api.management.JobMonitor;
import innowake.lib.job.internal.JobConfiguration;
import innowake.lib.job.internal.JobManagerInternal;
import innowake.lib.job.internal.Logging;
import innowake.lib.job.internal.ProgressMonitorInternal;
import innowake.lib.job.internal.executor.SerializableCallable;
import innowake.mining.shared.model.job.Message;

/**
 * Atomic code unit returning a {@link Result} that also can be empty to emulate simple {@link Runnable} behavior. Tasks are executed as part of a {@link Job}
 * and contribute to it's overall progress. They may or may not be executed asynchronously. Depending on the {@link JobManager} implementation execution may
 * take place in the same Java virtual machine or on a remote worker node of a cluster. It's up to the submitter of the task to process the result and handle
 * any errors. Task implementations should invoke {@link ProgressMonitor#checkCanceled()} in sensible intervals, but at least at the beginning.
 * 
 * @param <R> the return type of the task
 */
@SpringAware /* This annotation ensures that dependencies are automatically injected upon deserialization in a cluster node. */
public abstract class Task<R extends Serializable> implements SerializableCallable<Result<R>> {

	private static final long serialVersionUID = 1L;
	private static final Logger LOG = LoggerFactory.getLogger(Logging.TASK_API);
	
	/** Globally unique Id of the task. */
	protected final String taskId;
	
	/** Short Id of the task that is unique for the machine the task is executed on. It will be populated after the task has been scheduled. */
	protected long shortTaskId;
	
	/** The system UTC time when the execution of the task has been started. */
	@Nullable
	protected Instant startTime;
	
	@SuppressWarnings("null") /* Somehow eclipse can't handle @Qualifier with a static variable here and treats it as @NonNull */
	@Autowired
	@Qualifier(JobConfiguration.JOB_MANAGER_ID)
	private transient JobManager jobManager;
	@Nullable
	private transient JobMonitor jobMonitor;
	
	private final String jobId;
	private final ProgressMonitor progressMonitor;
	
	@Autowired
	protected transient Tracer tracer;
	@Autowired
	private transient TracingSerializer tracingSerializer;
	@Nullable
	protected transient Span parentSpan;
	@Nullable
	protected transient Span taskSpan;
	@Nullable
	protected Map<String, String> tracingContext;
	
	/**
	 * Constructor.
	 * 
	 * @param progressMonitor the {@link ProgressMonitor} that should be used for this task created by {@link ProgressMonitor#subMonitor(int)}
	 * @param jobId the Id of the job this task belongs to
	 */
	public Task(final ProgressMonitor progressMonitor, final String jobId) {
		this.taskId = UUID.randomUUID().toString();
		this.progressMonitor = progressMonitor;
		this.jobId = jobId;
	}
	
	@PostConstruct
	private void postProcess() {
		/* We have to do this here as opposed to the constructor, since a task is always instantiated by its constructor without
		 * using dependency injection. Therefore injection is done internally on the final object, which then triggers this code. */
		if (shortTaskId == 0) {
			shortTaskId = jobManager.createShortTaskId();
		}
		if (jobManager instanceof JobManagerInternal) {
			final ProgressMonitor monitor = getProgressMonitor();
			if (monitor instanceof ProgressMonitorInternal) {
				((ProgressMonitorInternal) monitor).initialize((JobManagerInternal) jobManager);
			}
		}
		
		if (parentSpan == null && tracingContext != null) {
			/* This will be executed after deserialization on the target cluster node right before the actual execution begins.
			 * In this case we have to create the parent span instance from the previously serialized tracing context. */
			parentSpan = tracingSerializer.deserializeTracingContext(tracingContext);
		} else if (tracingContext == null) {
			/* This will be executed before the task is being submitted for execution to serialize the current tracing context.
			 * The case above needs this information after deserialization. */
			tracingContext = tracingSerializer.serializeTracingContext();
		}
	}
	
	/**
	 * The actual logic of the task should be implemented here.
	 *
	 * @param progressMonitor the progress monitor to track the work of this task
	 * @return the result of the task
	 */
	protected abstract Result<R> run(ProgressMonitor progressMonitor);
	
	@Override
	public final Result<R> call() throws Exception {
		try (final Tracer.SpanInScope parentScope = tracer.withSpanInScope(parentSpan)) {
			taskSpan = tracer.nextSpan().start();
			try (final Tracer.SpanInScope jobScope = tracer.withSpanInScope(taskSpan)) {
				final JobMonitor monitor = jobMonitor = assertNotNull(jobManager.getJobMonitor(getJobId()));
				try {
					ThreadContext.put("job-id", jobId);
					jobManager.getClusterInformation().modifyActiveTaskCount(1);
					startTime = Instant.now();
					LOG.debug(() -> "Starting " + this);
					return run(getProgressMonitor());
				} catch (final OperationCanceledException e) {
					LOG.debug(() -> "Canceled task " + this);
					return new Result<>(new Status(Severity.CANCELED));
				} catch (final Throwable e) {
					LOG.error(() -> "Encountered error during execution of task " + this, e);
					if (taskSpan != null) {
						taskSpan.error(e);
					}
					return new Result<>(new Status(e));
				} finally {
					monitor.modifyPendingTasks(-1);
					jobManager.getClusterInformation().modifyActiveTaskCount(-1);
					final Instant finishTime = Instant.now();
					LOG.debug(() -> "Finished " + this + " in " + Duration.between(startTime, finishTime));
				}
			} finally {
				if (taskSpan != null) {
					taskSpan.finish();
				}
				if (DefaultProfiler.isProfilingEnabled()) {
					ProfilingFactory.getProfilingSession().flushCurrentThread();
				}
			}
		}
	}
	
	/**
	 * Writes a message that will be added to the {@link JobInformation} of the job this task belongs to.
	 * 
	 * @param severity the {@code Message.Severity} of the message
	 * @param text the actual text of the message
	 */
	protected final void writeMessage(final Message.Severity severity, final String text) {
		assertNotNull(jobMonitor).addMessage(new Message(severity, text));
	}

	/**
	 * @return the Id of the job this task belongs to
	 */
	public final String getJobId() {
		return jobId;
	}
	
	/**
	 * @return the unique Id of this task
	 */
	public final String getTaskId() {
		return taskId;
	}
	
	/**
	 * @return the {@link ProgressMonitor} in use by this task
	 */
	protected final ProgressMonitor getProgressMonitor() {
		return progressMonitor;
	}
	
	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.SHORT_PREFIX_STYLE).append("shortId", shortTaskId).append("id", taskId)
				.append("jobId", jobId).append("startTime", startTime).toString();
	}
}

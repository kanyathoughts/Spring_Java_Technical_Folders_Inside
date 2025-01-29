/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.internal.hazelcast;

import static innowake.lib.core.lang.Assert.assertEqual;
import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.Serializable;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ScheduledFuture;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.ObjectUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;

import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.cp.IAtomicLong;
import com.hazelcast.map.IMap;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.CapacityExceededException;
import innowake.lib.job.api.IllegalJobStateException;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.JobExecutionCallback;
import innowake.lib.job.api.JobInfoService;
import innowake.lib.job.api.JobInfoService.JobInfoInquiryBuilder;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Severity;
import innowake.lib.job.api.Status;
import innowake.lib.job.api.management.ClusterInformation;
import innowake.lib.job.api.management.JobInformation;
import innowake.lib.job.api.management.JobMonitor;
import innowake.lib.job.api.task.Task;
import innowake.lib.job.internal.HeartbeatRunnable;
import innowake.lib.job.internal.JobConfiguration;
import innowake.lib.job.internal.JobInfo;
import innowake.lib.job.internal.JobInfoUtil;
import innowake.lib.job.internal.JobManagerInternal;
import innowake.lib.job.internal.Logging;
import innowake.lib.job.internal.executor.ExecutorService;
import innowake.lib.job.internal.executor.SerializableCallable;
import innowake.mining.shared.access.Comperator;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.entities.JobInfoPojo;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.JobInfoFieldName;
import innowake.mining.shared.model.job.JobStatus;

/**
 * Job manager implementation for jobs that are executed within a Hazelcast cluster.
 */
public class HzJobManager implements JobManagerInternal {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.JOB_MANAGER);

	private static final JobExecutionCallback NOOP_EXECUTION_CALLBACK = new JobExecutionCallback() {
		@Override
		public void onCompletion() {
			/* no-op */
		}
		@Override
		public void onFailure(@Nullable final Throwable throwable) {
			/* no-op */
		}
	};

	@Autowired
	@Qualifier(JobConfiguration.HAZELCAST_INSTANCE)
	private HazelcastInstance hz;

	@Autowired
	@Qualifier(JobConfiguration.EXECUTOR_SERVICE_ID)
	private ExecutorService executorService;

	@Nullable
	private IMap<String, JobInfo> jobs;
	
	@Nullable
	private JobInfoService jobInfoService;

	@Autowired
	@Qualifier(JobConfiguration.SHORT_JOB_ID_SEQUENCE_ID)
	private IAtomicLong shortJobIdSequence;

	@Autowired
	@Qualifier(JobConfiguration.SHORT_TASK_ID_SEQUENCE_ID)
	private IAtomicLong shortTaskIdSequence;

	@Autowired
	private ClusterInformation clusterInfo;

	private final AutowireCapableBeanFactory beanFactory;
	
	@Autowired
	public void setJobInfoService(final JobInfoService jobInfoService) {
		this.jobInfoService = jobInfoService;
	}
	
	@Autowired
	@Qualifier(JobConfiguration.JOBS_ID)
	public void setJobs(final IMap<String, JobInfo> jobs) {
		this.jobs = jobs;
	}

	@Autowired
	public HzJobManager(final ApplicationContext context) {
		this.beanFactory = context.getAutowireCapableBeanFactory();
	}

	@Override
	public ClusterInformation getClusterInformation() {
		return clusterInfo;
	}

	@Override
	public <R extends Serializable> JobMonitor submit(final Job<R> job) {
		return submit(job, NOOP_EXECUTION_CALLBACK);
	}

	@Override
	public <R extends Serializable> JobMonitor submitLocal(final Job<R> job) {
		return submit(job, NOOP_EXECUTION_CALLBACK, true);
	}

	@Override
	public <R extends Serializable> Status submitFromJobAndWait(final Job<R> job, final JobMonitor parentJobMonitor) {
		return submitFromJobAndWait(job, parentJobMonitor, NOOP_EXECUTION_CALLBACK);
	}

	@Override
	public <R extends Serializable> Status submitFromJobAndWait
			(final Job<R> job, final JobMonitor parentJobMonitor, final JobExecutionCallback executionCallback) {
		if (parentJobMonitor.getStatus() != JobStatus.RUNNING) {
			LOG.error(() -> "Parent job must be running but the current status is: " + parentJobMonitor.getStatus());
			assertEqual(parentJobMonitor.getStatus(), JobStatus.RUNNING, "Parent job must be running");
		}
		final JobInformation jobInfo = assertNotNull(parentJobMonitor.getJobInformation(), "Parent job must have job information");
		final String userName = jobInfo.getUserName();

		final LinkedBlockingQueue<Status> statusQueue = new LinkedBlockingQueue<>();
		final JobExecutionCallback waitingCallback = new JobExecutionCallback() {

			@Override
			public void onFailure(@Nullable Throwable throwable) {
				try {
					executionCallback.onFailure(throwable);
				} finally {
					if (throwable != null) {
						statusQueue.add(new Status(throwable));
					} else {
						statusQueue.add(new Status(Severity.ERROR));
					}
				}
			}

			@Override
			public void onCompletion() {
				try {
					executionCallback.onCompletion();
				} finally {
					statusQueue.add(Status.OK);
				}
			}
		};

		submitJobInternal(job, waitingCallback, userName, false);
		try {
			return statusQueue.take();
		} catch (final InterruptedException exception) {
			Thread.currentThread().interrupt();
			return new Status(exception);
		}
	}

	@Override
	public <R extends Serializable> JobMonitor submit(final Job<R> job, final JobExecutionCallback executionCallback) {
		return submit(job, executionCallback, false);
	}

	private <R extends Serializable> JobMonitor submit(final Job<R> job, final JobExecutionCallback executionCallback, final boolean submitLocal) {
		final Authentication authentication = assertNotNull(SecurityContextHolder.getContext().getAuthentication(), "Authentication is required to submit a job");
		final Object principal = assertNotNull(authentication.getPrincipal(), "A valid principal is required to submit a job");
		final String userName;
		if (principal instanceof UserDetails) {
			userName = ((UserDetails) principal).getUsername();
		} else {
			userName = authentication.getName();
		}

		return submitJobInternal(job, executionCallback, userName, submitLocal);
	}

	private <R extends Serializable> JobMonitor submitJobInternal(final Job<R> job, final JobExecutionCallback executionCallback, final String userName,
			final boolean submitLocal) {
		final JobInfo jobInfo = new JobInfo(job.getJobId(), job.getJobName(), userName);
		final HzJobMonitor jobMonitor = createJobMonitor();
		jobMonitor.setJobInfo(jobInfo);
		jobMonitor.setScheduledStartTime(Instant.now());
		/* Jobs are always instantiated by their constructor, so we have to manually run the injection here. */
		prepareBean(job);
		LOG.debug(() -> "Submitting job for execution " + job);

		try {
			if (submitLocal) {
				executorService.submitLocal(job, executionCallback);
			} else {
				executorService.submit(job, executionCallback);
			}
		} catch (final CapacityExceededException e) {
			jobMonitor.destroy();
			throw e;
		}
		return jobMonitor;
	}

	@Override
	public <R extends Serializable> Future<Result<R>> submit(final JobMonitor jobMonitor, final Task<R> task) {
		return submitTaskInternal(jobMonitor, task, false);
	}

	@Override
	public <R extends Serializable> Future<Result<R>> submitLocal(final JobMonitor jobMonitor, final Task<R> task) {
		return submitTaskInternal(jobMonitor, task, true);
	}

	@SuppressWarnings("unchecked")
	private <R extends Serializable> Future<Result<R>> submitTaskInternal(final JobMonitor jobMonitor, final Task<R> task, final boolean submitLocal) {
		/* Tasks are always instantiated by their constructor, so we have to manually run the injection here. */
		prepareBean(task);

		final int maximum = getMaximumConcurrentTasksPerJob(assertNotNull(jobMonitor.getJobInformation()));
		if (jobMonitor.getPendingTasks() < maximum) {
			LOG.debug(() -> "Submitting task for execution " + task);
			final Future<Result<R>> future;
			if (submitLocal) {
				future = executorService.submitLocal((SerializableCallable<R>) task);
			} else {
				future = executorService.submit((SerializableCallable<R>) task);
			}
			jobMonitor.modifyPendingTasks(1);
			return future;
		} else {
			throw new CapacityExceededException("It's not allowed to start more than " + maximum + " tasks for this job. "
					+ "Please check the capacity by calling JobManager.getMaximumConcurrentTasksPerJob(...).");
		}
	}

	@Override
	public ScheduledFuture<?> submitJobHeartbeat(final HeartbeatRunnable heartbeatRunnable) {
		LOG.debug(() -> "Submitting job heartbeat for execution " + heartbeatRunnable);
		return executorService.submitHeartbeat(heartbeatRunnable);
	}

	@Override
	public final long createShortJobId() {
		return shortJobIdSequence.incrementAndGet();
	}

	@Override
	public final long createShortTaskId() {
		return shortTaskIdSequence.incrementAndGet();
	}


	@Override
	@Nullable
	public final JobMonitor getJobMonitor(final String jobId) {
		final JobInfo jobInfo = assertNotNull(jobs).get(jobId);
		if (jobInfo != null) {
			final HzJobMonitor jobMonitor = createJobMonitor();
			jobMonitor.setJobInfo(jobInfo);
			return jobMonitor;
		} else {
			return null;
		}
	}

	@Nullable
	@Override
	public final Serializable getJobResult(final String jobId) {
		final JobInfoPojo jobInfo = assertNotNull(jobInfoService).get(UUID.fromString(jobId));
		final var result = jobInfo.getResult();
		return result.isEmpty() ? null : JobInfoUtil.deserializeObject(result.get());
	}

	@Override
	public Paged<JobInformation> getJobs(final Pagination pageable, final BuildingConsumer<JobInfoInquiryBuilder> builder) {
		final JobInformationQueryBuilder jobBuilder = builder.prepare(new JobInformationQueryBuilder());
		if (jobBuilder.comparators.isEmpty()) {
			jobBuilder.sortName(SortDirection.ASCENDING);
		}

		return jobBuilder.find(pageable, builder);
	}

	@Override
	public final List<JobInformation> getJobs(final BuildingConsumer<JobInfoInquiryBuilder> builder) {
		final JobInformationQueryBuilder jobBuilder = builder.prepare(new JobInformationQueryBuilder());
		if (jobBuilder.comparators.isEmpty()) {
			jobBuilder.sortSubmitTime(SortDirection.DESCENDING);
		}
		return jobBuilder.find(builder);
	}

	@Override
	public final int getMaximumConcurrentTasksPerJob(final JobInformation jobInfo) {
		/* Hardcoded to 50 for now */
		return 50;
	}

	@Override
	public final void prepareBean(final Object bean) {
		beanFactory.autowireBean(bean);
		beanFactory.initializeBean(bean, bean.getClass().getSimpleName() + "#" + System.identityHashCode(bean));
	}

	@Override
	public int delete(final BuildingConsumer<JobInfoInquiryBuilder> builder) {
		return assertNotNull(jobInfoService).delete(q -> {
			builder.accept(q);
			q.notWithStatus(JobStatus.ACTIVE_JOB_STATUS);
		});
	}

	@Override
	public int delete(final String jobId) {
		final List<JobInformation> jobInfo = getJobs(q -> q.byId(UUID.fromString(jobId)));
		if (jobInfo.isEmpty()) {
			return 0;
		}
		if (JobStatus.isActive(jobInfo.get(0).getStatus())) {
			throw new IllegalJobStateException("Job cannot be deleted because the job is active");
		}
		return assertNotNull(jobInfoService).delete(q -> q.byId(UUID.fromString(jobId)));
	}

	private HzJobMonitor createJobMonitor() {
		return beanFactory.createBean(HzJobMonitor.class);
	}

	public class JobInformationQueryBuilder implements JobInfoInquiryBuilder {

		private final List<Predicate<JobInformation>> filters = new ArrayList<>();
		private final List<Comparator<JobInformation>> comparators = new ArrayList<>();

		protected Paged<JobInformation> find(Pagination paging, final BuildingConsumer<JobInfoInquiryBuilder> builder) {
			return Paged.ofContent(find(builder), paging);
		}

		protected List<JobInformation> find(final BuildingConsumer<JobInfoInquiryBuilder> builder) {
			final var jobMap = assertNotNull(jobInfoService).find(builder)
															.stream()
															.map(JobInfoUtil::toJobInformation)
															.collect(Collectors.toMap(job -> job.getJobId(), job -> job));

			final List<JobInformation> result;
			final Collection<JobInfo> runningJobs = assertNotNull(jobs).values();
			if ( ! runningJobs.isEmpty()) {
				result = Stream.concat(jobMap.values().stream(),
										runningJobs.stream()
													.filter(jobInfo -> isFiltered(jobInfo) && ! jobMap.containsKey(jobInfo.getJobId())))
								.collect(Collectors.toList());
			} else {
				result = new ArrayList<>(jobMap.values());
			}

			if ( ! comparators.isEmpty()) {
				Comparator<JobInformation> comparator = comparators.get(0);
				for (int i = 1; i < comparators.size(); i++) {
					comparator = comparator.thenComparing(comparators.get(i));
				}

				result.sort(comparator);
			}

			return result;
		}

		protected boolean isFiltered(final JobInformation jobInformation) {
			if (filters.isEmpty()) {
				return true;
			}

			return filters.stream().allMatch(filter -> filter.test(jobInformation));
		}

		@Override
		public JobInformationQueryBuilder byId(final UUID id) {
			filters.add(job -> id.equals(job.getId()));
			return this;
		}

		@Override
		public JobInformationQueryBuilder byIds(final Collection<UUID> ids) {
			final Set<UUID> set = ids instanceof Set ? (Set<UUID>) ids : new HashSet<>(ids);
			filters.add(job -> set.contains(job.getId()));
			return this;
		}

		@Override
		public JobInformationQueryBuilder withName(final String name) {
			filters.add(job -> name.equals(job.getJobName()));
			return this;
		}

		@Override
		public JobInformationQueryBuilder withDescription(final String description) {
			filters.add(job -> description.equals(job.getJobDescription()));
			return this;
		}

		@Override
		public JobInformationQueryBuilder withStatus(final JobStatus status) {
			filters.add(job -> status == job.getStatus());
			return this;
		}

		@Override
		public JobInformationQueryBuilder withStatus(final Collection<JobStatus> status) {
			final Set<JobStatus> set = status instanceof Set ? (Set<JobStatus>) status : new HashSet<>(status);
			filters.add(job -> set.contains(job.getStatus()));
			return this;
		}

		@Override
		public JobInformationQueryBuilder notWithStatus(final Collection<JobStatus> status) {
			final Set<JobStatus> set = status instanceof Set ? (Set<JobStatus>) status : new HashSet<>(status);
			filters.add(job -> ! set.contains(job.getStatus()));
			return this;
		}

		@Override
		public JobInformationQueryBuilder withSubmitTime(final Comperator comperator, final Instant time) {
			switch (comperator) {
				case EQUAL:
					filters.add(job -> time.equals(job.getSubmitTime()));
					break;
				case GREATER:
					filters.add(job -> job.getSubmitTime().compareTo(time) > 0);
					break;
				case GREATER_OR_EQUAL:
					filters.add(job -> job.getSubmitTime().compareTo(time) >= 0);
					break;
				case LESSER:
					filters.add(job -> job.getSubmitTime().compareTo(time) < 0);
					break;
				case LESSER_OR_EQUAL:
					filters.add(job -> job.getSubmitTime().compareTo(time) <= 0);
					break;
				case UNEQUAL:
					filters.add(job -> ! time.equals(job.getSubmitTime()));
					break;
				default:
					throw new UnsupportedOperationException("Operator " + comperator.operator() + " not supported for filtering submit time.");
			}
			return this;
		}

		@Override
		public JobInformationQueryBuilder withScheduledStartTime(final Comperator comperator, final Instant time) {
			switch (comperator) {
				case EQUAL:
					filters.add(job -> time.equals(job.getScheduledStartTime()));
					break;
				case GREATER:
					filters.add(job -> {
						final Instant value = job.getScheduledStartTime();
						return value != null && value.compareTo(time) > 0;
					});
					break;
				case GREATER_OR_EQUAL:
					filters.add(job -> {
						final Instant value = job.getScheduledStartTime();
						return value != null && value.compareTo(time) >= 0;
					});
					break;
				case LESSER:
					filters.add(job -> {
						final Instant value = job.getScheduledStartTime();
						return value != null && value.compareTo(time) < 0;
					});
					break;
				case LESSER_OR_EQUAL:
					filters.add(job -> {
						final Instant value = job.getScheduledStartTime();
						return value != null && value.compareTo(time) <= 0;
					});
					break;
				case UNEQUAL:
					filters.add(job -> ! time.equals(job.getScheduledStartTime()));
					break;
				default:
					throw new UnsupportedOperationException("Operator " + comperator.operator() + " not supported for filtering scheduled start time.");
			}
			return this;
		}

		@Override
		public JobInformationQueryBuilder withStartTime(final Comperator comperator, final Instant time) {
			switch (comperator) {
				case EQUAL:
					filters.add(job -> time.equals(job.getStartTime()));
					break;
				case GREATER:
					filters.add(job -> {
						final Instant value = job.getStartTime();
						return value != null && value.compareTo(time) > 0;
					});
					break;
				case GREATER_OR_EQUAL:
					filters.add(job -> {
						final Instant value = job.getStartTime();
						return value != null && value.compareTo(time) >= 0;
					});
					break;
				case LESSER:
					filters.add(job -> {
						final Instant value = job.getStartTime();
						return value != null && value.compareTo(time) < 0;
					});
					break;
				case LESSER_OR_EQUAL:
					filters.add(job -> {
						final Instant value = job.getStartTime();
						return value != null && value.compareTo(time) <= 0;
					});
					break;
				case UNEQUAL:
					filters.add(job -> ! time.equals(job.getStartTime()));
					break;
				default:
					throw new UnsupportedOperationException("Operator " + comperator.operator() + " not supported for filtering start time.");
			}
			return this;
		}

		@Override
		public JobInformationQueryBuilder withFinishTime(final Comperator comperator, final Instant time) {
			switch (comperator) {
				case EQUAL:
					filters.add(job -> time.equals(job.getFinishTime()));
					break;
				case GREATER:
					filters.add(job -> {
						final Instant value = job.getFinishTime();
						return value != null && value.compareTo(time) > 0;
					});
					break;
				case GREATER_OR_EQUAL:
					filters.add(job -> {
						final Instant value = job.getFinishTime();
						return value != null && value.compareTo(time) >= 0;
					});
					break;
				case LESSER:
					filters.add(job -> {
						final Instant value = job.getFinishTime();
						return value != null && value.compareTo(time) < 0;
					});
					break;
				case LESSER_OR_EQUAL:
					filters.add(job -> {
						final Instant value = job.getFinishTime();
						return value != null && value.compareTo(time) <= 0;
					});
					break;
				case UNEQUAL:
					filters.add(job -> ! time.equals(job.getFinishTime()));
					break;
				default:
					throw new UnsupportedOperationException("Operator " + comperator.operator() + " not supported for filtering finish time.");
			}
			return this;
		}

		@Override
		public JobInformationQueryBuilder withCreatedByUserId(final String user) {
			filters.add(job -> user.equals(job.getUserName()));
			return this;
		}

		@Override
		public JobInformationQueryBuilder sortName(final SortDirection direction) {
			return sortBy(JobInfoFieldName.NAME, direction);
		}

		@Override
		public JobInformationQueryBuilder sortSubmitTime(final SortDirection direction) {
			return sortBy(JobInfoFieldName.SUBMIT_TIME, direction);
		}

		@Override
		public JobInformationQueryBuilder sortBy(final JobInfoFieldName field, final SortDirection direction) {
			final Comparator<JobInformation> comparator;
			switch (field) {
				case ID:
					comparator = (j1, j2) -> j1.getJobId().compareTo(j2.getJobId());
					break;
				case NAME:
					comparator = (j1, j2) -> j1.getJobName().compareTo(j2.getJobName());
					break;
				case CREATED_BY_USER_ID:
					comparator = (j1, j2) -> j1.getUserName().compareTo(j2.getUserName());
					break;
				case DESCRIPTION:
					comparator = (j1, j2) -> ObjectUtils.compare(j1.getJobDescription(), j2.getJobDescription());
					break;
				case STATUS:
					comparator = (j1, j2) -> j1.getStatus().compareTo(j2.getStatus());
					break;
				case STEP_DESCRIPTION:
					comparator = (j1, j2) -> ObjectUtils.compare(j1.getStepDescription(), j2.getStepDescription());
					break;
				case PENDING_TASKS:
					comparator = (j1, j2) -> Integer.compare(j1.getPendingTasks(), j2.getPendingTasks());
					break;
				case TOTAL_WORK_UNITS:
					comparator = (j1, j2) -> Integer.compare(j1.getTotalWorkUnits(), j2.getTotalWorkUnits());
					break;
				case PROCESSED_WORK_UNITS:
					comparator = (j1, j2) -> Double.compare(j1.getProcessedWorkUnits(), j2.getProcessedWorkUnits());
					break;
				case SUBMIT_TIME:
					comparator = (j1, j2) -> ObjectUtils.compare(j1.getSubmitTime(), j2.getSubmitTime());
					break;
				case SCHEDULED_START_TIME:
					comparator = (j1, j2) -> ObjectUtils.compare(j1.getScheduledStartTime(), j2.getScheduledStartTime());
					break;
				case START_TIME:
					comparator = (j1, j2) -> ObjectUtils.compare(j1.getStartTime(), j2.getStartTime());
					break;
				case FINISH_TIME:
					comparator = (j1, j2) -> ObjectUtils.compare(j1.getFinishTime(), j2.getFinishTime());
					break;
				default:
					throw new UnsupportedOperationException(String.format("JobInfo field %s not supported", field));
			}

			comparators.add(direction == SortDirection.ASCENDING ? comparator : comparator.reversed());
			return this;
		}
	}
}

/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.internal;

import com.hazelcast.cluster.Member;
import com.hazelcast.config.Config;
import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.core.IExecutorService;
import com.hazelcast.cp.IAtomicLong;
import com.hazelcast.durableexecutor.DurableExecutorService;
import com.hazelcast.map.IMap;
import com.hazelcast.spring.context.SpringManagedContext;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.config.properties.ClusterProperties;
import innowake.lib.job.api.config.HazelcastConfigurationSupport;
import innowake.lib.job.api.config.properties.JobConfigurationProperties;
import innowake.lib.job.api.management.ClusterInformation;
import innowake.lib.job.api.management.JobManager;
import innowake.lib.job.internal.aws.AwsLifecycleHandler;
import innowake.lib.job.internal.executor.ExecutorService;
import innowake.lib.job.internal.executor.hazelcast.HzExecutorService;
import innowake.lib.job.internal.hazelcast.CapacityReporter;
import innowake.lib.job.internal.hazelcast.HzClusterInformation;
import innowake.lib.job.internal.hazelcast.HzJobManager;
import innowake.lib.job.internal.hazelcast.JobHeartbeatHandler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * Spring configuration for the job API.
 */
@Configuration
@EnableConfigurationProperties(JobConfigurationProperties.class)
public class JobConfiguration extends HazelcastConfigurationSupport {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.CLUSTER_INFO);

	/** Bean name of the {@link HazelcastInstance} */
	public static final String HAZELCAST_INSTANCE = "hazelcast-instance";
	/** Bean name of the {@link IAtomicLong} for the short jobIds. */
	public static final String SHORT_JOB_ID_SEQUENCE_ID = "short-job-id-sequence";
	/** Bean name of the {@link IAtomicLong} for the short taskIds. */
	public static final String SHORT_TASK_ID_SEQUENCE_ID = "short-task-id-sequence";
	/** Bean name of the {@link IMap} holding the active jobs in the cluster. */
	public static final String JOBS_ID = "jobs";
	/** Bean name of the {@link JobManager}. */
	public static final String JOB_MANAGER_ID = "job-manager";
	/** Bean name of the {@link ExecutorService}. */
	public static final String EXECUTOR_SERVICE_ID = "executor-service";
	/** Bean name of the {@link IExecutorService} to execute jobs in the cluster. */
	public static final String JOB_EXECUTOR_SERVICE_ID = "job-executor-service";
	/** Bean name of the {@link DurableExecutorService} to execute tasks in the cluster. */
	public static final String TASK_EXECUTOR_SERVICE_ID = "task-executor-service";
	/** Bean name of the {@link IExecutorService} to execute tasks locally on the current cluster member. */
	public static final String LOCAL_TASK_EXECUTOR_SERVICE_ID = "local-task-executor-service";
	/** Bean name of the {@link ScheduledExecutorService} to handle job heartbeats. */
	public static final String HEARTBEAT_EXECUTOR_SERVICE_ID = "heartbeat-executor-service";
	/** Bean name containing the resolved name of this cluster node. */
	public static final String NODE_NAME = "node-name";

	private static final String BACKGROUND_OPERATIONS_EXECUTOR_SERVICE_ID = "background-operations-executor";
	private static final int INITIAL_SCHEDULING_DELAY = 0;

	@Autowired
	private JobConfigurationProperties jobConfigProperties;

	@Autowired
	private NodeNameFactory nodeNameFactory;

	/**
	 * Constructor.
	 */
	public JobConfiguration() {
		/* These are internal system properties only meant to make logging specific information accessible in the log4j configuration. */
		/* We always have to clear them during application start. Otherwise spring may try to treat them as configuration parameters
		 * if they're already set and then fails because it can't map them. */
		System.clearProperty(PROPERTY_NODE_NAME);
		System.clearProperty(PROPERTY_LOG_FOLDER);
		System.clearProperty(PROPERTY_LOG_FILE_PREFIX);
	}

	/**
	 * @return the {@link IAtomicLong} for the short job Ids
	 */
	@Bean(name = SHORT_JOB_ID_SEQUENCE_ID)
	public IAtomicLong shortJobIdSequence() {
		return hz().getCPSubsystem().getAtomicLong(SHORT_JOB_ID_SEQUENCE_ID);
	}

	/**
	 * @return the {@link IAtomicLong} for the short task Ids
	 */
	@Bean(name = SHORT_TASK_ID_SEQUENCE_ID)
	public IAtomicLong shortTaskIdSequence() {
		return hz().getCPSubsystem().getAtomicLong(SHORT_TASK_ID_SEQUENCE_ID);
	}

	/**
	 * @return the jobs map of the hazelcast cluster
	 */
	@Bean(name = JOBS_ID)
	public IMap<String, JobInfo> jobs() {
		return hz().getMap(JOBS_ID);
	}

	/**
	 * @param context the {@link ApplicationContext}
	 * @return the {@link JobManager}
	 */
	@Bean(name = JOB_MANAGER_ID)
	@Autowired
	public JobManager jobManager(final ApplicationContext context) {
		return new HzJobManager(context);
	}

	/**
	 * @return the {@link ExecutorService} to submit jobs and tasks
	 */
	@Bean(name = EXECUTOR_SERVICE_ID)
	public ExecutorService executorService() {
		return new HzExecutorService();
	}

	/**
	 * @return the internal hazelcast {@link IExecutorService} to execute jobs in the cluster
	 */
	@Bean(name = JOB_EXECUTOR_SERVICE_ID)
	public IExecutorService hzJobExecutorService() {
		return hz().getExecutorService(JOB_EXECUTOR_SERVICE_ID);
	}

	/**
	 * @return the internal hazelcast {@link DurableExecutorService} to execute tasks in the cluster
	 */
	@Bean(name = TASK_EXECUTOR_SERVICE_ID)
	public DurableExecutorService hzTaskExecutorService() {
		return hz().getDurableExecutorService(TASK_EXECUTOR_SERVICE_ID);
	}

	/**
	 * @return the internal hazelcast {@link IExecutorService} to execute tasks on the current local machine
	 */
	@Bean(name = LOCAL_TASK_EXECUTOR_SERVICE_ID)
	public IExecutorService hzLocalTaskExecutorService() {
		return hz().getExecutorService(LOCAL_TASK_EXECUTOR_SERVICE_ID);
	}

	/**
	 * @return the {@link ScheduledExecutorService} to execute job heartbeats
	 */
	@Bean(name = HEARTBEAT_EXECUTOR_SERVICE_ID)
	public ScheduledExecutorService heartbeatExecutorService() {
		final ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(1);
		executor.setRemoveOnCancelPolicy(true);
		return executor;
	}

	/**
	 * @return the {@link MemberInfoRecord} of this cluster node
	 */
	@Bean
	public MemberInfoRecord getMemberInfoRecord() {
		return new MemberInfoRecord(hz().getCluster().getLocalMember().getUuid().toString(), jobConfigProperties.getMaximumLocalJobThreads(),
				jobConfigProperties.getMaximumLocalTaskThreads());
	}

	/**
	 * @return the {@link ClusterInformation} containing the global cluster information
	 */
	@Bean
	public ClusterInformation getClusterInformation() {
		return new HzClusterInformation(hz());
	}

	/**
	 * @return the hazelcast managed context for Spring. Required for the {@code @SpringAware} annotation to work.
	 */
	@Bean
	public SpringManagedContext springManagedContext() {
		return new SpringManagedContext();
	}

	@Bean(name = NODE_NAME)
	@Override
	public String getNodeName() {
		return super.getNodeName();
	}

	@Bean(name = HAZELCAST_INSTANCE)
	@Autowired
	public HazelcastInstance hz() {
		return getHazelcastInstance(springManagedContext(), jobConfigProperties, nodeNameFactory);
	}

	@Override
	protected Config getHazelcastConfig(final SpringManagedContext springManagedContext, final ClusterProperties clusterProperties) {
		final Config config = super.getHazelcastConfig(springManagedContext, clusterProperties);

		LOG.info(() -> "Detected " + JobConfigurationProperties.PROCESSORS + " logical processors");
		final int localJobThreads = jobConfigProperties.getMaximumLocalJobThreads();
		final int localTaskThreads = jobConfigProperties.getMaximumLocalTaskThreads();
		LOG.info(() -> "Configured local job executor with " + localJobThreads + " threads");
		config.getExecutorConfig(JOB_EXECUTOR_SERVICE_ID).setPoolSize(localJobThreads);
		LOG.info(() -> "Configured local task executor with " + localTaskThreads + " threads");
		config.getDurableExecutorConfig(TASK_EXECUTOR_SERVICE_ID).setPoolSize(localTaskThreads);
		config.getExecutorConfig(LOCAL_TASK_EXECUTOR_SERVICE_ID).setPoolSize(localTaskThreads);
		config.getScheduledExecutorConfig(BACKGROUND_OPERATIONS_EXECUTOR_SERVICE_ID).setPoolSize(3);
		config.getScheduledExecutorConfig(HEARTBEAT_EXECUTOR_SERVICE_ID).setPoolSize(1);

		return config;
	}

	/**
	 * Starts any background operations like scheduling the {@link CapacityReporter}, {@link JobHeartbeatHandler} and the {@link AwsLifecycleHandler}.
	 */
	public void startBackgroundOperations() {
		final Member localMember = hz().getCluster().getLocalMember();
		hz().getScheduledExecutorService(BACKGROUND_OPERATIONS_EXECUTOR_SERVICE_ID).scheduleOnMemberAtFixedRate(new CapacityReporter(), localMember,
				INITIAL_SCHEDULING_DELAY, ClusterInfoRecord.MEMBER_STATUS_REPORT_INTERVAL_IN_SECONDS, TimeUnit.SECONDS);

		LOG.info("Configured maximum job heartbeat age: " + jobConfigProperties.getMaximumHeartbeatAge() + "s");
		hz().getScheduledExecutorService(JobConfiguration.HEARTBEAT_EXECUTOR_SERVICE_ID).scheduleOnMemberAtFixedRate(new JobHeartbeatHandler(), localMember,
				INITIAL_SCHEDULING_DELAY, jobConfigProperties.getJobHeartbeatInterval(), TimeUnit.SECONDS);

		final String lifecycleQueueName = getAwsLifecycleQueueName(jobConfigProperties.getCluster());
		if (lifecycleQueueName != null) {
			final Runnable lifecycleHandler = new AwsLifecycleHandler(lifecycleQueueName);
			hz().getScheduledExecutorService(BACKGROUND_OPERATIONS_EXECUTOR_SERVICE_ID).scheduleOnMemberAtFixedRate(lifecycleHandler, localMember,
					INITIAL_SCHEDULING_DELAY, AwsLifecycleHandler.EXECUTION_INTERVAL_IN_SECONDS, TimeUnit.SECONDS);
		} else {
			LOG.trace(() -> "No AWS lifecycle queue has been provided. This instance will not listen to any autoscaling lifecycle events.");
		}
	}
}

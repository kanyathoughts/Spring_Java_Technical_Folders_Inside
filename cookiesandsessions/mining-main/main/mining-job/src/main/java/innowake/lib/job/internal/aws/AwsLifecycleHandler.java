/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.internal.aws;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.TimeUnit;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.core.IExecutorService;
import com.hazelcast.core.LifecycleEvent;
import com.hazelcast.core.LifecycleEvent.LifecycleState;
import com.hazelcast.core.LifecycleListener;
import com.hazelcast.spring.context.SpringAware;

import brave.Span;
import brave.Tracer;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.lib.job.internal.JobConfiguration;
import innowake.lib.job.internal.Logging;
import innowake.lib.job.internal.MemberInfoRecord;
import innowake.lib.job.internal.executor.SerializableRunnable;
import innowake.lib.job.internal.hazelcast.HzClusterInformation;
import innowake.lib.job.api.config.properties.AwsProperties;
import innowake.lib.job.api.config.properties.AwsProperties.Ec2Properties;
import innowake.lib.job.api.config.properties.AwsProperties.Ec2Properties.AutoscalingProperties;
import innowake.lib.job.api.config.properties.ClusterProperties;
import innowake.lib.job.api.config.properties.ClusterProperties.ClusterMode;
import innowake.lib.job.api.config.properties.JobConfigurationProperties;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.regions.internal.util.EC2MetadataUtils;
import software.amazon.awssdk.services.autoscaling.AutoScalingClient;
import software.amazon.awssdk.services.autoscaling.model.CompleteLifecycleActionRequest;
import software.amazon.awssdk.services.sqs.SqsClient;
import software.amazon.awssdk.services.sqs.model.DeleteMessageRequest;
import software.amazon.awssdk.services.sqs.model.GetQueueUrlRequest;
import software.amazon.awssdk.services.sqs.model.Message;
import software.amazon.awssdk.services.sqs.model.ReceiveMessageRequest;

/**
 * This handler will be executed periodically to check for lifecycle hook messages in an AWS SQS queue
 * and performs a graceful hazelcast shutdown in case the machine should be terminated.
 */
@SpringAware
public class AwsLifecycleHandler implements SerializableRunnable {
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.AWS_LIFECYCLE_HANDLER);
	private static final String LIFECYCLE_ACTION_RESULT_CONTINUE = "CONTINUE";
	private static final String LIFECYCLE_ACTION_RESULT_ABANDON = "ABANDON";
	
	/** The default execution interval for the AWS lifecycle handler. */
	public static final int EXECUTION_INTERVAL_IN_SECONDS = 60;
	
	@Autowired
	private transient HazelcastInstance hz;
	@Autowired
	private transient HzClusterInformation clusterInfo;
	@Autowired
	private transient MemberInfoRecord memberInfo;
	@Autowired
	@Qualifier(JobConfiguration.JOB_EXECUTOR_SERVICE_ID)
	private transient IExecutorService jobExecutorService;
	@Autowired
	private transient JobConfigurationProperties jobConfigProperties;
	@Autowired
	private ObjectMapper objectMapper;
	@Autowired
	private transient Tracer tracer;
	@Nullable
	private transient Span span;
	@Nullable
	private transient SqsClient sqsClient;
	@Nullable
	private transient AutoScalingClient autoscalingClient;
	
	private final String lifecycleQueueName;
	@Nullable
	private String lifecycleQueueUrl;
	private int activeJobsTimeout;
	private int memberShutdownTimeout;
	@Nullable
	private String localMemberId;
	
	/**
	 * Constructor.
	 * 
	 * @param lifecycleQueueName the name of the lifecycle SQS queue
	 */
	public AwsLifecycleHandler(final String lifecycleQueueName) {
		this.lifecycleQueueName = lifecycleQueueName;
	}
	
	@PostConstruct
	private void postConstruct() {
		/* We have to do this after deserialization and injection is done. */
		if (span == null) {
			span = tracer.newTrace();
		}
		
		String region = null;
		final ClusterProperties clusterProperties = jobConfigProperties.getCluster();
		final ClusterMode clusterMode = clusterProperties.getMode();
		if (clusterMode == ClusterMode.AWS) {
			final AwsProperties awsProperties = clusterProperties.getNetwork().getAws();
			if (awsProperties != null) {
				region = awsProperties.getRegion();
			}
		}
		
		if (region == null) {
			region = EC2MetadataUtils.getEC2InstanceRegion();
		}
		if (region != null) {
			final Region awsRegion = Region.of(region);
			LOG.trace(() -> "Configuring AWS SQS and Autoscaling clients for region: " + awsRegion.id());
			sqsClient = SqsClient.builder().region(awsRegion).build();
			autoscalingClient = AutoScalingClient.builder().region(awsRegion).build();
		} else {
			LOG.error(() -> "When configuring a AWS lifecycle queue, the region also has to be provided. "
				+ "Could not find an explicit configured one and also couldn't resolve it from the machine. "
				+ "This instance will therefore not listen to any autoscaling lifecycle events.");
		}
		
		final Tuple2<Integer, Integer> timeoutProperties = getTimeoutProperties(jobConfigProperties);
		activeJobsTimeout = timeoutProperties.a.intValue();
		memberShutdownTimeout = timeoutProperties.b.intValue();
		localMemberId = hz.getCluster().getLocalMember().getUuid().toString();
	}

	@Override
	public void run() {
		/* All executions will run in the scope of the same trace, but always with a new span. */
		try (final Tracer.SpanInScope rootScope = tracer.withSpanInScope(assertNotNull(span))) {
			final Span currentExecSpan = tracer.nextSpan().start();
			try (final Tracer.SpanInScope currentExecScope = tracer.withSpanInScope(currentExecSpan)) {
				if (lifecycleQueueUrl == null) {
					LOG.trace(() -> "Requesting lifecycle queue URL for: " + lifecycleQueueName);
					final GetQueueUrlRequest getQueueRequest = GetQueueUrlRequest.builder().queueName(lifecycleQueueName).build();
					lifecycleQueueUrl = assertNotNull(sqsClient).getQueueUrl(getQueueRequest).queueUrl();
					LOG.trace(() -> "Received lifecycle queue URL: " + lifecycleQueueUrl);
				}
				
				LOG.trace(() -> "Checking for lifecycle messages.");
				final ReceiveMessageRequest receiveRequest = ReceiveMessageRequest.builder()
						.queueUrl(lifecycleQueueUrl)
						.maxNumberOfMessages(Integer.valueOf(2))
						.build();
				final List<Message> messages = assertNotNull(sqsClient).receiveMessage(receiveRequest).messages();
				if ( ! messages.isEmpty()) {
					LOG.trace(() -> "Received " + messages.size() + " lifecycle messages.");
					messages.forEach(this::handleMessage);
				}
			} catch (final Exception e) {
				currentExecSpan.error(e);
				LOG.error(() -> "Error during AWS lifecycle processing.", e);
			} finally {
				currentExecSpan.finish();
			}
		} finally {
			assertNotNull(span).flush();
		}
	}
	
	private void handleMessage(final Message message) {
		final String messageBody = message.body();
		LOG.trace(() -> "Received message body: " + messageBody);
		
		boolean deleteMessage = true;
		if (messageBody.contains("LifecycleHookName")) {
			try {
				final LifecycleMessage lifecycleMessage = objectMapper.readValue(messageBody, LifecycleMessage.class);
				final String lifecycleTransition = lifecycleMessage.getLifecycleTransition();
				
				if (LifecycleMessage.LIFECYCLE_TRANSITION_LAUNCHING.equals(lifecycleTransition)) {
					/* Launch lifecycle transitions can be accepted by any instance. */
					LOG.trace(() -> "Received instance launch lifecycle transition. Confirming autoscaler to continue.");
					completeLifecycleActionRequest(lifecycleMessage, LIFECYCLE_ACTION_RESULT_CONTINUE);
				} else if (LifecycleMessage.LIFECYCLE_TRANSITION_TERMINATING.equals(lifecycleTransition)) {
					/* Termination lifecycle transitions must only be processed by the actual EC2 instance that is the message recipient. */
					if (EC2MetadataUtils.getInstanceId().equals(lifecycleMessage.getEc2InstanceId())) {
						handleTermination(lifecycleMessage);
					} else {
						LOG.trace(() -> "Message not meant for us. Ignoring.");
						deleteMessage = false;
					}
				} else {
					LOG.error(() -> "Received unknown lifecycle transition: " + lifecycleTransition);
				}
			} catch (final JsonProcessingException e) {
				LOG.error(() -> "Unable to parse lifecycle message", e);
			}
		}
		
		/* Delete all messages that are no lifecycle message and all lifecycle messages that have been processed by this instance. */
		if (deleteMessage) {
			LOG.trace(() -> "Requesting deletion of lifecycle message.");
			final DeleteMessageRequest deleteRequest = DeleteMessageRequest.builder()
					.queueUrl(lifecycleQueueUrl)
					.receiptHandle(message.receiptHandle())
					.build();
			assertNotNull(sqsClient).deleteMessage(deleteRequest);
		}
	}
	
	private void handleTermination(final LifecycleMessage message) {
		LOG.trace(() -> "Received instance terminate lifecycle transition. Starting shutdown process.");
		/* By setting this property this member will no longer be included in the list of members available for job execution. */
		memberInfo.setInShutdownState(true);
		clusterInfo.update(memberInfo);
		
		final long runningJobs = clusterInfo.getActiveMemberJobCount(assertNotNull(localMemberId));
		if (runningJobs > 0) {
			/* If there are running jobs, we wait for a specific amount of time for them to finish, so that we can immediately
			 * send a positive response in case there are only short running jobs remaining. */
			wait(activeJobsTimeout, () -> Boolean.valueOf(clusterInfo.getActiveMemberJobCount(assertNotNull(localMemberId)) == 0));
		}
		
		if (runningJobs > 0) {
			/* Since we don't know how long these jobs may still run, we cancel the shutdown and deny the termination of the instance.
			 * This way it will stay in service till the next try. */
			final long activeJobs = runningJobs;
			LOG.trace(() -> "There are still '" + activeJobs + "' active jobs. Canceling shutdown and denying autoscaler to terminate this instance for now.");
			/* Since we cancel the termination, the member should accept new jobs again. Otherwise we have an instance running doing nothing. */
			memberInfo.setInShutdownState(false);
			clusterInfo.update(memberInfo);
			completeLifecycleActionRequest(message, LIFECYCLE_ACTION_RESULT_ABANDON);
		} else {
			LOG.trace(() -> "No active jobs. Triggering hazelcast shutdown and waiting for successful termination.");
			final HazelcastShutdownListener listener = new HazelcastShutdownListener();
			hz.getLifecycleService().addLifecycleListener(listener);
			hz.shutdown();
			
			/* We wait for a specific amount of time for hazelcast to gracefully shutdown. */
			wait(memberShutdownTimeout, () -> Boolean.valueOf(listener.hasShutdown));
			
			if ( ! listener.hasShutdown) {
				LOG.trace(() -> "Hazelcast not shutdown yet. Denying autoscaler to terminate this instance for now.");
				completeLifecycleActionRequest(message, LIFECYCLE_ACTION_RESULT_ABANDON);
			} else {
				LOG.trace(() -> "Hazelcast successfully shutdown. Confirming autoscaler to continue termination.");
				completeLifecycleActionRequest(message, LIFECYCLE_ACTION_RESULT_CONTINUE);
			}
		}
	}
	
	private void completeLifecycleActionRequest(final LifecycleMessage message, final String lifecycleActionResult) {
		final CompleteLifecycleActionRequest completeRequest = CompleteLifecycleActionRequest.builder()
				.instanceId(message.getEc2InstanceId())
				.lifecycleHookName(message.getLifecycleHookName())
				.autoScalingGroupName(message.getAutoScalingGroupName())
				.lifecycleActionResult(lifecycleActionResult)
				.build();
		assertNotNull(autoscalingClient).completeLifecycleAction(completeRequest);
	}
	
	private void wait(final int duration, final Callable<Boolean> condition) {
		final Instant start = Instant.now();
		while (Duration.between(start, Instant.now()).getSeconds() % 60 < duration) {
			try {
				if (condition.call().booleanValue()) {
					break;
				}
				TimeUnit.MILLISECONDS.sleep(500);
			} catch (final InterruptedException e) {
				Thread.currentThread().interrupt();
				LOG.error(() -> "Got interrupted while waiting.", e);
			} catch (final Exception e) {
				LOG.error(() -> "Encountered error while waiting.", e);
			}
		}
	}
	
	private Tuple2<Integer, Integer> getTimeoutProperties(final JobConfigurationProperties jobConfigProperties) {
		/* If an instance of this class is being created then all of these property instances must also exist. */
		final ClusterProperties clusterProperties = jobConfigProperties.getCluster();
		final AwsProperties awsProperties = clusterProperties.getNetwork().getAws();
		final Ec2Properties ec2Properties = Assert.assertNotNull(awsProperties).getEc2();
		final AutoscalingProperties autoscalingProperties = Assert.assertNotNull(Assert.assertNotNull(ec2Properties).getAutoscaling());
		return Tuple2.of(Integer.valueOf(autoscalingProperties.getActiveJobsTimeout()),
				Integer.valueOf(autoscalingProperties.getMemberShutdownTimeout()));
	}
	
	private static class HazelcastShutdownListener implements LifecycleListener {

		private boolean hasShutdown = false;
		
		@Override
		public void stateChanged(@Nullable final LifecycleEvent event) {
			if (event != null) {
				LOG.trace(() -> "Hazelcast changed to state: " + event.getState());
				hasShutdown = event.getState() == LifecycleState.SHUTDOWN;
			}
		}
		
	}

}

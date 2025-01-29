/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.internal.aws;

import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.NonNullByDefault;

/**
 * A message triggered by AWS autoscaling lifecycle hooks.
 */
@NonNullByDefault(false)
public class LifecycleMessage {
	
	/** The transition name when an EC2 instance is about to be launched. */
	public static final String LIFECYCLE_TRANSITION_LAUNCHING = "autoscaling:EC2_INSTANCE_LAUNCHING";
	/** The transition name when an EC2 instance is about to be terminated. */
	public static final String LIFECYCLE_TRANSITION_TERMINATING = "autoscaling:EC2_INSTANCE_TERMINATING";
	
	@JsonProperty(value = "LifecycleHookName")
	private String lifecycleHookName;
	@JsonProperty(value = "AccountId")
	private String accountId;
	@JsonProperty(value = "RequestId")
	private String requestId;
	@JsonProperty(value = "LifecycleTransition")
	private String lifecycleTransition;
	@JsonProperty(value = "AutoScalingGroupName")
	private String autoScalingGroupName;
	@JsonProperty(value = "Service")
	private String service;
	@JsonProperty(value = "Time")
	private String time;
	@JsonProperty(value = "EC2InstanceId")
	private String ec2InstanceId;
	@JsonProperty(value = "LifecycleActionToken")
	private String lifecycleActionToken;
	
	/**
	 * @return the lifecycle hook name that triggered the message
	 */
	public String getLifecycleHookName() {
		return lifecycleHookName;
	}
	
	/**
	 * @return the AWS account Id
	 */
	public String getAccountId() {
		return accountId;
	}
	
	/**
	 * @return the Id of the request that triggered this message
	 */
	public String getRequestId() {
		return requestId;
	}
	
	/**
	 * @return the actual lifecycle transition type
	 */
	public String getLifecycleTransition() {
		return lifecycleTransition;
	}
	
	/**
	 * @return the name of the autoscaling group
	 */
	public String getAutoScalingGroupName() {
		return autoScalingGroupName;
	}
	
	/**
	 * @return the name of the AWS service
	 */
	public String getService() {
		return service;
	}
	
	/**
	 * @return the time when this message had been created
	 */
	public String getTime() {
		return time;
	}
	
	/**
	 * @return the Id of the the EC2 instance that is part of the lifecycle event
	 */
	public String getEc2InstanceId() {
		return ec2InstanceId;
	}
	
	/**
	 * @return the token of the lifecycle action
	 */
	public String getLifecycleActionToken() {
		return lifecycleActionToken;
	}
	
}

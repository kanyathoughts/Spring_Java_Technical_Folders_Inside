/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api.config.properties;

import javax.validation.constraints.Min;

import innowake.lib.core.api.lang.Nullable;

/**
 * The AWS configuration properties for the cluster.
 */
public class AwsProperties {

	@Nullable
	private String accessKey;
	@Nullable
	private String secretKey;
	@Nullable
	private String region;
	@Nullable
	private String hostHeader;
	private String connectionTimeout = "10";
	private String readTimeout = "10";
	private String connectionRetries = "3";
	@Nullable
	private String portRange;
	@Nullable
	private Ec2Properties ec2;
	@Nullable
	private FargateProperties fargate;
	
	/**
	 * @return the access key of the AWS account when IAM is not used
	 */
	@Nullable
	public String getAccessKey() {
		return accessKey;
	}
	
	/**
	 * Sets the access key of the AWS account when IAM is not used.
	 * <p>
	 * <b>Note: </b> This should never be called by clients.
	 * 
	 * @param accessKey the access key of the AWS account when IAM is not used
	 */
	public void setAccessKey(final String accessKey) {
		this.accessKey = accessKey;
	}

	/**
	 * @return the secret key of the AWS account when IAM is not used
	 */
	@Nullable
	public String getSecretKey() {
		return secretKey;
	}
	
	/**
	 * Sets the secret key of the AWS account when IAM is not used.
	 * <p>
	 * <b>Note: </b> This should never be called by clients.
	 * 
	 * @param secretKey the secret key of the AWS account when IAM is not used
	 */
	public void setSecretKey(final String secretKey) {
		this.secretKey = secretKey;
	}

	/**
	 * @return the region where the hazelcast members are running; defaults to the current region when not set
	 */
	@Nullable
	public String getRegion() {
		return region;
	}
	
	/**
	 * Sets the region where the hazelcast members are running.
	 * <p>
	 * <b>Note: </b> This should never be called by clients.
	 * 
	 * @param region the region where the hazelcast members are running
	 */
	public void setRegion(final String region) {
		this.region = region;
	}

	/**
	 * @return the host header either being 'ec2', 'ecs' or the URL of a specific EC2/ECS API endpoint
	 */
	@Nullable
	public String getHostHeader() {
		return hostHeader;
	}
	
	/**
	 * Sets host header.
	 * <p>
	 * <b>Note: </b> This should never be called by clients.
	 * 
	 * @param hostHeader the host header
	 */
	public void setHostHeader(final String hostHeader) {
		this.hostHeader = hostHeader;
	}

	/**
	 * @return the connection timeout in seconds when making calls to the AWS API
	 */
	public String getConnectionTimeout() {
		return connectionTimeout;
	}
	
	/**
	 * Sets the connection timeout in seconds when making calls to the AWS API.
	 * <p>
	 * <b>Note: </b> This should never be called by clients.
	 * 
	 * @param connectionTimeout the connection timeout in seconds when making calls to the AWS API
	 */
	public void setConnectionTimeout(final String connectionTimeout) {
		this.connectionTimeout = connectionTimeout;
	}

	/**
	 * @return the read timeout in seconds when making calls to the AWS API
	 */
	public String getReadTimeout() {
		return readTimeout;
	}
	
	/**
	 * Sets the read timeout in seconds when making calls to the AWS API.
	 * <p>
	 * <b>Note: </b> This should never be called by clients.
	 * 
	 * @param readTimeout the read timeout in seconds when making calls to the AWS API
	 */
	public void setReadTimeout(final String readTimeout) {
		this.readTimeout = readTimeout;
	}

	/**
	 * @return the number of retries while connecting to the AWS API
	 */
	public String getConnectionRetries() {
		return connectionRetries;
	}
	
	/**
	 * Sets the number of retries while connecting to the AWS API.
	 * <p>
	 * <b>Note: </b> This should never be called by clients.
	 * 
	 * @param connectionRetries the number of retries while connecting to the AWS API
	 */
	public void setConnectionRetries(final String connectionRetries) {
		this.connectionRetries = connectionRetries;
	}

	/**
	 * @return range of ports of relevant hazelcast members
	 */
	@Nullable
	public String getPortRange() {
		return portRange;
	}
	
	/**
	 * Sets the range of ports of relevant hazelcast members.
	 * <p>
	 * <b>Note: </b> This should never be called by clients.
	 * 
	 * @param portRange range of ports of relevant hazelcast members
	 */
	public void setPortRange(final String portRange) {
		this.portRange = portRange;
	}

	/**
	 * @return the EC2 specific {@link Ec2Properties}
	 */
	@Nullable
	public Ec2Properties getEc2() {
		return ec2;
	}
	
	/**
	 * Sets the {@link Ec2Properties}.
	 * <p>
	 * <b>Note: </b> This should never be called by clients.
	 * 
	 * @param ec2 the {@link Ec2Properties}
	 */
	public void setEc2(final Ec2Properties ec2) {
		this.ec2 = ec2;
	}

	/**
	 * @return the ECS/Fargate specific {@link FargateProperties}
	 */
	@Nullable
	public FargateProperties getFargate() {
		return fargate;
	}
	
	/**
	 * Sets the {@link FargateProperties}.
	 * <p>
	 * <b>Note: </b> This should never be called by clients.
	 * 
	 * @param fargate the {@link FargateProperties}
	 */
	public void setFargate(final FargateProperties fargate) {
		this.fargate = fargate;
	}

	/**
	 * EC2 specific configuration properties.
	 */
	public static class Ec2Properties {

		@Nullable
		private String iamRole;
		@Nullable
		private String securityGroupName;
		@Nullable
		private String tagKey;
		@Nullable
		private String tagValue;
		@Nullable
		private AutoscalingProperties autoscaling;
		
		/**
		 * @return the IAM role of the EC2 instances to use
		 */
		@Nullable
		public String getIamRole() {
			return iamRole;
		}
		
		/**
		 * Sets the IAM role of the EC2 instances to use.
		 * <p>
		 * <b>Note: </b> This should never be called by clients.
		 * 
		 * @param iamRole the IAM role of the EC2 instances to use
		 */
		public void setIamRole(final String iamRole) {
			this.iamRole = iamRole;
		}

		/**
		 * @return the security group name to filter EC2 instances during the lookup
		 */
		@Nullable
		public String getSecurityGroupName() {
			return securityGroupName;
		}
		
		/**
		 * Sets the security group name to filter EC2 instances during the lookup.
		 * <p>
		 * <b>Note: </b> This should never be called by clients.
		 * 
		 * @param securityGroupName the security group name to filter EC2 instances during the lookup
		 */
		public void setSecurityGroupName(final String securityGroupName) {
			this.securityGroupName = securityGroupName;
		}

		/**
		 * @return the tag key to filter EC2 instances during the lookup
		 * @see #getTagValue()
		 */
		@Nullable
		public String getTagKey() {
			return tagKey;
		}
		
		/**
		 * Sets the tag key to filter EC2 instances during the lookup.
		 * <p>
		 * <b>Note: </b> This should never be called by clients.
		 * 
		 * @param tagKey the tag key to filter EC2 instances during the lookup
		 */
		public void setTagKey(final String tagKey) {
			this.tagKey = tagKey;
		}

		/**
		 * @return the tag value to filter EC2 instances during the lookup
		 * @see #getTagKey()
		 */
		@Nullable
		public String getTagValue() {
			return tagValue;
		}
		
		/**
		 * Sets the tag value to filter EC2 instances during the lookup.
		 * <p>
		 * <b>Note: </b> This should never be called by clients.
		 * 
		 * @param tagValue the tag value to filter EC2 instances during the lookup
		 */
		public void setTagValue(final String tagValue) {
			this.tagValue = tagValue;
		}
		
		/**
		 * @return the {@link AutoscalingProperties}
		 */
		@Nullable
		public AutoscalingProperties getAutoscaling() {
			return autoscaling;
		}
		
		/**
		 * Sets the {@link AutoscalingProperties}.
		 * <p>
		 * <b>Note: </b> This should never be called by clients.
		 * 
		 * @param autoscaling the {@link AutoscalingProperties}
		 */
		public void setAutoscaling(final AutoscalingProperties autoscaling) {
			this.autoscaling = autoscaling;
		}
		
		/**
		 * EC2 specific autoscaling properties.
		 */
		public static class AutoscalingProperties {
			
			@Nullable
			private String lifecycleQueueName;
			@Min(60)
			private int activeJobsTimeout = 60;
			@Min(60)
			private int memberShutdownTimeout = 600;
			
			/**
			 * @return the name of the SQS queue receiving lifecycle hook messages from the autoscaler
			 */
			@Nullable
			public String getLifecycleQueueName() {
				return lifecycleQueueName;
			}
			
			/**
			 * Sets the name of the SQS queue receiving lifecycle hook messages from the autoscaler.
			 * <p>
			 * <b>Note: </b> This should never be called by clients.
			 *
			 * @param lifecycleQueueName the name of the SQS queue receiving lifecycle hook messages from the autoscaler
			 */
			public void setLifecycleQueueName(final String lifecycleQueueName) {
				this.lifecycleQueueName = lifecycleQueueName;
			}
			
			/**
			 * @return the timeout in seconds to wait on actively running jobs before giving up and denying the autoscaler to terminate the machine
			 */
			public int getActiveJobsTimeout() {
				return activeJobsTimeout;
			}
			
			/**
			 * Sets the timeout in seconds to wait on actively running jobs before giving up and denying the autoscaler to terminate the machine.
			 * <p>
			 * <b>Note: </b> This should never be called by clients.
			 * 
			 * @param activeJobsTimeout the timeout in seconds to wait on actively running jobs before giving up and denying the autoscaler to terminate the machine
			 */
			public void setActiveJobsTimeout(final int activeJobsTimeout) {
				this.activeJobsTimeout = activeJobsTimeout;
			}
			
			/**
			 * @return the timeout in seconds to wait for internal member shutdown before giving up and denying the autoscaler to terminate the machine 
			 */
			public int getMemberShutdownTimeout() {
				return memberShutdownTimeout;
			}
			
			/**
			 * Sets the timeout in seconds to wait for internal member shutdown before giving up and denying the autoscaler to terminate the machine.
			 * <p>
			 * <b>Note: </b> This should never be called by clients.
			 * 
			 * @param memberShutdownTimeout the timeout in seconds to wait for internal member shutdown before giving up and denying the autoscaler to terminate the machine
			 */
			public void setMemberShutdownTimeout(final int memberShutdownTimeout) {
				this.memberShutdownTimeout = memberShutdownTimeout;
			}
		}
	}

	/**
	 * Fargate specific configuration properties.
	 */
	public static class FargateProperties {

		@Nullable
		private String clusterShortName;
		@Nullable
		private String family;
		@Nullable
		private String serviceName;
		
		/**
		 * @return the ECS cluster short name or ARN
		 */
		@Nullable
		public String getClusterShortName() {
			return clusterShortName;
		}
		
		/**
		 * Sets the ECS cluster short name or ARN.
		 * <p>
		 * <b>Note: </b> This should never be called by clients.
		 * 
		 * @param clusterShortName the ECS cluster short name or ARN
		 */
		public void setClusterShortName(final String clusterShortName) {
			this.clusterShortName = clusterShortName;
		}
		
		/**
		 * @return filter to lookup only ECS tasks with the given family name
		 */
		@Nullable
		public String getFamily() {
			return family;
		}
		
		/**
		 * Sets the filter to lookup only ECS tasks with the given family name.
		 * <p>
		 * <b>Note: </b> This should never be called by clients.
		 * 
		 * @param family filter to lookup only ECS tasks with the given family name
		 */
		public void setFamily(final String family) {
			this.family = family;
		}
		
		/**
		 * @return filter to lookup only ECS tasks with the given service name
		 */
		@Nullable
		public String getServiceName() {
			return serviceName;
		}
		
		/**
		 * Sets the filter to lookup only ECS tasks with the given service name.
		 * <p>
		 * <b>Note: </b> This should never be called by clients.
		 * 
		 * @param serviceName filter to lookup only ECS tasks with the given service name
		 */
		public void setServiceName(final String serviceName) {
			this.serviceName = serviceName;
		}
	}
}

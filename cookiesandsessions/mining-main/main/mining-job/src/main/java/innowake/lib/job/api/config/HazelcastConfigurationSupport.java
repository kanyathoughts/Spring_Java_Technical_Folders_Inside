/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.lib.job.api.config;

import com.hazelcast.cluster.MembershipEvent;
import com.hazelcast.cluster.MembershipListener;
import com.hazelcast.config.AwsConfig;
import com.hazelcast.config.Config;
import com.hazelcast.config.JoinConfig;
import com.hazelcast.core.Hazelcast;
import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.spring.context.SpringManagedContext;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.config.properties.AwsProperties;
import innowake.lib.job.api.config.properties.ClusterProperties;
import innowake.lib.job.api.config.properties.ClusterProperties.NodeNameStrategy;
import innowake.lib.job.api.config.properties.JobConfigurationProperties;
import innowake.lib.job.api.config.properties.LogProperties;
import innowake.lib.job.api.config.properties.MulticastProperties;
import innowake.lib.job.api.config.properties.TcpIpProperties;
import innowake.lib.job.internal.Logging;
import innowake.lib.job.internal.NodeNameFactory;
import innowake.lib.job.internal.hazelcast.HzClusterInformation;
import org.apache.commons.lang.StringUtils;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

/**
 * Base-class for Configuration classes,
 * providing utility methods for configuring a Hazelcast instance.
 */
public class HazelcastConfigurationSupport {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.CLUSTER_INFO);

	protected static final String PROPERTY_NODE_NAME = "job-api.cluster.nodeName";
	protected static final String PROPERTY_LOG_FOLDER = "job-api.log.logFolder";
	protected static final String PROPERTY_LOG_FILE_PREFIX = "job-api.log.logFilePrefix";

	@Nullable
	protected String nodeName;

	/**
	 * @param springManagedContext the {@link SpringManagedContext}
	 * @param jobConfig the {@link JobConfigurationProperties}
	 * @param nodeNameFactory the {@link NodeNameFactory}
	 * @return the {@link HazelcastInstance}
	 */
	public HazelcastInstance getHazelcastInstance(final SpringManagedContext springManagedContext, final JobConfigurationProperties jobConfig,
			final NodeNameFactory nodeNameFactory) {
		final ClusterProperties.NodeNameStrategy strategy = jobConfig.getCluster().getNodeNameStrategy();
		/* All types of strategies can directly resolve the node name here, except when using the hazelcast node id.
		 * In this case the hazelcast startup sequence can also not be part of any node name specific logging. */
		if (strategy != ClusterProperties.NodeNameStrategy.INTERNAL_NODE_ID) {
			handleLogConfiguration(nodeNameFactory, strategy, jobConfig.getLog(), jobConfig.getCluster(), null);
		}

		final Config config = getHazelcastConfig(springManagedContext, jobConfig.getCluster());
		config.setClassLoader(Thread.currentThread().getContextClassLoader());

		final HazelcastInstance instance = Hazelcast.newHazelcastInstance(config);
		/* If the node name is based on the hazelcast node id, we first have to create the whole hazelcast instance. */
		if (strategy == ClusterProperties.NodeNameStrategy.INTERNAL_NODE_ID) {
			handleLogConfiguration(nodeNameFactory, strategy, jobConfig.getLog(), jobConfig.getCluster(), instance);
		}

		final HzClusterInformation clusterInformation = new HzClusterInformation(instance);
		/* We use the membership listener to get instantly notified if a node has been removed from the cluster to update the internal cluster information. */
		instance.getCluster().addMembershipListener(new MembershipListener() {

			@Override
			public void memberRemoved(@Nullable final MembershipEvent membershipEvent) {
				if (membershipEvent != null) {
					clusterInformation.removeMember(membershipEvent.getMember().getUuid().toString());
				}
			}

			@Override
			public void memberAdded(@Nullable final MembershipEvent membershipEvent) {
				/* no-op */
			}
		});

		return instance;
	}

	protected void handleLogConfiguration(final NodeNameFactory nodeNameFactory, final ClusterProperties.NodeNameStrategy nodeNameStrategy,
			final LogProperties logProperties, final ClusterProperties clusterProperties, @Nullable final HazelcastInstance instance) {
		final ClusterProperties.ClusterMode mode = clusterProperties.getMode();
		nodeName = nodeNameFactory.createNodeName(nodeNameStrategy, instance);
		LOG.info(() -> "Resolved node name: " + nodeName);

		/* These system properties are not intended to be used in java code, but only to make logging specific properties
		 * accessible in the log4j configuration. */
		System.setProperty(PROPERTY_NODE_NAME, nodeName);
		/* When in any clustered mode, we append the name of the current node to the default configured log folder. */
		System.setProperty(PROPERTY_LOG_FOLDER, logProperties.getLogFolder() + (mode != ClusterProperties.ClusterMode.STANDALONE ? "/" + nodeName : ""));
		System.setProperty(PROPERTY_LOG_FILE_PREFIX, logProperties.getLogFilePrefix());
	}

	protected Config getHazelcastConfig(final SpringManagedContext springManagedContext, final ClusterProperties clusterProperties) {
		final ClusterProperties.NetworkProperties networkProperties = clusterProperties.getNetwork();

		final Config config = new Config()
				/* same as <hz:spring-aware/> */
				.setManagedContext(springManagedContext)
				.setClusterName(clusterProperties.getName())
				.setProperty("hazelcast.member.naming.moby.enabled", "true");

		config.getNetworkConfig()
				.setPort(networkProperties.getPort())
				.setPortAutoIncrement(networkProperties.isPortAutoIncrement());

		final String interfaces = networkProperties.getInterfaces();
		if (interfaces != null) {
			config.getNetworkConfig().getInterfaces().setEnabled(true).setInterfaces(Arrays.asList(interfaces.split(",")));
		}

		final TcpIpProperties tcpIpProperties = networkProperties.getTcpIp();
		final JoinConfig joinConfig = config.getNetworkConfig().getJoin();
		final ClusterProperties.ClusterMode clusterMode = clusterProperties.getMode();
		switch (clusterMode) {
			case STANDALONE:
				configureStandaloneOrTcpIpCluster(joinConfig, tcpIpProperties);
				break;
			case DEFAULT:
				final MulticastProperties multicastProperties = networkProperties.getMulticast();
				if (tcpIpProperties != null) {
					configureStandaloneOrTcpIpCluster(joinConfig, tcpIpProperties);
				} else if (multicastProperties != null) {
					configureMulticastCluster(joinConfig, multicastProperties);
				} else {
					/* Not providing any explicit TCP-IP or multicast configuration, will start hazelcast with default multicast configuration. */
					LOG.debug(() -> "No explicit TCP-IP or multicast configuration provided. Will run job-api with default multicast configuration");
				}
				break;
			case AWS:
				configureAwsCluster(joinConfig, networkProperties.getAws());
				break;
			default:
				throw new IllegalStateException("Unsupported cluster mode: " + clusterMode.name());
		}

		return config;
	}

	protected void configureStandaloneOrTcpIpCluster(final JoinConfig joinConfig, @Nullable final TcpIpProperties tcpIpProperties) {
		/* Running in standalone mode or with explicit TCP-IP properties. Multicast needs to be explicitly disabled as it's enabled by default.
		 * For standalone mode the empty TCP-IP config will use the local machine as its own single member. */
		joinConfig.getMulticastConfig().setEnabled(false);
		joinConfig.getTcpIpConfig().setEnabled(true);

		/* Explicit provided TCP-IP properties */
		if (tcpIpProperties != null) {
			LOG.debug(() -> "Configuring job-api with provided TCP-IP cluster settings");
			joinConfig.getTcpIpConfig().setConnectionTimeoutSeconds(tcpIpProperties.getConnectionTimeout());

			final String members = tcpIpProperties.getMembers();
			if (StringUtils.isNotBlank(members)) {
				joinConfig.getTcpIpConfig().addMember(tcpIpProperties.getMembers());
			}
		} else {
			LOG.debug(() -> "Configuring job-api to run in non-clustered standalone mode");
		}
	}

	protected void configureMulticastCluster(final JoinConfig joinConfig, final MulticastProperties multicastProperties) {
		LOG.debug(() -> "Configuring job-api with provided multicast cluster settings");
		joinConfig.getMulticastConfig()
				.setEnabled(true)
				.setLoopbackModeEnabled(multicastProperties.isEnableLoopback())
				.setMulticastGroup(multicastProperties.getGroup())
				.setMulticastPort(multicastProperties.getPort())
				.setMulticastTimeoutSeconds(multicastProperties.getConnectionTimeout())
				.setMulticastTimeToLive(multicastProperties.getTtl());

		final String trustedInterfaces = multicastProperties.getTrustedInterfaces();
		if (trustedInterfaces != null) {
			final Set<String> interfaces = new HashSet<>(Arrays.asList(trustedInterfaces.split(",")));
			joinConfig.getMulticastConfig().setTrustedInterfaces(interfaces);
		}
	}

	protected void configureAwsCluster(final JoinConfig joinConfig, @Nullable final AwsProperties awsProperties) {
		LOG.debug(() -> "Configuring job-api with provided AWS settings");
		joinConfig.getMulticastConfig().setEnabled(false);
		final AwsConfig awsConfig = joinConfig.getAwsConfig();
		awsConfig.setEnabled(true);

		if (awsProperties != null) {
			awsConfig.setProperty("connection-timeout-seconds", awsProperties.getConnectionTimeout())
					.setProperty("read-timeout-seconds", awsProperties.getReadTimeout())
					.setProperty("connection-retries", awsProperties.getConnectionRetries());

			final String accessKey = awsProperties.getAccessKey();
			if (accessKey != null && awsProperties.getSecretKey() != null) {
				awsConfig.setProperty("access-key", accessKey).setProperty("secret-key", awsProperties.getSecretKey());
			}
			if (awsProperties.getRegion() != null) {
				awsConfig.setProperty("region", awsProperties.getRegion());
			}
			final String hostHeader = awsProperties.getHostHeader();
			if (hostHeader != null) {
				awsConfig.setProperty("host-header", hostHeader);
			}
			if (awsProperties.getPortRange() != null) {
				awsConfig.setProperty("hz-port", awsProperties.getPortRange());
			}

			final AwsProperties.Ec2Properties ec2Properties = awsProperties.getEc2();
			final AwsProperties.FargateProperties fargateProperties = awsProperties.getFargate();
			if (ec2Properties != null && fargateProperties != null) {
				throw new IllegalStateException("Either only provide 'ec2' or 'fargate' specific properties");
			} else if (ec2Properties != null) {
				if (ec2Properties.getIamRole() != null) {
					if (accessKey != null) {
						throw new IllegalStateException("Either 'accessKey' or 'iamRole' has to be provided, but not both.");
					}
					awsConfig.setProperty("iam-role", ec2Properties.getIamRole());
				}
				if (ec2Properties.getSecurityGroupName() != null) {
					awsConfig.setProperty("security-group-name", ec2Properties.getSecurityGroupName());
				}
				if (ec2Properties.getTagKey() != null && ec2Properties.getTagValue() != null) {
					awsConfig.setProperty("tag-key", ec2Properties.getTagKey()).setProperty("tag-value", ec2Properties.getTagValue());
				}
			} else if (fargateProperties != null) {
				if (fargateProperties.getClusterShortName() != null) {
					awsConfig.setProperty("cluster", fargateProperties.getClusterShortName());
				}
				final String family = fargateProperties.getFamily();
				final String serviceName = fargateProperties.getServiceName();
				if (family != null && serviceName != null) {
					throw new IllegalStateException("The parameters 'family' and 'serviceName' are mutually exclusive.");
				} else if (family != null) {
					awsConfig.setProperty("family", family);
				} else if (serviceName != null) {
					awsConfig.setProperty("service-name", serviceName);
				}
			}
		}
	}

	@Nullable
	protected String getAwsLifecycleQueueName(final ClusterProperties clusterProperties) {
		final ClusterProperties.ClusterMode clusterMode = clusterProperties.getMode();
		if (clusterMode == ClusterProperties.ClusterMode.AWS) {
			final AwsProperties awsProperties = clusterProperties.getNetwork().getAws();
			if (awsProperties != null) {
				final AwsProperties.Ec2Properties ec2Properties = awsProperties.getEc2();
				if (ec2Properties != null) {
					final AwsProperties.Ec2Properties.AutoscalingProperties autoscalingProperties = ec2Properties.getAutoscaling();
					if (autoscalingProperties != null && StringUtils.isNotBlank(autoscalingProperties.getLifecycleQueueName())) {
						return autoscalingProperties.getLifecycleQueueName();
					}
				}
			}
		}
		return null;
	}

	/**
	 * @return the resolved name of this node, depending on the configured {@link NodeNameStrategy}
	 */
	public String getNodeName() {
		return Assert.assertNotNull(nodeName);
	}
}

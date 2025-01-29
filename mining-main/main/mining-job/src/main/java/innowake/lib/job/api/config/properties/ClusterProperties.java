/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api.config.properties;

import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotBlank;

import org.springframework.boot.context.properties.ConfigurationPropertiesBinding;
import org.springframework.boot.context.properties.ConstructorBinding;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import com.hazelcast.config.NetworkConfig;

import innowake.lib.core.api.lang.Nullable;

/**
 * Cluster specific properties.
 */
public class ClusterProperties {
	
	/**
	 * The supported cluster modes.
	 */
	public enum ClusterMode {
		/** No cluster will be formed between different hazelcast instances. */
		STANDALONE,
		
		/** Forms a default cluster either via TCP-IP or multicast. */
		DEFAULT,
		
		/** Forms an AWS EC2 or ECS/Fargate cluster. */
		AWS;
	}
	
	/**
	 * The supported strategies to resolve the cluster node name.
	 */
	public enum NodeNameStrategy {
		
		/** Uses a random UUID. This will change with every start of the application. */
		RANDOM_UUID("random-uuid"),
		
		/** Uses the internal hazelcast node Id. This will change with every start of the application. */
		INTERNAL_NODE_ID("internal-node-id"),
		
		/** Uses the primary outbound IP address. */
		IP("ip"),
		
		/** Uses the hostname. This is the default. */
		HOSTNAME("hostname"),
		
		/** Uses the instance Id of the current EC2 machine. */
		EC2_INSTANCE_ID("ec2-instance-id");
		
		private String configName;
		
		private NodeNameStrategy(final String configName) {
			this.configName = configName;
		}
	}

	@NotBlank
	private String name = "job-api-standalone";
	private ClusterMode mode = ClusterMode.STANDALONE;
	private NodeNameStrategy nodeNameStrategy = NodeNameStrategy.HOSTNAME;
	@Valid
	private NetworkProperties network = new NetworkProperties();

	/**
	 * @return the name of the cluster. Defaults to {@code job-api-cluster}
	 */
	public String getName() {
		return name;
	}
	
	/**
	 * Sets the name of the cluster.
	 * <p>
	 * <b>Note: </b> This should never be called by clients.
	 * 
	 * @param name the name of the cluster
	 */
	public void setName(final String name) {
		this.name = name;
	}
	
	/**
	 * @return the {@link ClusterMode}
	 */
	public ClusterMode getMode() {
		return mode;
	}
	
	/**
	 * Sets the {@link ClusterMode}
	 * <p>
	 * <b>Note: </b> This should never be called by clients.
	 * 
	 * @param mode the {@link ClusterMode}
	 */
	public void setMode(final ClusterMode mode) {
		this.mode = mode;
	}
	
	/**
	 * @return the configured {@link NodeNameStrategy}
	 */
	public NodeNameStrategy getNodeNameStrategy() {
		return nodeNameStrategy;
	}
	
	/**
	 * Sets the {@link NodeNameStrategy}
	 * <p>
	 * <b>Note: </b> This should never be called by clients.
	 * 
	 * @param nodeNameStrategy the {@link NodeNameStrategy}
	 */
	public void setNodeNameStrategy(final NodeNameStrategy nodeNameStrategy) {
		this.nodeNameStrategy = nodeNameStrategy;
	}
	
	/**
	 * @return the {@link NetworkProperties} containing the network configuration properties for the cluster
	 */
	public NetworkProperties getNetwork() {
		return network;
	}
	
	/**
	 * Sets the {@link NetworkProperties}.
	 * <p>
	 * <b>Note: </b> This should never be called by clients.
	 * 
	 * @param network the {@link NetworkProperties}
	 */
	public void setNetwork(final NetworkProperties network) {
		this.network = network;
	}

	/**
	 * The network configuration properties for the cluster.
	 */
	@ConstructorBinding
	public static class NetworkProperties {

		@Min(0)
		@Max(65535)
		private int port = NetworkConfig.DEFAULT_PORT;
		private boolean portAutoIncrement = true;
		@Nullable
		private TcpIpProperties tcpIp;
		@Nullable
		private MulticastProperties multicast;
		@Nullable
		private AwsProperties aws;
		@Nullable
		private String interfaces;
		
		/**
		 * @return the port that the local node will start on. Defaults to 5701
		 */
		public int getPort() {
			return port;
		}
		
		/**
		 * Sets the port to be used.
		 * <p>
		 * <b>Note: </b> This should never be called by clients.
		 * 
		 * @param port the port
		 */
		public void setPort(final int port) {
			this.port = port;
		}
		
		/**
		 * @return {@code true} (default) to increment the port provided with {@link #getPort()} in case it's already in use; {@code false} to fail startup
		 */
		public boolean isPortAutoIncrement() {
			return portAutoIncrement;
		}
		
		/**
		 * Sets if the port should be auto incremented if already in use.
		 * <p>
		 * <b>Note: </b> This should never be called by clients.
		 * 
		 * @param portAutoIncrement {@code true} to auto increment; {@code false} otherwise
		 */
		public void setPortAutoIncrement(final boolean portAutoIncrement) {
			this.portAutoIncrement = portAutoIncrement;
		}

		/**
		 * @return the {@link TcpIpProperties} containing the TCP-IP configuration properties for the cluster if multicast is not being used
		 */
		@Nullable
		public TcpIpProperties getTcpIp() {
			return tcpIp;
		}
		
		/**
		 * Sets the {@link TcpIpProperties}.
		 * <p>
		 * <b>Note: </b> This should never be called by clients.
		 * 
		 * @param tcpIp the {@link TcpIpProperties}
		 */
		public void setTcpIp(final TcpIpProperties tcpIp) {
			this.tcpIp = tcpIp;
		}
		
		/**
		 * @return the {@link MulticastProperties} containing the multicast configuration properties for the cluster if explicit TCP-IP is not used
		 */
		@Nullable
		public MulticastProperties getMulticast() {
			return multicast;
		}
		
		/**
		 * Sets the {@link MulticastProperties}.
		 * <p>
		 * <b>Note: </b> This should never be called by clients.
		 * 
		 * @param multicast the {@link MulticastProperties}
		 */
		public void setMulticast(final MulticastProperties multicast) {
			this.multicast = multicast;
		}

		/**
		 * @return the {@link AwsProperties} containing AWS specific configuration properties for the cluster
		 */
		@Nullable
		public AwsProperties getAws() {
			return aws;
		}
		
		/**
		 * Sets the {@link AwsProperties}.
		 * <p>
		 * <b>Note: </b> This should never be called by clients.
		 * 
		 * @param aws the {@link AwsProperties}
		 */
		public void setAws(final AwsProperties aws) {
			this.aws = aws;
		}

		/**
		 * @return relevant list of provided comma separated network interfaces for cluster creation
		 */
		@Nullable
		public String getInterfaces() {
			return interfaces;
		}
		
		/**
		 * Sets a comma separated list of newtork interfaces for cluster creation.
		 * <p>
		 * <b>Note: </b> This should never be called by clients.
		 * 
		 * @param interfaces a comma separated list of newtork interfaces for cluster creation
		 */
		public void setInterfaces(final String interfaces) {
			this.interfaces = interfaces;
		}

	}
	
	/**
	 * Converter implementation for the {@link NodeNameStrategy} configuration property.
	 */
	@Component
	@ConfigurationPropertiesBinding
	public static class NodeNameStrategyConverter implements Converter<String, NodeNameStrategy> {
		
		@Override
		@Nullable
		public NodeNameStrategy convert(final String configValue) {
			for (final NodeNameStrategy strategy : NodeNameStrategy.values()) {
				if (strategy.name().equalsIgnoreCase(configValue) || strategy.configName.equalsIgnoreCase(configValue)) {
					return strategy;
				}
			}
			return null;
		}
		
	}

}

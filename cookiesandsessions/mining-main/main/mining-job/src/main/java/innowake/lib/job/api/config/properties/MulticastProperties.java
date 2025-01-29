/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api.config.properties;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;

import com.hazelcast.config.MulticastConfig;

import innowake.lib.core.api.lang.Nullable;

/**
 * The multicast configuration properties for the cluster.
 */
public class MulticastProperties {

	private boolean enableLoopback = false;
	private String group = MulticastConfig.DEFAULT_MULTICAST_GROUP;
	@Min(0)
	private int port = MulticastConfig.DEFAULT_MULTICAST_PORT;
	private int connectionTimeout = MulticastConfig.DEFAULT_MULTICAST_TIMEOUT_SECONDS;
	@Min(0)
	@Max(255)
	private int ttl = MulticastConfig.DEFAULT_MULTICAST_TTL;
	@Nullable
	private String trustedInterfaces;

	/**
	 * @return {@code true} to enable loopback during node discovery; {@code false} to exclude the local machine. Disabled by default
	 */
	public boolean isEnableLoopback() {
		return enableLoopback;
	}
	
	/**
	 * Sets if loopback lookup should be enabled.
	 * <p>
	 * <b>Note: </b> This should never be called by clients.
	 * 
	 * @param enableLoopback {@code true} to enable loopback discovery; {@code false} otherwise
	 */
	public void setEnableLoopback(final boolean enableLoopback) {
		this.enableLoopback = enableLoopback;
	}
	
	/**
	 * @return the multicast group. Defaults to {@code 224.2.2.3}
	 */
	public String getGroup() {
		return group;
	}
	
	/**
	 * Sets the multicast group.
	 * <p>
	 * <b>Note: </b> This should never be called by clients.
	 * 
	 * @param group the group
	 */
	public void setGroup(final String group) {
		this.group = group;
	}
	
	/**
	 * @return the multicast port. Defaults to 54327
	 */
	public int getPort() {
		return port;
	}
	
	/**
	 * Sets the mutlicast port.
	 * <p>
	 * <b>Note: </b> This should never be called by clients.
	 * 
	 * @param port the port
	 */
	public void setPort(final int port) {
		this.port = port;
	}
	
	/**
	 * @return the time in seconds a node will wait for a multicast response of a different node, before declaring itself to a master of its own
	 * cluster. Defaults to 2
	 */
	public int getConnectionTimeout() {
		return connectionTimeout;
	}
	
	/**
	 * Sets the connection timeout for multicast responses.
	 * <p>
	 * <b>Note: </b> This should never be called by clients.
	 * 
	 * @param connectionTimeout the timeout
	 */
	public void setConnectionTimeout(final int connectionTimeout) {
		this.connectionTimeout = connectionTimeout;
	}
	
	/**
	 * @return the TTL of a multicast packet. Defaults to 32
	 */
	public int getTtl() {
		return ttl;
	}
	
	/**
	 * Sets the TTL for multicast packets.
	 * <p>
	 * <b>Note: </b> This should never be called by clients.
	 * 
	 * @param ttl the TTL
	 */
	public void setTtl(final int ttl) {
		this.ttl = ttl;
	}
	
	/**
	 * @return comma separated list of IPs that are eligible in the multicast lookup
	 */
	@Nullable
	public String getTrustedInterfaces() {
		return trustedInterfaces;
	}
	
	/**
	 * Sets the comma separated list of IPs eligible for multicast lookup.
	 * <p>
	 * <b>Note: </b> This should never be called by clients.
	 * 
	 * @param trustedInterfaces comma separated list of IPs
	 */
	public void setTrustedInterfaces(final String trustedInterfaces) {
		this.trustedInterfaces = trustedInterfaces;
	}

}

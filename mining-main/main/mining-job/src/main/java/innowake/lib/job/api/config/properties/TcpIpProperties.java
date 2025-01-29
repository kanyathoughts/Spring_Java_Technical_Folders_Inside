/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api.config.properties;

import javax.validation.constraints.Min;

import innowake.lib.core.api.lang.Nullable;

/**
 * The TCP-IP configuration properties for the cluster.
 */
public class TcpIpProperties {

	@Min(0)
	private int connectionTimeout = 5;
	@Nullable
	private String members;
	
	/**
	 * @return the timeout to wait until giving up to connect to a configured cluster member. Defaults to 5
	 */
	public int getConnectionTimeout() {
		return connectionTimeout;
	}
	
	/**
	 * Sets the connection timeout to connect to other cluster members.
	 * <p>
	 * <b>Note: </b> This should never be called by clients.
	 * 
	 * @param connectionTimeout the timeout in seconds
	 */
	public void setConnectionTimeout(final int connectionTimeout) {
		this.connectionTimeout = connectionTimeout;
	}
	
	/**
	 * @return comma separated list of IPs of all cluster members to connect to
	 */
	@Nullable
	public String getMembers() {
		return members;
	}
	
	/**
	 * Sets the comma separated list of IPs of cluster members.
	 * <p>
	 * <b>Note: </b> This should never be called by clients.
	 * 
	 * @param members the members
	 */
	public void setMembers(final String members) {
		this.members = members;
	}

}

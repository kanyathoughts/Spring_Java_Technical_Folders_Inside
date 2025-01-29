/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api.config.properties;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotBlank;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.validation.annotation.Validated;

/**
 * Spring configuration properties for the job API.
 */
@ConfigurationProperties(prefix = "job-api")
@Validated
public class JobConfigurationProperties {

	/** The amount of available logical processors. */
	public static final int PROCESSORS = Runtime.getRuntime().availableProcessors();

	@Min(4)
	/* By default we allow one job per four logical processor cores on the local cluster node, as jobs are parallelized themselves via tasks.
	 * To avoid deadlocks in jobs that fork other jobs, we set the minimum to 4. While this does not completely prevent a deadlock situation,
	 * it should make it less likely. */
	private int maximumLocalJobThreads = Math.max(4, PROCESSORS / 4);
	@Min(1)
	private int maximumLocalTaskThreads = PROCESSORS;
	@Min(1)
	private int maximumHeartbeatAge = 5 * 60;
	@NotBlank
	private String jobResultFolder = "jobResults";
	private ClusterProperties cluster = new ClusterProperties();
	private LogProperties log = new LogProperties();
	@Min(1)
	private int jobHeartbeatInterval = 90;

	/**
	 * @return the maximum amount of threads available to the local cluster node to execute jobs
	 */
	public int getMaximumLocalJobThreads() {
		return maximumLocalJobThreads;
	}

	/**
	 * Sets the maximum amount of threads available to the local cluster node to execute jobs.
	 * <p>
	 * <b>Note: </b> This should never be called by clients.
	 *
	 * @param maximumLocalJobThreads the maximum amount of threads available to the local cluster node to execute jobs
	 */
	public void setMaximumLocalJobThreads(final int maximumLocalJobThreads) {
		this.maximumLocalJobThreads = maximumLocalJobThreads;
	}

	/**
	 * @return the maximum amount of threads available to the local cluster node to execute tasks
	 */
	public int getMaximumLocalTaskThreads() {
		return maximumLocalTaskThreads;
	}

	/**
	 * Sets the maximum amount of threads available to the local cluster node to execute tasks.
	 * <p>
	 * <b>Note: </b> This should never be called by clients.
	 *
	 * @param maximumLocalTaskThreads the maximum amount of threads available to the local cluster node to execute tasks
	 */
	public void setMaximumLocalTaskThreads(final int maximumLocalTaskThreads) {
		this.maximumLocalTaskThreads = maximumLocalTaskThreads;
	}

	/**
	 * @return the maximum allowed age of the last job heartbeat in seconds
	 */
	public int getMaximumHeartbeatAge() {
		return maximumHeartbeatAge;
	}

	/**
	 * Sets the maximum allowed age of the last job heartbeat in seconds.
	 * <p>
	 * <b>Note: </b> This should never be called by clients.
	 *
	 * @param maximumHeartbeatAge the maximum allowed age of the last job heartbeat in seconds.
	 */
	public void setMaximumHeartbeatAge(final int maximumHeartbeatAge) {
		this.maximumHeartbeatAge = maximumHeartbeatAge;
	}

	/**
	 * @return the {@link ClusterProperties} containing cluster specific configuration properties
	 */
	public ClusterProperties getCluster() {
		return cluster;
	}

	/**
	 * Sets the {@link ClusterProperties}.
	 * <p>
	 * <b>Note: </b> This should never be called by clients.
	 *
	 * @param cluster the {@link ClusterProperties}
	 */
	public void setCluster(final ClusterProperties cluster) {
		this.cluster = cluster;
	}

	/**
	 * @return The {@link LogProperties}.
	 */
	public LogProperties getLog() {
		return log;
	}

	/**
	 * Sets the {@link LogProperties}.
	 *
	 * @param log The {@link LogProperties}.
	 */
	public void setLog(final LogProperties log) {
		this.log = log;
	}


	/**
	 * Returns the path on the server's file system where Job results are stored.
	 * <p>
	 * Note: this will only be used for large Job results. Otherwise the results are stored in the database.
	 * <p>
	 * Note: when running in clustered mode, this path must be accessible by all nodes in the cluster. In other words
	 * it must be located on some shared network drive or similar.
	 *
	 * @return the path on the server's file system where Job results are stored.
	 */
	public String getJobResultFolder() {
		return jobResultFolder;
	}


	/**
	 * Sets the path on the server's file system where Job results are stored.
	 * <p>
	 * Note: this will only be used for large Job results. Otherwise the results are stored in the database.
	 * <p>
	 * Note: when running in clustered mode, this path must be accessible by all nodes in the cluster. In other words
	 * it must be located on some shared network drive or similar.
	 *
	 * @param jobResultFolder the path on the server's file system where Job results are stored.
	 */
	public void setJobResultFolder(final String jobResultFolder) {
		this.jobResultFolder = jobResultFolder;
	}

	/**
	 * Returns the interval to execute the job heartbeat handler.
	 * 
	 * @return the heartbeat interval in seconds 
	 */
	public int getJobHeartbeatInterval() {
		return jobHeartbeatInterval;
	}

	/**
	 * Sets the job heartbeat interval.
	 * 
	 * @param jobHeartbeatInterval the heartbeat interval in seconds.
	 */
	public void setJobHeartbeatInterval(final int jobHeartbeatInterval) {
		this.jobHeartbeatInterval = jobHeartbeatInterval;
	}

}
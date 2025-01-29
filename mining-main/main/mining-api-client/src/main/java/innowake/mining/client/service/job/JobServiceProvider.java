/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.job;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ServiceProvider;

/**
 * Provides access to Job API services.
 */
public class JobServiceProvider extends ServiceProvider<JobServiceProvider> {

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	public JobServiceProvider(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Access to {@link CancelJob}.
	 *
	 * @return the service instance
	 */
	public CancelJob cancelJob() {
		return new CancelJob(connectionInfo);
	}
	
	/**
	 * Access to {@link GetJobInfo}.
	 *
	 * @return the service instance
	 */
	public GetJobInfo getJobInfo() {
		return new GetJobInfo(connectionInfo);
	}
	
	/**
	 * Access to {@link GetJobInfos}.
	 *
	 * @return the service instance
	 */
	public GetJobInfos getJobInfos() {
		return new GetJobInfos(connectionInfo);
	}
	
	/**
	 * Access to {@link GetJobResult}.
	 *
	 * @return the service instance
	 */
	public GetJobResult getJobResult() {
		return new GetJobResult(connectionInfo);
	}
	
	/**
	 * Access to {@link GetJobLog}.
	 *
	 * @return the service instance
	 */
	public GetJobLog getJobLog() {
		return new GetJobLog(connectionInfo);
	}
	
	/**
	 * Access to {@link GetJobLogStreamed}.
	 *
	 * @return the service instance
	 */
	public GetJobLogStreamed getJobLogStreamed() {
		return new GetJobLogStreamed(connectionInfo);
	}
	
	/**
	 * Access to {@link SubmitJobExtension}.
	 *
	 * @return the service instance
	 */
	public SubmitJobExtension submitJobExtension() {
		return new SubmitJobExtension(connectionInfo);
	}
	
}

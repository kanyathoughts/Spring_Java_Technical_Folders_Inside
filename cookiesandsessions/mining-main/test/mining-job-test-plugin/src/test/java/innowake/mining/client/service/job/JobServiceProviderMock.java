/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.job;

import static org.junit.Assert.assertNotNull;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;

/**
 * Extends the {@link JobServiceProvider} to be able to "mock" any REST calls during plugin-tests.
 */
public class JobServiceProviderMock extends JobServiceProvider {
	
	@Nullable
	private GetJobInfo getJobInfo;
	@Nullable
	private GetJobInfos getJobInfos;
	@Nullable
	private CancelJob cancelJob;

	/**
	 * Constructor.
	 * 
	 * @param connectionInfo the {@link ConnectionInfo}
	 */
	public JobServiceProviderMock(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	@Override
	public GetJobInfo getJobInfo() {
		if (getJobInfo == null) {
			getJobInfo = new GetJobInfoMock(connectionInfo);
		}
		assertNotNull(getJobInfo);
		return getJobInfo;
	}
	
	@Override
	public GetJobInfos getJobInfos() {
		if (getJobInfos == null) {
			getJobInfos = new GetJobInfosMock(connectionInfo);
		}
		assertNotNull(getJobInfos);
		return getJobInfos;
	}
	
	@Override
	public CancelJob cancelJob() {
		if (cancelJob == null) {
			cancelJob = new CancelJobMock(connectionInfo);
		}
		assertNotNull(cancelJob);
		return cancelJob;
	}

}

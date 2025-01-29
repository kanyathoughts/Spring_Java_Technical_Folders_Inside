/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.job;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.ResultMock;
import innowake.mining.shared.model.job.JobInformation;

/**
 * Allows to "mock" the REST call being executed by {@link GetJobInfo}.
 * <p>
 * Use {@link #addJobInfoToReturn(String, JobInformation)} to add {@link JobInformation} instances to be returned.
 */
public class GetJobInfoMock extends GetJobInfo {
	
	private final Map<String, JobInformation> jobInfoToReturn = new HashMap<>();

	/**
	 * Constructor.
	 * 
	 * @param connectionInfo the {@link ConnectionInfo}
	 */
	public GetJobInfoMock(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Adds a {@link JobInformation} instance that will be returned by {@link #execute()} when using the same job Id.
	 *
	 * @param jobId the job Id
	 * @param jobInfo the {@link JobInformation}
	 */
	public void addJobInfoToReturn(final String jobId, final JobInformation jobInfo) {
		jobInfoToReturn.put(jobId, jobInfo);
	}

	@Override
	public Result<JobInformation> execute() throws IOException {
		validate();
		return new ResultMock<>(new TypeReference<JobInformation>() {}, true, jobInfoToReturn.get(jobId));
	}
}

/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.job;

import java.io.IOException;
import java.net.ConnectException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.ResultMock;
import innowake.mining.shared.model.JobInfoFieldName;
import innowake.mining.shared.model.job.JobInformation;

/**
 * Allows to "mock" the REST call being executed by {@link GetJobInfos}.
 * <p>
 * Use {@link #addJobInfoToReturn(JobInformation)} to add {@link JobInformation} instances to be returned.
 */
public class GetJobInfosMock extends GetJobInfos {
	
	private final List<JobInformation> jobInfosToReturn = new ArrayList<>();
	
	@Nullable
	private Map<JobInfoFieldName, Map<String, Object>> filterObject;
	private boolean simulateOffline = false;

	/**
	 * Constructor.
	 * 
	 * @param connectionInfo the {@link ConnectionInfo}
	 */
	public GetJobInfosMock(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Adds a {@link JobInformation} instance that should be returned by {@link #execute()}.
	 * 
	 * @param jobInfo the {@link JobInformation}
	 */
	public void addJobInfoToReturn(final JobInformation jobInfo) {
		jobInfosToReturn.add(jobInfo);
	}
	
	/**
	 * Clears the list of {@link JobInformation} instances to be returned by {@link #execute()}.
	 */
	public void clearJobInfoToReturn() {
		jobInfosToReturn.clear();
	}
	
	/**
	 * @return the RSQL query that had originally been set to execute.
	 */
	@Nullable
	public Map<JobInfoFieldName, Map<String, Object>> getUsedFilters() {
		return filterObject;
	}
	
	/**
	 * Set to {@code true} to simulate the server not being reachable.
	 * 
	 * @param simulateOffline {@code true} to simulate the server not being reachable; {@code false} otherwise
	 */
	public void simulateOffline(final boolean simulateOffline) {
		this.simulateOffline = simulateOffline;
	}
	
	@Override
	public GetJobInfos setFilter(Map<JobInfoFieldName, Map<String, Object>> filterObject) {
		this.filterObject = filterObject;
		return super.setFilter(filterObject);
	}
	
	@Override
	public Result<JobInformation[]> execute() throws IOException {
		if (simulateOffline) {
			throw new ConnectException();
		}
		
		validate();
		return new ResultMock<>(new TypeReference<JobInformation[]>() {}, true, jobInfosToReturn.toArray(new JobInformation[] {}));
	}

}

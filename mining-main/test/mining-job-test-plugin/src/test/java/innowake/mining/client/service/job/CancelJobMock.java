/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.job;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.ResultMock;

/**
 * Allows to "mock" the REST call being executed by {@link CancelJob}.
 */
public class CancelJobMock extends CancelJob {
	
	private int executionAmount;

	/**
	 * Constructor.
	 * 
	 * @param connectionInfo the {@link ConnectionInfo}
	 */
	public CancelJobMock(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * @return the amount this REST call has been executed
	 */
	public int getExecutionAmount() {
		return executionAmount;
	}
	
	@Override
	public Result<Void> execute() throws IOException {
		executionAmount++;
		return new ResultMock<>(new TypeReference<Void>() {}, true, null);
	}

}

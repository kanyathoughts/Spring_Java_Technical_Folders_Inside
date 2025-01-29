/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.candidate;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ServiceProvider;


/**
 * Provide access to Candidate services.
 */
public class CandidateServiceProvider extends ServiceProvider<CandidateServiceProvider> {

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection information to use
	 */
	public CandidateServiceProvider(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Access to {@link IdentifyAllCandidates}.
	 * 
	 * @return the service instance
	 */
	public IdentifyAllCandidates identifyAllCandidates() {
		return new IdentifyAllCandidates(connectionInfo);
	}
}

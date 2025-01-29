/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.candidate;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ModuleBasedService;
import innowake.mining.client.service.RouteConfiguration;

/**
 * HTTP REST service for identifying all candidates for the given modules.
 */
public class IdentifyAllCandidates extends ModuleBasedService<IdentifyAllCandidates> {
	
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/identify-candidates";
	IdentifyAllCandidates(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	@Override
	public String getEndPoint() {
		return ENDPOINT;
	}
}

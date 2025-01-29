/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.controlflow;


import innowake.mining.client.service.ModuleBasedService;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.RouteConfiguration;

/**
 * HTTP REST service for calculating the control flow graph of one or multiple modules of a given project.
 */
public class CalculateControlFlowGraphs extends ModuleBasedService<CalculateControlFlowGraphs> {
	
	/**
	 * ENDPOINT. the server end point to get Control Flow Graph for multiple modules.
	 */
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/control-flow";

	/**
	 * Constructor
	 * @param connectionInfo the connection info to use
	 */
	public CalculateControlFlowGraphs(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	@Override
	public String getEndPoint() {
		return ENDPOINT;
	}

}

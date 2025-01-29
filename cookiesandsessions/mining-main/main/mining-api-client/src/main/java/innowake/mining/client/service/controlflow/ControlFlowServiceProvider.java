/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.controlflow;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ServiceProvider;

/**
 * Provide access to Control Flow Graph.
 */
public class ControlFlowServiceProvider extends ServiceProvider<ControlFlowServiceProvider> {

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	public ControlFlowServiceProvider(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Access to {@link GetControlFlowGraphForModule}.
	 *
	 * @return the service instance
	 */
	public GetControlFlowGraphForModule getControlFlowGraphForModule() {
		return new GetControlFlowGraphForModule(connectionInfo);
	}
	
	/**
	 * Access to {@link CalculateControlFlowGraphForModule}.
	 *
	 * @return the service instance
	 */
	public CalculateControlFlowGraphForModule calculateControlFlowGraphForModule() {
		return new CalculateControlFlowGraphForModule(connectionInfo);
	}
	
	/**
	 * Access to {@link CalculateControlFlowGraphs}.
	 *
	 * @return the service instance
	 */
	public CalculateControlFlowGraphs calculateControlFlowGraphs() {
		return new CalculateControlFlowGraphs(connectionInfo);
	}

}

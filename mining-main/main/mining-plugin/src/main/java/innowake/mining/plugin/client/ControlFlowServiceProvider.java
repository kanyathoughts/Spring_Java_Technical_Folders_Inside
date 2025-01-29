/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.client;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.controlflow.CalculateControlFlowGraphs;
import innowake.mining.plugin.preferences.ProjectData;

/**
 * Provides control flow services with project id already set. 
 */
public class ControlFlowServiceProvider extends innowake.mining.client.service.controlflow.ControlFlowServiceProvider {

	private final ProjectData projectData;

	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 * @param projectData the {@link ProjectData} to set the project ID into the provides services
	 */
	ControlFlowServiceProvider(final ConnectionInfo connectionInfo, final ProjectData projectData) {
		super(connectionInfo);
		this.projectData = projectData;
	}
	
	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public CalculateControlFlowGraphs calculateControlFlowGraphs() {
		return init(super.calculateControlFlowGraphs());
	}

	@SuppressWarnings("unchecked")
	private <T extends ProjectIdService<?, ?>> T init(final ProjectIdService<?, ?> service) {
		return (T) service.setProjectId(projectData.getProjectId());
	}
}

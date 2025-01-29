/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.client;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.candidate.IdentifyAllCandidates;
import innowake.mining.plugin.preferences.ProjectData;

/**
 * Provides Candidate services with the project ID already set.
 */
public class CandidateServiceProvider extends innowake.mining.client.service.candidate.CandidateServiceProvider {

	private ProjectData projectData;

	CandidateServiceProvider(final ConnectionInfo connectionInfo, final ProjectData projectData) {
		super(connectionInfo);
		this.projectData = projectData;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * The project ID is already set.
	 */
	@Override
	public IdentifyAllCandidates identifyAllCandidates() {
		return init(super.identifyAllCandidates());
	}

	@SuppressWarnings("unchecked")
	private <T extends ProjectIdService<?, ?>> T init(final ProjectIdService<?, ?> service) {
		return (T) service.setProjectId(projectData.getProjectId());
	}

}

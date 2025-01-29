/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.client;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.reference.CreateReference;
import innowake.mining.client.service.reference.DeleteReference;
import innowake.mining.client.service.reference.FindAllReferences;
import innowake.mining.client.service.reference.FindAllReferencesForModule;
import innowake.mining.client.service.reference.FindReferenceById;
import innowake.mining.plugin.preferences.ProjectData;

/**
 * Provides reference services with project id already set. 
 */
public class ReferenceServiceProvider extends innowake.mining.client.service.reference.ReferenceServiceProvider{

	@Nullable private final ProjectData projectData;

	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 * @param projectData the {@link ProjectData} to set the project ID into the provides services
	 */
	ReferenceServiceProvider(final ConnectionInfo connectionInfo, final ProjectData projectData) {
		super(connectionInfo);
		this.projectData = projectData;
	}
	
	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 * @param projectData the {@link ProjectData} to set the project ID into the provides services
	 */
	ReferenceServiceProvider(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
		projectData = null;
	}

	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public FindAllReferences findAllReferences() {
		return init(super.findAllReferences());
	}
	
	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public FindAllReferencesForModule findAllReferencesForModule() {
		return init(super.findAllReferencesForModule());
	}
	
	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public FindReferenceById findReferenceById() {
		return init(super.findReferenceById());
	}
	
	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public CreateReference createReference() {
		return init(super.createReference());
	}
	
	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public DeleteReference deleteReference() {
		return init(super.deleteReference());
	}

	@SuppressWarnings("unchecked")
	private <T extends ProjectIdService<?, ?>> T init(final ProjectIdService<?, ?> service) {
		if (projectData != null) {
			return (T) service.setProjectId(projectData.getProjectId());
		}
		return (T) service;
	}
}

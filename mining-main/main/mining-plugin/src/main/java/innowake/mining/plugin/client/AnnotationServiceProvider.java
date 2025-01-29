/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.client;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.annotation.CreateAnnotation;
import innowake.mining.client.service.annotation.DeleteAnnotation;
import innowake.mining.client.service.annotation.FindAllAnnotations;
import innowake.mining.client.service.annotation.FindAnnotationById;
import innowake.mining.client.service.annotation.FindModule;
import innowake.mining.client.service.annotation.SearchAnnotations;
import innowake.mining.client.service.annotation.UpdateAnnotation;
import innowake.mining.client.service.annotation.UpdateAnnotationState;
import innowake.mining.plugin.preferences.ProjectData;

/**
 * Provides annotation services with project id already set. 
 */
public class AnnotationServiceProvider extends innowake.mining.client.service.annotation.AnnotationServiceProvider {

	@Nullable private final ProjectData projectData;

	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 * @param projectData the {@link ProjectData} to set the project ID into the provides services
	 */
	AnnotationServiceProvider(final ConnectionInfo connectionInfo, final ProjectData projectData) {
		super(connectionInfo);
		this.projectData = projectData;
	}
	
	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 */
	AnnotationServiceProvider(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
		projectData = null;
	}

	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public SearchAnnotations searchAnnotations() {
		return init(super.searchAnnotations());
	}
	
	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public FindAllAnnotations findAllAnnotations() {
		return init(super.findAllAnnotations());
	}

	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public FindAnnotationById findAnnotationById() {
		return init(super.findAnnotationById());
	}

	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public FindModule findModule() {
		return init(super.findModule());
	}
	
	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public CreateAnnotation createAnnotation() {
		return init(super.createAnnotation());
	}

	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public UpdateAnnotation updateAnnotation() {
		return init(super.updateAnnotation());
	}
	
	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public UpdateAnnotationState updateAnnotationState() {
		return init(super.updateAnnotationState());
	}

	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public DeleteAnnotation deleteAnnotation() {
		return init(super.deleteAnnotation());
	}

	@SuppressWarnings("unchecked")
	private <T extends ProjectIdService<?, ?>> T init(final ProjectIdService<?, ?> service) {
		if (projectData != null) {
			return (T) service.setProjectId(projectData.getProjectId());
		}
		return (T) service;
	}
}

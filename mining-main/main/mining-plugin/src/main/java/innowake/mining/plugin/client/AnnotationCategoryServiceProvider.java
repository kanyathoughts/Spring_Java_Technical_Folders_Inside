/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.client;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.annotationcategory.CreateAnnotationCategory;
import innowake.mining.client.service.annotationcategory.FindAllAnnotationCategories;
import innowake.mining.client.service.annotationcategory.FindAnnotationCategoryById;
import innowake.mining.client.service.annotationcategory.UpdateAnnotationCategory;
import innowake.mining.plugin.preferences.ProjectData;

/**
 * Provides annotation category services with project id already set. 
 */
public class AnnotationCategoryServiceProvider extends innowake.mining.client.service.annotationcategory.AnnotationCategoryServiceProvider {

	private final ProjectData projectData;

	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 * @param projectData the {@link ProjectData} to set the project ID into the provides services
	 */
	AnnotationCategoryServiceProvider(final ConnectionInfo connectionInfo, final ProjectData projectData) {
		super(connectionInfo);
		this.projectData = projectData;
	}

	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public FindAllAnnotationCategories findAllAnnotationCategories() {
		return init(super.findAllAnnotationCategories());
	}

	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public FindAnnotationCategoryById findAnnotationCategoryById() {
		return init(super.findAnnotationCategoryById());
	}

	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public CreateAnnotationCategory createAnnotationCategory() {
		return init(super.createAnnotationCategory());
	}

	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public UpdateAnnotationCategory updateAnnotationCategory() {
		return init(super.updateAnnotationCategory());
	}
	
	@SuppressWarnings("unchecked")
	private <T extends ProjectIdService<?, ?>> T init(final ProjectIdService<?, ?> service) {
		return (T) service.setProjectId(projectData.getProjectId());
	}
}

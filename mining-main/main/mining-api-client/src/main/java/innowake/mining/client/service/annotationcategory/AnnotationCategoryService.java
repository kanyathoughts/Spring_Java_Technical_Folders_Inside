/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.annotationcategory;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.shared.model.AnnotationCategory;

/**
 * Base service for annotation categories endpoints.
 */
public abstract class AnnotationCategoryService extends ProjectIdService<AnnotationCategoryService, AnnotationCategory> {
	
	@Nullable
	protected AnnotationCategory annotationCategory;

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	protected AnnotationCategoryService(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Sets the annotation category.
	 *
	 * @param annotationCategory the annotation category
	 * @return {@code this}
	 */
	public AnnotationCategoryService setAnnotationCategory(final AnnotationCategory annotationCategory) {
		this.annotationCategory = annotationCategory;
		return this;
	}

	@Override
	protected void validate() {
		super.validate();
		if (annotationCategory == null) {
			throw new IllegalStateException("Annotation category must be set.");
		}
	}
}

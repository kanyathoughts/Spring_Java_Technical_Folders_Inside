/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.annotationcategory;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ServiceProvider;

/**
 * Provides access to annotation category services.
 */
public class AnnotationCategoryServiceProvider extends ServiceProvider<AnnotationCategoryServiceProvider> {

	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 */
	public AnnotationCategoryServiceProvider(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Access to {@link FindAllAnnotationCategories}.
	 *
	 * @return the service instance
	 */
	public FindAllAnnotationCategories findAllAnnotationCategories() {
		return new FindAllAnnotationCategories(connectionInfo);
	}
	
	/**
	 * Access to {@link FindAnnotationCategoryById}.
	 *
	 * @return the service instance
	 */
	public FindAnnotationCategoryById findAnnotationCategoryById() {
		return new FindAnnotationCategoryById(connectionInfo);
	}
	
	/**
	 * Access to {@link CreateAnnotationCategory}.
	 *
	 * @return the service instance
	 */
	public CreateAnnotationCategory createAnnotationCategory() {
		return new CreateAnnotationCategory(connectionInfo);
	}
	
	/**
	 * Access to {@link UpdateAnnotationCategory}.
	 *
	 * @return the service instance
	 */
	public UpdateAnnotationCategory updateAnnotationCategory() {
		return new UpdateAnnotationCategory(connectionInfo);
	}
}
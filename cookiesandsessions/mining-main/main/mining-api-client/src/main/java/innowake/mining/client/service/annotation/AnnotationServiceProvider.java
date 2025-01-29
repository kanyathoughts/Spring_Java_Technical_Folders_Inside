/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.annotation;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ServiceProvider;

/**
 * Provides access to annotation services.
 */
public class AnnotationServiceProvider extends ServiceProvider<AnnotationServiceProvider> {

	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 */
	public AnnotationServiceProvider(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Access to {@link FindAllAnnotations}.
	 *
	 * @return the service instance
	 */
	public FindAllAnnotations findAllAnnotations() {
		return new FindAllAnnotations(connectionInfo);
	}
	
	/**
	 * Access to {@link FindDistinctAnnotationTypes}.
	 *
	 * @return the service instance
	 */
	public FindDistinctAnnotationTypes findDistinctAnnotationTypes() {
		return new FindDistinctAnnotationTypes(connectionInfo);
	}
	
	/**
	 * Access to {@link SearchAnnotations}.
	 *
	 * @return the service instance
	 */
	public SearchAnnotations searchAnnotations() {
		return new SearchAnnotations(connectionInfo);
	}

	/**
	 * Access to {@link FindAnnotationById}.
	 *
	 * @return the service instance
	 */
	public FindAnnotationById findAnnotationById() {
		return new FindAnnotationById(connectionInfo);
	}
	
	/**
	 * Access to {@link FindAnnotationByName}.
	 * 
	 * @return the service instance
	 */
	public FindAnnotationByName findAnnotationByName() {
		return new FindAnnotationByName(connectionInfo);
	}
	
	/**
	 * Access to {@link FindModule}.
	 *
	 * @return the service instance
	 */
	public FindModule findModule() {
		return new FindModule(connectionInfo);
	}
	
	/**
	 * Access to {@link CreateAnnotation}.
	 *
	 * @return the service instance
	 */
	public CreateAnnotation createAnnotation() {
		return new CreateAnnotation(connectionInfo);
	}
	
	/**
	 * Access to {@link UpdateAnnotation}.
	 *
	 * @return the service instance
	 */
	public UpdateAnnotation updateAnnotation() {
		return new UpdateAnnotation(connectionInfo);
	}
	
	/**
	 * Access to {@link UpdateAnnotationState}.
	 *
	 * @return the service instance
	 */
	public UpdateAnnotationState updateAnnotationState() {
		return new UpdateAnnotationState(connectionInfo);
	}
	
	/**
	 * Access to {@link DeleteAnnotation}.
	 *
	 * @return the service instance
	 */
	public DeleteAnnotation deleteAnnotation() {
		return new DeleteAnnotation(connectionInfo);
	}
}
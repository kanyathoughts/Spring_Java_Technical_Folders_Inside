/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.annotation;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;

/**
 * Base service for annotation endpoints holding an annotation entity.
 * 
 * @param <S> the type of the actual service 
 */
public abstract class AnnotationService<S extends AnnotationService<S>> extends ProjectIdService<S, AnnotationPojo> {
	
	@Nullable
	protected AnnotationPojoPrototype annotation;

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	protected AnnotationService(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Sets the annotation.
	 *
	 * @param annotation the annotation
	 * @return {@code this}
	 */
	public S setAnnotation(final AnnotationPojoPrototype annotation) {
		this.annotation = annotation;
		return getThis();
	}

	@Override
	protected void validate() {
		super.validate();
		if (annotation == null) {
			throw new IllegalStateException("Annotation must be set.");
		}
	}
}

/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.annotation;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.shared.access.EntityId;

/**
 * Base service for annotation endpoints holding an annotation id.
 * 
 * @param <T> the type of the service result 
 * @param <S> the type of the actual service 
 */
public abstract class AnnotationIdService<S extends AnnotationIdService<S, T>, T> extends ProjectIdService<S, T> {
	
	@Nullable
	protected EntityId annotationId;

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	protected AnnotationIdService(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Sets the annotation id.
	 *
	 * @param annotationId the annotation id
	 * @return {@code this}
	 */
	@SuppressWarnings("unchecked")
	public S setAnnotationId(final EntityId annotationId) {
		this.annotationId = annotationId;
		return (S) this;
	}

	@Override
	protected void validate() {
		super.validate();
		if (annotationId == null)  {
			throw new IllegalStateException("Annotation id must be set.");
		}
	}
}

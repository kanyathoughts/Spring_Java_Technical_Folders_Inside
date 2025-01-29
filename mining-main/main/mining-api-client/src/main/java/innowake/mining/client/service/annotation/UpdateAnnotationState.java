/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.annotation;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;

import org.apache.http.client.methods.HttpPut;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.model.WorkingState;

/**
 * HTTP REST service for updating the {@link WorkingState} of an Annotation.
 */
public class UpdateAnnotationState extends ProjectIdService<UpdateAnnotationState, Void> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/annotation-search/%s";

	@Nullable
	private AnnotationPojoPrototype annotation;

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	UpdateAnnotationState(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Updates the state of an annotation by sending a HTTP PUT request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>204</strong>: on success
	 * <li><strong>400</strong>: if the given annotation is not valid
	 * <li><strong>404</strong>: if the given annotation does not exist
	 * <li><strong>404</strong>: if the given project does not exist
	 * 
	 * @return a result holding no content but only HTTP status information
	 */
	@Override
	public Result<Void> execute() throws IOException {
		validate();
		final Object annotationId = assertNotNull(annotation).uid.isPresent() ? assertNotNull(annotation).uid.getNonNull() : assertNotNull(annotation).nid.getNonNull();
		setServiceUrl(String.format(ENDPOINT, encode(projectId), annotationId));
		final HttpPut put = httpPut();
		put.setEntity(new StringEntity(PojoMapper.jsonWriter().writeValueAsString(annotation), ContentType.APPLICATION_JSON));
		return execute(put, new TypeReference<Void>() {});
	}

	/**
	 * Sets the annotation.
	 *
	 * @param annotation the annotation.
	 * @return {@code this}
	 */
	public UpdateAnnotationState setAnnotation(final AnnotationPojoPrototype annotation) {
		this.annotation = annotation;
		return getThis();
	}
	
	@Override
	protected void validate() {
		super.validate();
		if (annotation == null) {
			throw new IllegalStateException("Annotation must be set.");
		} else if ( ! assertNotNull(annotation).uid.isPresent() && ! assertNotNull(annotation).nid.isPresent()) {
			throw new IllegalStateException("Annotation numeric or unique id must be set.");
		}
	}
}

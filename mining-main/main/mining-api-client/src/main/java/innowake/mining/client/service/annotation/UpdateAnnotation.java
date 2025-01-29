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

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.entities.AnnotationPojo;

/**
 * HTTP REST service for update existing annotation.
 * <p>
 * The source attachment of an annotation is immutable and cannot be changed.
 */
public class UpdateAnnotation extends AnnotationService<UpdateAnnotation> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/annotations/%s";

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	UpdateAnnotation(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Updates an annotation by sending a HTTP PUT request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>400</strong>: if the given annotation is not valid
	 * <li><strong>404</strong>: if the given annotation does not exist
	 * <li><strong>404</strong>: if the given project does not exist
	 * 
	 * @return a result holding the updated Annotation if the call was successful
	 */
	@Override
	public Result<AnnotationPojo> execute() throws IOException {
		validate();
		final Object annotationId = assertNotNull(annotation).uid.isPresent() ? assertNotNull(annotation).uid.getNonNull() : assertNotNull(annotation).nid.getNonNull();
		setServiceUrl(String.format(ENDPOINT, encode(projectId), annotationId));
		final HttpPut put = httpPut();
		put.setEntity(new StringEntity(PojoMapper.jsonWriter().writeValueAsString(annotation), ContentType.APPLICATION_JSON));
		return execute(put, new TypeReference<AnnotationPojo>() {});
	}

	@Override
	protected void validate() {
		super.validate();

		if ( ! assertNotNull(annotation).uid.isPresent() && ! assertNotNull(annotation).nid.isPresent()) {
			throw new IllegalStateException("Annotation numeric or unique id must be set.");
		}
	}
}

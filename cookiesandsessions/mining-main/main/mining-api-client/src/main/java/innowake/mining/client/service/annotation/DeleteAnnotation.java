/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.annotation;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;

/**
 * HTTP REST service for delete annotation.
 */
public class DeleteAnnotation extends AnnotationIdService<DeleteAnnotation, Void> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/annotations/%s";

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	DeleteAnnotation(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Deletes an annotation by sending a HTTP DELETE request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status code:
	 * <li><strong>204</strong>: regardless if the annotation exists or not 
	 * <li><strong>404</strong>: if the given project does not exist
	 * 
	 * @return a result holding only the status code of the response
	 */
	@Override
	public Result<Void> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId), encode(annotationId)));
		return execute(httpDelete(), new TypeReference<Void>() {});
	}
}

/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.annotation;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.entities.AnnotationPojo;

/**
 * HTTP REST service for find single annotation.
 */
public class FindAnnotationById extends AnnotationIdService<FindAnnotationById, AnnotationPojo> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/annotations/%s";
	
	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	FindAnnotationById(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Finds an annotation by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given annotation id does not exist
	 * <li><strong>404</strong>: if the given project does not exist
	 * 
	 * @return a result holding the found Annotation if the call was successful
	 */
	@Override
	public Result<AnnotationPojo> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId), encode(annotationId)));
		return execute(httpGet(), new TypeReference<AnnotationPojo>() {});
	}
}

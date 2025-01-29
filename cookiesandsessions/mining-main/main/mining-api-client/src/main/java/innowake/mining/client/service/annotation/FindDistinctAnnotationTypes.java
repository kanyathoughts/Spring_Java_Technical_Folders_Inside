/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.annotation;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.model.AnnotationType;

/**
 * HTTP REST service for finding Distinct annotation types.
 */
public class FindDistinctAnnotationTypes extends ProjectIdService<FindDistinctAnnotationTypes, AnnotationType[]> {

	/**
	 * End point to fetch distinct annotation Types. 
	 */
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/annotation-types";
	
	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	FindDistinctAnnotationTypes(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Finds all distinct annotation types by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given project does not exist
	 * 
	 * @return a result holding all found {@linkplain AnnotationType annotationTypes} if the call was successful
	 */
	@Override
	public Result<AnnotationType[]> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId)));
		return execute(httpGet(), new TypeReference<AnnotationType[]>() {});
	}
}

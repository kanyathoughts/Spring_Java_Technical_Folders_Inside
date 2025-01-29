/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.annotationcategory;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.model.AnnotationCategory;

/**
 * HTTP REST service for find all annotation categories.
 */
public class FindAllAnnotationCategories extends ProjectIdService<FindAllAnnotationCategories, AnnotationCategory[]> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/annotation-categories";

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	FindAllAnnotationCategories(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Finds all annotation category by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given project does not exist
	 * 
	 * @return a result holding all found {@linkplain AnnotationCategory annotation categories} if the call was successful
	 */
	@Override
	public Result<AnnotationCategory[]> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId)));
		return execute(httpGet(), new TypeReference<AnnotationCategory[]>() {});
	}
}

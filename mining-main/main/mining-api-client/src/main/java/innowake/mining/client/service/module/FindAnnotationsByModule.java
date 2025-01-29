/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.module;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ModuleIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.entities.AnnotationPojo;

/**
 * HTTP REST service for find all annotations for a module end point.
 */
public class FindAnnotationsByModule extends ModuleIdService<FindAnnotationsByModule, AnnotationPojo[]> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/modules/%s/annotations";

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	FindAnnotationsByModule(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Finds all annotations of a given module by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given project or module does not exist
	 * 
	 * @return a result holding all Annotations on success
	 */
	@Override
	public Result<AnnotationPojo[]> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId), encode(moduleId)));
		return execute(httpGet(), new TypeReference<AnnotationPojo[]>() {});
	}
}

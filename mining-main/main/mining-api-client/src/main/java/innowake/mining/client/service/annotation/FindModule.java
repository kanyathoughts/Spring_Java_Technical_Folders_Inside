/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.annotation;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.entities.ModulePojo;

/**
 * HTTP REST service for finding the module associated with a given annotation.
 */
public class FindModule extends AnnotationIdService<FindModule, ModulePojo> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/annotations/%s/modules";
	
	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	FindModule(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Finds a module that has an annotation by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given annotation id does not exist
	 * <li><strong>404</strong>: if the given project does not exist
	 * 
	 * @return a result holding the found {@link ModulePojo} if the call was successful
	 */
	@Override
	public Result<ModulePojo> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId), encode(annotationId)));
		return execute(httpGet(), new TypeReference<ModulePojo>() {});
	}
}

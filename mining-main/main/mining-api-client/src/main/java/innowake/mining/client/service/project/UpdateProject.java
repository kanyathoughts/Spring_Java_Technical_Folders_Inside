/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.project;

import java.io.IOException;

import org.apache.http.client.methods.HttpPut;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.lang.Assert;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.entities.ProjectPojo;

/**
 * HTTP REST service for update existing project.
 */
public class UpdateProject extends ProjectService {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s";

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	UpdateProject(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Updates a project by sending a HTTP PUT request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>400</strong>: if the given project is not valid
	 * <li><strong>404</strong>: if the given project does not exist
	 * 
	 * @return a result holding the updated Project if the call was successful
	 */
	@Override
	public Result<ProjectPojo> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(Assert.assertNotNull(project).identityProvisional())));
		final HttpPut put = httpPut();
		put.setEntity(new StringEntity(PojoMapper.jsonWriter().writeValueAsString(project), ContentType.APPLICATION_JSON));
		return execute(put, new TypeReference<ProjectPojo>() {});
	}
}

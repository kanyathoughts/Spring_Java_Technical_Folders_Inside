/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.project;

import java.io.IOException;

import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.entities.ProjectPojo;

/**
 * HTTP REST service for create new project.
 */
public class CreateProject extends ProjectService {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects";

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	CreateProject(final ConnectionInfo connectionInfo) {
		super(connectionInfo, ENDPOINT);
	}

	/**
	 * Creates a new project by sending a HTTP POST request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>201</strong>: on success
	 * <li><strong>400</strong>: if the given project is not valid
	 * 
	 * @return a result holding the newly created Project if the call was successful
	 */
	@Override
	public Result<ProjectPojo> execute() throws IOException {
		validate();
		final HttpPost post = httpPost();
		post.setEntity(new StringEntity(PojoMapper.jsonWriter().writeValueAsString(project), ContentType.APPLICATION_JSON));
		return execute(post, new TypeReference<ProjectPojo>() {});
	}
}

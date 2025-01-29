/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.module;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.List;

import org.apache.http.client.utils.URIBuilder;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.model.LinkedModule;

/**
 * HTTP REST service for finding list of linked modules for the given module searched by path.
 */
public class FindAllLinkedModules extends ProjectIdService<FindAllLinkedModules, List<LinkedModule>> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/linkedModules/search";

	@Nullable
	private String path;
	
	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	FindAllLinkedModules(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Sets the path to use.
	 *
	 * @param path the path
	 * @return {@code this}
	 */
	public FindAllLinkedModules setPath(final String path) {
		this.path = path;
		return this;
	}

	/**
	 * Finds the linked modules of the current module given a path by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * The request includes a string parameter called {@code path}.
 	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given project does not exist or no module could be found
	 * 
	 * @return a result holding the list of {@link LinkedModule} if the call was successful
	 */
	@Override
	public Result<List<LinkedModule>> execute() throws IOException {
		validate();
		try {
			final URIBuilder uri = new URIBuilder(String.format(ENDPOINT, encode(projectId)));
			uri.addParameter("path", path);
			setServiceUrl(uri.toString());
		} catch (final URISyntaxException e) {
			throw new IllegalStateException("Error while building the URI for the endpoint : " + e, e);
		}
		return execute(httpGet(), new TypeReference<List<LinkedModule>>() {});
	}

	@Override
	protected void validate() {
		super.validate();
		if (path == null) {
			throw new IllegalStateException("Path must be set.");
		}
	}
}

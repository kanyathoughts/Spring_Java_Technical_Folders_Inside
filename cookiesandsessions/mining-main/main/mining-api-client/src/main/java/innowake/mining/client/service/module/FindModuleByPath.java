/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.module;

import java.io.IOException;
import java.net.URISyntaxException;

import org.apache.http.client.utils.URIBuilder;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.entities.ModulePojo;

/**
 * HTTP REST service for find single module by path end point.
 */
public class FindModuleByPath extends ProjectIdService<FindModuleByPath, ModulePojo> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/modules/search";

	@Nullable
	private String path;
	private boolean includeContent = false;
	
	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	FindModuleByPath(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Sets the path to use.
	 *
	 * @param path the path
	 * @return {@code this}
	 */
	public FindModuleByPath setPath(final String path) {
		this.path = path;
		return this;
	}

	/**
	 * Sets the value for the includeContent.
	 * 
	 * @param includeContent determines if content needs to be included.
	 * @return {@code this}
	 */
	public FindModuleByPath setIncludeContent(final boolean includeContent) {
		this.includeContent = includeContent;
		return this;
	}

	/**
	 * Finds a module given a path by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * The request includes a string parameter called {@code path}.
 	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given project does not exist or no module could be found
	 * 
	 * @return a result holding the found {@link ModulePojo} if the call was successful
	 */
	@Override
	public Result<ModulePojo> execute() throws IOException {
		validate();
		try {
			final URIBuilder uri = new URIBuilder(String.format(ENDPOINT, encode(projectId)));
			uri.addParameter("path", path);
			uri.addParameter("includeContent", String.valueOf(this.includeContent));
			setServiceUrl(uri.toString());
		} catch (final URISyntaxException e) {
			throw new IllegalStateException(e);
		}
		return execute(httpGet(), new TypeReference<ModulePojo>() {});
	}

	@Override
	protected void validate() {
		super.validate();
		if (path == null) {
			throw new IllegalStateException("Path must be set.");
		}
	}
}

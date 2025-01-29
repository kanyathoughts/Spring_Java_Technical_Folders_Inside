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
 * HTTP REST service for finding modules by name.
 */
public class FindModuleByName extends ProjectIdService<FindModuleByName, ModulePojo[]> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/modules/search";

	@Nullable
	private String name;
	
	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	FindModuleByName(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Sets the name to use.
	 *
	 * @param name the name
	 * @return {@code this}
	 */
	public FindModuleByName setName(final String name) {
		this.name = name;
		return this;
	}

	/**
	 * Finds module given a name by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * The request includes a string parameter called {@code name}.
 	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given project does not exist
	 * 
	 * @return a result holding the found {@linkplain ModulePojo modules} if the call was successful
	 */
	@Override
	public Result<ModulePojo[]> execute() throws IOException {
		validate();
		try {
			final URIBuilder uri = new URIBuilder(String.format(ENDPOINT, encode(projectId)));
			uri.addParameter("name", name);
			setServiceUrl(uri.toString());
		} catch (final URISyntaxException e) {
			throw new IllegalStateException(e);
		}
		return execute(httpGet(), new TypeReference<ModulePojo[]>() {});
	}

	@Override
	protected void validate() {
		super.validate();
		if (name == null) {
			throw new IllegalStateException("Name must be set.");
		}
	}
}

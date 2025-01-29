/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.module;

import java.io.IOException;
import java.net.URISyntaxException;

import org.apache.http.client.utils.URIBuilder;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ModuleIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.entities.ModulePojo;

/**
 * HTTP REST service for find single module.
 */
public class FindModuleById extends ModuleIdService<FindModuleById, ModulePojo> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/modules/%s";
	private boolean includeContent = false;
	
	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	FindModuleById(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Sets the value for the includeContent.
	 * 
	 * @param includeContent determines if content needs to be included.
	 * @return {@code this}
	 */
	public FindModuleById setIncludeContent(final boolean includeContent) {
		this.includeContent = includeContent;
		return this;			
	}

	/**
	 * Finds a module by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given module id does not exist
	 * <li><strong>404</strong>: if the given project does not exist
	 * 
	 * @return a result holding the found {@link ModulePojo} if the call was successful
	 */
	@Override
	public Result<ModulePojo> execute() throws IOException {
		validate();
		try {
			final URIBuilder uri = new URIBuilder(String.format(ENDPOINT, encode(projectId), encode(moduleId)));
			uri.addParameter("includeContent", String.valueOf(this.includeContent));
			setServiceUrl(uri.toString());
		} catch (final URISyntaxException e) {
			throw new IllegalStateException(e);
		}
		return execute(httpGet(), new TypeReference<ModulePojo>() {});
	}
}

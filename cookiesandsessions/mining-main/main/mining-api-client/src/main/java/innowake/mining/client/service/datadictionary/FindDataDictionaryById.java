/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.datadictionary;

import java.io.IOException;

import org.apache.http.client.utils.URIBuilder;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.FindByIdWithProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.DataDictionaryPojo;

/**
 * HTTP REST service for find single data dictionary entry by record id.
 */
public class FindDataDictionaryById extends FindByIdWithProjectIdService<FindDataDictionaryById, DataDictionaryPojo> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/modules/%s/data-dictionary/%s";
	
	@Nullable
	protected EntityId moduleId;
	
	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	FindDataDictionaryById(final ConnectionInfo connectionInfo) {
		super(connectionInfo, ENDPOINT, new TypeReference<DataDictionaryPojo>() {});
	}
	
	/**
	 * Finds by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given record id, or the given project id, or the given module id does not exist
	 * 
	 * @return a result holding the found {@link DataDictionaryPojo} if the call was successful
	 */
	@Override
	public Result<DataDictionaryPojo> execute() throws IOException {
		validate();
		final URIBuilder uri = new URIBuilder();
		uri.setPath(String.format(endpoint, encode(projectId), encode(moduleId), encode(id)));
		setServiceUrl(uri.toString());
		return execute(httpGet(), type);
	}

	/**
	 * Sets the module id to use.
	 *
	 * @param moduleId the module id
	 * @return {@code this}
	 */
	public FindDataDictionaryById setModuleId(final EntityId moduleId) {
		this.moduleId = moduleId;
		return getThis();
	}
	
	@Override
	protected void validate() {
		super.validate();
		if (moduleId == null) {
			throw new IllegalStateException("Module ID must be set.");
		}
	}
}

/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.datadictionary;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ModuleIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.entities.DataDictionaryPojo;

/**
 * HTTP REST service for getting all data dictionary entries of a given module.
 */
public class FindAllDataDictionaryEntries extends ModuleIdService<FindAllDataDictionaryEntries, DataDictionaryPojo[]> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/modules/%s/data-dictionary";

	FindAllDataDictionaryEntries(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Finds all data dictionary entries of a given module by sending a HTTP GET request to {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given project or module does not exist
	 * 
	 * @return a result holding all {@linkplain DataDictionaryPojo data dictionary entries} on success
	 */
	@Override
	public Result<DataDictionaryPojo[]> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId), encode(moduleId)));
		return execute(httpGet(), new TypeReference<DataDictionaryPojo[]>() {});
	}

}

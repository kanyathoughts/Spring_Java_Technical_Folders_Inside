/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.module;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Prototype;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ModuleIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;

/**
 * Service used to store the module description of a given module- and project-id.
 * No result, only a status code 200 is provided for success.
 * <br><b>Note:</b> The service can respond with a 501 if the feature is not enabled.
 */
@Prototype(irisId = "WMIN-597")
public class StoreModuleDescription extends ModuleIdService<StoreModuleDescription, Void> {

	/**
	 * Base endpoint url.
	 */
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/modules/%s/store-description";
	
	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	StoreModuleDescription(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	@Override
	public Result<Void> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId), encode(moduleId)));
		return execute(httpGet(), new TypeReference<Void>() {});
	}
}
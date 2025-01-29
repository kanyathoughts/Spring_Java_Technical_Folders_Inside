/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.module;

import innowake.mining.client.service.ModuleBasedService;
import innowake.lib.core.api.lang.Prototype;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.RouteConfiguration;

/**
 * HTTP REST service for identifying Module descriptions on the given modules
 */
@Prototype(irisId = "WMIN-597")
public class IdentifyModuleDescriptions extends ModuleBasedService<IdentifyModuleDescriptions> {

	/**
	 * The REST end point URL called by this service.
	 */
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/identify-module-descriptions";

	IdentifyModuleDescriptions(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	@Override
	public String getEndPoint() {
		return ENDPOINT;
	}
}
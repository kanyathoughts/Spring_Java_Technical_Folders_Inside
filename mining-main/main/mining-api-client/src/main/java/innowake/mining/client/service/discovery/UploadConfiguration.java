/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.discovery;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.client.service.io.UploadService;

/**
 * HTTP REST service to upload configurations for a project.
 */
public class UploadConfiguration extends UploadService<UploadConfiguration, Void> {
	
	/**
	 * The endpoint for importing discovery configurations and search orders as Zip stream.
	 */
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/discovery/config/";

	/**
	 * Constructor.
	 * 
	 * @param connectionInfo the {@link ConnectionInfo}
	 */
	protected UploadConfiguration(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	@Override
	protected String getEndpoint() {
		return ENDPOINT;
	}
}

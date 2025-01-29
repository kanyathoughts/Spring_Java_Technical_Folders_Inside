/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.io;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.RouteConfiguration;

/**
 * HTTP REST service for importing a Discovery Excel file as stream.
 */
public class ImportCSV extends UploadService<ImportCSV, Void> {

	/**
	 * The endpoint for importing a Discovery Excel file as stream.
	 */
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/csv/";
	
	/**
	 * Constructor.
	 * 
	 * @param connectionInfo the {@link ConnectionInfo}
	 */
	ImportCSV(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	@Override
	protected String getEndpoint() {
		return ENDPOINT;
	}
}

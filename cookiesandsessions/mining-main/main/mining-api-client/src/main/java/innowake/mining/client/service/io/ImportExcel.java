/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.io;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.RouteConfiguration;

/**
 * HTTP REST service for importing a Discovery Excel file as stream.
 */
public class ImportExcel extends UploadService<ImportExcel, Void> {

	/**
	 * The endpoint for importing a Discovery Excel file as stream.
	 */
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/excel/";
	
	/**
	 * Constructor.
	 * 
	 * @param connectionInfo the {@link ConnectionInfo}
	 */
	ImportExcel(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	@Override
	protected String getEndpoint() {
		return ENDPOINT;
	}
}

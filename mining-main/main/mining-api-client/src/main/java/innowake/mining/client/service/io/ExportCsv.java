/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.io;

import java.io.IOException;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;

/**
 * HTTP REST service for exporting a Discovery CSV file.
 */
public class ExportCsv extends AbstractExportService<ExportCsv> {

	/**
	 * The end point for exporting a Discovery CSV file.
	 */
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/csv/";
	
	
	/**
	 * Constructor.
	 *
	 * @param connectionInfo the {@link ConnectionInfo}
	 */
	ExportCsv(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	

	@Override
	public Result<Tuple2<String, byte[]>> execute() throws IOException {
		setServiceUrl(String.format(ENDPOINT, encode(projectId)));
		return super.execute();
	}
}

/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.io;

import java.io.IOException;
import java.util.List;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.io.ExportFormatDescription;

/**
 * HTTP REST service for retrieving the available export formats.
 */
public class GetExportFormats extends ProjectIdService<GetExportFormats, List<ExportFormatDescription>> {

	/**
	 * The end-point for exporting a to Format.
	 */
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/export-formats";	

	/**
	 * Constructor.
	 *
	 * @param connectionInfo the {@link ConnectionInfo}
	 */
	GetExportFormats(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	@Override
	public Result<List<ExportFormatDescription>> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId)));
		return execute(httpGet(), new TypeReference<List<ExportFormatDescription>>() {});
	}	
}

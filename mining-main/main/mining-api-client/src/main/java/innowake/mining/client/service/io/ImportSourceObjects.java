/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.io;

import java.io.IOException;
import com.fasterxml.jackson.core.type.TypeReference;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;

/**
 * HTTP REST service for importing source code as Zip stream.
 */
public class ImportSourceObjects extends UploadService<ImportSourceObjects, String> {

	/**
	 * The endpoint for importing source code as Zip stream.
	 */
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/source-objects/";

	/**
	 * Constructor.
	 * 
	 * @param connectionInfo the {@link ConnectionInfo}
	 */
	ImportSourceObjects(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	@Override
	protected String getEndpoint() {
		return ENDPOINT;
	}
	
	@Override
	public Result<String> execute() throws IOException {
		return execute(httpPost(), new TypeReference<String>() {});
	}
}

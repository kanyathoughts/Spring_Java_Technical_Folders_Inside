/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service;

import java.io.IOException;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.Logging;
import innowake.mining.client.service.info.InfoServiceProvider;

/**
 * Command line runner to test info services.
 */
public class InfoExample {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.MINING_CLIENT_SERVICE);
	
	public static void main(final String[] args) throws IOException {
		final ConnectionInfo connectionInfo = new ConnectionInfo("http://localhost:8080", "");
		
		LOG.info("Call info");
		LOG.info("Status message: " + new InfoServiceProvider(connectionInfo).info().execute().getStatusMessage());
	}
}

/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service;

import static innowake.mining.shared.model.FeatureId.INCREMENTAL_SCAN;

import java.io.IOException;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.Logging;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.feature.FeatureServiceProvider;
import innowake.mining.client.service.feature.FindFeatureById;
import innowake.mining.shared.model.Feature;

/**
 * Command line runner to test {@link FindFeatureById}.
 */
public class FeatureExample {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.MINING_CLIENT_SERVICE);
	private static final String MARK = " >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>";
	private static final String ACCESS_TOKEN = "5COgpjv7HdUiRQrj0LwEsyiJnhE";
	
	public static void main(final String[] args) throws IOException {
		final ConnectionInfo connectionInfo = new ConnectionInfo("http://localhost:8080", ACCESS_TOKEN);
		final FeatureServiceProvider service = MiningApiClient.featureService(connectionInfo);

		LOG.info("find feature '" + INCREMENTAL_SCAN + "' by ID" + MARK);
		final Result<Feature> result = service.findFeatureById().setFeatureId(INCREMENTAL_SCAN).execute();
		LOG.info("Status message: " + result.getStatusMessage());
		final Feature feature = result.getValue().get();
		LOG.info("Feature " + feature.getId() + " is " + (feature.isEnabled() ? "enabled" : "disabled"));
		
		LOG.info("toggle feature '" + INCREMENTAL_SCAN + "' to disabled" + MARK);
		final Result<Void> result2 = service.toggleFeature().setFeatureId(INCREMENTAL_SCAN).setState(Boolean.FALSE).execute();
		LOG.info("Status message: " + result2.getStatusMessage());
		
		LOG.info("find feature '" + INCREMENTAL_SCAN + "' by ID" + MARK);
		final Result<Feature> result3 = service.findFeatureById().setFeatureId(INCREMENTAL_SCAN).execute();
		LOG.info("Status message: " + result3.getStatusMessage());
		final Feature feature3 = result3.getValue().get();
		LOG.info("Feature " + feature3.getId() + " is " + (feature3.isEnabled() ? "enabled" : "disabled"));
	}
}

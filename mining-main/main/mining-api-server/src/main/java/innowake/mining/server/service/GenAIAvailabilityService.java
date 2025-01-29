/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.service;

import java.net.HttpURLConnection;
import java.net.URL;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.properties.GenericConfigProperties;

/**
 * Can be used to check whether the service for Generative AI is reachable or not.
 */
@Service
public class GenAIAvailabilityService {
	
	private static final Logger LOG = LoggerFactory.getLogger(GenAIAvailabilityService.class);

	@Autowired
	private transient GenericConfigProperties configProperties;
	
	/**
	 * Checks whether service for Generative AI is reachable or not.
	 *
	 * @return {@code true} if service for Generative AI is available - {@code false} if not
	 */
	public boolean isGenAIServiceAvailable() {
		final String hostURL = configProperties.getGenAiURL();
		try {
			final var url = new URL(hostURL);
			final HttpURLConnection connection = (HttpURLConnection) url.openConnection();
			connection.setRequestMethod("GET");
			connection.connect();
			final int responseCode = connection.getResponseCode();
			connection.disconnect();
			return responseCode == 200;
		} catch (final Exception e) {
			LOG.warn("Could not connect to GenAI service at {}", hostURL);
			LOG.debug(e::getMessage, e);
			return false;
		}
	}

}

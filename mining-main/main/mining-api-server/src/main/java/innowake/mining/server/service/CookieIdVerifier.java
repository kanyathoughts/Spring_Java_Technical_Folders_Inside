
/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.service;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.info.BuildProperties;
import org.springframework.stereotype.Service;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;

/**
 * Service to Verify cookieId is configured or not
 */
@Service
public class CookieIdVerifier {

	private static final Logger LOG = LoggerFactory.getLogger(CookieIdVerifier.class);

	private static final String COOKIE_DISABLED_KEYWORD = "DISABLED";

	@Autowired
	private BuildProperties buildProperties;

	@Nullable
	@Value("${mining.cookieId}")
	private String cookieId;

	/**
	 * Verify whether correct CookieID is set for distributed artifact
	 */
	public void verifyCookieId() {
		verifyArtifactCookieId(buildProperties.getArtifact());
	}
	
	/**
	 * Verify distributed artifact and correct cookieId is provided
	 * 
	 * @param artifact the artifact name
	 */
	public void verifyArtifactCookieId(final String artifact) {
		LOG.info(String.format("%nartifact: %s%ncookieId: %s", artifact, StringUtils.isBlank(cookieId) ? "<blank>" : cookieId));
		if (StringUtils.isNotEmpty(artifact) && artifact.contains("dist")) {
			if (StringUtils.isBlank(cookieId)) {
				LOG.error("Cookie ID not set. Aborting server start!");
				throw new IllegalArgumentException(
					"Running in distribution mode requires the Cookie ID to be set. Please provide it via the property 'mining.cookieId'.");
			} else {
				LOG.info("Cookie ID set for distribution mode: " + cookieId);
			}
		} else {
			LOG.info("Skipping Cookie ID check as server is not running in distribution mode");
		}
	}

	/**
	 * Verify whether CookieID verification is enabled
	 * @return boolean CookieID verification is enable or not
	 */
	public boolean cookieIdVerificationIsEnabled() {
		return ! COOKIE_DISABLED_KEYWORD.equals(cookieId);
	}

}

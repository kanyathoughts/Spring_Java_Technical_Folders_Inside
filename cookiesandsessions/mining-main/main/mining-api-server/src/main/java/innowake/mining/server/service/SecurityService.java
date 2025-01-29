/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import java.util.Arrays;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

import innowake.mining.server.config.Profiles;

/**
 * Security related service methods.
 */
@Service
public class SecurityService {
	
	@Autowired
	private Environment environment;
	
	/**
	 * @return {@code true} if the server is running in an authorized environment, {@code false} otherwise
	 */
	public boolean isAuthorized() {
		final Optional<String> profilesOpt = Arrays.stream(environment.getActiveProfiles())
				.filter(profile -> profile.equals(Profiles.IAM))
				.findFirst();
		return profilesOpt.isPresent();
	}

}

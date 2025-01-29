/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.SecurityFilterChain;

/**
 * Security configuration for functional integration tests, disabling any authentication/authorization.
 * <p>
 * There will still be an {@link Authentication} available, but the principal contained will be an anonymous user.
 * <p>
 * This will only be active when the {@value Profiles#NO_AUTH} profile is active.
 */
@Configuration
@EnableWebSecurity
@Profile(Profiles.NO_AUTH)
public class NoAuthSecurityConfig {

	@Bean
	protected SecurityFilterChain securityFilterChain(final HttpSecurity httpSecurity) throws Exception {
		httpSecurity
				.csrf().disable()
				.authorizeRequests().anyRequest().permitAll();
		return httpSecurity.build();
	}
	
}

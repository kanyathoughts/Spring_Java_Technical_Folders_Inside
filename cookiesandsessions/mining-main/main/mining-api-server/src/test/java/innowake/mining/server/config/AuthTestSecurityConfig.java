/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.web.SecurityFilterChain;

/**
 * Basic security configuration for tests using mocked authentication.
 */
@Configuration
@EnableWebSecurity
@Profile(Profiles.AUTH_TEST)
public class AuthTestSecurityConfig {

	@Bean
	protected SecurityFilterChain configure(final HttpSecurity httpSecurity) throws Exception {
		httpSecurity
				.csrf().disable()
				.authorizeRequests(rq -> GeneralSecurityConfig.configureAuthorizedUrls(rq, false)
						.anyRequest().authenticated());
		return httpSecurity.build();
	}

}

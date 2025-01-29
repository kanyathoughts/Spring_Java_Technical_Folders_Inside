/* Copyright (c) 2019 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.http.HttpStatus;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.HttpStatusEntryPoint;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
/**
 * Web security configuration for legacy Authentication. 
 */
@Configuration
@EnableWebSecurity
@Profile(Profiles.LEGACY_AUTH)
public class LegacySecurityConfig {

	@Bean
	public SecurityFilterChain securityFilterChain(final JwtTokenFilter jwtTokenFilter, final HttpSecurity http) throws Exception{
		http.csrf().disable()
				.exceptionHandling(exceptionHandling -> exceptionHandling.authenticationEntryPoint(new HttpStatusEntryPoint(HttpStatus.UNAUTHORIZED)))
				.authorizeRequests(rq -> GeneralSecurityConfig.configureAuthorizedUrls(rq, false).anyRequest().authenticated());
		http.addFilterBefore(jwtTokenFilter, UsernamePasswordAuthenticationFilter.class);
		return http.build();
	}
}

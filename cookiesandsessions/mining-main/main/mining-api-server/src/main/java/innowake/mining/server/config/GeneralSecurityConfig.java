/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.config;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.Ordered;
import org.springframework.security.config.annotation.web.configurers.ExpressionUrlAuthorizationConfigurer;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;
import org.springframework.web.filter.CorsFilter;

import innowake.mining.shared.security.RoleType;

/**
 * Web security related configuration common to all authentication schemes.
 */
@Configuration
public class GeneralSecurityConfig {

	@Value("${cors.allowed-origins}")
	private List<String> allowedOrigins = new ArrayList<>();
	
	public static ExpressionUrlAuthorizationConfigurer<?>.ExpressionInterceptUrlRegistry configureAuthorizedUrls(
			final ExpressionUrlAuthorizationConfigurer<?>.ExpressionInterceptUrlRegistry rq, boolean withRoles) {
		rq
			/* Mining Web UI */
			.antMatchers("/", "/error", "/download/**", "/assets/**", "/**/**.js", "/**/**.css").permitAll()
			/* Fonts */
			.antMatchers("/**/**.ttf", "/**/**.woff2", "/**/**.woff").permitAll()
			/* Images */
			.antMatchers("/**/**.svg", "/**/**.png", "/**/**.jpg", "/**/**.ico").permitAll()
			/* Licenses */
			.antMatchers("/innoWake-Third-Party-Licenses.html", "/innoWake-Third-Party-Licenses-Frontend.html", "/licenses/**").permitAll()
			/* Swagger UI */
			.antMatchers("/swagger-ui/index.html", "/swagger-ui.html", "/swagger-resources/**", "/v3/api-docs/**", "/webjars/springdoc-openapi-ui/**").permitAll()
			/* FF4J Feature Console */
			.antMatchers("/feature-console/**").hasAuthority(RoleType.ADMIN.getValue())
			/* Keycloak configuration files are public, otherwise the clients can not authenticate */
			.antMatchers("/keycloak-eclipse.json", "/keycloak-web.json").permitAll()
			/* Version retrieval */
			.antMatchers("/api/v1/version").permitAll()
			/* Label mappings retrieval */
			.antMatchers("/api/v1/label-mappings").permitAll()
			/* CFG supported types retrieval */
			.antMatchers("/api/v1/control-flow-support").permitAll()
			/* Licence Expiry check */
			.antMatchers("/api/license-expiry-info").permitAll()
			/* Login legacy */
			.antMatchers("/api/v1/legacy-auth/login").permitAll()
			/* Login error */
			.antMatchers("/login-error").permitAll();
		if (withRoles) {
			rq
				.antMatchers("/feature-console/**").hasAuthority(RoleType.ADMIN.getValue())
				.antMatchers("/actuator/**").hasAuthority(RoleType.ADMIN.getValue());
		} else {
			rq
				.antMatchers("/feature-console/**").permitAll();
		}
		
		return rq;
	}
	
	@Bean
	public FilterRegistrationBean<CorsFilter> corsFilter() {
		final UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
		final CorsConfiguration config = new CorsConfiguration();
		config.setAllowCredentials(Boolean.TRUE);
		config.addAllowedHeader("*");
		config.addAllowedMethod("*");
		config.setAllowedOrigins(allowedOrigins);
		source.registerCorsConfiguration("/**", config);
		final FilterRegistrationBean<CorsFilter> bean = new FilterRegistrationBean<>(new CorsFilter(source));
		bean.setOrder(Ordered.HIGHEST_PRECEDENCE);
		return bean;
	}
	
}

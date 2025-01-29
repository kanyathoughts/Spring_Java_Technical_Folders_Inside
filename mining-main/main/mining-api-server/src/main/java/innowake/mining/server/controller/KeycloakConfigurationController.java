/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import java.util.HashMap;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.web.bind.annotation.GetMapping;

import innowake.mining.server.config.Profiles;
import innowake.mining.server.properties.KeycloakApplicationConfiguration;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * Unauthenticated HTTP controller for providing the Keycloak configuration files.
 * <p>
 * As this only sends out the configuration files for Keycloak, this controller intentionally
 * does not extend the {@link BaseController} as the request validation is unnecessary.
 */
@MiningUnsecuredRestController
@Profile(Profiles.IAM)
public class KeycloakConfigurationController {

	@Autowired
	private KeycloakApplicationConfiguration keycloakApplicationConfiguration;
	
	/**
	 * @return the Keycloak configuration for the Eclipse client
	 */
	@GetMapping("/keycloak-eclipse.json")
	@MiningUnsecuredRestEndpoint
	@Operation(
			summary = "Retrieve the Keycloak configuration file for the Eclipse client in order to connect to the corresponding Keycloak instance.",
			operationId = "getKeycloakEclipseConfiguration",
			description = "The returned value contains all relevant information for the Eclipse client to connect to the corresponding Keycloak instance."
			)
	public Map<String, Object> getEclipseConfig() {
		final Map<String, Object> result = getDefaultValues();
		result.put("resource", "eclipse");
		return result;
	}
	
	/**
	 * @return the Keycloak configuration for the web client
	 */
	@GetMapping("/keycloak-web.json")
	@MiningUnsecuredRestEndpoint
	@Operation(
			summary = "Retrieve the Keycloak configuration file for the web client in order to connect to the corresponding Keycloak instance.",
			operationId = "getKeycloakWebConfiguration",
			description = "The returned value contains all relevant information for the web client to connect to the corresponding Keycloak instance."
			)
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	public Map<String, Object> getWebConfig() {
		final Map<String, Object> result = getDefaultValues();
		result.put("resource", "web");
		return result;
	}
	
	private Map<String, Object> getDefaultValues() {
		final Map<String, Object> result = new HashMap<>();
		/* The configuration options are documented at https://www.keycloak.org/docs/latest/securing_apps/index.html#_java_adapter_config */
		result.put("auth-server-url", keycloakApplicationConfiguration.getAuthServerUrl());
		result.put("realm", keycloakApplicationConfiguration.getRealm());
		result.put("ssl-required", keycloakApplicationConfiguration.getSslRequired());
		result.put("client-id", keycloakApplicationConfiguration.getClientId());
		result.put("public-client", Boolean.TRUE);
		/* Proof Key for Code Exchange by OAuth Public Clients */
		result.put("enable-pkce", Boolean.TRUE);
		result.put("principal-attribute", "preferred_username");
		return result;
	}
}

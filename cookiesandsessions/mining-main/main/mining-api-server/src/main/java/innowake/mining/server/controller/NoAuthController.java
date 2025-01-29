/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.config.security.MiningRole;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import org.springframework.context.annotation.Profile;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import java.time.Instant;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

@MiningUnsecuredRestController
@Profile(Profiles.NO_AUTH)
public class NoAuthController {
	
	private static final Logger LOG = LoggerFactory.getLogger(NoAuthController.class);
	
	@Operation(summary = "Get information about the current session, start a new one if not authenticated", operationId = "login")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@GetMapping("/api/v1/auth/login")
	@MiningUnsecuredRestEndpoint
	public ResponseEntity<Map<String, Object>> getLogin(final HttpServletRequest request) {
		final LinkedHashMap<String, Object> result = new LinkedHashMap<>();
		result.put("now", Instant.now().toString());
		
		result.put("subject", "NO_AUTH");
		
		final HttpSession session = request.getSession(false);
		if (session != null) {
			result.put("session_id", session.getId());
			result.put("session_created", Instant.ofEpochMilli(session.getCreationTime()).toString());
			result.put("session_accessed", Instant.ofEpochMilli(session.getLastAccessedTime()).toString());
			final int timeout = session.getMaxInactiveInterval();
			if (timeout > 0) {
				result.put("session_expires", Instant.ofEpochMilli(session.getLastAccessedTime() + timeout * 1000).toString());
			}
		}
		
		result.put("user_logon_name", "admin");
		result.put("user_full_name", "NoAuth Admin");
		result.put("user_given_name", "Admin");
		result.put("user_family_name", "NoAuth");
		result.put("user_email", "admin@no.auth");

		result.put("mining_roles", Collections.singletonList(new MiningRole("admin")));
		
		return ResponseEntity.ok(result);
	}
	
	@Operation(summary = "Invalidate the current session", operationId = "logout")
	@GetMapping("/api/v1/auth/logout")
	@ApiResponse(responseCode = "204", description = "if logout was successfully")
	@MiningUnsecuredRestEndpoint
	public ResponseEntity<Void> doLogout(final HttpServletRequest request) {
		final HttpSession session = request.getSession(false);
		if (session != null) {
			LOG.debug(() -> session.getId() + " session terminated");
			session.invalidate();
		}
		return ResponseEntity.noContent().build();
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
	public String getWebConfig() {
		return "{}";
	}
	
}

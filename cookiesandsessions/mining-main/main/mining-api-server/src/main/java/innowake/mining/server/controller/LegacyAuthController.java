/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.PostMapping;

import innowake.mining.server.config.Profiles;
import innowake.mining.shared.model.LegacyToken;
import innowake.mining.shared.model.LoginRequest;
import innowake.mining.shared.model.User;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.parameters.RequestBody;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * Security Controller for legacy Auth.
 */
@Profile(Profiles.LEGACY_AUTH)
@MiningUnsecuredRestController
public class LegacyAuthController {
	
	@Autowired
	private User user;
	@Autowired
	private AuthenticationManager authenticationManager;

	@Operation(summary = "Check user credential and return token", operationId = "login")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@PostMapping(value = "/api/v1/legacy-auth/login", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
	@MiningUnsecuredRestEndpoint
	public ResponseEntity<LegacyToken> getLogin(@RequestBody @org.springframework.web.bind.annotation.RequestBody final LoginRequest loginRequest) {
		try {
			final UsernamePasswordAuthenticationToken authenticationToken = new UsernamePasswordAuthenticationToken(loginRequest.getUsername(),
					loginRequest.getPassword());
			final Authentication authentication = authenticationManager.authenticate(authenticationToken);
			SecurityContextHolder.getContext().setAuthentication(authentication);
			final LegacyToken legacyToken = new LegacyToken(user.getToken(), loginRequest.getUsername(), "bearer");
			return ResponseEntity.ok(legacyToken);
		} catch (final BadCredentialsException badCredentialsException) {
			return ResponseEntity.badRequest().build();
		}
	}
}
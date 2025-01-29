/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import java.io.IOException;
import java.time.Instant;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import javax.annotation.security.RolesAllowed;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.apache.commons.codec.binary.Base64;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Profile;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.oauth2.client.registration.ClientRegistration;
import org.springframework.security.oauth2.client.registration.ClientRegistrationRepository;
import org.springframework.security.oauth2.core.OAuth2Token;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.security.oauth2.jwt.NimbusJwtDecoder;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.support.ServletUriComponentsBuilder;
import org.springframework.web.servlet.view.RedirectView;
import org.springframework.web.util.UriComponentsBuilder;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.config.security.MiningVoter;
import innowake.mining.server.config.security.TokenService;
import innowake.mining.shared.security.OfflineTokenInfo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * Service endpoints for session management and user information.
 */
@MiningUnsecuredRestController
@Profile(Profiles.AUTH)
public class AuthController {
	
	private static final Logger LOG = LoggerFactory.getLogger(AuthController.class);
	
	@Autowired
	private ClientRegistrationRepository clientRegistrationRepository;
	
	@Autowired
	private TokenService tokenService;
	
	@Value("${keycloak.registration-id}")
	private String authClientReg;
	
	@Value("${keycloak.user-name-attribute}")
	private String authUserAttr;
	
	@Value("${ui.base-url:#{null}}")
	private String uiBaseUrl;
	
	@Operation(summary = "Initiate a new OAuth offline token request", operationId = "initOffline")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@PostMapping("/api/v1/auth/offline")
	@RolesAllowed("offline_access")
	public ResponseEntity<String> initOfflineToken(final HttpServletRequest request,
			@RequestBody final Map<String, String> attr) throws JsonProcessingException {
		final ClientRegistration auth = clientRegistrationRepository.findByRegistrationId(authClientReg);
		
		final String redirectUri = ServletUriComponentsBuilder.fromRequestUri(request).build().toUriString();
		final Map<String, String> state = new HashMap<>(attr);
		state.put("id", UUID.randomUUID().toString());
		state.put("redirect_uri", redirectUri);
		
		final MultiValueMap<String, String> query = new LinkedMultiValueMap<>();
		query.add("response_type", "code");
		query.add("client_id", auth.getClientId());
		query.add("scope", "offline_access openid profile");
		query.add("prompt", "consent");
		query.add("state", Base64.encodeBase64URLSafeString(new ObjectMapper().writeValueAsBytes(state)));
		query.add("redirect_uri", redirectUri);
		
		return ResponseEntity.ok().contentType(MediaType.TEXT_PLAIN)
				.body(UriComponentsBuilder.fromHttpUrl(auth.getProviderDetails().getAuthorizationUri()).queryParams(query).build().toString());
	}
	
	@Operation(summary = "Perform the redirect cycle for obtaining an OAuth offline refresh token", operationId = "redirectOffline")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@GetMapping("/api/v1/auth/offline")
	@RolesAllowed("offline_access")
	public RedirectView procOfflineToken(final HttpServletRequest request,
			final String state, final String code) throws IOException {
		@SuppressWarnings("unchecked")
		Map<String, String> attr = new ObjectMapper().readValue(Base64.decodeBase64(state), Map.class);
		
		final UUID id = UUID.fromString(attr.get("id"));
		final ClientRegistration auth = clientRegistrationRepository.findByRegistrationId(authClientReg);
		
		final Map<String, String> tokenData = tokenService.requestToken(
				auth.getProviderDetails().getTokenUri(), auth.getClientId(), code, attr.get("redirect_uri"));
		final Jwt jwtId = NimbusJwtDecoder.withJwkSetUri(auth.getProviderDetails().getJwkSetUri()).build().decode(tokenData.get("id_token"));
		final OfflineTokenInfo token = new OfflineTokenInfo(id, jwtId.getSubject(), jwtId.getClaim(authUserAttr),
				attr.get("description"), tokenService.createToken(), tokenData.get("refresh_token"));
		tokenService.storeToken(token);
		
		final String returnUri = attr.get("return_uri");
		return new RedirectView(returnUri == null ? ServletUriComponentsBuilder.fromRequestUri(request)
				.pathSegment(state).build().toUriString() : returnUri + id.toString());
	}
	
	@Operation(summary = "Retrieve all tokens stored for the current user", operationId = "getTokens")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@GetMapping("/api/v1/auth/token")
	@RolesAllowed("offline_access")
	public ResponseEntity<List<OfflineTokenInfo>> getTokensBySubject() {
		final Authentication auth = SecurityContextHolder.getContext().getAuthentication();
		return ResponseEntity.ok(tokenService.findTokensBySubject(auth, auth.getName()));
	}
	
	@Operation(summary = "Retrieve a certain stored token by its ID", operationId = "getToken")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@GetMapping("/api/v1/auth/token/{id}")
	@RolesAllowed("offline_access")
	public ResponseEntity<OfflineTokenInfo> getTokenById(@PathVariable final UUID id) {
		return ResponseEntity.of(tokenService.findTokenById(SecurityContextHolder.getContext().getAuthentication(), id));
	}
	
	@Operation(summary = "Test if an access token can be obtained using a stored refresh token", operationId = "probeToken")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@GetMapping("/api/v1/auth/token/{id}/probe")
	@RolesAllowed("offline_access")
	public ResponseEntity<Void> probeTokenById(@PathVariable final UUID id) {
		Optional<OfflineTokenInfo> tokenInfo = tokenService.findTokenById(SecurityContextHolder.getContext().getAuthentication(), id);
		if (tokenInfo.isPresent()) {
			Optional<String> refreshToken = tokenService.findRefreshTokenByBearerToken(tokenInfo.get().getBearerToken());
			if (refreshToken.isPresent()) {
				final ClientRegistration auth = clientRegistrationRepository.findByRegistrationId(authClientReg);
				final Map<String, String> tokenData = tokenService.refreshToken(
						auth.getProviderDetails().getTokenUri(), auth.getClientId(), refreshToken.get());
				assert(tokenData.containsKey("access_token") && tokenData.containsKey("id_token") && tokenData.containsKey("refresh_token"));
				return ResponseEntity.noContent().build();
			}
		}
		return ResponseEntity.notFound().build();
	}
	
	@Operation(summary = "Delete a certain stored token of the current user by its ID", operationId = "deleteToken")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@DeleteMapping("/api/v1/auth/token/{id}")
	@RolesAllowed("offline_access")
	public ResponseEntity<Void> deleteToken(final @PathVariable UUID id) {
		return ResponseEntity.status(
				tokenService.deleteToken(id, SecurityContextHolder.getContext().getAuthentication().getName())
				? HttpStatus.OK : HttpStatus.NOT_FOUND).build();
	}
	
	@Operation(summary = "Update a certain stored token of the current user", operationId = "updateToken")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the token is unknown")
	@PutMapping("/api/v1/auth/token")
	@RolesAllowed("offline_access")
	public ResponseEntity<Void> updateToken(@RequestBody final OfflineTokenInfo tokenInfo) {
		return ResponseEntity.status(
				tokenService.updateToken(SecurityContextHolder.getContext().getAuthentication().getName(), tokenInfo)
				? HttpStatus.OK : HttpStatus.NOT_FOUND).build();
	}
	
	@Operation(summary = "Get information about the current session, start a new one if not authenticated", operationId = "login")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@GetMapping("/api/v1/auth/login")
	@MiningUnsecuredRestEndpoint
	public ResponseEntity<Map<String, Object>> getLogin(final HttpServletRequest request) {
		final LinkedHashMap<String, Object> result = new LinkedHashMap<>();
		result.put("now", Instant.now().toString());
		
		final Authentication auth = SecurityContextHolder.getContext().getAuthentication();
		result.put("subject", auth.getName());
		
		final Optional<OAuth2Token> accessToken = tokenService.getAccessToken(auth);
		if (accessToken.isPresent()) {
			result.put("access_token_issued", String.valueOf(accessToken.get().getIssuedAt()));
			result.put("access_token_expires", String.valueOf(accessToken.get().getExpiresAt()));
		}
		
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
		
		final Map<String, Object> claims = tokenService.getUserClaims(auth);
		if (! claims.isEmpty()) {
			result.put("user_logon_name", claims.get("preferred_username"));
			result.put("user_full_name", claims.get("name"));
			result.put("user_given_name", claims.get("given_name"));
			result.put("user_family_name", claims.get("family_name"));
			result.put("user_email", claims.get("email"));
		}
		
		result.put("mining_roles", MiningVoter.getMiningRoles(auth.getAuthorities()));
		
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
	
	@Operation(summary = "Invalidate session and redirect to root (for Keycloak JS adapter compatibility)", operationId = "sso_logout")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@GetMapping("/sso/logout")
	@MiningUnsecuredRestEndpoint
	@Deprecated(forRemoval = true)  /* use /api/v1/auth/logout after Keycloak JS adapter is removed */
	public RedirectView ssoLogout(final HttpServletRequest request) {
		doLogout(request);
		return new RedirectView(ServletUriComponentsBuilder.fromCurrentContextPath().toUriString());
	}
	
	@Operation(summary = "Ensure session is authorized (possibly redirecting to Keycloak) and then redirect to to the UI.", operationId = "redirectUI")
	@GetMapping("/api/v1/auth/ui")
	@MiningUnsecuredRestEndpoint
	public RedirectView redirectUI(
			@Parameter(description = "Fragment to append to the redirection URI.")
			@RequestParam final Optional<String> context) {
		final UriComponentsBuilder url = uiBaseUrl != null ? UriComponentsBuilder.fromUriString(uiBaseUrl) : ServletUriComponentsBuilder.fromCurrentContextPath();
		if(context.isPresent() && ! context.get().isBlank()) {
			url.fragment(context.get());
		}
		return new RedirectView(url.toUriString());
	}
	
}

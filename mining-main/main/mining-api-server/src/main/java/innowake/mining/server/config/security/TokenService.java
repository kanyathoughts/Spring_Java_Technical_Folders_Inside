/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.config.security;

import java.security.SecureRandom;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

import org.apache.commons.codec.binary.Base64;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Profile;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.security.core.Authentication;
import org.springframework.security.oauth2.client.OAuth2AuthorizedClientService;
import org.springframework.security.oauth2.client.authentication.OAuth2AuthenticationToken;
import org.springframework.security.oauth2.core.OAuth2Token;
import org.springframework.security.oauth2.core.oidc.user.OidcUser;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;

import innowake.mining.data.access.postgres.TokenPgDao;
import innowake.mining.server.config.JwtAuthentication;
import innowake.mining.server.config.Profiles;
import innowake.mining.shared.access.TokenInfoService;
import innowake.mining.shared.security.OfflineTokenInfo;

/**
 * Provides access to user authorization information in the database and JSON Web Tokens.
 */
@Service
@Profile(Profiles.AUTH)
public class TokenService implements TokenInfoService {
	
	private final TokenPgDao dao;
	private final OAuth2AuthorizedClientService authClientService;
	
	@Autowired
	public TokenService(
			final OAuth2AuthorizedClientService authClientService,
			@Qualifier("postgres") final JdbcTemplate jdbcTemplate) {
		this.authClientService = authClientService;
		dao = new TokenPgDao(jdbcTemplate);
	}
	
	/**
	 * Retrieves the current access token form a web session's authentication context.
	 * @param auth Authentication context.
	 * @return The token, if available.
	 */
	public Optional<OAuth2Token> getAccessToken(final Authentication auth) {
		if (auth instanceof OAuth2AuthenticationToken) {
			final OAuth2AuthenticationToken authToken = (OAuth2AuthenticationToken) auth;
			return Optional.of(authClientService.loadAuthorizedClient(authToken.getAuthorizedClientRegistrationId(), authToken.getName()).getAccessToken());
		} else if (auth instanceof JwtAuthentication) {
			final JwtAuthentication authToken = (JwtAuthentication) auth;
			return Optional.of(authToken.getToken());
		} else {
			return Optional.empty();
		}
	}
	
	/**
	 * Retrieves the claims (identity and roles) of a user's access token or principal in the authentication context.
	 * @param auth Authentication context.
	 * @return Map of claims, containing Maps and Lists of strings as specified by OAuth/JWT/OpenID. 
	 */
	public Map<String, Object> getUserClaims(final Authentication auth) {
		if (auth instanceof OAuth2AuthenticationToken) {
			final OidcUser user = (OidcUser) ((OAuth2AuthenticationToken) auth).getPrincipal();
			return user.getUserInfo().getClaims();
		} else if (auth instanceof JwtAuthentication) {
			final Jwt token = ((JwtAuthentication) auth).getToken();
			return token.getClaims();
		} else {
			return Collections.emptyMap();
		}
	}
	
	/**
	 * Retrieves the refresh token form a web session's authentication context.
	 * This is only available for login but not resource server (Bearer) sessions.
	 * @param auth Authentication context.
	 * @return The token, if available.
	 */
	public Optional<OAuth2Token> getRefreshToken(final Authentication auth) {
		if (auth instanceof OAuth2AuthenticationToken) {
			final OAuth2AuthenticationToken authToken = (OAuth2AuthenticationToken) auth;
			return Optional.of(authClientService.loadAuthorizedClient(authToken.getAuthorizedClientRegistrationId(), authToken.getName()).getRefreshToken());
		} else {
			return Optional.empty();
		}
	}
	
	/**
	 * Stores the provided offline token data in the database.
	 * @param token Token details.
	 */
	public void storeToken(final OfflineTokenInfo token) {
		dao.storeToken(token);
	}
	
	/**
	 * Finds all offline tokens stored for a certain user principal.
	 * Security critical attributes will only be returned if it belongs to the currently authenticated user.
	 * @param auth Authentication context, to determine the current user.
	 * @param subject ID of the user principal at the authentication provider.
	 * @return List of tokens.
	 */
	public List<OfflineTokenInfo> findTokensBySubject(final Authentication auth, final String subject) {
		return dao.findOfflineTokens(auth.getName(), q -> q.ofSubject(subject));
	}
	
	/**
	 * Retrieves information on an offline token by its ID.
	 * Security critical attributes will only be returned if it belongs to the currently authenticated user.
	 * @param auth Authentication context, to determine the current user.
	 * @param id ID of the token in the database.
	 * @return Token details, if found.
	 */
	public Optional<OfflineTokenInfo> findTokenById(final Authentication auth, final UUID id) {
		return dao.findOfflineTokens(auth.getName(), q -> q.byId(id)).stream().findFirst();
	}
	
	/**
	 * Removes an offline token from the database.
	 * @param id ID of the token entry.
	 * @param subject ID of the user principal the token belongs to.
	 * @return If anything was deleted.
	 */
	public boolean deleteToken(final UUID id, final String subject) {
		return dao.deleteToken(q -> q.byId(id).ofSubject(subject)) > 0;
	}
	
	/**
	 * Changes the description of a stored offline token.
	 * @param subject ID of the user principal the token belongs to.
	 * @param tokenInfo Token details.
	 * @return If anything was updated.
	 */
	public boolean updateToken(final String subject, final OfflineTokenInfo tokenInfo) {
		return dao.updateTokenDescription(tokenInfo.getId(), subject, tokenInfo.getDescription());
	}
	
	/**
	 * Retrieves a stored offline token.
	 * @param bearerToken The key provided in the authentication header of the request.
	 * @return The refresh token, if available.
	 */
	public Optional<String> findRefreshTokenByBearerToken(final String bearerToken) {
		return dao.findRefreshToken(bearerToken);
	}
	
	/**
	 * Generates a random String to be used as a token key.
	 * @return String URL-safe Base64 string of 264 bits / 44 characters.
	 */
	public String createToken() {
		final byte[] secret = new byte[33];
		new SecureRandom().nextBytes(secret);
		return Base64.encodeBase64URLSafeString(secret);
	}
	
	/**
	 * Requests session tokens from an authentication service.
	 * @param tokenUri Address of the token endpoint.
	 * @param client ID of the requesting OAuth client.
	 * @param code Authorization code obtained during the login process.
	 * @param redirUri Redirection URI used during the login process (must match the original request).
	 * @return Map of tokens and validity attributes as specified by OAuth.
	 */
	@SuppressWarnings("unchecked")
	public Map<String, String> requestToken(final String tokenUri, final String client, final String code, final String redirUri) {
		final RestTemplate tokenRequest = new RestTemplate();
		final HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
		final MultiValueMap<String, Object> attrs = new LinkedMultiValueMap<>();
		attrs.add("client_id", client);
		attrs.add("grant_type", "authorization_code");
		attrs.add("code", code);
		attrs.add("redirect_uri", redirUri);
		return Objects.requireNonNull(tokenRequest.postForEntity(tokenUri, new HttpEntity<MultiValueMap<String, Object>>(attrs, headers), Map.class).getBody());
	}
	
	/**
	 * Performs a token refresh request.
	 * @param tokenUri Address of the token endpoint.
	 * @param client ID of the requesting OAuth client.
	 * @param refreshToken The refresh token.
	 * @return Map of tokens and validity attributes as specified by OAuth.
	 */
	@SuppressWarnings("unchecked")
	public Map<String, String> refreshToken(final String tokenUri, final String client, final String refreshToken) {
		final RestTemplate tokenRequest = new RestTemplate();
		final HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
		final MultiValueMap<String, Object> attrs = new LinkedMultiValueMap<>();
		attrs.add("client_id", client);
		attrs.add("grant_type", "refresh_token");
		attrs.add("refresh_token", refreshToken);
		return Objects.requireNonNull(tokenRequest.postForEntity(tokenUri, new HttpEntity<MultiValueMap<String, Object>>(attrs, headers), Map.class).getBody());
	}
	
}

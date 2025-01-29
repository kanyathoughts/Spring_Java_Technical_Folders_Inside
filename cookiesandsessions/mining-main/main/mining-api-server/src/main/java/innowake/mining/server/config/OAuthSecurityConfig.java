/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.config;

import static org.springframework.security.oauth2.client.web.OAuth2AuthorizationRequestRedirectFilter.DEFAULT_AUTHORIZATION_REQUEST_BASE_URI;

import java.io.IOException;
import java.time.Instant;
import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpRequest;
import org.springframework.http.MediaType;
import org.springframework.http.client.ClientHttpRequestExecution;
import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.http.client.ClientHttpResponse;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.oauth2.client.oidc.userinfo.OidcUserRequest;
import org.springframework.security.oauth2.client.oidc.userinfo.OidcUserService;
import org.springframework.security.oauth2.client.registration.ClientRegistration;
import org.springframework.security.oauth2.client.registration.ClientRegistrationRepository;
import org.springframework.security.oauth2.core.OAuth2AccessToken;
import org.springframework.security.oauth2.core.OAuth2Token;
import org.springframework.security.oauth2.core.oidc.user.DefaultOidcUser;
import org.springframework.security.oauth2.core.oidc.user.OidcUser;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.security.oauth2.jwt.NimbusJwtDecoder;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.web.client.RestTemplate;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.config.security.MiningRole;
import innowake.mining.server.config.security.TokenService;

/**
 * Security configuration for using Keycloak.
 */
@Configuration
@Profile(Profiles.AUTH)
@EnableWebSecurity
public class OAuthSecurityConfig {
	
	private static final Logger LOG = LoggerFactory.getLogger(OAuthSecurityConfig.class);
	
	private static final String SECURED_PATTERN = "/**";
	private static final String AUTH_TYPE_BEARER = "Bearer ";
	private static final String OFFLINE_BEARER_PREFIX = "offline.";
	private static final String OFFLINE_BEARER_ATTRIBUTE = "OFFLINE_BEARER";
	private static final String REFRESH_TOKEN_ATTRIBUTE = "REFRESH_TOKEN";
	private static final int OFFLINE_BEARER_LENGTH = 52;
	private static final int TOKEN_MIN_VALID = 30;
	
	@Autowired
	private ClientRegistrationRepository authReg;

	@Autowired
	private TokenService tokenService;

	@Value("${keycloak.registration-id}")
	private String authClientReg;

	@Value("${keycloak.lookup-timeout : 30}")
	private int lookupTimeout;
	
	@Bean
	public RestTemplate oauthRestTemplate() {
		final HttpComponentsClientHttpRequestFactory httpRequestFactory = new HttpComponentsClientHttpRequestFactory();
		httpRequestFactory.setReadTimeout(lookupTimeout * 1000);
		final RestTemplate rt = new RestTemplate(httpRequestFactory);
		rt.getInterceptors().add(new ClientHttpRequestInterceptor() {
			@Override
			public ClientHttpResponse intercept(final HttpRequest request, final byte[] body, final ClientHttpRequestExecution execution) throws IOException {
				final Optional<OAuth2Token> token = tokenService.getAccessToken(SecurityContextHolder.getContext().getAuthentication());
				if (token.isPresent()) {
					request.getHeaders().add(HttpHeaders.AUTHORIZATION, AUTH_TYPE_BEARER + token.get().getTokenValue());
				}
				if (request.getHeaders().getContentType() == null) {
					/* default to JSON content */
					request.getHeaders().setContentType(MediaType.APPLICATION_JSON);
				}
				return execution.execute(request, body);
			}
		});
		return rt;
	}
	
	@Bean
	public SecurityFilterChain securityFilterChain(final HttpSecurity http) throws Exception {
		final OidcUserService oidcUserService = new OidcUserService();
		final ClientRegistration authClient = authReg.findByRegistrationId(authClientReg);
		http.csrf().disable()
			.requestMatchers(rqm -> rqm.antMatchers(SECURED_PATTERN))
			.sessionManagement(smgt -> smgt.sessionCreationPolicy(SessionCreationPolicy.IF_REQUIRED))
			.oauth2Login(login -> login.loginPage(DEFAULT_AUTHORIZATION_REQUEST_BASE_URI + "/" + authClientReg).failureUrl("/login-error")
				.userInfoEndpoint().oidcUserService(userRequest -> processUserRequest(oidcUserService, userRequest)))
			.authorizeRequests(rq -> GeneralSecurityConfig.configureAuthorizedUrls(rq, true)
					.anyRequest().authenticated())
			.oauth2ResourceServer(rs -> {
				rs.bearerTokenResolver(rq -> processBearer(authClient, rq));
				rs.jwt(jwtCfg -> jwtCfg.jwtAuthenticationConverter(jwt -> new JwtAuthentication(jwt, extractRoles(jwt, jwt.getSubject()))));
			});
		return http.build();
	}
	
	private String processBearer(final ClientRegistration authClient, final HttpServletRequest request) {
		final Authentication auth = SecurityContextHolder.getContext().getAuthentication();
		final Optional<OAuth2Token> accessToken = tokenService.getAccessToken(auth);
		Optional<String> refreshToken = Optional.empty();
		final AtomicReference<HttpSession> session = new AtomicReference<>(request.getSession(false));
		String bearer = request.getHeader("Authorization");
		
		LOG.debug(() -> {
			if (session.get() != null) {
				return session.get().getId() + " (" + (auth != null ? auth.getName() : "unauthorized") + ") " + request.getRequestURI();
			} else {
				return "(no session) " + request.getRequestURI();
			}
		});
		
		/* attempt to map and load offline token */
		if (bearer != null) {
			bearer = bearer.startsWith(AUTH_TYPE_BEARER) ? bearer.substring(AUTH_TYPE_BEARER.length()) : null;
			if (bearer != null && bearer.length() == OFFLINE_BEARER_LENGTH && bearer.startsWith(OFFLINE_BEARER_PREFIX)) {
				session.set(request.getSession(true));
				final String offlineBearer = bearer.substring(OFFLINE_BEARER_PREFIX.length());
				bearer = null;
				if ( ! offlineBearer.equals(session.get().getAttribute(OFFLINE_BEARER_ATTRIBUTE))) {
					LOG.debug(() -> session.get().getId() + " loading offline token");
					session.get().removeAttribute(REFRESH_TOKEN_ATTRIBUTE);
					session.get().setAttribute(OFFLINE_BEARER_ATTRIBUTE, offlineBearer);
					refreshToken = tokenService.findRefreshTokenByBearerToken(offlineBearer);
					if ( ! refreshToken.isPresent()) {
						LOG.debug(() -> session.get().getId() + " offline token not found");
						SecurityContextHolder.getContext().setAuthentication(null);
					}
				}
			}
		}
		
		/* attempt to refresh expired/expiring access token or get initial access token using offline token */
		if (refreshToken.isPresent() || isTokenExpired(accessToken, Instant.now().plusSeconds(TOKEN_MIN_VALID))) {
			session.set(request.getSession(true));
			LOG.debug(() -> session.get().getId() + " token to be refreshed");
			
			refreshToken = Optional.ofNullable(
				Optional.ofNullable(refreshToken
					.orElseGet(() -> (String) session.get().getAttribute(REFRESH_TOKEN_ATTRIBUTE))
				).orElseGet(() -> tokenService.getRefreshToken(auth).flatMap(t -> Optional.of(t.getTokenValue())).orElse(null))
			);
			
			if (refreshToken.isPresent()) {
				LOG.debug(() -> session.get().getId() + " refreshing token");
				try {
					final Map<String, String> tokenResponse = tokenService.refreshToken(authClient.getProviderDetails().getTokenUri(),
							authClient.getClientId(), refreshToken.get());
					bearer = tokenResponse.get("access_token");
					session.get().setAttribute(REFRESH_TOKEN_ATTRIBUTE, tokenResponse.get("refresh_token"));
				} catch (final Exception e) {
					LOG.debug(() -> session.get().getId() + " token refresh failed", e);
				}
				LOG.debug(() -> session.get().getId() + " token refreshed");
			} else {
				LOG.debug(() -> session.get().getId() + " no refresh token available");
			}
		}
		
		/* terminate session if access token has expired and could not be refreshed */
		if (bearer == null && isTokenExpired(accessToken, Instant.now())) {
			if (session.get() != null) {
				LOG.debug(() -> session.get().getId() + " token expired");
			}
			SecurityContextHolder.getContext().setAuthentication(null);
		}
		
		return bearer;
	}
	
	private boolean isTokenExpired(final Optional<OAuth2Token> token, final Instant at) {
		if (token.isPresent()) {
			final Instant exp = token.get().getExpiresAt();
			if (exp != null) {
				return exp.isBefore(at);
			}
		}
		return false;
	}
	
	private OidcUser processUserRequest(final OidcUserService userSvc, final OidcUserRequest userRequest) {
		final OidcUser oidcUser = userSvc.loadUser(userRequest);
		LOG.debug(() -> "Login: " + oidcUser.toString());
		final OAuth2AccessToken accessToken = userRequest.getAccessToken();
		final Jwt jwtAccess = NimbusJwtDecoder.withJwkSetUri(userRequest.getClientRegistration().getProviderDetails().getJwkSetUri())
			.build().decode(accessToken.getTokenValue());
		return new DefaultOidcUser(extractRoles(jwtAccess, oidcUser.getName()), oidcUser.getIdToken(), oidcUser.getUserInfo());
	}
	
	private static Set<GrantedAuthority> extractRoles(final Jwt jwt, final String reference) {
		final Set<GrantedAuthority> mappedAuthorities = new HashSet<>();
		final Map<String, Object> access = jwt.getClaim("realm_access");
		if (access != null) {
			@SuppressWarnings("unchecked")
			final Collection<Object> roles = (Collection<Object>) access.get("roles");
			if (roles != null) {
				for (final Object role : roles) {
					final String strRole = role.toString();
					try {
						mappedAuthorities.add(new MiningRole(strRole));
						LOG.debug(() -> reference + " -> MiningRole: " + strRole);
					} catch (final IllegalArgumentException e) {
						mappedAuthorities.add(new SimpleGrantedAuthority(strRole));
						LOG.debug(() -> reference + " -> Role: " + strRole);
					}
				}
			}
		}
		return mappedAuthorities;
	}
	
}

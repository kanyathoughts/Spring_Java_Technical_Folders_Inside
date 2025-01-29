/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.config;

import java.util.Collection;
import java.util.Map;

import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.security.oauth2.server.resource.authentication.AbstractOAuth2TokenAuthenticationToken;

/**
 * Authentication for a JSON Web Token (JWT).
 */
public class JwtAuthentication extends AbstractOAuth2TokenAuthenticationToken<Jwt> {
	
	/**
	 * Constructs a JWT authentication.
	 * @param jwt The token.
	 * @param authorities Authorities extracted/mapped from the token.
	 */
	public JwtAuthentication(final Jwt jwt, final Collection<? extends GrantedAuthority> authorities) {
		super(jwt, authorities);
		this.setAuthenticated(true);
	}
	
	@Override
	public Map<String, Object> getTokenAttributes() {
		return this.getToken().getClaims();
	}
	
	@Override
	public String getName() {
		return this.getToken().getSubject();
	}
	
}

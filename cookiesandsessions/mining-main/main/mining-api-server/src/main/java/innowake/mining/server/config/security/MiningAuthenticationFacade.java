package innowake.mining.server.config.security;

import java.util.Arrays;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.error.MiningAuthenticationException;

/**
 * AuthenticationFacade implementation for Mining.
 */
@Component
public class MiningAuthenticationFacade implements AuthenticationFacade {
 
	private static final String ANONYMOUS_ROLE_NAME = "ROLE_ANONYMOUS";

	@Autowired
	private Environment environment;

    @Override
    public Authentication getAuthentication() {
        final Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication == null) {
        	throw new MiningAuthenticationException("Not authenticated.");
        }
        return authentication;
    }
    
    @Override
	public String getUserId() {
		@Nullable final Authentication authentication = getAuthentication();
    	final boolean noAuthorizationActive = Arrays.stream(environment.getActiveProfiles()).anyMatch(Profiles.NO_AUTH::equals);
    	final boolean anonymousUser = authentication.getAuthorities().stream().anyMatch(authority -> ANONYMOUS_ROLE_NAME.equals(authority.getAuthority()));

    	if (noAuthorizationActive && anonymousUser) {
    		/* This is only relevant for functional integration tests for which no authentication is active. */
    		return "admin";
    	} else {
    		return Assert.assertNotNull(authentication.getName(), "No user name present in the Spring authentication.");
    	}
    }
}
/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.service;

import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;

import innowake.lib.core.api.lang.Nullable;


/**
 * Security context with a hard-coded {@link SystemUserAuthentication authentication}.
 * <p>
 * This can be used for example when submitting background jobs.
 */
public class SystemUserSecurityContext implements SecurityContext {
	
	private static final Authentication AUTHENTICATION = new SystemUserAuthentication();
	private static final SecurityContext INSTANCE = new SystemUserSecurityContext(); 
	
	private SystemUserSecurityContext() { }
	
	/**
	 * @return the security context
	 */
	public static SecurityContext get() {
		return INSTANCE;
	}

	@Override
	public Authentication getAuthentication() {
		return AUTHENTICATION;
	}

	@Override
	public void setAuthentication(@Nullable final Authentication authentication) {
		/* authentication not settable */
	}

}

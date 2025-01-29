/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.service;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.Job;
import innowake.mining.server.config.security.MiningRole;
import innowake.mining.shared.security.RoleType;

import static innowake.mining.server.service.UserNameService.SYSTEM_USER_ID;

/**
 * An authentication with the user name "SYSTEM".
 * <p>
 * ATTENTION: All methods return {@code false}, {@code null} or empty lists, except {@link #getPrincipal()} whose {@code #getUsername} returns "SYSTEM".
 * This was initially implemented in order to submit background {@link Job jobs} which are not tight to a particular user.
 * <p>
 * Used in conjunction with {@link SystemUserSecurityContext}.
 */
public class SystemUserAuthentication implements Authentication {

	/* the system user has "admin" authority */
	private static final List<GrantedAuthority> AUTHORITIES = Collections.singletonList(new MiningRole(RoleType.ADMIN.getValue()));
	
	@Override
	@Nullable
	public String getName() {
		return null;
	}

	@Override
	public void setAuthenticated(boolean isAuthenticated) throws IllegalArgumentException {
		/* do nothing */
	}

	@Override
	public boolean isAuthenticated() {
		return false;
	}

	@Override
	public Object getPrincipal() {
		return new UserDetails() {

			@Override
			public boolean isEnabled() {
				return false;
			}

			@Override
			public boolean isCredentialsNonExpired() {
				return false;
			}

			@Override
			public boolean isAccountNonLocked() {
				return false;
			}

			@Override
			public boolean isAccountNonExpired() {
				return false;
			}

			@Override
			public String getUsername() {
				return SYSTEM_USER_ID;
			}

			@Override
			@Nullable
			public String getPassword() {
				return null;
			}

			@Override
			public Collection<? extends GrantedAuthority> getAuthorities() {
				return AUTHORITIES;
			}
		};
	}

	@Override
	@Nullable
	public Object getDetails() {
		return null;
	}

	@Override
	@Nullable
	public Object getCredentials() {
		return null;
	}

	@Override
	public Collection<? extends GrantedAuthority> getAuthorities() {
		return AUTHORITIES;
	}
}

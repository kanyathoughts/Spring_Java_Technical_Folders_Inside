/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.ProviderManager;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.provisioning.InMemoryUserDetailsManager;

import innowake.lib.util.StringUtil;
import innowake.mining.shared.model.User;

/**
 * Spring Security configuration class for Legacy Authentication
 */
@Configuration
@Profile(Profiles.LEGACY_AUTH)
public class AuthorizationServerConfig {
	
	@Value("${mining.user.username}")
	private String username;
	
	@Value("${mining.user.password}")
	private String password;

	@Value("${mining.user.token}")
	private String token;

	@Bean
	public AuthenticationManager authenticationManager(
			final UserDetailsService userDetailsService) {
		final DaoAuthenticationProvider authenticationProvider = new DaoAuthenticationProvider();
		authenticationProvider.setUserDetailsService(userDetailsService);
		authenticationProvider.setPasswordEncoder(encoder());
		return new ProviderManager(authenticationProvider);
	}
	
	@Bean
	public User getUser() {
		if ( StringUtil.isEmpty(username) || StringUtil.isEmpty(password) || StringUtil.isEmpty(token)) {
			throw new IllegalStateException(" Using the default profile requires an username, password and token to be configured via the application.yml.\r\n"
					+ "\r\n"
					+ "Please add the following properties:\r\n"
					+ "\r\n"
					+ "mining:\r\n"
					+ "    user:\r\n"
					+ "        username:\r\n"
					+ "        password: \r\n"
					+ "        token:\r\n"
					+ "");
		}
		return new User(username, password, token);
	}

	@Bean
	public UserDetailsService userDetailsService() {
		final InMemoryUserDetailsManager inMemoryUserDetailsManager = new InMemoryUserDetailsManager();
		final UserDetails user1 = org.springframework.security.core.userdetails.User.builder().username(getUser().getUserId()).password(encoder().encode(getUser().getPassword())).roles(innowake.mining.shared.security.RoleType.ADMIN.getValue()).build();
		inMemoryUserDetailsManager.createUser(user1);
		return inMemoryUserDetailsManager;
	}

	@Bean
	public BCryptPasswordEncoder encoder() {
		return new BCryptPasswordEncoder();
	}
}

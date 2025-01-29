/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import static innowake.lib.core.lang.Assert.assertNotNull;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Model class for a user.
 */
public class User {

	private String userId;
	private String password;
	private String token;
	@JsonCreator
	public User(@JsonProperty("userId") final String userId, @JsonProperty("password") final String password, @JsonProperty("token") final String token) {
		this.userId = userId;
		this.password = password;
		this.token = token;
	}
	/**
	 * Get the user id.
	 * The user id is used for unique identification of the user.
	 *
	 * @return The user id.
	 */
	public String getUserId() {
		return assertNotNull(userId, "User Id must not be null.");
	}
	
	/**
	 * Set the user id.
	 *
	 * @param userId The new user id.
	 */
	public void setUserId(final String userId) {
		this.userId = userId;
	}

	/**
	 * Get the encrypted password 
	 *
	 * @return The encrypted password.
	 */
	public String getPassword() {
		return assertNotNull(password, "Password must not be null.");
	}

	/**
	 * Set the encrypted password.
	 *
	 * @param password The new encrypted password.
	 */
	public void setPassword(final String password) {
		this.password = password;
	}
	
	/**
	 * Get the token
	 *
	 * @return The token.
	 */
	public String getToken() {
		return assertNotNull(token, "token must not be null.");
	}
}

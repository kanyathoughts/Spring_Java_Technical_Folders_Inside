/*
 *  Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model;


import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Login request model for Legacy authentication.
 */
public class LoginRequest {
	
	private final String username;
	private final String password;

	@JsonCreator
	public LoginRequest(@JsonProperty("username") final String username, @JsonProperty("password") final String password) {
		this.username = username;
		this.password = password;
	}
	
	public String getUsername() {
		return username;
	}
	public String getPassword() {
		return password;
	}
}

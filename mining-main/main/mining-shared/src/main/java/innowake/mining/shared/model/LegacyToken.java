/*
 *  Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Legacy Token type in response of Legacy authentication login.
 */
public class LegacyToken {

	private final String accessToken;
	private final String username;
	private final String tokenType;

	@JsonCreator
	public LegacyToken(final String accessToken, final String username,
			final String tokenType) {
		this.accessToken = accessToken;
		this.username = username;
		this.tokenType = tokenType;
	}
	
	@JsonProperty("username") 
	public String getUsername() {
		return username;
	}
	
	@JsonProperty("access_token") 
	public String getAccessToken() {
		return accessToken;
	}
	
	@JsonProperty("token_type") 
	public String getTokenType() {
		return tokenType;
	}
}

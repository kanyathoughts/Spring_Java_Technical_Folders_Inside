/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.keycloak.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * POJO class to represent the access information.
 * This class contains the access token, as well as
 * the refresh token whenever it is requested for a user.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class AccessInformation {

	private final String accessToken;
	private final int expiresIn;
	private final int refreshExpriresIn;
	private final String refreshToken;
	private final String tokenType;

	/**
	 * Constructor to initialize the Access Token Information.
	 * 
	 * @param accessToken The Access Token
	 * @param expiresIn The time duration of the validity of the Access Token
	 * @param refreshExpiresIn The time duration of the validity of the Refresh Token
	 * @param refreshToken The Refresh Token
	 * @param tokenType The authorization type of the Access Token
	 */
	@JsonCreator
	public AccessInformation(
			@JsonProperty("access_token") final String accessToken,
			@JsonProperty("expires_in") final int expiresIn,
			@JsonProperty("refresh_expires_in") final int refreshExpiresIn,
			@JsonProperty("refresh_token") final String refreshToken,
			@JsonProperty("token_type") final String tokenType) {
		this.accessToken = accessToken;
		this.expiresIn = expiresIn;
		this.refreshExpriresIn = refreshExpiresIn;
		this.refreshToken = refreshToken;
		this.tokenType = tokenType;
	}

	/**
	 * Returns the access token as a String.
	 *
	 * @return Access Token
	 */
	public String getAccessToken() {
		return accessToken;
	}

	/**
	 * Returns the time duration (in milliseconds) after which the access token expires.
	 *
	 * @return Time duration in milliseconds of the validity of the access token
	 */
	public int getExpiresIn() {
		return expiresIn;
	}

	/**
	 * Returns the time duration after (in milliseconds) which the refresh token expires.
	 *
	 * @return Time duration in milliseconds of the validity of the refresh token
	 */
	public int getRefreshExpriresIn() {
		return refreshExpriresIn;
	}

	/**
	 * Returns the refresh token as a String.
	 *
	 * @return Refresh Token
	 */
	public String getRefreshToken() {
		return refreshToken;
	}

	/**
	 * Returns the authorization type of the access token, for example: "bearer"
	 *
	 * @return Authorization type of the access token
	 */
	public String getTokenType() {
		return tokenType;
	}
}

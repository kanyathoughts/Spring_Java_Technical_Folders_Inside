/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.security;

import java.time.Instant;
import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;

/**
 * Properties of an OAuth/Keycloak token for offline authentication.
 */
public class OfflineTokenInfo {
	
	private final UUID id;
	private final String subject;
	private final String username;
	private final String description;
	private final String bearerToken;
	private final String refreshToken;
	
	@Nullable
	private Instant dateCreated;
	
	/**
	 * Information about a user's offline refresh token.
	 * @param id Unique ID of the token under which it is stored in the database.
	 * @param subject Unique ID of the user principal at the authentication service.
	 * @param username Logon name of the user.
	 * @param description User defined description of the token.
	 * @param bearerToken Key to be sent in the authorization header to gain access using this token.
	 * @param refreshToken The actual JWT offline refresh token.
	 */
	@JsonCreator
	public OfflineTokenInfo(
			@JsonProperty("id") final UUID id,
			@JsonProperty("subject") final String subject,
			@JsonProperty("username") final String username,
			@JsonProperty("description") final String description,
			@JsonProperty("bearerToken") final String bearerToken,
			@JsonProperty("refreshToken") final String refreshToken) {
		this.id = id;
		this.subject = subject;
		this.username = username;
		this.description = description;
		this.bearerToken = bearerToken;
		this.refreshToken = refreshToken;
	}
	
	public UUID getId() {
		return id;
	}
	
	public String getSubject() {
		return subject;
	}
	
	public String getUsername() {
		return username;
	}
	
	public String getDescription() {
		return description;
	}
	
	public String getBearerToken() {
		return bearerToken;
	}
	
	public String getRefreshToken() {
		return refreshToken;
	}
	
	@Nullable
	public Instant getDateCreated() {
		return dateCreated;
	}
	
	public OfflineTokenInfo setDateCreated(@Nullable final Instant created) {
		this.dateCreated = created;
		return this;
	}
	
}

/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.config.security;

import static innowake.mining.shared.security.RoleType.*;

import java.util.Arrays;
import java.util.Optional;
import java.util.stream.Collectors;

import innowake.lib.core.api.lang.Nullable;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.springframework.security.core.GrantedAuthority;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Representation of a Mining role based on the raw Keycloak role value.
 */
public class MiningRole implements GrantedAuthority {

	@Nullable
	private final Long clientId;
	@Nullable
	private final Long projectId;
	private final String value;
	private final String authority;

	/**
	 * Creates a mining role based on the raw Keycloak role value.
	 * 
	 * @param keycloakRole the raw keycloak role value, e.g. client-1-project-1-mining / client-1-admin
	 */
	public MiningRole(final String keycloakRole) {
		final String[] tokens = keycloakRole.split("-");
		if (isClientProjectRole(tokens)) {
			clientId = Long.valueOf(tokens[1]);
			projectId = Long.valueOf(tokens[3]);
			value = Arrays.stream(tokens, 4, tokens.length).collect(Collectors.joining("-"));
		} else if (isClientAdminRole(tokens)) {
			clientId = Long.valueOf(tokens[1]);
			projectId = null;
			value = CLIENT_ADMIN.getValue();
		} else if (isAdminRole(tokens)) {
			clientId = null;
			projectId = null;
			value = ADMIN.getValue();
		} else {
			throw new IllegalArgumentException("Expected the following patterns: client-ID-project-ID-value, client-ID-admin or admin but got: " + keycloakRole);
		}
		authority = keycloakRole;
	}

	/**
	 * Returns the ID of the client if available.
	 * <p>
	 * The administrator role (i.e. admin) does not have a client associated, 
	 * in this case the client ID will be empty.
	 * 
	 * @return the ID of the client if applicable, otherwise {@link Optional#empty()}.
	 */
	@JsonProperty("client")
	public Optional<Long> clientId() {
		return Optional.ofNullable(clientId);
	}
	
	/**
	 * Returns the ID of the project if available.
	 * <p>
	 * The client administrator role (e.g. client-1-admin) does not have a project associated, 
	 * in this case the project ID will be empty.
	 * 
	 * @return the ID of the project if applicable, otherwise {@link Optional#empty()}.
	 */
	@JsonProperty("project")
	public Optional<Long> projectId() {
		return Optional.ofNullable(projectId);
	}
	
	/**
	 * @return the value associated with this role, e.g. "admin" or "mining"
	 */
	@JsonProperty("value")
	public String value() {
		return value;
	}

	@Override
	public String toString() {
		final ToStringBuilder builder = new ToStringBuilder(this);
		builder.append("clientId", clientId != null ? clientId : "not available");
		builder.append("projectId", projectId != null ? projectId : "not available");
		builder.append("value", value);
		return builder.toString();
	}

	private boolean isClientProjectRole(final String[] tokens) {
		return tokens.length >= 5 && "client".equals(tokens[0]) && "project".equals(tokens[2]);
	}

	private boolean isClientAdminRole(final String[] tokens) {
		return tokens.length == 3 && "client".equals(tokens[0]) && ADMIN.getValue().equals(tokens[2]);
	}
	
	private boolean isAdminRole(final String[] tokens) {
		return tokens.length == 1 && ADMIN.getValue().equals(tokens[0]);
	}

	@Override
	@JsonIgnore
	public String getAuthority() {
		return authority;
	}

}

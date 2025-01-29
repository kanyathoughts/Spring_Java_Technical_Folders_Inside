/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.ScopeEnum;

/**
 * {@code saved_search} entity class.
 */
public class SavedSearchPojo {

	private final Long id;
	private final Optional<EntityId> client;
	private final Optional<EntityId> project;
	private final String name;
	private final String savedSearch;
	private final ScopeEnum scope;
	private final String usage;
	private final List<String> modifiers;
	private final Optional<String> createdByUserId;

	private Optional<String> createdByUserName;

	public SavedSearchPojo(
			@JsonProperty("id") final Long id,
			@JsonProperty("clientEntity") @Nullable final EntityId client,
			@JsonProperty("client") @Nullable final UUID clientUid,
			@JsonProperty("clientId") @Nullable final Long clientNid,
			@JsonProperty("projectEntity") @Nullable final EntityId project,
			@JsonProperty("project") @Nullable final UUID projectUid,
			@JsonProperty("projectId") @Nullable final Long projectNid,
			@JsonProperty("name") final String name,
			@JsonProperty("savedSearch") final String savedSearch,
			@JsonProperty("scope") final ScopeEnum scope,
			@JsonProperty("usage") final String usage,
			@JsonProperty("modifiers") final List<String> modifiers,
			@JsonProperty("createdByUserId") @Nullable final String createdByUserId) {
		this.id = id;

		if (client == null && clientUid == null && clientNid == null) {
			this.client = Optional.empty();
		} else {
			this.client = Optional.of(client != null ? client : EntityId.of(clientUid, clientNid));
		}

		if (project == null && projectUid == null && projectNid == null) {
			this.project = Optional.empty();
		} else {
			this.project = Optional.of(project != null ? project : EntityId.of(projectUid, projectNid));
		}

		this.name = name;
		this.savedSearch = savedSearch;
		this.scope = scope;
		this.usage = usage;
		this.modifiers = modifiers;
		this.createdByUserId = Optional.ofNullable(createdByUserId);
		
		createdByUserName = Optional.empty();
	}

	public Long getId() {
		return id;
	}

	@JsonIgnore
	public Optional<EntityId> getClient() {
		return client;
	}

	@JsonProperty("client")
	public Optional<UUID> getClientUid() {
		return client.map(EntityId::getUid);
	}

	@JsonProperty("clientId")
	public Optional<Long> getClientNid() {
		return client.map(EntityId::getNid);
	}

	@JsonIgnore
	public Optional<EntityId> getProject() {
		return project;
	}

	@JsonProperty("project")
	public Optional<UUID> getProjectUid() {
		return project.map(EntityId::getUid);
	}

	@JsonProperty("projectId")
	public Optional<Long> getProjectNid() {
		return project.map(EntityId::getNid);
	}

	public String getName() {
		return name;
	}

	public String getSavedSearch() {
		return savedSearch;
	}

	public ScopeEnum getScope() {
		return scope;
	}

	public String getUsage() {
		return usage;
	}

	public List<String> getModifiers() {
		return modifiers;
	}

	public Optional<String> getCreatedByUserId() {
		return createdByUserId;
	}

	public Optional<String> getCreatedByUserName() {
		return createdByUserName;
	}

	public void setCreatedByUserName(@Nullable final String userName) {
		this.createdByUserName = Optional.ofNullable(userName);
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id)
				.append("client", client)
				.append("project", project)
				.append("name", name)
				.append("savedSearch", savedSearch)
				.append("scope", scope)
				.append("usage", usage)
				.append("modifiers", modifiers)
				.append("createdByUserId", createdByUserId)
				.toString();
	}
}
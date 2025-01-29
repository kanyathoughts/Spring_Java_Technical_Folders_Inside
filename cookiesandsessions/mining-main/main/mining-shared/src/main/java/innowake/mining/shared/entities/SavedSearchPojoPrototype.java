/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.Collection;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonAlias;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.ScopeEnum;

/**
 * {@code saved_search} entity request class.
 */
public class SavedSearchPojoPrototype implements PojoPrototype {

	public final Definable<Long> id = new Definable<>(false, "SavedSearch.id");
	public final Definable<EntityId> client = new Definable<>(true, "SavedSearch.client");
	public final Definable<EntityId> project = new Definable<>(true, "SavedSearch.project");
	public final Definable<String> name = new Definable<>(false, "SavedSearch.name");
	public final Definable<String> savedSearch = new Definable<>(false, "SavedSearch.savedSearch");
	public final Definable<ScopeEnum> scope = new Definable<>(false, "SavedSearch.scope");
	public final Definable<String> usage = new Definable<>(false, "SavedSearch.usage");
	public final Definable<Collection<String>> modifiers = new Definable<>(true, "SavedSearch.modifiers");
	public final Definable<String> createdByUserId = new Definable<>(true, "SavedSearch.createdByUserId");

	public SavedSearchPojoPrototype setId(final Long id) {
		this.id.set(id);
		return this;
	}

	@JsonAlias("clientId")
	public SavedSearchPojoPrototype setClient(@Nullable final EntityId client) {
		this.client.set(client == null ? null : this.client.orElseNonNull(EntityId.VOID).merge(client));
		return this;
	}

	@JsonAlias("projectId")
	public SavedSearchPojoPrototype setProject(@Nullable final EntityId project) {
		this.project.set(project == null ? null : this.project.orElseNonNull(EntityId.VOID).merge(project));
		return this;
	}

	public SavedSearchPojoPrototype setName(final String name) {
		this.name.set(name);
		return this;
	}

	public SavedSearchPojoPrototype setSavedSearch(final String savedSearch) {
		this.savedSearch.set(savedSearch);
		return this;
	}

	public SavedSearchPojoPrototype setScope(final ScopeEnum scope) {
		this.scope.set(scope);
		return this;
	}

	public SavedSearchPojoPrototype setUsage(final String usage) {
		this.usage.set(usage);
		return this;
	}

	public SavedSearchPojoPrototype setModifiers(final Collection<String> modifiers) {
		this.modifiers.set(modifiers);
		return this;
	}

	public SavedSearchPojoPrototype setCreatedByUserId(@Nullable final String createdByUserId) {
		this.createdByUserId.set(createdByUserId);
		return this;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id.orElse(null))
				.append("client", client.orElse(null))
				.append("project", project.orElse(null))
				.append("name", name.orElse(null))
				.append("savedSearch", savedSearch.orElse(null))
				.append("scope", scope.orElse(null))
				.append("usage", usage.orElse(null))
				.append("modifiers", modifiers.orElse(null))
				.append("createdBy", createdByUserId.orElse(null))
				.toString();
	}
}
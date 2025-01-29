/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.Objects;
import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.annotations.MiningDataType;

/**
 * Model class for a TaxonomyCategoryPojo entity
 */
@MiningDataType(name = MiningEnitityNames.TAXONOMY_CATEGORY)
public class TaxonomyCategoryPojo {
	private final Long id;
	private final EntityId project;
	private final String name;

	@JsonCreator
	public TaxonomyCategoryPojo(
			@JsonProperty("id") final Long id,
			@JsonProperty("projectEntity") @Nullable final EntityId project,
			@JsonProperty("project") @Nullable final UUID projectUid,
			@JsonProperty("projectId") @Nullable final Long projectNid,
			@JsonProperty("name") final String name) {
		this.id = id;
		this.project = project != null ? project : EntityId.of(projectUid, projectNid);
		this.name = name;
	}

	@JsonIgnore
	public EntityId getProject() {
		return project;
	}

	@JsonProperty("project")
	public UUID getProjectUid() {
		return project.getUid();
	}

	@JsonProperty("projectId")
	public Long getProjectNid() {
		return project.getNid();
	}

	/**
	 * @return the name of this category
	 */
	public String getName() {
		return name;
	}

	/**
	 * @return the id of this category
	 */
	public Long getId() {
		return id;
	}

	/**
	 * @return prototype equivalent of this pojo
	 */
	public TaxonomyCategoryPojoPrototype convertToPrototype() {
		return new TaxonomyCategoryPojoPrototype()
				.setId(this.getId())
				.setProject(this.getProject())
				.setName(this.getName());
	}
	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id)
				.append("project", project)
				.append("name", name)
				.toString();
	}

	@Override
	public int hashCode() {
		return Objects.hash(id, name, project);
	}

	@Override
	public boolean equals(@Nullable final Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		final TaxonomyCategoryPojo other = (TaxonomyCategoryPojo) obj;
		return Objects.equals(id, other.id) 
				&& Objects.equals(name, other.name) 
				&& Objects.equals(project, other.project);
	}
}

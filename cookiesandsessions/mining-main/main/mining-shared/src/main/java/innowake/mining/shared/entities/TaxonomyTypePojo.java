/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.Objects;
import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.annotations.MiningDataType;

/**
 * Model class for a taxonomy type.
 */
@MiningDataType(name = MiningEnitityNames.TAXONOMY_TYPE)
public class TaxonomyTypePojo {

	private final UUID id;
	private final EntityId project;
	private final TaxonomyCategoryPojo category;
	private final String name;

	@JsonCreator
	public TaxonomyTypePojo(
			@JsonProperty("id") final UUID id,
			@JsonProperty("projectEntity") @Nullable final EntityId project,
			@JsonProperty("project") @Nullable final UUID projectUid,
			@JsonProperty("projectId") @Nullable final Long projectNid,
			@JsonProperty("category") final TaxonomyCategoryPojo category,
			@JsonProperty("name") final String name) {
		this.id = id;
		this.project = project != null ? project : EntityId.of(projectUid, projectNid);
		this.category = category;
		this.name = name;
	}

	/**
	 * @return the Id of this type
	 */
	public UUID getId() {
		return id;
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
	 * @return the related category
	 */
	public TaxonomyCategoryPojo getCategory () {
		return category;
	}

	/**
	 * @return the name of this Type
	 */
	public String getName() {
		return name;
	}

	/**
	 * Creates a Prototype and assigns it the ID of this Pojo.
	 *
	 * @return prototype for updating this pojo
	 */
	@SuppressWarnings("null")
	public TaxonomyTypePojoPrototype convertToPrototype() {
		final TaxonomyTypePojoPrototype proto = new TaxonomyTypePojoPrototype()
				.setProject(project)
				.setCategoryId(category.getId())
				.setName(name);
		proto.setId(getId());
		return proto;
	}
	
	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id)
				.append("project", project)
				.append("category", category)
				.append("name", name)
				.toString();
	}

	@Override
	public int hashCode() {
		return Objects.hash(category, id, name, project);
	}

	@Override
	public boolean equals(@Nullable final Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		final TaxonomyTypePojo other = (TaxonomyTypePojo) obj;
		return Objects.equals(category, other.category) 
				&& Objects.equals(id, other.id) 
				&& Objects.equals(name, other.name)
				&& Objects.equals(project, other.project);
	}
}

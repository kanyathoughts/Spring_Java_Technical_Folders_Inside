/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.Objects;
import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.annotations.MiningDataType;

/**
 * TaxonomyPojo entity class.
 */
@MiningDataType(name = MiningEnitityNames.TAXONOMY)
public class TaxonomyPojo extends MiningPojo {
	private final EntityId project;
	private final TaxonomyTypePojo type;
	private final String name;
	private final long taxonomyReferenceCount;

	@JsonCreator
	public TaxonomyPojo(
			@JsonProperty("uid") final UUID uid,
			@JsonProperty("nid") @JsonAlias("id") final Long id,
			@JsonProperty("projectEntity") @Nullable final EntityId project,
			@JsonProperty("project") @Nullable final UUID projectUid,
			@JsonProperty("projectId") @Nullable final Long projectNid,
			@JsonProperty("type") final TaxonomyTypePojo type,
			@JsonProperty("name") final String name,
			@JsonProperty("taxonomyReferenceCount") final long taxonomyReferenceCount,
			@JsonProperty("customProperties") final CustomPropertiesMap customProperties
			) {
		super(EntityId.of(uid, id), customProperties);
		this.project = project != null ? project : EntityId.of(projectUid, projectNid);
		this.type = type;
		this.name = name;
		this.taxonomyReferenceCount = taxonomyReferenceCount;
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
	 * @return the related type
	 */
	public TaxonomyTypePojo getType() {
		return type;
	}

	/**
	 * @return the name of this Type
	 */
	public String getName() {
		return name;
	}
	
	/**
	 * @return the count of modules referencing this Taxonomy
	 */
	public long getTaxonomyReferenceCount() {
		return taxonomyReferenceCount;
	}

	/**
	 * @return prototype equivalent of this pojo
	 */
	@SuppressWarnings("null")
	public TaxonomyPojoPrototype convertToPrototype() {
		final var proto = new TaxonomyPojoPrototype()
				.setCustomProperties(this.getCustomProperties())
				.withId(this.identity())
				.setProject(this.getProject())
				.setName(this.getName())
				.setCustomProperties(getCustomProperties());

		/* id of type can be null in metadata import */
		if (this.getType().getId() != null) {
			proto.setType(this.getType().getId());
		}

		return proto;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("name", name)
				.append("project", project)
				.append("type", type)
				.toString();
	}

	@Override
	public int hashCode() {
		return Objects.hash(name, project, taxonomyReferenceCount, type);
	}

	@Override
	public boolean equals(@Nullable final Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		final TaxonomyPojo other = (TaxonomyPojo) obj;
		return Objects.equals(name, other.name) 
				&& Objects.equals(project, other.project) 
				&& taxonomyReferenceCount == other.taxonomyReferenceCount
				&& Objects.equals(type, other.type);
	}
}

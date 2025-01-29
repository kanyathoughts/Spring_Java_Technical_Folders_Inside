/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package db.migration.model.legacy;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.model.IdentifiableAndNameableEntity;

/**
 * Model class for a TaxonomyCategory entity
 */
@Deprecated(forRemoval = true)
public class TaxonomyCategory extends IdentifiableAndNameableEntity {

	/**
	 * When there is no actual TaxonomyCategory available this INVALID TaxonomyCategory is used as default, which helps to decide on setting the project's
	 * default taxonomy category
	 */
	public static final TaxonomyCategory INVALID = new TaxonomyCategory("Invalid Category Name", Long.valueOf(-1));

	private Long projectId;

	/**
	 * Constructor to initialize the fields
	 * 
	 * @param name the TaxonomyCategory name to set
	 * @param projectId the id of the project
	 */
	@JsonCreator
	public TaxonomyCategory(@JsonProperty("name") final String name, @JsonProperty("projectId") final Long projectId) {
		this.name = name;
		this.projectId = projectId;
	}
	
	/**
	 * Gets the projectId.
	 *
	 * @return projectId
	 */
	@Nullable
	public Long getProjectId() {
		return projectId;
	}

	/**
	 * Sets the id of the linked Project.
	 *
	 * @param projectId the projectId to set
	 */
	public void setProjectId(final Long projectId) {
		this.projectId = projectId;
	}
}

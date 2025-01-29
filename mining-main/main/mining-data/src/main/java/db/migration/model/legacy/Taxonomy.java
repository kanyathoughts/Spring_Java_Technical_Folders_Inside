/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package db.migration.model.legacy;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.Objects;

import org.apache.commons.lang.builder.ToStringBuilder;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.model.IdentifiableAndNameableEntity;

/**
 * Model class for a taxonomy.
 */
@Deprecated(forRemoval = true)
public class Taxonomy extends IdentifiableAndNameableEntity {
	
	@Nullable
	private Long projectId;
	@Nullable
	private TaxonomyType type;
	@Nullable
	private Integer taxonomyReferenceCount;
	/**
	 * Constructor to initialize the fields
	 */
	public Taxonomy() {	
	}
	/**
	 * Constructor to initialize the fields
	 * 
	 * @param name the taxonomy name
	 * @param type the TaxonomyType
	 */
	public Taxonomy(final String name, final TaxonomyType type) {
		this.name = name;
		this.type = type;
	}

	/**
	 * Gets the id of the linked Project.
	 *
	 * @return the id of the linked Project
	 */
	public Long getProjectId() {
		return assertNotNull(projectId, "Project id must not be null.");
	}

	/**
	 * Sets the id of the linked Project.
	 *
	 * @param projectId the id of the linked Project
	 */
	public void setProjectId(final long projectId) {
		this.projectId = Long.valueOf(projectId);
	}

	/**
	 * Sets the id of the linked Project.
	 *
	 * @param projectId the id of the linked Project
	 */
	public void setProjectId(final Long projectId) {
		this.projectId = projectId;
	}
	
	/**
	 * Gets the taxonomy type.
	 *
	 * @return the taxonomy type
	 */
	public TaxonomyType getType() {
		return assertNotNull(type, "Type must not be null.");
	}

	/**
	 * Sets the taxonomy type.
	 *
	 * @param type the taxonomy type
	 */
	public void setType(final TaxonomyType type) {
		this.type = type;
	}

	/**
	 * Returns the reference count for a project id.
	 *
	 * @return the count of reference for a project id
	 */
	@Nullable
	public Integer getTaxonomyReferenceCount() {
		return taxonomyReferenceCount;
	}

	/**
	 * Sets the count of reference for a project id.
	 *
	 * @param taxonomyReferenceCount the count of reference for a project id
	 */
	public void setTaxonomyReferenceCount(int taxonomyReferenceCount) {
		this.taxonomyReferenceCount = Integer.valueOf(taxonomyReferenceCount);
	}

	@Override
	public String toString() {
		final ToStringBuilder builder = new ToStringBuilder(this);
		builder.append("id", id);
		builder.append("recordId", recordId);
		builder.append("projectId", projectId);
		builder.append("type", type);
		builder.append("taxonomyReferenceCount", taxonomyReferenceCount);
		builder.append("name", name);
		builder.append("customProperties", customProperties);
		return builder.toString();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(projectId, type, name);
		return result;
	}

	@Override
	public boolean equals(@Nullable final Object obj) {
		if (this == obj) {
			return true;
		}
		if ( ! super.equals(obj)) {
			return false;
		}
		final Object nonNullObj = assertNotNull(obj);
		if (getClass() != nonNullObj.getClass()) {
			return false;
		}
		final Taxonomy other = (Taxonomy) nonNullObj;
		return Objects.equals(projectId, other.projectId) && Objects.equals(type, other.type) && Objects.equals(name, other.name);
	}
	
}

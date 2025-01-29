/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package db.migration.model.legacy;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.Serializable;

import com.google.common.base.Objects;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.model.NameableEntity;

/**
 * Model class for a taxonomy type.
 */
@Deprecated(forRemoval = true)
public class TaxonomyType extends NameableEntity implements Serializable {
	
	private static final long serialVersionUID = 1L;

	@Nullable
	private Long projectId;
	
	private TaxonomyCategory category;
	
	/**
	 * Default constructor which creates default taxonomy category
	 */
	public TaxonomyType() {
		this.category = TaxonomyCategory.INVALID;
	}
	
	/**
	 * Constructor to initialize the fields and which also creates default taxonomy category
	 * 
	 * @param name the taxonomy type name 
	 * @param projectId the project id
	 */
	public TaxonomyType(final String name, final Long projectId) {
		this(name, projectId, TaxonomyCategory.INVALID);
	}

	/**
	 * Constructor to initialize the fields
	 * 
	 * @param name the taxonomy type name
	 * @param projectId the project id
	 * @param category the taxonomy category object
	 */
	public TaxonomyType(final String name, final Long projectId, final TaxonomyCategory category) {
		this.name = name;
		this.projectId = projectId;
		this.category = category;
	}
	
	/**
	 * Constructor to initialize the fields
	 * 
	 * @param name the taxonomy type name
	 */
	public TaxonomyType(final String name) {
		this.category = TaxonomyCategory.INVALID;

		this.name = name;
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
	 * Gets the taxonomy category
	 *
	 * @return taxonomy category object
	 */
	public TaxonomyCategory getCategory() {
		return assertNotNull(category, "Taxonomy category must not be null");
	}

	/**
	 * Sets the taxonomy category
	 *
	 * @param category taxonomy category object
	 */
	public void setCategory(final TaxonomyCategory category) {
		this.category = category;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + category.hashCode();
		result = prime * result + ((name != null) ? name.hashCode() : 0);
		result = prime * result + ((projectId != null) ? projectId.hashCode() : 0);
		return result;
	}

	@Override
	public boolean equals(@Nullable final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null || getClass() != obj.getClass() || ! super.equals(obj)) {
			return false;
		}

		final TaxonomyType other = (TaxonomyType) obj;
		return Objects.equal(category, other.category) && 
				   Objects.equal(name, other.name) && 
				   Objects.equal(projectId, other.projectId);
	}
	
	
}

/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import static innowake.lib.core.lang.Assert.assertNotNull;

import innowake.lib.core.api.lang.NonNull;
import innowake.lib.core.api.lang.Nullable;

/**
 * Model class for a feature entity.
 */
public class Feature extends Entity {
	
	@Nullable
	private FeatureId id;
	@Nullable
	private String description;

	private boolean enabled;
	
	/**
	 * Constructor.
	 */
	public Feature() {
	}

	/**
	 * Constructor.
	 * 
	 * @param id the feature id
	 * @param description the feature description
	 * @param enabled the feature status
	 */
	public Feature(final FeatureId id, final String description, final boolean enabled) {
		this.id = id;
		this.description = description;
		this.enabled = enabled;
	}

	/**
	 * Sets the feature ID.
	 *
	 * @param id the feature ID
	 */
	public void setId(final FeatureId id) {
		this.id = id;
	}

	/**
	 * Returns the feature ID.
	 * 
	 * @return the feature ID
	 */
	@Nullable
	public FeatureId getId() {
		return id;
	}
	
	/**
	 * Sets the feature description.
	 *
	 * @param description the feature description
	 */
	public void setDescription(final String description) {
		this.description = description;
	}

	/**
	 * Returns the feature description.
	 * 
	 * @return the feature description
	 */
	@Nullable
	public String getDescription() {
		return description;
	}
	
	/**
	 * Sets if the feature is enabled.
	 *
	 * @param enabled {@code true} if the feature is enabled
	 */
	public void setEnabled(final boolean enabled) {
		this.enabled = enabled;
	}

	/**
	 * Returns if the feature is enabled.
	 * 
	 * @return {@code true} if the feature is enabled
	 */
	public boolean isEnabled() {
		return enabled;
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((description == null) ? 0 : assertNotNull(description).hashCode());
		result = prime * result + (enabled ? 1231 : 1237);
		result = prime * result + ((id == null) ? 0 : assertNotNull(id).hashCode());
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
		if (getClass() != assertNotNull(obj).getClass()) {
			return false;
		}
		@NonNull
		final Feature other = assertNotNull((Feature) obj);
		if (description == null) {
			if (other.description != null) {
				return false;
			}
		} else if ( ! assertNotNull(description).equals(other.description)) {
			return false;
		}
		if (enabled != other.enabled) {
			return false;
		}
		if (id == null) {
			if (other.id != null) {
				return false;
			}
		} else if ( id != other.id) {
			return false;
		}
		return true;
	}

}

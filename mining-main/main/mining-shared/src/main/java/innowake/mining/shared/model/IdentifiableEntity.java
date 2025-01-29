/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import static innowake.lib.core.lang.Assert.assertNotNull;

import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.commons.lang.builder.ToStringStyle;

import innowake.lib.core.api.lang.Nullable;

/**
 * Base class for an entity with an id.
 */
public abstract class IdentifiableEntity extends Entity {

	private static final long serialVersionUID = 1L;
	
	@Nullable
	protected Long id;

	/**
	 * Gets the id.
	 *
	 * @return the id
	 */
	public Long getId() {
		return assertNotNull(id, "Id must not be null.");
	}
	
	/**
	 * Sets the id.
	 *
	 * @param id the id
	 */
	public void setId(final long id) {
		this.id = Long.valueOf(id);
	}
	
	/**
	 * Sets the id.
	 *
	 * @param id the id
	 */
	public void setId(final Long id) {
		this.id = id;
	}
	
	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.SHORT_PREFIX_STYLE)
				.append("rid", recordId)
				.append("id", id)
				.toString();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((id == null) ? 0 : getId().hashCode());
		return result;
	}

	@Override
	public boolean equals(@Nullable final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		final IdentifiableEntity other = (IdentifiableEntity) obj;
		if (id == null) {
			if (other.id != null) {
				return false;
			}
		} else if ( ! getId().equals(other.id)) {
			return false;
		}
		return true;
	}
}

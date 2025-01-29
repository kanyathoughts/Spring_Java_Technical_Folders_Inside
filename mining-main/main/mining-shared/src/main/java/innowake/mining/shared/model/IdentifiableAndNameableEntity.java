/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.Objects;

import org.apache.commons.lang.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnore;
import innowake.lib.core.api.lang.Nullable;

/**
 * Base class for an entity with an id and a name.
 */
public abstract class IdentifiableAndNameableEntity extends IdentifiableEntity {

	@Nullable
	protected String name;
	
	/**
	 * Gets the name.
	 *
	 * @return the name
	 */
	public String getName() {
		return assertNotNull(name, "Name must not be null.");
	}
	
	/**
	 * Sets the name.
	 *
	 * @param name the name
	 */
	public void setName(final String name) {
		this.name = name;
	}
	
	/**
	 * Checks whether the id is null or not
	 *
	 * @return true if the id is null else false
	 */
	@JsonIgnore
	public boolean isIdNull() {
		return id == null;
	}
	
	@Override
	public String toString() {
		final ToStringBuilder builder = new ToStringBuilder(this);
		builder.appendSuper(super.toString());
		builder.append("id", id);
		builder.append("name", name);
		return builder.toString();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(id, name);
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
		final IdentifiableAndNameableEntity other = (IdentifiableAndNameableEntity) nonNullObj;
		return Objects.equals(id, other.id) && Objects.equals(name, other.name);
	}
	
}

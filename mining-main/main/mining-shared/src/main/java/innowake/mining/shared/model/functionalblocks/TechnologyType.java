/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.shared.model.functionalblocks;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

import java.util.Objects;

/**
 * Pojo used for Holding a pair of Technology and Type.
 */
public class TechnologyType {

	private final Technology technology;
	private final Type type;

	/**
	 * Constructor for TechnologyType.
	 *
	 * @param technology Technology
	 * @param type Type
	 */
	@JsonCreator
	public TechnologyType(@JsonProperty("technology") final Technology technology, @JsonProperty("type") final Type type) {
		this.technology = technology;
		this.type = type;
	}

	public TechnologyType() {
		this.technology = null;
		this.type = null;
	}

	/**
	 * Gets the technology.
	 *
	 * @return the technology
	 */
	public Technology getTechnology() {
		return technology;
	}

	/**
	 * Gets the type.
	 *
	 * @return the type
	 */
	public Type getType() {
		return type;
	}

	@Override
	public boolean equals(@Nullable final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null || getClass() != obj.getClass()) {
			return false;
		}
		final TechnologyType that = (TechnologyType) obj;
		return Objects.equals(technology, that.technology) && Objects.equals(type, that.type);
	}

	@Override
	public int hashCode() {
		return Objects.hash(technology, type);
	}
}

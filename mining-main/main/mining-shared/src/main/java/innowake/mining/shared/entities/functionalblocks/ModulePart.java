/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.entities.functionalblocks;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.model.ModuleLocation;

import java.util.Objects;
import java.util.Optional;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * References a part of a module or an entire module. This class is part of the definition of a functional block.
 */
public class ModulePart {

	private final String moduleLinkHash;
	@Nullable
	private final ModuleLocation location;

	/**
	 * Creates a ModulePart that references an entire module
	 * @param moduleLinkHash the link hash of the referenced module
	 */
	public ModulePart(@JsonProperty("moduleLinkHash") final String moduleLinkHash) {
		this(moduleLinkHash, null);
	}

	/**
	 * Creates a ModulePart that references a specific source code location in a module.
	 * @param moduleLinkHash the link hash of the referenced module
	 * @param location the source code location inside the module
	 */
	@JsonCreator
	public ModulePart(@JsonProperty("moduleLinkHash") final String moduleLinkHash, @JsonProperty("location") @Nullable final ModuleLocation location) {
		this.moduleLinkHash = moduleLinkHash;
		this.location = location;
	}

	/**
	 * Returns the link hash of the referenced module
	 * @return the link hash of the referenced module
	 */
	public String getModuleLinkHash() {
		return moduleLinkHash;
	}

	/**
	 * Returns the referenced location inside the referenced module or {@link Optional#empty()}
	 * if the entire module is referenced.
	 * @return the referenced location inside the referenced module
	 */
	public Optional<ModuleLocation> getLocation() {
		return Optional.ofNullable(location);
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) return true;
		if (o == null || getClass() != o.getClass()) return false;
		final ModulePart that = (ModulePart) o;
		return Objects.equals(moduleLinkHash, that.moduleLinkHash) && Objects.equals(location, that.location);
	}

	@Override
	public int hashCode() {
		return Objects.hash(moduleLinkHash, location);
	}

	@Override
	public String toString() {
		return "ModulePart{" +
				"moduleLinkHash='" + moduleLinkHash + '\'' +
				", location=" + location +
				'}';
	}
}

/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.entities.functionalblocks;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.model.ModuleLocation;

import java.util.Objects;
import java.util.Optional;

/**
 * References a part of a module or an entire module. This class defines an effective module part that is associated with a functional block,
 * computed from the union of module parts of the block itself and its children.
 */
@MiningDataType(name = "ResolvedModulePart")
public class ResolvedModulePart {

	private final EntityId moduleId;
	@Nullable
	private final ModuleLocation location;

	/**
	 * Creates a ModulePart that references an entire module
	 * @param moduleId the id of the referenced module
	 */
	public ResolvedModulePart(final EntityId moduleId) {
		this(moduleId, null);
	}

	/**
	 * Creates a ModulePart that references a specific source code location in a module.
	 * @param moduleId the id of the referenced module
	 * @param location the source code location inside the module
	 */
	public ResolvedModulePart(final EntityId moduleId, @Nullable final ModuleLocation location) {
		this.moduleId = moduleId;
		this.location = location;
	}

	/**
	 * Returns the id of the referenced module.
	 * @return the id of the referenced module
	 */
	public EntityId getModuleId() {
		return moduleId;
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
		if (this == o) {
			return true;
		}
		if (o == null || getClass() != o.getClass()) {
			return false;
		}
		final ResolvedModulePart that = (ResolvedModulePart) o;
		return Objects.equals(moduleId, that.moduleId) && Objects.equals(location, that.location);
	}

	@Override
	public int hashCode() {
		return Objects.hash(moduleId, location);
	}

	@Override
	public String toString() {
		return "ResolvedModulePart{" +
				"moduleId=" + moduleId +
				", location=" + location +
				'}';
	}
}

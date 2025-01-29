/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.shared.entities;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

import java.util.Objects;
import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Lightweight POJO representing a Module.
 */
public class ModuleLightweightPojo extends ModuleBasePojo {

	private final EntityId project;
	@Nullable
	private final EntityId parent;
	@Nullable
	private final String parentPath;

	public ModuleLightweightPojo(final ModulePojo module) {
		super(
			module.identity(),
			module.getName(),
			module.getPath().orElse(null),
			module.getTechnology(),
			module.getType(),
			module.getLinkHash(),
			module.getRepresentation().orElse(null),
			module.getIdentification()
		);
		project = module.getProject();
		parent = module.getParent().orElse(null);
		parentPath = module.getParentPath().orElse(null);
	}

	@JsonCreator
	public ModuleLightweightPojo(
			@JsonProperty("uid") final UUID uid,
			@JsonProperty("id") final Long id,
			@JsonProperty("projectEntity") @Nullable final EntityId project,
			@JsonProperty("project") @Nullable final UUID projectUid,
			@JsonProperty("projectId") @Nullable final Long projectNid,
			@JsonProperty("name") final String name,
			@JsonProperty("path") @Nullable final String path,
			@JsonProperty("technology") final Technology technology,
			@JsonProperty("type") final Type type,
			@JsonProperty("linkHash") @Nullable final String linkHash,
			@JsonProperty("representation") @Nullable final String representation,
			@JsonProperty("identified") final boolean identified,
			@JsonProperty("parentEntity") @Nullable final EntityId parent,
			@JsonProperty("parent") @Nullable final UUID parentUid,
			@JsonProperty("parentId") @Nullable final Long parentNid,
			@JsonProperty("parentPath") @Nullable final String parentPath) {
		super(EntityId.of(uid, id), name, path, technology, type, linkHash,
				representation == null ? null : Representation.valueOf(representation),
				identified ? Identification.IDENTIFIED : Identification.MISSING);
		this.project = project != null ? project : EntityId.of(projectUid, projectNid);
		this.parent = parent != null ? parent : EntityId.orNull(parentUid, parentNid);
		this.parentPath = parentPath;
	}
	
	/**
	 * @return Project ID.
	 */
	@JsonIgnore
	public EntityId getProject() {
		return project;
	}

	/**
	 * @return Unique Project ID.
	 */
	@Nullable
	@JsonProperty("project")
	public UUID getProjectUid() {
		return project.getUid();
	}

	/**
	 * @return Numeric Project ID.
	 */
	@Nullable
	@JsonProperty("projectId")
	public Long getProjectNid() {
		return project.getNid();
	}

	/**
	 * @return the {@link EntityId} of the parent (containing module)
	 */
	@Nullable
	@JsonIgnore
	public EntityId getParent() {
		return parent;
	}

	/**
	 * @return the {@link UUID} of the parent (containing module)
	 */
	@Nullable
	@JsonProperty("parent")
	public UUID getParentUid() {
		return parent != null ? parent.getUid() : null;
	}

	/**
	 * @return the numeric id of the parent (containing module)
	 */
	@Nullable
	@JsonProperty("parentId")
	public Long getParentNid() {
		return parent != null ? parent.getNid() : null;
	}

	/**
	 * @return the path of the parent (containing module)
	 */
	@Nullable
	public String getParentPath() {
		return parentPath;
	}

	@Override
	public boolean equals(@Nullable final Object o) {
		if (this == o) {
			return true;
		}
		if (o == null || getClass() != o.getClass()) {
			return false;
		}
		final ModuleLightweightPojo that = (ModuleLightweightPojo) o;
		return getId().equals(that.getId());
	}

	@Override
	public int hashCode() {
		return Objects.hash(getId());
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id)
				.append("name", name)
				.append("technology", technology)
				.append("type", type)
				.append("path", path)
				.append("parent", parent)
				.append("parentPath", parentPath)
				.append("identification", identification)
				.append("linkHash", linkHash)
				.append("representation", representation)
			.build();
	}
	
}

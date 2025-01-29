/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.entities.functionalblocks;

import java.time.Instant;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.entities.MiningPojo;

/**
 * Pojo representing the <i>definition part</i> of a Functional Block.
 * <p>
 * This object contains the static, persistent information about a block from which other computed properties can be derived.
 */
@MiningDataType(name = MiningEnitityNames.FUNCTIONAL_BLOCK)
public class FunctionalBlockPojo extends MiningPojo {

	private final EntityId project;
	private final List<ModulePart> moduleParts;
	private final List<UUID> parents;
	private final List<UUID> children;
	private final String name;
	private final String description;
	private final Map<String, Object> flags;
	private final Instant updated;

	@JsonCreator
	public FunctionalBlockPojo(@JsonProperty("uid") final UUID uid,
							   @JsonProperty("customProperties") final CustomPropertiesMap customProperties,
							   @JsonProperty("projectId") final EntityId project,
							   @JsonProperty("moduleParts") final List<ModulePart> moduleParts,
							   @JsonProperty("parents") final List<UUID> parents,
							   @JsonProperty("children") final List<UUID> children,
							   @JsonProperty("name") final String name,
							   @JsonProperty("description") final String description,
							   @JsonProperty("flags") final Map<String, Object> flags,
							   @JsonProperty("updated") final Instant updated) {
		super(EntityId.of(uid), customProperties);
		this.project = project;
		this.moduleParts = moduleParts;
		this.parents = parents;
		this.children = children;
		this.name = name;
		this.description = description;
		this.flags = flags;
		this.updated = updated;
	}
	
	public FunctionalBlockPojo(@JsonProperty("uid") final UUID uid, @JsonProperty("name") final String name, @JsonProperty("children") final UUID childUid,
			@JsonProperty("projectId")  final EntityId project) {
		super(EntityId.of(uid), CustomPropertiesMap.empty());
		this.name = name;
		this.project = project;
		this.moduleParts = Collections.emptyList();
		this.parents = Collections.emptyList();
		this.children = Arrays.asList(childUid);
		this.description = "";
		this.flags = Collections.emptyMap();
		this.updated = null;
	}

	@Override
	@JsonIgnore /* overridden to fix JSON serialization, because FunctionalBlockPojo has no numeric identifier */
	public Long getId() {
		return super.getId();
	}

	/**
	 * Returns the project that contains this functional block.
	 * @return the id of the project
	 */
	public EntityId getProject() {
		return project;
	}

	/**
	 * Returns the list of module parts associated with this block.
	 * @return list of module parts
	 */
	public List<ModulePart> getModuleParts() {
		return moduleParts;
	}

	/**
	 * Returns the ids of parent blocks (other blocks that contain this block as child).
	 * @return the list of parents
	 */
	public List<UUID> getParents() {
		return parents;
	}

	/**
	 * Returns the ids of child blocks of this block.
	 * @return the list of children
	 */
	public List<UUID> getChildren() {
		return children;
	}

	/**
	 * Returns the name of this block.
	 * @return the block name
	 */
	public String getName() {
		return name;
	}

	/**
	 * Returns the description of this block.
	 * @return the block description
	 */
	public String getDescription() {
		return description;
	}

	/**
	 * Returns the flags set for this functional block. Flags are key-value pairs meant for internal use (i.e. not user-definable) to record
	 * additional data about a functional block.
	 * <p>
	 * The keys of the flags map should be taken from the {@link FunctionalBlockFlag} enum.
	 * @return the flags of the functional block
	 */
	public Map<String, Object> getFlags() {
		return flags;
	}


	/**
	 * Returns the timestamp when the block was last modified.
	 * @return the last modified timestamp
	 */
	public Instant getUpdated() {
		return updated;
	}
	
	/**
	 * Determines whether the functional block is of a certain type
	 * @param type functional block type
	 * @return {@code true} if types of block contain specified type in Enum or String representation - {@code false} otherwise
	 */
	public boolean isOfType(final FunctionalBlockType type) {
		if (flags == null) {
			return false;
		}
		final Object types = flags.get("TYPE");
		final String typeAsString = type.toString();
		return types instanceof Collection<?> && ((Collection<?>) types).parallelStream()
				.anyMatch(o -> type.equals(o) || (o instanceof String && typeAsString.equalsIgnoreCase((String) o)));
	}

	@Override
	public String toString() {
		return "FunctionalBlockPojo{" +
				"identity=" + identity() +
				", project=" + project +
				", moduleParts=" + moduleParts +
				", parents=" + parents +
				", children=" + children +
				", name='" + name + '\'' +
				", description='" + description + '\'' +
				", flags=" + flags +
				", updated=" + updated +
				", customProperties=" + getCustomProperties() +
				'}';
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) {
			return true;
		}
		if ( ! (o instanceof FunctionalBlockPojo)) {
			return false;
		}
		final FunctionalBlockPojo that = (FunctionalBlockPojo) o;
		return Objects.equals(getUid(), that.getUid())
				&& Objects.equals(getProject(), that.getProject())
				&& Objects.equals(getModuleParts(), that.getModuleParts())
				&& Objects.equals(getParents(), that.getParents())
				&& Objects.equals(getChildren(), that.getChildren())
				&& Objects.equals(getName(), that.getName())
				&& Objects.equals(getDescription(), that.getDescription())
				&& Objects.equals(getFlags(), that.getFlags())
				&& Objects.equals(getUpdated(), that.getUpdated());
	}

	@Override
	public int hashCode() {
		return Objects.hash(getUid(), getProject(), getModuleParts(), getParents(), getChildren(), getName(), getDescription(), getFlags(), getUpdated());
	}
}

/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.Collections;
import java.util.Map;
import java.util.UUID;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Basic representation of a Module.
 */
public class ModuleBasePojo {
	
	protected final EntityId id;
	protected final String name;
	protected final Technology technology;
	protected final Type type;
	@Nullable
	protected final String path;
	@Nullable
	protected final Identification identification;
	@Nullable
	protected final String linkHash;
	@Nullable
	protected final Representation representation;
	@Nullable
	private final Map<String, Object> info;

	private final int errorCount;

	public ModuleBasePojo(
			final EntityId id,
			final String name,
			@Nullable final String path,
			final Technology technology,
			final Type type,
			@Nullable final String linkHash,
			@Nullable final Representation representation,
			final Identification identified) {
		this.id = id;
		this.name = name;
		this.technology = technology;
		this.type = type;
		this.path = path;
		this.identification = identified;
		this.linkHash = linkHash;
		this.representation = representation;
		this.info = Collections.emptyMap();
		this.errorCount = -1;
	}
	
	public ModuleBasePojo(
			final EntityId id,
			final String name,
			@Nullable final String path,
			final Technology technology,
			final Type type,
			@Nullable final String linkHash,
			@Nullable final Representation representation,
			final Identification identified,
			@Nullable final Map<String, Object> info,
			final int errorCount) {
		this.id = id;
		this.name = name;
		this.technology = technology;
		this.type = type;
		this.path = path;
		this.identification = identified;
		this.linkHash = linkHash;
		this.representation = representation;
		this.info = info;
		this.errorCount = errorCount;
	}
	
	/**
	 * Gets the ID of the entity.
	 * @return Wrapped identifier(s) of the entity.
	 */
	public EntityId identity() {
		return id;
	}

	/**
	 * @return the moduleType of the module
	 */
	public ModuleType getModuleType() {
		return ModuleType.fromTechnologyAndType(technology, type);
	}

	/**
	 * @return the {@link Technology} of the module
	 */
	public Technology getTechnology() {
		return technology;
	}

	/**
	 * @return the {@link Type} of the module
	 */
	public Type getType() {
		return type;
	}

	/**
	 * @return the path of the module
	 */
	@Nullable
	public String getPath() {
		return path;
	}

	/**
	 * @return {@code true} if this module was identified. Otherwise {@code false}
	 */
	@Nullable
	public Identification getIdentification() {
		return identification;
	}

	/**
	 * @return the link hash
	 */
	@Nullable
	public String getLinkHash() {
		return linkHash;
	}

	/**
	 * @return the id of the module
	 */
	public Long getId() {
		return id.getNid();
	}

	/**
	 * @return the uid of the module
	 */
	public UUID getUid() {
		return id.getUid();
	}

	/**
	 * @return the representation of this module
	 */
	@Nullable
	public Representation getRepresentation() {
		return representation;
	}

	/**
	 * @return the name of the module
	 */
	public String getName() {
		return name;
	}
	
	/**
	 * @return a map containing additional meta information of this module
	 */
	@Nullable
	public Map<String, Object> getInfo() {
		return info;
	}

	/**
	 * @return the error count of the module, return -1 if error count is not fetched from database.
	 */
	public int getErrorCount() {
		return errorCount;
	}

}

/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.entities.functionalblocks;

import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.MiningPojoPrototype;

import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * Pojo prototype used to update the definition of a functional block.
 */
public class FunctionalBlockPojoPrototype extends MiningPojoPrototype<FunctionalBlockPojoPrototype> {

	public final Definable<EntityId> project = new Definable<>(false, "FunctionalBlock.project");
	public final Definable<List<ModulePart>> moduleParts = new Definable<>(false, "FunctionalBlock.moduleParts");
	public final Definable<List<UUID>> children = new Definable<>(false, "FunctionalBlock.children");
	public final Definable<String> name = new Definable<>(false, "FunctionalBlock.name");
	public final Definable<String> description = new Definable<>(false, "FunctionalBlock.description");
	public final Definable<Map<String, Object>> flags = new Definable<>(false, "FunctionalBlock.flags");

	public FunctionalBlockPojoPrototype() {
		super("FunctionalBlock");
	}
	
	/**
	 * Sets the projectId of the block.
	 * @param project the projectId of the block
	 * @return this object
	 */
	public FunctionalBlockPojoPrototype setProject(final EntityId project) {
		this.project.set(project);
		return this;
	}

	/**
	 * Sets (replaces) the module parts definition of the block.
	 * @param moduleParts the new module parts to set on the block
	 * @return this object
	 */
	public FunctionalBlockPojoPrototype setModuleParts(final List<ModulePart> moduleParts) {
		this.moduleParts.set(moduleParts);
		return this;
	}

	/**
	 * Sets (replaces) the children of the block.
	 * @param children the new children of the block
	 * @return this object
	 */
	public FunctionalBlockPojoPrototype setChildren(final List<UUID> children) {
		this.children.set(children);
		return this;
	}

	/**
	 * Sets a new name for the block.
	 * @param name the new name
	 * @return this object
	 */
	public FunctionalBlockPojoPrototype setName(final String name) {
		this.name.set(name);
		return this;
	}

	/**
	 * Sets a new description for the block.
	 * @param description the new description
	 * @return this object
	 */
	public FunctionalBlockPojoPrototype setDescription(final String description) {
		this.description.set(description);
		return this;
	}

	/**
	 * Sets flags for the functional block. Flags are key-value pairs meant for internal use (i.e. not user-definable) to record
	 * additional data about a functional block.
	 * <p>
	 * The keys of the flags map should be taken from the {@link FunctionalBlockFlag} enum.
	 * @param flags the new flags
	 * @return this object
	 */
	public FunctionalBlockPojoPrototype setFlags(final Map<String, Object> flags) {
		this.flags.set(flags);
		return this;
	}

}

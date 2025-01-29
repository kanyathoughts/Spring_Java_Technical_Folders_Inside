/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.shared.entities.functionalblocks;

import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.MiningPojoPrototype;

import java.util.Collection;

/**
 * Pojo prototype used to update the definition of a ReachabilityData block.
 */
public class ReachabilityDataPojoPrototype extends MiningPojoPrototype<ReachabilityDataPojoPrototype> {

	public final Definable<EntityId> upperBoundModuleId = new Definable<>(true, "ReachabilityData.upperBoundModuleId");
	public final Definable<EntityId> lowerBoundModuleId = new Definable<>(false, "ReachabilityData.lowerBoundModuleId");
	public final Definable<EntityId> accessModuleId = new Definable<>(false, "ReachabilityData.accessModuleId");
	public final Definable<Collection<String>> accessTypes = new Definable<>(false, "ReachabilityData.accessTypes");
	public final Definable<Collection<EntityId>> intermediateModules = new Definable<>(false, "ReachabilityData.intermediateModules");

	public ReachabilityDataPojoPrototype() {
		super("ReachabilityData");
	}

	/**
	 * Sets the id of the "upper bound" module.
	 * @param upperBoundModuleId the id of the "upper bound" module.
	 * @return this object
	 */
	public ReachabilityDataPojoPrototype setUpperBoundModuleId(final EntityId upperBoundModuleId) {
		this.upperBoundModuleId.set(upperBoundModuleId);
		return this;
	}

	/**
	 * Sets the id of the "lower bound" module.
	 * @param lowerBoundModuleId the id of the "lower bound" module.
	 * @return this object
	 */
	public ReachabilityDataPojoPrototype setLowerBoundModuleId(final EntityId lowerBoundModuleId) {
		this.lowerBoundModuleId.set(lowerBoundModuleId);
		return this;
	}

	/**
	 * Sets the id of the access module.
	 * @param accessModuleId the id of the access module
	 * @return this object
	 */
	public ReachabilityDataPojoPrototype setAccessModuleId(final EntityId accessModuleId) {
		this.accessModuleId.set(accessModuleId);
		return this;
	}

	/**
	 * Sets the access types by which the upper bound accesses the lower bound.
	 * @param accessTypes the accessTypes of the block
	 * @return this object
	 */
	public ReachabilityDataPojoPrototype setAccessTypes(final Collection<String> accessTypes) {
		this.accessTypes.set(accessTypes);
		return this;
	}

	/**
	 * Sets the intermediateModules of the block.
	 * @param intermediateModules the intermediateModules of the block
	 * @return this object
	 */
	public ReachabilityDataPojoPrototype setIntermediateModules(final Collection<EntityId> intermediateModules) {
		this.intermediateModules.set(intermediateModules);
		return this;
	}
}

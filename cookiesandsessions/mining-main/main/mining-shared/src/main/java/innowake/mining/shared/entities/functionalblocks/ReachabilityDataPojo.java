/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.entities.functionalblocks;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.annotations.MiningDataType;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

/**
 * Entity storing "tabular data" for blocks of type {@link FunctionalBlockType#REACHABILITY}.
 * <p>
 * Basically one such entity (i.e. "one row") is stored for each combination of
 * <ul>
 *     <li>{@linkplain FunctionalBlockType#RA_UPPER_BOUND upper bound} module</li>
 *     <li>{@linkplain FunctionalBlockType#RA_LOWER_BOUND lower bound} module</li>
 *     <li>{@linkplain FunctionalBlockType#RA_ACCESS_MODULE access} module</li>
 *     <li>access statement (if supported - not implemented yet)</li>
 * </ul>
 * However, data is usually consumed in aggregated form - e.g. grouped by upper and lower bound. In this case {@link #getAccessModuleIds()}
 * and {@link #getAccessTypes()} may return multiple values.
 */
@MiningDataType(name = "ReachabilityData")
public class ReachabilityDataPojo {

	private final UUID uid;
	private final UUID functionalBlock;
	private final EntityId upperBoundModuleId;
	private final Optional<EntityId> lowerBoundModuleId;
	private final List<EntityId> accessModuleIds;
	private final List<String> accessTypes;
	private final List<EntityId> intermediateModules;

	public ReachabilityDataPojo(final UUID uid, final UUID functionalBlock, final EntityId upperBoundModuleId, final Optional<EntityId> lowerBoundModuleId,
							final List<EntityId> accessModuleIds, final List<String> accessTypes, final List<EntityId> intermediateModules) {
		this.uid = uid;
		this.functionalBlock = functionalBlock;
		this.upperBoundModuleId = upperBoundModuleId;
		this.lowerBoundModuleId = lowerBoundModuleId;
		this.accessModuleIds = accessModuleIds;
		this.accessTypes = accessTypes;
		this.intermediateModules = intermediateModules;
	}

	/**
	 * Returns the uid of this {@code ReachabilityData} entity.
	 * @return the uid of this entity
	 */
	public UUID getUid() {
		return uid;
	}

	/**
	 * Returns the uid of the functional block to which this {@code ReachabilityData} entity is attached.
	 * @return uid of the functional block
	 */
	public UUID getFunctionalBlock() {
		return functionalBlock;
	}

	/**
	 * Returns the id of the "upper bound" module.
	 * @return id of the upper bound module
	 */
	public EntityId getUpperBoundModuleId() {
		return upperBoundModuleId;
	}

	/**
	 * Returns the id of the "lower bound" module.
	 * @return id of the lower bound module
	 */
	public Optional<EntityId> getLowerBoundModuleId() {
		return lowerBoundModuleId;
	}

	/**
	 * Returns the ids of the access module. The access module is the module that sits directly before the "lower bound" in the call chain.
	 * List may contain 0 entries when the access module is unknown and more than 1 entry when grouping by upper bound / lower bound.
	 * @return ids of the access modules
	 */
	public List<EntityId> getAccessModuleIds() {
		return accessModuleIds;
	}

	/**
	 * Returns the access types by which the upper bound accesses the lower bound.
	 * List may contain 0 entries when the access type is unknown and more than 1 entry when grouping by upper bound / lower bound.
	 * @return the access types
	 */
	public List<String> getAccessTypes() {
		return accessTypes;
	}

	/**
	 * Returns the intermediateModules of the block.
	 * @return the intermediateModules of the block
	 */
	public List<EntityId> getIntermediateModules() {
		return intermediateModules;
	}
}

/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.model.discovery.dna;

import java.util.UUID;

import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.datapoints.annotations.Usage;
import innowake.mining.shared.datapoints.annotations.UsageAttribute;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.miningui.TableAttributes;
import innowake.mining.shared.entities.dna.DnaCommunityPojo;

/**
 * Mining Data Type class for {@code ModuleInDNACluster} query
 */
@MiningDataType(name = "ModuleInDNACluster")
public class ModuleInDNACluster {
	
	private final ModulePojo module;
	private final Integer clusterIndex;
	private final UUID communityId;
	
	/**
	 * The constructor
	 * 
	 * @param module the module details
	 * @param clusterIndex the index of the cluster
	 * @param communityId  the uuid of the {@link DnaCommunityPojo}
	 */
	public ModuleInDNACluster(final ModulePojo module, final Integer clusterIndex, final UUID communityId) {
		this.module = module;
		this.clusterIndex = clusterIndex;
		this.communityId = communityId;
	}
	
	/**
	 * Get the module details
	 * 
	 * @return the module
	 */
	@MiningDataPoint(displayName = "Module", description = "The Module in the Cluster")
	public ModulePojo getModule() {
		return module;
	}
	
	/**
	 * Get the index of the cluster
	 * 
	 * @return the cluster index
	 */
	@MiningDataPoint(displayName = "DNA Cluster Index", description = "The index of the DNA Cluster this Module belongs to")
	@Usage(value = Usages.MINING_UI_MODULES_IN_DNA_CLUSTER_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "2")
	})
	public Integer getClusterIndex() {
		return clusterIndex;
	}

	/**
	 * Get the DNA Community uuid
	 *
	 * @return the DNA Community uuid
	 */
	@MiningDataPoint(displayName = "DNA Community UUID", description = "The UUID of the DNA Community")
	public UUID getCommunityId() {
		return communityId;
	}
}

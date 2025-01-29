/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.data.model.discovery.dna;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.google.gson.annotations.SerializedName;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.datapoints.annotations.MiningDataType;

/**
 * DNA Clusters are formed by applying multiple algorithms over the application code repository. 
 * Each algorithm may or may not produce cluster and such clusters are stored in this model.
 */
@MiningDataType(name = "ModelCluster")
public class ModelCluster {

	@SerializedName("moduleCount")
	private int moduleCount;
	@Nullable
	@SerializedName("clusterDescription")
	private String clusterDescription;
	@SerializedName("clusterIndex")
	private int clusterIndex;
	@SerializedName("communityId")
	private String communityId;
	@Nullable
	@SerializedName("clusterTitle")
	private String clusterTitle;

	/**
	 * Parameterized Constructor for ModelCluster.
	 * 
	 * @param moduleCount The number of modules under the cluster index of an algorithm.
	 * @param clusterDescription The DNA cluster description.
	 * @param clusterIndex The index of the cluster
	 * @param communityId The DNA community UUID
	 * @param clusterTitle The DNA cluster title
	 */
	@JsonCreator
	public ModelCluster(@JsonProperty("moduleCount") final int moduleCount, @JsonProperty("clusterDescription") @Nullable final String clusterDescription,
			@JsonProperty("clusterIndex") final int clusterIndex, @JsonProperty("communityId") final String communityId, 
			@JsonProperty("clusterTitle") @Nullable final String clusterTitle) {
		this.moduleCount = moduleCount;
		this.clusterIndex = clusterIndex;
		this.communityId = communityId;
		this.clusterTitle = clusterTitle;
		this.clusterDescription = clusterDescription;
	}

	/**
	 * Retrieves the number of modules under the cluster index of an algorithm.
	 *
	 * @return module count.
	 */
	public int getModuleCount() {
		return moduleCount;
	}

	/**
	 * Sets the count of modules under the cluster index of an algorithm.
	 *
	 * @param moduleCount to set.
	 */
	public void setModuleCount(final int moduleCount) {
		this.moduleCount = moduleCount;
	}

	/**
	 * Retrieves the description of how the cluster is formed.
	 *
	 * @return String containing the description.
	 */
	@Nullable
	public String getClusterDescription() {
		return clusterDescription;
	}

	/**
	 * Sets the description of how the cluster is formed.
	 *
	 * @param description to set.
	 */
	public void setClusterDescription(final String description) {
		this.clusterDescription = description;
	}
	
	/**
	 * Gets the cluster index of a DNA community.
	 *
	 * @return cluster index
	 */
	public int getClusterIndex() {
		return clusterIndex;
	}

	/**
	 * Sets the cluster index of a DNA community.
	 *
	 * @param clusterIndex the index of cluster
	 */
	public void setClusterIndex(final int clusterIndex) {
		this.clusterIndex = clusterIndex;
	}

	/**
	 * Gets the DNA community UUID
	 *
	 * @return the DNA community UUID
	 */
	public String getCommunityId() {
		return communityId;
	}

	/**
	 * Sets the DNA community UUID
	 *
	 * @param communityId  the DNA community UUID
	 */
	public void setCommunityId(final String communityId) {
		this.communityId = communityId;
	}

	/**
	 * Gets the DNA Cluster title.
	 *
	 * @return the DNA Cluster title
	 */
	@Nullable
	public String getClusterTitle() {
		return clusterTitle;
	}

	/**
	 * Sets the DNA Cluster title
	 *
	 * @param clusterTitle the DNA cluster title
	 */
	public void setClusterTitle(final String clusterTitle) {
		this.clusterTitle = clusterTitle;
	}
}

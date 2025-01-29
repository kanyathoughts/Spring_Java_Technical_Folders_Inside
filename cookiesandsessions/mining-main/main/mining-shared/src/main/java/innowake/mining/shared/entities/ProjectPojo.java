/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.time.Instant;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.discovery.config.searchorder.SearchOrder;

/**
 * Project entity class.
 */
@MiningDataType(name = MiningEnitityNames.PROJECT)
public class ProjectPojo extends MiningPojo {

	private final EntityId client;
	
	private final String name;
	
	private final boolean toBeDeleted;
	
	@Nullable
	private final Long sourceCodeRevision;
	
	@Nullable
	private final Long metricsBaseRevision;
	
	private final String metricsVersion;
	
	@Nullable
	private final Instant metricsDate;
	
	private final List<SearchOrder> searchOrders;
	
	@Nullable
	private final Long technicalTaxonomyCategory;
	
	@Nullable
	private final Long defaultTaxonomyCategory;
	
	private final Map<String, Set<String>> customPropertyClasses;

	@Nullable
	private final String metaDataBackupId;
	
	@JsonCreator
	public ProjectPojo(
			@JsonProperty("uid") final UUID uid,
			@JsonProperty("customProperties") final CustomPropertiesMap customProperties,
			@JsonProperty("clientEntity") @Nullable final EntityId client,
			@JsonProperty("client") @Nullable final UUID clientUid,
			@JsonProperty("clientId") @Nullable final Long clientNid,
			@JsonProperty("id") final Long nid,
			@JsonProperty("name") final String name,
			@JsonProperty("markedDeleted") final boolean toBeDeleted,
			@JsonProperty("sourceCodeRevision") @Nullable final Long sourceCodeRevision,
			@JsonProperty("metricsBaseRevision") @Nullable final Long metricsBaseRevision,
			@JsonProperty("metricsVersion") final String metricsVersion,
			@JsonProperty("metricsDate") @Nullable final Instant metricsDate,
			@JsonProperty("searchOrders") final List<SearchOrder> searchOrders,
			@JsonProperty("defaultTaxonomyCategoryId") @Nullable final Long defaultTaxonomyCategory,
			@JsonProperty("technicalTaxonomyCategoryId") @Nullable final Long technicalTaxonomyCategory,
			@JsonProperty("customPropertyClasses") Map<String, Set<String>> customPropertyClasses,
			@JsonProperty("metaDataBackupId") @Nullable final String metaDataBackupId) {
		super(EntityId.of(uid, nid), customProperties);
		this.client = client != null ? client : EntityId.of(clientUid, clientNid);
		this.name = name;
		this.toBeDeleted = toBeDeleted;
		this.sourceCodeRevision = sourceCodeRevision;
		this.metricsBaseRevision = metricsBaseRevision;
		this.metricsVersion = metricsVersion;
		this.metricsDate = metricsDate;
		this.searchOrders = searchOrders.isEmpty()? Arrays.asList(new SearchOrder()):searchOrders;
		this.defaultTaxonomyCategory = defaultTaxonomyCategory;
		this.technicalTaxonomyCategory = technicalTaxonomyCategory;
		this.customPropertyClasses = customPropertyClasses;
		this.metaDataBackupId = metaDataBackupId;
	}
	
	/**
	 * Gets the Client of the Project.
	 * @return Identity of the Client this project belongs to.
	 */
	@JsonIgnore
	public EntityId getClient() {
		return client;
	}
	
	/**
	 * Gets the Client of the Project.
	 * @return Unique ID of the Client this project belongs to.
	 */
	@JsonProperty("client")
	public UUID getClientUid() {
		return client.getUid();
	}
	
	/**
	 * Gets the Client of the Project.
	 * @return Numeric ID of the Client this project belongs to.
	 */
	@JsonProperty("clientId")
	public Long getClientNid() {
		return client.getNid();
	}
	
	/**
	 * Gets the name of the Project.
	 * @return Project name.
	 */
	public String getName() {
		return name;
	}
	
	/**
	 * Returns if the Client was marked for deletion.
	 * @return Whether the client is to be deleted.
	 */
	public boolean isMarkedDeleted() {
		return toBeDeleted;
	}
	
	/**
	 * RETURNS metrics base revision
	 *
	 * @return Long: metrics base revision
	 */
	public @Nullable Long getMetricsBaseRevision() {
		return metricsBaseRevision;
	}
	
	/**
	 * Gets MetricsVersion
	 *
	 * @return String
	 */
	public @Nullable String getMetricsVersion() {
		return metricsVersion;
	}
	
	/**
	 * Gets MetricsDate
	 *
	 * @return Instant
	 */
	public @Nullable Instant getMetricsDate() {
		return metricsDate;
	}
	
	/**
	 * Gets the SourceCodeRevision
	 *
	 * @return Long: SourceCodeRevision
	 */
	public @Nullable Long getSourceCodeRevision() {
		return sourceCodeRevision;
	}
	
	/**
	 * Gets Search Orders
	 *
	 * @return List<SearchOrder>: Search Orde rs
	 */
	public List<SearchOrder> getSearchOrders() {
		return searchOrders;
	}
	
	/**
	 * Gets the Id of TechnicalTaxonomyCategory
	 *
	 * @return Long
	 */
	public @Nullable Long getTechnicalTaxonomyCategoryId() {
		return technicalTaxonomyCategory;
	}
	
	/**
	 * Gets the Id of DefaultTaxonomyCategory
	 *
	 * @return Long
	 */
	public @Nullable Long getDefaultTaxonomyCategoryId() {
		return defaultTaxonomyCategory;
	}
	
	/**
	 *Gets the CustomPropertyClasses
	 *
	 * @return Map<String, Set<String>>`
	 */
	public Map<String, Set<String>> getCustomPropertyClasses() {
		return customPropertyClasses;
	}

	/**
	 * Gets the MetaDataBackupId
	 *
	 * @return String
	 */
	public @Nullable String getMetaDataBackupId() {
		return metaDataBackupId;
	}
	
	@Override
	public String toString() {
		return new ToStringBuilder(this)
			.appendSuper(super.toString())
			.append("name", name)
			.append("client", client)
			.append("toBeDeleted", toBeDeleted)
			.toString();
	}
	
}

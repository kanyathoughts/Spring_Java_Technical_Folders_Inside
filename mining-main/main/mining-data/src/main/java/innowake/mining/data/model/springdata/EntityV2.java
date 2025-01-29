/* Copyright (c) 2019 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.data.model.springdata;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.apache.commons.lang.builder.ToStringBuilder;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.datapoints.annotations.MiningDataPointIgnore;
import innowake.mining.shared.springdata.EdgeDirection;
import innowake.mining.shared.springdata.annotations.CustomProperties;
import innowake.mining.shared.springdata.annotations.RId;
import innowake.mining.shared.springdata.annotations.Relationship;

/**
 * Base class for an entity with a record id.
 */
public abstract class EntityV2 {

	/**
	 * The record id.
	 */
	@RId
	protected @Nullable String recordId;

	/**
	 * CustomProperties.
	 */
	@CustomProperties
	@MiningDataPointIgnore
	protected Map<String, Object> customProperties = new HashMap<>();

	@Relationship(name = "HasAdditionalInfo", direction = EdgeDirection.OUT)
	@Nullable
	private List<AdditionalInfoV2> additionalInfo;

	/**
	 * Gets the record id.
	 *
	 * @return the record id
	 */
	@Nullable
	public String getRecordId() {
		/* must be not null after all DAOs read the record id */
		return recordId;
	}

	/**
	 * Sets the record id.
	 *
	 * @param rid the record id
	 */
	public void setRecordId(final String rid) {
		this.recordId = rid;
	}

	/**
	 * Returns the {@link Map} containing {@link List} of CustomProperties.
	 *
	 * @return the {@link Map} containing {@link List} of CustomProperties.
	 */
	public Map<String, Object> getCustomProperties() {
		return customProperties;
	}

	/**
	 * Sets the {@link Map} containing {@link List} of CustomProperties
	 *
	 * @param customProperties the {@link Map} containing {@link List} of CustomProperties.
	 */
	public void setCustomProperties(final Map<String, Object> customProperties) {
		this.customProperties = customProperties;
	}

	/**
	 * Gets additional information attached to the entity.
	 * @return list of objects containing additional information
	 */
	@Nullable
	public List<AdditionalInfoV2> getAdditionalInfo() {
		return additionalInfo;
	}

	/**
	 * Attaches additional information to the entity.
	 * @param additionalInfo list of objects containing additional information
	 */
	public void setAdditionalInfo(@Nullable final List<AdditionalInfoV2> additionalInfo) {
		this.additionalInfo = additionalInfo;
	}

	@Override
	public String toString() {
		final ToStringBuilder builder = new ToStringBuilder(this);
		builder.append("recordId", recordId);
		builder.append("customProperties", customProperties);
		builder.append("additionalInfo", additionalInfo);
		return builder.toString();
	}

	@Override
	public int hashCode() {
		return Objects.hash(recordId);
	}

	@Override
	public boolean equals(@Nullable final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		final EntityV2 other = (EntityV2) obj;
		return Objects.equals(recordId, other.recordId);
	}
}

/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.apache.commons.lang.builder.ToStringBuilder;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.datapoints.annotations.MiningDataPointIgnore;

/**
 * Base class for an entity with a record id.
 */
public abstract class Entity implements Serializable {

	@Nullable
	protected String recordId;
	
	@MiningDataPointIgnore
	protected Map<String, Object> customProperties = new HashMap<>();
	
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
	 * Returns the {@link Map} containing  {@link List} of CustomProperties.
	 *
	 * @return the {@link Map} containing  {@link List} of CustomProperties.
	 */
	public Map<String, Object> getCustomProperties() {
		return customProperties;
	}

	/**
	 * Sets the {@link Map} containing  {@link List} of CustomProperties
	 *
	 * @param customProperties the {@link Map} containing  {@link List} of CustomProperties.
	 */
	public void setCustomProperties(final Map<String, Object> customProperties) {
		this.customProperties = customProperties;
	}
	
	@Override
	public String toString() {
		final ToStringBuilder builder = new ToStringBuilder(this);
		builder.append("recordId", recordId);
		builder.append("customProperties", customProperties);
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
		final Entity other = (Entity) obj;
		return Objects.equals(recordId, other.recordId);
	}
	
	
}

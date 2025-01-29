/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.data.model.springdata;

import innowake.mining.shared.springdata.annotations.Entity;
import innowake.mining.shared.springdata.annotations.Property;
import innowake.mining.shared.springdata.annotations.RId;

import java.io.Serializable;
import java.util.Objects;
import innowake.lib.core.api.lang.Nullable;

/**
 * Base class for additional information that can be attached to mining entities.
 */
@Entity
public class AdditionalInfoV2 implements Serializable {

	/**
	 * The record id.
	 */
	@RId
	@Nullable
	protected String recordId;

	@Nullable
	@Property("@class")
	protected String className;

	/**
	 * Gets the record id.
	 *
	 * @return the record id
	 */
	@Nullable
	public String getRecordId() {
		return recordId;
	}

	/**
	 * Sets the record id.
	 *
	 * @param recordId the record id
	 */
	public void setRecordId(@Nullable final String recordId) {
		this.recordId = recordId;
	}

	/**
	 * Returns the actual database class name ("table name") of this additional info record.
	 *
	 * @return the database class name
	 */
	@Nullable
	public String getClassName() {
		return className;
	}

	/**
	 * Sets the database class name of this additional info record.
	 *
	 * @param className the database class name
	 */
	public void setClassName(@Nullable final String className) {
		this.className = className;
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
		final AdditionalInfoV2 other = (AdditionalInfoV2) obj;
		return Objects.equals(recordId, other.recordId);
	}
}

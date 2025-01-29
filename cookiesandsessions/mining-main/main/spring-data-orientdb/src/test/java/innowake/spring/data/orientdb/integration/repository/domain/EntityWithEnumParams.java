/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository.domain;

import innowake.mining.shared.springdata.annotations.Entity;

/**
 * An entity with enum fields.
 */
@Entity
public class EntityWithEnumParams {

	private EnumData enumData;

	/**
	 * Constructor.
	 * @param enumData the enum value
	 */
	public EntityWithEnumParams(final EnumData enumData) {
		this.enumData = enumData;
	}

	/**
	 * Gets the enum data.
	 *
	 * @return the enum data
	 */
	public EnumData getEnumData() {
		return enumData;
	}

	/**
	 * Sets the enum data.
	 *
	 * @param enumData the enum data
	 */
	public void setEnumData(final EnumData enumData) {
		this.enumData = enumData;
	}

	
	@Entity
	public enum EnumData {
		ENUM1, ENUM2, ENUM3;
	}
}


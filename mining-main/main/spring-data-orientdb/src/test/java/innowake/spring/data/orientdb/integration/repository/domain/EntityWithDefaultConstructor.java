/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository.domain;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.springdata.annotations.Entity;

/**
 * An entity class with only default constructor.
 */
@Entity
public class EntityWithDefaultConstructor {

	@Nullable
	private Long id;
	private boolean booleanVal;

	/**
	 * Gets the id.
	 *
	 * @return the id
	 */
	@Nullable
	public Long getId() {
		return id;
	}

	/**
	 * Sets the id.
	 *
	 * @param id the id
	 */
	public void setId(final Long id) {
		this.id = id;
	}

	/**
	 * Gets the boolean value.
	 *
	 * @return the boolean value
	 */
	public boolean isBooleanVal() {
		return booleanVal;
	}

	/**
	 * Sets the boolean value.
	 *
	 * @param booleanVal the boolean value
	 */
	public void setBooleanVal(boolean booleanVal) {
		this.booleanVal = booleanVal;
	}

}

/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository.domain;

import innowake.mining.shared.springdata.annotations.Entity;

/**
 * An entity with embedded field
 */
@Entity
public class EntityWithEmbeddedParam {

	private Address address;

	/**
	 * Constructor.
	 * @param address the address
	 */
	public EntityWithEmbeddedParam(final Address address) {
		this.address = address;
	}

	/**
	 * Gets the address.
	 *
	 * @return the address
	 */
	public Address getAddress() {
		return address;
	}

	/**
	 * Sets the address.
	 *
	 * @param address the address
	 */
	public void setAddress(final Address address) {
		this.address = address;
	}
	
}

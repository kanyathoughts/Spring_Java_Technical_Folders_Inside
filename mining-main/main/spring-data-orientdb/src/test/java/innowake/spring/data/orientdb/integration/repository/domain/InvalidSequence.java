/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.integration.repository.domain;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.springdata.annotations.Entity;
import innowake.mining.shared.springdata.annotations.Id;

/**
 * Test case to validate scenario with invalid sequence name for @Id.
 */
@Entity
public class InvalidSequence {

	@Id(sequence = "Invalid sequence")
	@Nullable
	private Long id;
	@Nullable
	private String name;

	/**
	 * Returns the id value.
	 *
	 * @return the id value
	 */
	@Nullable
	public Long getId() {
		return id;
	}

	/**
	 * Sets the id value.
	 *
	 * @param id the id value
	 */
	public void setId(final Long id) {
		this.id = id;
	}

	/**
	 * Returns the name.
	 *
	 * @return the name
	 */
	@Nullable
	public String getName() {
		return name;
	}

	/**
	 * Sets the name
	 *
	 * @param name the name
	 */
	public void setName(final String name) {
		this.name = name;
	}

}

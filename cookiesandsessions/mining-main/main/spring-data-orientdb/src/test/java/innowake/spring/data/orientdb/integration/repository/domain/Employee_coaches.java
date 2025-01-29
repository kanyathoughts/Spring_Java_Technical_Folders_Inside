/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository.domain;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.springdata.annotations.Entity;
import innowake.mining.shared.springdata.annotations.RId;
import innowake.mining.shared.springdata.annotations.RelationshipProperties;

/**
 * Edge class linking {@link Employee}'s.
 */
@Entity
@RelationshipProperties
public class Employee_coaches {
	
	@Nullable
	@RId
	private String rid;
	@Nullable
	private Employee in;
	@Nullable
	private Employee out;
	
	/**
	 * Default Constructor.
	 */
	public Employee_coaches() {} 
	
	/**
	 * Parameterized constructor.
	 * @param in coached {@link Employee}
	 * @param out coach {@link Employee}
	 */
	public Employee_coaches(final Employee in, final Employee out) {
		super();
		this.in = in;
		this.out = out;
	}

	/**
	 * Returns coached {@link Employee}.
	 *
	 * @return coached {@link Employee}
	 */
	@Nullable
	public Employee getIn() {
		return in;
	}
	
	/**
	 * Sets coached {@link Employee}.
	 *
	 * @param in coached {@link Employee}
	 */
	public void setIn(final Employee in) {
		this.in = in;
	}
	
	/**
	 * Returns coach {@link Employee}.
	 *
	 * @return coach {@link Employee}
	 */
	@Nullable
	public Employee getOut() {
		return out;
	}
	
	/**
	 * Sets coach {@link Employee}.
	 *
	 * @param out coach {@link Employee}
	 */
	public void setOut(final Employee out) {
		this.out = out;
	}

}

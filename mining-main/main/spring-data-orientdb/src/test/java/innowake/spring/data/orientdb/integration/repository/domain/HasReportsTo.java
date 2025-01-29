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
public class HasReportsTo {
	
	@Nullable
	@RId
	private String rid;
	@Nullable
	private Employee in;
	@Nullable
	private Employee out;
	
	/**
	 * Default constructor.
	 */
	public HasReportsTo() {} 
	
	/**
	 * Parameterized constructor.
	 * @param in coached {@link Employee}
	 * @param out coach {@link Employee}
	 */
	public HasReportsTo(final Employee in, final Employee out) {
		super();
		this.in = in;
		this.out = out;
	}

	/**
	 * Returns the {@link Employee}.
	 *
	 * @return the {@link Employee}
	 */
	@Nullable
	public Employee getIn() {
		return in;
	}
	
	/**
	 * Sets the {@link Employee}.
	 *
	 * @param in the {@link Employee}
	 */
	public void setIn(final Employee in) {
		this.in = in;
	}
	
	/**
	 * Returns the {@link Employee}.
	 *
	 * @return the {@link Employee}
	 */
	@Nullable
	public Employee getOut() {
		return out;
	}
	
	/**
	 * Sets the {@link Employee}.
	 *
	 * @param out the {@link Employee}
	 */
	public void setOut(final Employee out) {
		this.out = out;
	}

}

/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository.domain;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.springdata.annotations.Entity;
import innowake.mining.shared.springdata.annotations.RId;
import innowake.mining.shared.springdata.annotations.RelationshipProperties;

/**
 * Edge class linking {@link Employee} and {@link Project}.
 */
@Entity
@RelationshipProperties
public class Employee_projects {

	@Nullable
	@RId
	private String rid;
	@Nullable
	private Employee out;
	@Nullable
	private Project in;
	
	/**
	 * Default Constructor.
	 */
	public Employee_projects() {}
	
	/**
	 * Parameterized constructor.
	 * @param out the {@link Employee}
	 * @param in the  {@link Project}
	 */
	public Employee_projects(final Employee out, final Project in) {
		super();
		this.out = out;
		this.in = in;
	}

	/**
	 * Return {@link Employee}
	 *
	 * @return the {@link Employee}
	 */
	@Nullable
	public Employee getOut() {
		return out;
	}
	
	/**
	 * Sets the {@link Employee}
	 *
	 * @param out the {@link Employee}
	 */
	public void setOut(final Employee out) {
		this.out = out;
	}
	
	/**
	 * Returns the {@link Project}
	 *
	 * @return the {@link Project}
	 */
	@Nullable
	public Project getIn() {
		return in;
	}
	
	/**
	 * Sets the {@link Project}
	 *
	 * @param in the {@link Project}
	 */
	public void setIn(final Project in) {
		this.in = in;
	}
}

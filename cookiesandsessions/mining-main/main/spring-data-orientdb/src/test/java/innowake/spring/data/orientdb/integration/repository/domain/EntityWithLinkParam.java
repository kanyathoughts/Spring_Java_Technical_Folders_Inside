/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository.domain;

import innowake.mining.shared.springdata.annotations.Entity;

/**
 * An entity with link field.
 */
@Entity
public class EntityWithLinkParam {
	
	private Employee employee;
	
	/**
	 * Constructor.
	 * @param employee the employee
	 */
	public EntityWithLinkParam(final Employee employee) {
		super();
		this.employee = employee;
	}

	/**
	 * Sets the employee.
	 *
	 * @return the employee
	 */
	public Employee getEmployee() {
		return employee;
	}

	/**
	 * Gets the employee
	 *
	 * @param employee the employee
	 */
	public void setEmployee(final Employee employee) {
		this.employee = employee;
	}

}

/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository.domain;

import java.util.List;
import innowake.mining.shared.springdata.annotations.Entity;

/**
 * An entity with collection link field.
 */
@Entity
public class EntityWithCollectionLink {
	
	private List<Employee> employees;
	
	/**
	 * Constructor.
	 * @param employees the employees
	 */
	public EntityWithCollectionLink(final List<Employee> employees) {
		super();
		this.employees = employees;
	}

	/**
	 * Gets the employees.
	 *
	 * @return the employees
	 */
	public List<Employee> getEmployees() {
		return employees;
	}

	/**
	 * Sets the employees.
	 *
	 * @param employees the employees
	 */
	public void setEmployees(final List<Employee> employees) {
		this.employees = employees;
	}

}

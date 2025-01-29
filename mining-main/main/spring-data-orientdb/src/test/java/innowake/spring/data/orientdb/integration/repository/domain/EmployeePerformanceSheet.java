/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository.domain;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.springdata.annotations.CustomProperties;
import innowake.mining.shared.springdata.annotations.Entity;
import innowake.mining.shared.springdata.annotations.RId;
import innowake.spring.data.orientdb.ogm.mapping.custom.CustomProperty;

/**
 * A document class mapped in orientDB.
 */
@JsonInclude(Include.NON_NULL)
@Entity(isDocumentClass = true)
public class EmployeePerformanceSheet {
	
	@JsonIgnore
	@RId
	@Nullable
	private String rid;
	
	@Nullable
	private Employee employeeLink;
	
	@Nullable
	private String performance;
	
	@CustomProperties
	protected Map<String, List<CustomProperty>> customProperties = new HashMap<>();
	
	/**
	 * Default constructor required to create EmployeePerformanceSheet proxy instance.
	 */
	public EmployeePerformanceSheet() {
	}
	
	/**
	 * Constructor
	 * 
	 * @param employeeLink Employee linked to.
	 */
	public EmployeePerformanceSheet(final Employee employeeLink) {
		super();
		this.employeeLink = employeeLink;
	}


	/**
	 * Gets the record Id.
	 *
	 * @return record Id of performance Sheet
	 */
	@Nullable
	public String getRid() {
		return rid;
	}

	
	/**
	 * Set the record id.
	 *
	 * @param rid Rid to set
	 */
	public void setRid(final String rid) {
		this.rid = rid;
	}

	
	/**
	 * Gets the Employee.
	 *
	 * @return employee
	 */
	@Nullable
	public Employee getEmployeeLink() {
		return employeeLink;
	}

	
	/**
	 * Sets the employee to be linked.
	 *
	 * @param employeeLink employee to be linked
	 */
	public void setEmployeeLink(final Employee employeeLink) {
		this.employeeLink = employeeLink;
	}

	
	/**
	 * Gets the performance of Employee.
	 *
	 * @return performance string of employee.
	 */
	@Nullable
	public String getPerformance() {
		return performance;
	}

	
	/**
	 * Sets the performance of employee.
	 *
	 * @param performance to be set for employee
	 */
	public void setPerformance(final String performance) {
		this.performance = performance;
	}
	
	/**
	 * Returns the {@link Map} containing {@link List} of {@link CustomProperty}'s.
	 *
	 * @return the {@link Map} containing {@link List} of {@link CustomProperty}'s.
	 */
	public Map<String, List<CustomProperty>> getCustomProperties() {
		return customProperties;
	}

	/**
	 * Sets the {@link Map} containing {@link List} of {@link CustomProperty}'s
	 *
	 * @param customProperties the {@link Map} containing {@link List} of {@link CustomProperty}'s.
	 */
	public void setCustomProperties(final Map<String, List<CustomProperty>> customProperties) {
		this.customProperties = customProperties;
	}
	
}

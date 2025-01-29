/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.repository;

import java.util.List;

import innowake.spring.data.orientdb.integration.repository.domain.EmployeePerformanceSheet;

/**
 * A Document entity for Employee as Performance Sheet.
 */
public interface EmployeePerformanceRepository extends OrientRepository<EmployeePerformanceSheet> {
	
	/**
	 * Find the performance sheets by employeeId.
	 * 
	 * @param employeeId id of the employee
	 * @return list of employeePerformanceSheet of the employee
	 */
	List<EmployeePerformanceSheet> findAllByEmployeeLinkId(final Long employeeId);
	
	/**
	 * Find the performance sheets by employee first name.
	 * 
	 * @param firstName firstName of the employee
	 * @return list of employeePerformanceSheet of the employee
	 */
	List<EmployeePerformanceSheet> findAllByEmployeeLinkFirst(final String firstName);
	
	/**
	 * Delete the performance sheets by employee first name.
	 * 
	 * @param firstName firstName of the employee
	 */
	void deleteAllByEmployeeLinkFirst(final String firstName);
	
}

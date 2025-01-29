/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import innowake.spring.data.orientdb.repository.EmployeePerformanceRepository;
import innowake.spring.data.orientdb.repository.EmployeeRepository;

/**
 * TransactionTest class for testing transaction management in spring data.
 */
public class TranscationTestMethod {
	
	@Autowired
	private EmployeeRepository employeeRepo;
	
	@Autowired
	private EmployeePerformanceRepository employeePerformanceRepo;
	
	/**
	 * Method to test transaction roll back.
	 */
	@Transactional("orientdb-transaction")
	public void deleteEmployeeAndEPF() {
		employeeRepo.deleteByFirst("user1");
		employeeRepo.findById("unknown");
		employeePerformanceRepo.deleteAllByEmployeeLinkFirst("user1");
	}
	
	/**
	 * Method to test nested transaction roll back.
	 */
	@Transactional("orientdb-transaction")
	public void deleteEmployeeAndEPFNested() {
		employeePerformanceRepo.deleteAllByEmployeeLinkFirst("user2");
		employeeRepo.deleteByFirst("user2");
		deleteEmployeeAndEPF();
	}
}

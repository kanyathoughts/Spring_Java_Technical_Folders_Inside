/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository;

import static org.junit.Assert.fail;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import innowake.spring.data.orientdb.commons.exception.NoRecordFoundException;
import innowake.spring.data.orientdb.integration.repository.domain.Employee;
import innowake.spring.data.orientdb.repository.EmployeeRepository;

/**
 * Service providing test methods executed by test cases.
 */
@Service
public class EmployeeRepositoryTestService {
	
	private final EmployeeRepository employeeRepository;
	
	@Autowired
	public EmployeeRepositoryTestService(final EmployeeRepository employeeRepository) {
		this.employeeRepository = employeeRepository;
	}

	/**
	 * Test method for {@link EmployeeRepositorySaveTests#testSaveAfterFind()},
	 * running inside a transaction.
	 */
	@Transactional("orientdb-transaction")
	public void saveAfterFindTransaction() {
		try {
			/* does not exist, so we expect this to fail */
			employeeRepository.findEmployeeByEmail("user_save_after_find@deloitte.com");
			fail("Expected employee with e-mail user_save_after_find@deloitte.com to not exist.");
		} catch (NoRecordFoundException e) {
			final Employee employee = new Employee("user_save_after_find", "user_save_after_find", "user_save_after_find@deloitte.com");
			employeeRepository.save(employee);
		}
	}
}

/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.lang.Assert;
import innowake.spring.data.orientdb.commons.exception.MetadataException;
import innowake.spring.data.orientdb.integration.repository.domain.Employee;
import innowake.spring.data.orientdb.integration.repository.domain.InvalidSequence;
import innowake.spring.data.orientdb.repository.InvalidSequenceRepo;

import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Test case for sequences.
 */
public class SequenceTests extends AbstractEmployeeRepositoryIntegrationTests {
	
	@Autowired
	private InvalidSequenceRepo invalidSequenceRepo;
	
	/**
	 * Test case to check the generation of id using sequence.
	 */
	@Test
	public void testSequenceId() {
		final Employee employee1 = new Employee("user_10", "admin_10", "user_10@deloitte.com");
		final Employee employee2 = new Employee("user_11", "admin_11", "user_11@deloitte.com");
		Assert.assertNotNull(employeeRepository.save(employee1));
		Assert.assertNotNull(employeeRepository.save(employee2));
		final Employee retrievedEmp1 = Assert.assertNotNull(employeeRepository.findEmployeeByEmail("user_10@deloitte.com"));
		final Employee retrievedEmp2 = Assert.assertNotNull(employeeRepository.findEmployeeByEmail("user_11@deloitte.com"));
		final Long firstEmpId = Assert.assertNotNull(retrievedEmp1.getId());
		final Long secondEmpId = employeeRepository.findEmployeeById(Long.valueOf(Assert.assertNotNull(firstEmpId).intValue() + 1)).getId();
		Assert.assertEqual(retrievedEmp2.getId(), secondEmpId);
	}
	
	/**
	 * Test case to invalid sequence.
	 */
	@Test
	public void testInvalidSequenceName() {
		assertThrows(MetadataException.class, () -> {
			final InvalidSequence invalidSequence = new InvalidSequence();
			invalidSequence.setName("Test invalid sequence name");
			invalidSequenceRepo.save(invalidSequence);
		});
	}
	
}

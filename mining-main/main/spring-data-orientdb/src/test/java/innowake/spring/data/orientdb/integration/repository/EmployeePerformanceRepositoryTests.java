/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import com.orientechnologies.orient.core.id.ORID;

import innowake.spring.data.orientdb.integration.config.AbstractIntegrationTests;
import innowake.spring.data.orientdb.integration.repository.domain.Employee;
import innowake.spring.data.orientdb.integration.repository.domain.EmployeePerformanceSheet;
import innowake.spring.data.orientdb.ogm.proxy.IEntityProxy;
import innowake.spring.data.orientdb.repository.EmployeePerformanceRepository;
import innowake.spring.data.orientdb.repository.EmployeeRepository;


/**
 * Test cases for EmployeePerformanceRepository.
 */
public class EmployeePerformanceRepositoryTests extends AbstractIntegrationTests{
	
	private static final String PERFORMANCE_STATS = "All good but not promoted";
	private static final String PERFORMANCE_STATS_2 = "Out of bounds performance";

	@Autowired
	private EmployeeRepository employeeRepo;
	
	@Autowired
	private EmployeePerformanceRepository employeePerformanceRepo;
	
	@Autowired
	private TranscationTestMethod transcationTestMethod;
	
	/**
	 * Clear data in orient db.
	 */
	@Before
	public void clearData() {
		clearData("Employee", "Project");
		clearNonVertexData("EmployeePerformanceSheet");
	}
	
	/**
	 * Tests if {@link EmployeeRepository} and {@link EmployeePerformanceRepository} instance is created.
	 */
	@Test
	public void repositoryAutowiring() {
		assertNotNull(employeeRepo);
		assertNotNull(employeePerformanceRepo);
	}
	
	/**
	 * Test cases for saving Document
	 */
	@Test
	public void testDocumentSaveOperation() {
		final Employee employee1 = new Employee("user1", "user lastname 1", "user1@deloitte.com");
		employee1.setIsEmployeeActive(Boolean.TRUE);
		final EmployeePerformanceSheet ePSheet = new EmployeePerformanceSheet(employee1);
		ePSheet.setPerformance(PERFORMANCE_STATS);
		final EmployeePerformanceSheet savedSnapShot = employeePerformanceRepo.save(ePSheet);
		assertEquals(PERFORMANCE_STATS, savedSnapShot.getPerformance());
	}
	
	/**
	 * Test cases for deleting Document.
	 */
	@Test
	public void testDocumentDeleteOperation() {
		final Employee employee2 = new Employee("user2", "user lastname 2", "user2@deloitte.com");
		employee2.setIsEmployeeActive(Boolean.TRUE);
		final EmployeePerformanceSheet ePSheet = new EmployeePerformanceSheet(employee2);
		ePSheet.setPerformance(PERFORMANCE_STATS);
		final EmployeePerformanceSheet savedSnapShot = employeePerformanceRepo.save(ePSheet);
		assertEquals(PERFORMANCE_STATS, savedSnapShot.getPerformance());
		final List<EmployeePerformanceSheet> beforeDelete = employeePerformanceRepo.findAllByEmployeeLinkFirst("user2");
		assertEquals(1, beforeDelete.size());
		employeePerformanceRepo.delete(savedSnapShot);
		final List<EmployeePerformanceSheet> afterDelete = employeePerformanceRepo.findAllByEmployeeLinkFirst("user2");
		assertEquals(0, afterDelete.size());
	}
	
	/**
	 * Test case to find EmployeePerformanceSheet by EmployeeId.
	 *
	 */
	@Test
	public void testDocumentFindByEmployeeId() {
		final Employee employee1 = new Employee("user1", "user lastname 1", "user1@deloitte.com");
		employee1.setIsEmployeeActive(Boolean.TRUE);
		final EmployeePerformanceSheet ePSheet = new EmployeePerformanceSheet(employee1);
		ePSheet.setPerformance(PERFORMANCE_STATS);
		final EmployeePerformanceSheet savedSnapShot = employeePerformanceRepo.save(ePSheet);
		assertEquals(PERFORMANCE_STATS, savedSnapShot.getPerformance());
		final Employee employeeLink = savedSnapShot.getEmployeeLink();
		assertNotNull(employeeLink);
		final Long id = employeeLink.getId();
		assertNotNull(id);
		final List<EmployeePerformanceSheet> foundSheets = employeePerformanceRepo.findAllByEmployeeLinkId(id);
		assertEquals(1, foundSheets.size());
		assertEquals(savedSnapShot.getPerformance(), foundSheets.get(0).getPerformance());
	}
	
	/**
	 * Test case to delete EmployeePerformanceSheet by Employee.
	 *
	 */
	@Test
	public void testDocumentDeleteByEmployee() {
		final Employee employee1 = new Employee("user1", "user lastname 1", "user1@deloitte.com");
		employee1.setIsEmployeeActive(Boolean.TRUE);
		final EmployeePerformanceSheet ePSheet1 = new EmployeePerformanceSheet(employee1);
		ePSheet1.setPerformance(PERFORMANCE_STATS);
		final EmployeePerformanceSheet ePSheet2 = new EmployeePerformanceSheet(employee1);
		ePSheet2.setPerformance(PERFORMANCE_STATS);
		employeePerformanceRepo.save(ePSheet1);
		employeePerformanceRepo.save(ePSheet2);
		final List<EmployeePerformanceSheet> foundSheets = employeePerformanceRepo.findAllByEmployeeLinkFirst("user1");
		assertEquals(2, foundSheets.size());
		employeePerformanceRepo.deleteAllByEmployeeLinkFirst("user1");
		final List<EmployeePerformanceSheet> afterDelete = employeePerformanceRepo.findAllByEmployeeLinkFirst("user1");
		assertEquals(0, afterDelete.size());
	}
	
	/**
	 * Test case to update EmployeePerformanceSheet.
	 *
	 */
	@Test
	public void testDocumentUpdateByEmployeeLink() {
		final Employee employee1 = new Employee("user1", "user lastname 1", "user1@deloitte.com");
		employee1.setIsEmployeeActive(Boolean.TRUE);
		final EmployeePerformanceSheet ePSheet1 = new EmployeePerformanceSheet(employee1);
		ePSheet1.setPerformance(PERFORMANCE_STATS);
		final EmployeePerformanceSheet savedSheet = employeePerformanceRepo.save(ePSheet1);
		final ORID recordId = ((IEntityProxy) savedSheet).__getRid();
		assertEquals(PERFORMANCE_STATS, savedSheet.getPerformance());
		final EmployeePerformanceSheet modified = new EmployeePerformanceSheet(employee1);
		modified.setRid(recordId.toString());
		modified.setPerformance(PERFORMANCE_STATS_2);
		final EmployeePerformanceSheet afterUpdate = employeePerformanceRepo.save(modified);
		assertEquals(PERFORMANCE_STATS_2, afterUpdate.getPerformance());
	}
	
	/**
	 * Test case for transaction roll back.
	 */
	@Test
	public void testTransactionalOperations() {
		final Employee employee1 = new Employee("user1", "user lastname 1", "user1@deloitte.com");
		employee1.setIsEmployeeActive(Boolean.TRUE);
		final EmployeePerformanceSheet ePSheet1 = new EmployeePerformanceSheet(employee1);
		ePSheet1.setPerformance(PERFORMANCE_STATS);
		employeePerformanceRepo.save(ePSheet1);
		try {
			transcationTestMethod.deleteEmployeeAndEPF();
		} catch (IllegalArgumentException e) {
			final List<Employee> findByFirstName = employeeRepo.findByFirst("user1");
			assertEquals(1, findByFirstName.size());
		}
	}
	
	/**
	 * Test case for nested transaction method transaction roll back.
	 */
	@Test
	public void testNestedTransactionalOperations() {
		final Employee employee1 = new Employee("user1", "user lastname 1", "user1@deloitte.com");
		final Employee employee2 = new Employee("user2", "user lastname 2", "user2@deloitte.com");
		employee1.setIsEmployeeActive(Boolean.TRUE);
		employee2.setIsEmployeeActive(Boolean.TRUE);
		final EmployeePerformanceSheet ePSheet1 = new EmployeePerformanceSheet(employee1);
		ePSheet1.setPerformance(PERFORMANCE_STATS);
		final EmployeePerformanceSheet ePSheet2 = new EmployeePerformanceSheet(employee2);
		ePSheet2.setPerformance(PERFORMANCE_STATS);
		employeePerformanceRepo.save(ePSheet1);
		employeePerformanceRepo.save(ePSheet2);
		try {
			transcationTestMethod.deleteEmployeeAndEPFNested();
		} catch (IllegalArgumentException e) {
			List<Employee> findByFirstName = employeeRepo.findByFirst("user1");
			assertEquals(1, findByFirstName.size());
			findByFirstName = employeeRepo.findByFirst("user2");
			assertEquals(1, findByFirstName.size());
		}
	}
}

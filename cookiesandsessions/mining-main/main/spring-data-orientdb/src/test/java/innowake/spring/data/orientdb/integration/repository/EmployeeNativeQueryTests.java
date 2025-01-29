/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository;

import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.junit.Test;
import org.mockito.internal.matchers.apachecommons.ReflectionEquals;

import innowake.lib.core.lang.Assert;
import innowake.spring.data.orientdb.integration.repository.domain.Employee;
import innowake.spring.data.orientdb.repository.EmployeeRepository;

/**
 * Test cases to validate some of the native queries on {@link EmployeeRepository}
 */
public class EmployeeNativeQueryTests extends AbstractEmployeeRepositoryIntegrationTests {


	/**
	 * Test cases for native count query
	 */
	@Test
	public void testNativeCountQuery() {
		final Employee employee1 = new Employee("user1", "user lastname 1", "user1@deloitte.com");
		final Employee employee2 = new Employee("user2", "user2", "user2@deloitte.com");
		final Employee employee3 = new Employee("user3", "user3", "user3@deloitte.com");
		employee1.setIsEmployeeActive(Boolean.TRUE);
		employee3.setIsEmployeeActive(Boolean.TRUE);
		final List<Employee> employees = Arrays.asList(employee1, employee2, employee3);
		employeeRepository.saveAll(employees);

		final Long count = employeeRepository.countByFirstNameAndActive("user1", Boolean.TRUE);
		Assert.assertEqual(Long.valueOf(1), count, "count does not match");
	}

	/**
	 * Test cases for native query
	 */
	@Test
	public void testNativeQuery() {
		final Employee employee1 = new Employee("employee1", "user lastname 4", "user4@deloitte.com");
		final Employee employee2 = new Employee("emp2", "user name 5", "user5@deloitte.com");
		final Employee employee3 = new Employee("employee3", "user name 6", "user6@deloitte.com");
		final Set<Employee> employees = new TreeSet<>(Arrays.asList(employee1, employee2, employee3));
		employeeRepository.saveAll(employees);

		final List<Employee> fetchedEmployees = employeeRepository.findEmployeeByFirst("emp");
		assertCollectionObjects(employees, new TreeSet<>(fetchedEmployees), "rid", "id");
	}

	/**
	 * Test cases for delete native query
	 */
	@Test
	public void testNativeDeleteQuery() {
		final Employee employee1 = new Employee("employee6", "user lastname 6", "user10@deloitte.com");
		final Employee employee2 = new Employee("emp7", "user7", "user7@deloitte.com");
		final Employee employee3 = new Employee("employee8", "user8", "user8@deloitte.com");
		final List<Employee> employees = Arrays.asList(employee1, employee2, employee3);
		employeeRepository.saveAll(employees);

		final Long employeesDeleted = employeeRepository.deleteEmployeeByFirstName("EMP7");
		Assert.assertEqual(Long.valueOf(1), employeesDeleted, "deleted employee count does not match");
	}
	
	/**
	 * Test case for {@link EmployeeRepository#findEmployeeByAgeIn(java.util.Collection)} method.
	 */
	@Test
	public void testFindEmployeeByAgeIn() {
		final Employee employee1 = new Employee("user4", "user4", "user4@deloitte.com");
		final Employee employee2 = new Employee("user5", "user5", "user5@deloitte.com");
		final Employee employee3 = new Employee("user6", "user6", "user6@deloitte.com");
		final Integer age1 = Integer.valueOf(25);
		final Integer age2 = Integer.valueOf(27);
		employee1.setUserAge(age1);
		employee2.setUserAge(age2);
		employee3.setUserAge(Integer.valueOf(26));
		
		final Set<Employee> employees = new TreeSet<>(Arrays.asList(employee1, employee2, employee3));
		employeeRepository.saveAll(employees);
		employees.remove(employee3);
		
		final List<Employee> fetchedEmployees = employeeRepository.findEmployeeByAgeIn(Arrays.asList(age1, age2));
		final Set<Employee> orderedEmployees = new TreeSet<>(fetchedEmployees);
		assertCollectionObjects(employees, orderedEmployees, "rid", "id");
	}
	
	/**
	 * Test case for {@link EmployeeRepository#findEmployeByEmailIgnoreCase(String)}  method.
	 */
	@Test
	public void testfindEmployeByEmailIdIgnoreCase() {
		final Employee employee1 = new Employee("employee22", "employee22", "employee22@deloitte.com");
		final Employee employee2 = new Employee("employee23", "employee23", "employee23@deloitte.com");
		final Employee employee3 = new Employee("employee24", "employee24", "employee24@deloitte.com");
		final List<Employee> employees = Arrays.asList(employee1, employee2, employee3);
		employeeRepository.saveAll(employees);

		final Employee fetchedEmployee = employeeRepository.findEmployeByEmailIgnoreCase("EMPLOYEE24@deloitte.com");
		assertTrue(new ReflectionEquals(employee3, "rid", "id").matches(fetchedEmployee));
	}

}

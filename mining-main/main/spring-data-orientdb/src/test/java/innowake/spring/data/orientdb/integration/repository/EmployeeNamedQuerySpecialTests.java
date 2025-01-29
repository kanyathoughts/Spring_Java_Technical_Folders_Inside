/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.junit.Before;
import org.junit.Test;
import org.mockito.internal.matchers.apachecommons.ReflectionEquals;

import innowake.lib.core.lang.Assert;
import innowake.spring.data.orientdb.commons.exception.NoRecordFoundException;
import innowake.spring.data.orientdb.integration.repository.domain.Employee;
import innowake.spring.data.orientdb.integration.repository.domain.Employee.Designation;
import innowake.spring.data.orientdb.repository.EmployeeRepository;

/**
 * Named queries for {@link Enum}, {@link Boolean} data types.
 */
public class EmployeeNamedQuerySpecialTests extends AbstractEmployeeRepositoryIntegrationTests {

	private static final String USER = "user1";
	private final Employee employee1 = new Employee(USER, USER, "user1@deloitte.com");
	private final Employee employee2 = new Employee("user2", "user2", "user2@deloitte.com");
	private final Employee employee3 = new Employee("user3", "user3", "user3@deloitte.com");

	/**
	 * Initializes the objects values.
	 */
	@Before
	public void initialize() {
		employee1.setIsEmployeeActive(Boolean.TRUE);
		employee2.setIsEmployeeActive(Boolean.FALSE);
		employee3.setIsEmployeeActive(Boolean.TRUE);
		employee1.setEmpDesignation(Designation.CONSULTANT);
		employee2.setEmpDesignation(Designation.MANAGER);
		employee3.setEmpDesignation(Designation.CONSULTANT);
	}

	/**
	 * Test case for {@link EmployeeRepository#findEmployeeByIsEmployeeActiveTrue()}
	 */
	@Test
	public void testBooleanTrueNamedQuery() {
		final Set<Employee> employees = new TreeSet<>(Arrays.asList(employee1, employee2, employee3));
		employeeRepository.saveAll(employees);
		employees.remove(employee2);

		final List<Employee> fetchedEmployees = employeeRepository.findEmployeeByIsEmployeeActiveTrue();
		final Set<Employee> orderedEmployees = new TreeSet<>(fetchedEmployees);
		assertCollectionObjects(employees, orderedEmployees, "rid", "id");
	}

	/**
	 * Test case for {@link EmployeeRepository#findEmployeeByIsEmployeeActiveFalse()}
	 */
	@Test
	public void testBooleanFalseNamedQuery() {
		final Set<Employee> employees = new TreeSet<>(Arrays.asList(employee1, employee2, employee3));
		employeeRepository.saveAll(employees);

		final List<Employee> fetchedEmployees = employeeRepository.findEmployeeByIsEmployeeActiveFalse();
		assertEquals(1, fetchedEmployees.size());
		assertTrue(new ReflectionEquals(employee2, "rid", "id").matches(fetchedEmployees.get(0)));
	}

	/**
	 * Test case for {@link EmployeeRepository#findEmployeeByEmpDesignation(Designation)}
	 */
	@Test
	public void testEnumNamedQuery() {
		final Set<Employee> employees = new TreeSet<>(Arrays.asList(employee1, employee2, employee3));
		employeeRepository.saveAll(employees);
		employees.remove(employee2);

		final List<Employee> fetchedEmployees = employeeRepository.findEmployeeByEmpDesignation(Designation.CONSULTANT);
		final Set<Employee> orderedEmployees = new TreeSet<>(fetchedEmployees);
		assertCollectionObjects(employees, orderedEmployees, "rid", "id");
	}
	
	/**
	 * Test case for {@link EmployeeRepository#findEmployeeByEmpDesignationIn(Collection)})}
	 */
	@Test
	public void testEnumNamedQueryWithInClauseOnDesignation() {
		final Set<Employee> employees = new TreeSet<>(Arrays.asList(employee1, employee2, employee3));
		employeeRepository.saveAll(employees);
		final List<Designation> designations = new ArrayList<>();
		designations.add(Designation.CONSULTANT);
		designations.add(Designation.MANAGER);

		final List<Employee> fetchedEmployees = employeeRepository.findEmployeeByEmpDesignationIn(designations);
		final Set<Employee> orderedEmployees = new TreeSet<>(fetchedEmployees);
		assertCollectionObjects(employees, orderedEmployees, "rid", "id");
	}
	
	/**
	 * Test case for {@link EmployeeRepository#findEmployeeByIdIn(Collection)})}
	 */
	@Test
	public void testNamedQueryWithInClauseOnId() {
		final Set<Employee> employees = new TreeSet<>(Arrays.asList(employee1, employee2, employee3));
		Iterable<Employee> saveAll = employeeRepository.saveAll(employees);
		Iterator<Employee> iterator = saveAll.iterator();
		final List<Long> ids = new ArrayList<>();
		while(iterator.hasNext()) {
			Long id = iterator.next().getId();
			assertNotNull(id);
			ids.add(Long.valueOf(id.longValue()));
		}

		final List<Employee> fetchedEmployees = employeeRepository.findEmployeeByIdIn(ids);
		final Set<Employee> orderedEmployees = new TreeSet<>(fetchedEmployees);
		assertCollectionObjects(employees, orderedEmployees, "rid", "id");
	}
	
	/**
	 * Test case for {@link EmployeeRepository#findEmployeeByIdIn(Collection)})} where Collection is empty.
	 */
	@Test
	public void testNamedQueryWithInClauseOnIdWithEmptyInputs() {
		final Set<Employee> employees = new TreeSet<>(Arrays.asList(employee1, employee2, employee3));
		employeeRepository.saveAll(employees);
		final List<Long> ids = new ArrayList<>();
		final List<Employee> fetchedEmployees = employeeRepository.findEmployeeByIdIn(ids);
		assertEquals(0, fetchedEmployees.size());
	}
	
	/**
	 * Test case for {@link EmployeeRepository#findEmployeeByAgeIn(java.util.Collection)})}
	 */
	@Test
	public void testNamedQueryWithInClauseOnAge() {
		employee1.setUserAge(Integer.valueOf(24));
		employee2.setUserAge(Integer.valueOf(25));
		employee3.setUserAge(Integer.valueOf(26));
		final Set<Employee> employees = new TreeSet<>(Arrays.asList(employee1, employee2, employee3));
		Iterable<Employee> saveAll = employeeRepository.saveAll(employees);
		Iterator<Employee> iterator = saveAll.iterator();
		final List<Integer> ageList = new ArrayList<>();
		while(iterator.hasNext()) {
			Integer id = iterator.next().getUserAge();
			assertNotNull(id);
			ageList.add(Integer.valueOf(id.intValue()));
		}
		final List<Employee> fetchedEmployees = employeeRepository.findEmployeeByAgeIn(ageList);
		final Set<Employee> orderedEmployees = new TreeSet<>(fetchedEmployees);
		assertCollectionObjects(employees, orderedEmployees, "rid", "id");
	}

	/**
	 * Test case when no data record is found
	 */
	@Test(expected = NoRecordFoundException.class)
	public void testWhenNoRecordsAreReturned() {
		employeeRepository.findEmployeeByEmail("user1@deloitte.com");
	}

	/**
	 * Test cases for named count query.
	 */
	@Test
	public void testCountNamedQuery() {
		final List<Employee> employees = Arrays.asList(employee1, employee2, employee3);
		employeeRepository.saveAll(employees);

		final Long count = employeeRepository.countByIsEmployeeActive(Boolean.TRUE);
		Assert.assertEqual(count, Long.valueOf(2), "Count does not match");
	}

	/**
	 * Test cases for named count query with and condition.
	 */
	@Test
	public void testCountNamedQueryWithCriteria() {
		final List<Employee> employees = Arrays.asList(employee1, employee2, employee3);
		employee3.setEmpDesignation(Designation.MANAGER);
		employeeRepository.saveAll(employees);

		final Long count = employeeRepository.countByEmpDesignationAndIsEmployeeActive(Designation.CONSULTANT, Boolean.TRUE);
		Assert.assertEqual(count, Long.valueOf(1), "Count does not match");
	}

	/**
	 * Test cases for delete named query.
	 */
	@Test
	public void testDeleteNamedQuery() {
		final List<Employee> employees = Arrays.asList(employee1, employee2, employee3);
		employeeRepository.saveAll(employees);

		final Long count = employeeRepository.deleteByFirst(USER);
		Assert.assertEqual(count, Long.valueOf(1), "item was not deleted successfully");
	}

}

/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository;

import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.junit.Test;
import org.mockito.internal.matchers.apachecommons.ReflectionEquals;

import innowake.spring.data.orientdb.commons.exception.UnsupportedQueryTypeException;
import innowake.spring.data.orientdb.integration.repository.domain.Employee;
import innowake.spring.data.orientdb.repository.EmployeeRepository;

/**
 * Test cases for named query creation for {@link Employee} class's {@link String} data fields.
 */
public class EmployeeNamedQueryStringTests extends AbstractEmployeeRepositoryIntegrationTests {

	/**
	 * Test case for {@link EmployeeRepository#findByLast(String)}  method.
	 */
	@Test
	public void testFindEmployeeByLastName() {
		final String lastName = "employee";
		final Employee employee1 = new Employee("employee1", lastName, "employee1@deloitte.com");
		final Employee employee2 = new Employee("employee2", "employee2", "employee2@deloitte.com");
		final Employee employee3 = new Employee("employee3", lastName, "employee3@deloitte.com");
		final Set<Employee> employees = new TreeSet<>(Arrays.asList(employee1, employee2, employee3));
		employeeRepository.saveAll(employees);
		employees.remove(employee2);

		final List<Employee> fetchedEmployees = employeeRepository.findByLast(lastName);
		final Set<Employee> orderedEmployees = new TreeSet<>(fetchedEmployees);
		assertCollectionObjects(employees, orderedEmployees, "rid", "id");
	}

	/**
	 * Test case for {@link EmployeeRepository#findByFirstAndLast(String, String)}  method.
	 */
	@Test
	public void testFindEmployeeByFirstNameAndLastName() {
		final String employeeToBeSearchedFirstName = "employee5";
		final String employeeToBeSearchedLastName = "employee5 lastName";
		final Employee employee1 = new Employee("employee4", "employee4", "employee4@deloitte.com");
		final Employee employee2 = new Employee(employeeToBeSearchedFirstName, employeeToBeSearchedLastName, "employee5@deloitte.com");
		final Employee employee3 = new Employee("employee6", "employee6", "employee6@deloitte.com");
		final List<Employee> employees = Arrays.asList(employee1, employee2, employee3);
		employeeRepository.saveAll(employees);

		final List<Employee> fetchedEmployees = employeeRepository.findByFirstAndLast(employeeToBeSearchedFirstName, employeeToBeSearchedLastName);
		assertEquals(1, fetchedEmployees.size());
		assertTrue(new ReflectionEquals(employee2, "rid", "id").matches(fetchedEmployees.get(0)));
	}

	/**
	 * Test case for {@link EmployeeRepository#findEmployeeByFirstOrEmail(String, String)}  method.
	 */
	@Test
	public void testFindEmployeeByFirstNameOrEmailId() {
		final String emailIdToBeSearched = "employee8@deloitte.com";
		final String firstNameToBeSearched = "employee9";
		final Employee employee1 = new Employee("employee7", "employee7", "employee7@deloitte.com");
		final Employee employee2 = new Employee("employee8", "employee8", emailIdToBeSearched);
		final Employee employee3 = new Employee(firstNameToBeSearched, "employee9", "employee9@deloitte.com");
		final Set<Employee> employees = new TreeSet<>(Arrays.asList(employee1, employee2, employee3));
		employeeRepository.saveAll(employees);

		/* Matching emailId */
		final List<Employee> fetchedEmployee2 = employeeRepository.findEmployeeByFirstOrEmail("employee61", emailIdToBeSearched);
		assertEquals(1, fetchedEmployee2.size());
		assertTrue(new ReflectionEquals(employee2, "rid", "id").matches(fetchedEmployee2.get(0)));

		/* Matching name */
		final List<Employee> fetchedEmployee3 = employeeRepository.findEmployeeByFirstOrEmail(firstNameToBeSearched, "employee10@deloitte.com");
		assertEquals(1, fetchedEmployee3.size());
		assertTrue(new ReflectionEquals(employee3, "rid", "id").matches(fetchedEmployee3.get(0)));

		/* When both name and emailId is present*/
		final List<Employee> fetchedEmployees = employeeRepository.findEmployeeByFirstOrEmail(firstNameToBeSearched, emailIdToBeSearched);
		assertTrue(fetchedEmployees.size() > 1);
		employees.remove(employee1);
		assertCollectionObjects(employees, new TreeSet<>(fetchedEmployees), "rid", "id");
	}

	/**
	 * Test case for {@link EmployeeRepository#findEmployeeByEmail(String)}  method.
	 */
	@Test
	public void testFindEmployeeByEmailId() {
		final Employee employee1 = new Employee("employee10", "employee10", "employee10@deloitte.com");
		final Employee employee2 = new Employee("employee11", "employee11", "employee11@deloitte.com");
		final Employee employee3 = new Employee("employee12", "employee12", "employee12@deloitte.com");
		final List<Employee> employees = Arrays.asList(employee1, employee2, employee3);
		employeeRepository.saveAll(employees);

		/* Matching emailId */
		final Employee fetchedEmployee2 = employeeRepository.findEmployeeByEmail("employee11@deloitte.com");
		assertTrue(new ReflectionEquals(employee2, "rid", "id").matches(fetchedEmployee2));
	}

	/**
	 * Test case for {@link EmployeeRepository#findEmployeeByFirstStartingWith(String)}  method.
	 */
	@Test(expected = UnsupportedQueryTypeException.class)
	public void testFindEmployeeByFirstNameStartingWith() {
		final Employee employee1 = new Employee("employee13", "employee13", "employee13@deloitte.com");
		final Employee employee2 = new Employee("employee14", "employee14", "employee14@deloitte.com");
		final Employee employee3 = new Employee("employee15", "employee15", "employee15@deloitte.com");
		final List<Employee> employees = Arrays.asList(employee1, employee2, employee3);
		employeeRepository.saveAll(employees);

		employeeRepository.findEmployeeByFirstStartingWith("employee");
	}

	/**
	 * Test case for {@link EmployeeRepository#findEmployeeByFirstLike(String)}  method.
	 */
	@Test
	public void testFindEmployeeByFirstNameLike() {
		final Employee employee1 = new Employee("employee16", "employee16", "employee16@deloitte.com");
		final Employee employee2 = new Employee("employee17", "employee17", "employee17@deloitte.com");
		final Employee employee3 = new Employee("employee18", "employee18", "employee18@deloitte.com");
		final Set<Employee> employees = new TreeSet<>(Arrays.asList(employee1, employee2, employee3));
		employeeRepository.saveAll(employees);

		final List<Employee> fetchedEmployees = employeeRepository.findEmployeeByFirstLike("employee%");
		final Set<Employee> orderedEmployees = new TreeSet<>(fetchedEmployees);
		assertCollectionObjects(employees, orderedEmployees, "rid", "id");
	}

	/**
	 * Test case for {@link EmployeeRepository#findEmployeeByFirstNotLike(String)}  method.
	 */
	@Test
	public void testFindEmployeeByFirstNameNotLike() {
		final Employee employee1 = new Employee("employee19", "employee19", "employee19@deloitte.com");
		final Employee employee2 = new Employee("emp20", "employee20", "employee20@deloitte.com");
		final Employee employee3 = new Employee("employee21", "employee21", "employee9@deloitte.com");
		final List<Employee> employees = Arrays.asList(employee1, employee2, employee3);
		employeeRepository.saveAll(employees);

		final List<Employee> fetchedEmployees = employeeRepository.findEmployeeByFirstNotLike("employee%");
		assertEquals(1, fetchedEmployees.size());
		assertTrue(new ReflectionEquals(employee2, "rid", "id").matches(fetchedEmployees.get(0)));
	}

	/**
	 * Test case for {@link EmployeeRepository#findEmployeeByFirstIsNotNull()}  method.
	 */
	@Test
	public void testFindEmployeeByFirstNameIsNotNull() {
		final Employee employee1 = new Employee("employee25", "employee25", "employee25@deloitte.com");
		final Employee employee2 = new Employee("emp26", "employee26", "employee26@deloitte.com");
		final Employee employee3 = new Employee("employee27", "employee27", "employee27@deloitte.com");
		final Set<Employee> employees = new TreeSet<>(Arrays.asList(employee1, employee2, employee3));
		employeeRepository.saveAll(employees);

		final List<Employee> fetchedEmployees = employeeRepository.findEmployeeByFirstIsNotNull();
		assertEquals(3, fetchedEmployees.size());
		assertCollectionObjects(employees, new TreeSet<>(fetchedEmployees), "rid", "id");
	}

	/**
	 * Test case for {@link EmployeeRepository#findEmployeeByFirstNot(String)}  method.
	 */
	@Test
	public void testFindEmployeeByFirstNameNot() {
		final Employee employee1 = new Employee("employee28", "employee28", "employee28@deloitte.com");
		final Employee employee2 = new Employee("emp29", "employee29", "employee29@deloitte.com");
		final Employee employee3 = new Employee("employee30", "employee30", "employee30@deloitte.com");
		final Set<Employee> employees = new TreeSet<>(Arrays.asList(employee1, employee2, employee3));
		employeeRepository.saveAll(employees);
		employees.remove(employee2);
		final List<Employee> fetchedEmployees = employeeRepository.findEmployeeByFirstNot("emp29");
		assertEquals(2, fetchedEmployees.size());
		assertCollectionObjects(employees, new TreeSet<>(fetchedEmployees), "rid", "id");
	}

}

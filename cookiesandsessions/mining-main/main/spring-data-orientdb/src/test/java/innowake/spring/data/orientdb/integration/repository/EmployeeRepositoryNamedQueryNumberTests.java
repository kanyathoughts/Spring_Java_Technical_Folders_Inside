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

import innowake.spring.data.orientdb.integration.repository.domain.Employee;
import innowake.spring.data.orientdb.repository.EmployeeRepository;

/**
 * Test cases for data types of type {@link Number} type.
 */
public class EmployeeRepositoryNamedQueryNumberTests extends AbstractEmployeeRepositoryIntegrationTests {
	
	private final Employee employee1 = new Employee("user1", "user1", "user1@deloitte.com");
	private final Employee employee2 = new Employee("user2", "user2", "user2@deloitte.com");
	private final Employee employee3 = new Employee("user3", "user3", "user3@deloitte.com");
	
	/**
	 * Test case for {@link EmployeeRepository#findEmployeeByUserAge(int)} method.
	 */
	@Test
	public void testFindEmployeeByAge() {
		employee1.setUserAge(Integer.valueOf(25));
		employee2.setUserAge(Integer.valueOf(29));
		employee3.setUserAge(Integer.valueOf(25));
		
		final Set<Employee> employees = new TreeSet<>(Arrays.asList(employee1, employee2, employee3));
		employeeRepository.saveAll(employees);
		employees.remove(employee2);
		
		final List<Employee> fetchedEmployees = employeeRepository.findEmployeeByUserAge(25);
		final Set<Employee> orderedEmployees = new TreeSet<>(fetchedEmployees);
		assertCollectionObjects(employees, orderedEmployees, "rid", "id");
	}
	
	/**
	 * Test case for {@link EmployeeRepository#findEmployeeByUserAgeNot(int)} method.
	 */
	@Test
	public void testFindEmployeeByAgeNot() {
		employee1.setUserAge(Integer.valueOf(25));
		employee2.setUserAge(Integer.valueOf(29));
		employee3.setUserAge(Integer.valueOf(25));
		
		final List<Employee> employees = Arrays.asList(employee1, employee2, employee3);
		employeeRepository.saveAll(employees);
		
		final List<Employee> fetchedEmployees = employeeRepository.findEmployeeByUserAgeNot(25);
		assertEquals(1, fetchedEmployees.size());
		assertTrue(new ReflectionEquals(employee2, "rid", "id").matches(fetchedEmployees.get(0)));
	}
	
	/**
	 * Test case for {@link EmployeeRepository#findEmployeeByUserAgeGreaterThan(int)} method.
	 */
	@Test
	public void testFindEmployeeByAgeGreaterThan() {
		employee1.setUserAge(Integer.valueOf(25));
		employee2.setUserAge(Integer.valueOf(29));
		employee3.setUserAge(Integer.valueOf(25));
		
		final List<Employee> employees = Arrays.asList(employee1, employee2, employee3);
		employeeRepository.saveAll(employees);
		
		final List<Employee> fetchedEmployees = employeeRepository.findEmployeeByUserAgeGreaterThan(25);
		assertEquals(1, fetchedEmployees.size());
		assertTrue(new ReflectionEquals(employee2, "rid", "id").matches(fetchedEmployees.get(0)));
	}
	
	/**
	 * Test case for {@link EmployeeRepository#findEmployeeByUserAgeGreaterThanEqual(int)} method.
	 */
	@Test
	public void testFindEmployeeByAgeGreaterThanEqual() {
		employee1.setUserAge(Integer.valueOf(25));
		employee2.setUserAge(Integer.valueOf(29));
		employee3.setUserAge(Integer.valueOf(25));
		
		final Set<Employee> employees = new TreeSet<>(Arrays.asList(employee1, employee2, employee3));
		employeeRepository.saveAll(employees);
		
		final List<Employee> fetchedEmployees = employeeRepository.findEmployeeByUserAgeGreaterThanEqual(25);
		final Set<Employee> orderedEmployees = new TreeSet<>(fetchedEmployees);
		assertCollectionObjects(employees, orderedEmployees, "rid", "id");
	}
	
	/**
	 * Test case for {@link EmployeeRepository#findEmployeeByUserAgeLessThanEqual(int)} method.
	 */
	@Test
	public void testFindEmployeeByAgeLesserThanEqual() {
		employee1.setUserAge(Integer.valueOf(25));
		employee2.setUserAge(Integer.valueOf(26));
		employee3.setUserAge(Integer.valueOf(24));
		
		final Set<Employee> employees = new TreeSet<>(Arrays.asList(employee1, employee2, employee3));
		employeeRepository.saveAll(employees);
		employees.remove(employee2);
		
		final List<Employee> fetchedEmployees = employeeRepository.findEmployeeByUserAgeLessThanEqual(25);
		final Set<Employee> orderedEmployees = new TreeSet<>(fetchedEmployees);
		assertCollectionObjects(employees, orderedEmployees, "rid", "id");
	}
	
	/**
	 * Test case for {@link EmployeeRepository#findEmployeeByUserAgeLessThan(int)} method.
	 */
	@Test
	public void testFindEmployeeByAgeLessThan() {
		employee1.setUserAge(Integer.valueOf(25));
		employee2.setUserAge(Integer.valueOf(29));
		employee3.setUserAge(Integer.valueOf(21));
		
		final List<Employee> employees = Arrays.asList(employee1, employee2, employee3);
		employeeRepository.saveAll(employees);
		
		final List<Employee> fetchedEmployees = employeeRepository.findEmployeeByUserAgeLessThan(25);
		assertEquals(1, fetchedEmployees.size());
		assertTrue(new ReflectionEquals(employee3, "rid", "id").matches(fetchedEmployees.get(0)));
	}
	
	/**
	 * Test case for {@link EmployeeRepository#findEmployeeByUserAgeNull()} method.
	 */
	@Test
	public void testFindEmployeeByAgeNull() {
		employee1.setUserAge(Integer.valueOf(25));
		employee3.setUserAge(Integer.valueOf(21));
		
		final List<Employee> employees = Arrays.asList(employee1, employee2, employee3);
		employeeRepository.saveAll(employees);
		
		final List<Employee> fetchedEmployees = employeeRepository.findEmployeeByUserAgeNull();
		assertEquals(1, fetchedEmployees.size());
		assertTrue(new ReflectionEquals(employee2, "rid", "id").matches(fetchedEmployees.get(0)));
	}
	
}

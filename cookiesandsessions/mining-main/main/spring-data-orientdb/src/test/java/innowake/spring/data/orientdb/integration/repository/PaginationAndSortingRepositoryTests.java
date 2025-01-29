/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;

import innowake.lib.core.lang.Assert;
import innowake.spring.data.orientdb.integration.repository.domain.Employee;
import innowake.spring.data.orientdb.integration.repository.domain.Employee.Designation;
import innowake.spring.data.orientdb.repository.query.Pagination;

/**
 * Pagination and sorting tests for the findAll CRUD methods.
 */
public class PaginationAndSortingRepositoryTests extends AbstractEmployeeRepositoryIntegrationTests {

	private final PageRequest oneElementPageRequest = PageRequest.of(0, 1);
	private final PageRequest twoElementPageRequest = PageRequest.of(0, 2);
	private final PageRequest fiveElementPageRequestSorted = PageRequest.of(1, 5, Direction.ASC, "firstName");

	/**
	 * Test pagination for one element page request.
	 */
	@Test
	public void testPagination() {
		final Employee coach1 = new Employee("user_11", "admin_11", "user_11@deloitte.com");
		final Employee coach2 = new Employee("user_12", "admin_13", "user_11@deloitte.com");

		employeeRepository.save(coach1);
		employeeRepository.save(coach2);

		final Page<Employee> page = employeeRepository.findAll(oneElementPageRequest);

		assertEquals(1, page.getNumberOfElements());
		assertEquals(2, page.getTotalPages());

		final Page<Employee> page2 = employeeRepository.findAll(twoElementPageRequest);

		assertEquals(2, page2.getNumberOfElements());
		assertEquals(1, page2.getTotalPages());
	}

	/**
	 * Test pagination for one element page request with ascending order sorting.
	 */
	@Test
	public void testPaginationWithSorting() {
		final Employee coach1 = new Employee("user_41", "admin_11", "user_11@deloitte.com");
		final Employee coach2 = new Employee("user_40", "admin_13", "user_11@deloitte.com");

		employeeRepository.save(coach1);
		employeeRepository.save(coach2);

		final PageRequest oneElementPageRequestSorted = PageRequest.of(0, 1, Direction.ASC, "firstName");
		final Page<Employee> page = employeeRepository.findAll(oneElementPageRequestSorted);
		assertEquals("user_40", page.getContent().get(0).getFirst());
		assertEquals("user_41", employeeRepository.findAll(page.nextPageable()).getContent().get(0).getFirst());
	}

	/**
	 * Test for pagination with page request of getting the second page.
	 */
	@Test
	public void testPaginationWithSorting2() {
		final Employee coach1 = new Employee("user_41", "admin_11", "user_11@deloitte.com");
		final Employee coach2 = new Employee("user_40", "admin_13", "user_11@deloitte.com");
		final Employee coach3 = new Employee("user_43", "admin_13", "user_11@deloitte.com");
		final Employee coach4 = new Employee("user_44", "admin_13", "user_11@deloitte.com");
		final Employee coach5 = new Employee("user_45", "admin_13", "user_11@deloitte.com");
		final Employee coach6 = new Employee("user_46", "admin_13", "user_11@deloitte.com");
		final Employee coach7 = new Employee("user_47", "admin_13", "user_11@deloitte.com");

		employeeRepository.saveAll(Arrays.asList(coach1, coach2, coach3, coach4, coach5, coach6, coach7));
		final Page<Employee> page = employeeRepository.findAll(fiveElementPageRequestSorted);
		assertEquals(2, page.getContent().size());
	}

	/**
	 * Test for pagination with unpaged request.
	 */
	@Test
	public void testPaginationWithSortingUnpaged() {
		final Employee coach1 = new Employee("user_41", "admin_11", "user_11@deloitte.com");
		final Employee coach2 = new Employee("user_40", "admin_13", "user_11@deloitte.com");
		final Employee coach3 = new Employee("user_43", "admin_13", "user_11@deloitte.com");
		final Employee coach4 = new Employee("user_44", "admin_13", "user_11@deloitte.com");
		final Employee coach5 = new Employee("user_45", "admin_13", "user_11@deloitte.com");
		final Employee coach6 = new Employee("user_46", "admin_13", "user_11@deloitte.com");
		final Employee coach7 = new Employee("user_47", "admin_13", "user_11@deloitte.com");

		employeeRepository.saveAll(Arrays.asList(coach1, coach2, coach3, coach4, coach5, coach6, coach7));
		final Page<Employee> page = employeeRepository.findAll(Pageable.unpaged());
		assertEquals(7, page.getContent().size());
	}

	/**
	 * Test {@link Pagination} with invalid value for Page Size - Exceeding maximum value allowed.
	 */
	@Test
	public void testPaginationSizeExceedingLimit() {
		assertThrows(IllegalArgumentException.class, () -> new Pagination(1, Pagination.ORIENT_RECORDS_LIMIT + 1));
	}

	/**
	 * Test {@link Pagination} with invalid value for Page Size - Lesser than minimum value allowed.
	 */
	@Test
	public void testPaginationSizeLessThanOne() {
		assertThrows(IllegalArgumentException.class, () -> new Pagination(1, 0));
	}

	/**
	 * Test sorting by one property of an entity.
	 */
	@Test
	public void testSorting() {
		final Employee coach1 = new Employee("user_41", "admin_11", "user_11@deloitte.com");
		final Employee coach2 = new Employee("user_40", "admin_13", "user_11@deloitte.com");

		employeeRepository.save(coach1);
		employeeRepository.save(coach2);

		final Iterable<Employee> employees = employeeRepository.findAll(Sort.by("firstName"));
		assertEquals("user_40", employees.iterator().next().getFirst());
	}

	/**
	 * Test sorting by one property of an entity in descending order.
	 */
	@Test
	public void testSortingInDescendingOrder() {
		final Employee coach1 = new Employee("user_41", "admin_11", "user_11@deloitte.com");
		final Employee coach2 = new Employee("user_40", "admin_13", "user_11@deloitte.com");

		employeeRepository.save(coach1);
		employeeRepository.save(coach2);

		final Iterable<Employee> employees = employeeRepository.findAll(Sort.by("firstName").descending());

		assertEquals("user_41", employees.iterator().next().getFirst());
	}

	/**
	 * Test case to validate pagination using named queries.
	 */
	@Test
	public void testNamedQueryWithPageable() {
		saveEmployees();
		final Pageable pageable = PageRequest.of(0, 3);
		final Page<Employee> page = employeeRepository.findEmployeeByEmpDesignation(Designation.CONSULTANT, pageable);
		assertEquals(0, page.getNumber());
		assertEquals(3, page.getNumberOfElements());
		assertEquals(3, page.getSize());
		assertEquals(25, page.getTotalElements());
		assertEquals(9, page.getTotalPages());
	}

	/**
	 * Test case for validating named query with sorting
	 */
	@Test
	public void testNamedQueryWithSorting() {
		final List<Employee> expectedEmployees = saveEmployees();
		final List<Employee> fetchedEmployees = employeeRepository.findEmployeeByEmpDesignationOrderByUserAgeAsc(Designation.CONSULTANT);
		assertCollectionObjects(expectedEmployees, fetchedEmployees, "rid", "id");
	}

	/**
	 * Test case for validating named query with pagination and sorting
	 */
	@Test
	public void testNamedQueryWithPaginationAndSorting() {
		final List<Employee> expectedEmployees = saveEmployees();
		final Pageable pageable = PageRequest.of(0, 3);
		final Page<Employee> page = employeeRepository.findEmployeeByEmpDesignationOrderByUserAgeAsc(Designation.CONSULTANT, pageable);
		assertEquals(0, page.getNumber());
		assertEquals(3, page.getNumberOfElements());
		assertEquals(3, page.getSize());
		assertEquals(25, page.getTotalElements());
		assertEquals(9, page.getTotalPages());
		final String first = page.getContent().get(0).getFirst();
		assertEquals(expectedEmployees.get(0).getFirst(), first);
		Assert.assertEqual(Integer.valueOf(4), page.getContent().get(2).getUserAge(), "value is not same");
	}
	
	/**
	 * Test case for validating named query with pagination and sorting
	 */
	@Test
	public void testNamedQueryWithPaginationAndSortingWithPagable() {
		final List<Employee> expectedEmployees = saveEmployees();
		final Pageable pageable = PageRequest.of(0, 3, Direction.ASC, "age");
		final Page<Employee> page = employeeRepository.findEmployeeByEmpDesignation(Designation.CONSULTANT, pageable);
		assertEquals(0, page.getNumber());
		assertEquals(3, page.getNumberOfElements());
		assertEquals(3, page.getSize());
		assertEquals(25, page.getTotalElements());
		assertEquals(9, page.getTotalPages());
		assertEquals(expectedEmployees.get(0).getFirst(), page.getContent().get(0).getFirst());
		Assert.assertEqual(Integer.valueOf(4), page.getContent().get(2).getUserAge(), "value is not same");
	}

	private List<Employee> saveEmployees() {
		final Set<Employee> employees = new TreeSet<>();
		final List<Employee> expectedEmployees = new ArrayList<>();
		for (int i = 0; i < 50; i++) {
			final String user = "user" + i;
			final Employee employee = new Employee(user, user, user);
			employee.setUserAge(Integer.valueOf(i));
			if (i%2 == 0) {
				employee.setEmpDesignation(Designation.CONSULTANT);
				expectedEmployees.add(employee);
			} else {
				employee.setEmpDesignation(Designation.MANAGER);
			}
			employees.add(employee);
		}
		employeeRepository.saveAll(employees);
		return expectedEmployees;
	}
}

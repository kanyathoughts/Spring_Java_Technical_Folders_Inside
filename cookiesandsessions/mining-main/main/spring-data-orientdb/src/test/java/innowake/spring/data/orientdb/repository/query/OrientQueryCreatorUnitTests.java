/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.repository.query;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Arrays;

import org.junit.Before;
import org.junit.Test;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.repository.query.ParameterAccessor;
import org.springframework.data.repository.query.parser.PartTree;

import innowake.spring.data.orientdb.integration.repository.domain.Employee;
import innowake.spring.data.orientdb.repository.support.OrientEntityInformation;

/**
 * Test cases for {@link OrientQueryCreator} to validate named queries.
 */
public class OrientQueryCreatorUnitTests {
	
	private final OrientQueryMethod method = mock(OrientQueryMethod.class);
	private final ParameterAccessor accessor = mock(ParameterAccessor.class);
	
	/**
	 * Initializes entity class information.
	 *
	 */
	@SuppressWarnings({
			"unchecked", "rawtypes"
	})
	@Before
	public void init() {
		when(method.getEntityInformation()).thenReturn(new OrientEntityInformation(Employee.class));
	}
	
	/**
	 * Test case for a delete query.
	 */
	@Test
	public void testDeleteQuery() {
		final PartTree tree = new PartTree("deleteByUserAgeAndEmail", Employee.class);
		final Object[] values = {Integer.valueOf(25), "Employee.123"};
		when(accessor.iterator()).thenReturn(Arrays.asList(values).iterator());
		final OrientQueryCreator queryCreator = new OrientQueryCreator(tree, method, accessor);
		
		final String query = "DELETE VERTEX Employee WHERE age = 25 AND emailId = 'Employee.123'";
		assertEquals(query, queryCreator.createQuery());
	}
	
	/**
	 * Test case for count query.
	 */
	@Test
	public void testCountQuery() {
		final PartTree tree = new PartTree("countByIsEmployeeActive", Employee.class);
		final Object[] values = {Boolean.TRUE};
		when(accessor.iterator()).thenReturn(Arrays.asList(values).iterator());
		final OrientQueryCreator queryCreator = new OrientQueryCreator(tree, method, accessor);
		final String query = "SELECT count(*) FROM Employee WHERE isActive = true";
		assertEquals(query, queryCreator.createQuery());
	}
	
	/**
	 * Test case for sorting with ignore case query.
	 */
	@Test
	public void testSortingIgnoreCaseQuery() {
		final PartTree tree = new PartTree("findEmployeeByFirst", Employee.class);
		final Sort sort = Sort.by(new Sort.Order(Sort.Direction.ASC, "firstName").ignoreCase());
		final PageRequest page = PageRequest.of(1, 5, sort);
		final Object[] values = {"Employee.123", page};
		when(accessor.iterator()).thenReturn(Arrays.asList(values).iterator());
		when(accessor.getPageable()).thenReturn(page);
		when(accessor.getSort()).thenReturn(sort);
		final OrientQueryCreator queryCreator = new OrientQueryCreator(tree, method, accessor);
		final String query = "SELECT *, $sort0 FROM Employee LET $sort0 = firstName.toLowerCase() WHERE firstName = 'Employee.123' "
				+ "ORDER BY $sort0 ASC LIMIT 5 OFFSET 5";
		assertEquals(query, queryCreator.createQuery());
	}
	
}

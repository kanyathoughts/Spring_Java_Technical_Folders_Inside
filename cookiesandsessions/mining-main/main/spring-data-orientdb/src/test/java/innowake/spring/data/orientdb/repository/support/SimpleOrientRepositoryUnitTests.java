/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.repository.support;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert.AssertionException;
import innowake.spring.data.orientdb.commons.core.OrientOperations;
import innowake.spring.data.orientdb.integration.repository.domain.Employee;

/**
 * Unit tests for {@link SimpleOrientRepository} class.
 */
@RunWith(MockitoJUnitRunner.class)
public class SimpleOrientRepositoryUnitTests {
	
	@Nullable private SimpleOrientRepository<Employee> repo;
	@Mock @Nullable private OrientEntityInformation<Employee> information;
	@Mock @Nullable private OrientOperations<Employee> operations;
	
	/**
	 * Initialize the repository.
	 */
	@Before
	public void setUp() {
		repo = new SimpleOrientRepository<>(Employee.class, assertNotNull(operations));
	}
	
	/**
	 * Test case to validate save function invocation.
	 */
	@Test
	public void testSaveWhenEntityNotNull() {
		final SimpleOrientRepository<Employee> repository = assertNotNull(repo);
		final Employee employee = new Employee("test1" , "test1", "test1@deloitte.com");
		repository.save(employee);
		verify(operations, times(1)).save(employee);
	}
	
	/**
	 * Test case to validate save when entity is null.
	 */
	@Test(expected = AssertionException.class)
	public void testSaveWhenEntityNull() {
		final SimpleOrientRepository<Employee> repository = assertNotNull(repo);
		repository.save(null);
	}

	/**
	 * Test existsById method when id is null.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testExistsByIdNull() {
		final SimpleOrientRepository<Employee> repository = assertNotNull(repo);
		final String id = null;
		repository.existsById(id);
	}
	
	/**
	 * Test deleteById method when id is null.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testDeleteByIdEmpty() {
		final SimpleOrientRepository<Employee> repository = assertNotNull(repo);
		final String id = "";
		repository.deleteById(id);
	}
}

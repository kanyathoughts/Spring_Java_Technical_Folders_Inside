/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.repository.support;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.lib.core.lang.Assert.assertTrue;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.data.repository.core.EntityInformation;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.springdata.annotations.Entity;
import innowake.mining.shared.springdata.annotations.RId;
import innowake.spring.data.orientdb.commons.core.OrientOperations;
import innowake.spring.data.orientdb.commons.exception.MetadataException;
import innowake.spring.data.orientdb.integration.repository.domain.Employee;
import innowake.spring.data.orientdb.integration.repository.domain.Reference;
import innowake.spring.data.orientdb.repository.OrientRepository;
import innowake.spring.data.orientdb.repository.ReferenceRepository;

/**
 * Unit tests for {@link OrientRepositoryFactory} class.
 */
@RunWith(MockitoJUnitRunner.class)
public class OrientRepositoryFactoryUnitTests {
	
	@Mock @Nullable private OrientOperations<Employee> operations;
	@Mock @Nullable private OrientOperations<User> userOperations;
	@Mock @Nullable private OrientOperations<Person> personOperations;
	@Mock @Nullable private OrientOperations<Reference> referenceOperations;
	
	/**
	 * Test case to validate the type of entity information.
	 */
	@Test
	public void testOrientEntityInformation() {
		final OrientRepositoryFactory<Employee> factory = new OrientRepositoryFactory<>(assertNotNull(operations));
		final EntityInformation<Employee, Object> entityInformation = factory.getEntityInformation(Employee.class);
		assertTrue(entityInformation instanceof OrientEntityInformation);
	}
	
	/**
	 * Test case to validate the repository creation.
	 */
	@Test
	public void testCreateRepository() {
		final OrientRepositoryFactory<Employee> factory = new OrientRepositoryFactory<>(assertNotNull(operations));
		final MyEmployeeRepository repository = factory.getRepository(MyEmployeeRepository.class);
		assertNotNull(repository);
	}
	
	/**
	 * Test case to ensure only one {@link RId} field is present in entity.
	 */
	@Test(expected = MetadataException.class)
	public void testMoreThanOneRId() {
		final OrientRepositoryFactory<User> factory = new OrientRepositoryFactory<>(assertNotNull(userOperations));
		final EntityInformation<User, Object> entityInformation = factory.getEntityInformation(User.class);
		final User user = new User();
		entityInformation.getId(user);
	}
	
	/**
	 * Test case to ensure only {@link String} type {@link RId} field is present in entity.
	 */
	@Test(expected = IllegalStateException.class)
	public void testRIdOfTypeLong() {
		final OrientRepositoryFactory<Person> factory = new OrientRepositoryFactory<>(assertNotNull(personOperations));
		final EntityInformation<Person, Object> entityInformation = factory.getEntityInformation(Person.class);
		final Person employee = new Person();
		entityInformation.getId(employee);
	}
	
	/**
	 * Test case to validate abstract class as repository.
	 */
	@Test
	public void testEdgeInheritance() {
		final OrientRepositoryFactory<Reference> factory = new OrientRepositoryFactory<>(assertNotNull(referenceOperations));
		final ReferenceRepository repository = factory.getRepository(ReferenceRepository.class);
		assertNotNull(repository);
	}
	
	/**
	 * Invalid entity with more than one @Rid field.
	 */
	@Entity
	static class User {
		
		@RId
		@Nullable private String rid;
		@RId
		@Nullable private String recordId;
	}
	
	@Entity
	static class Person {
		
		@RId
		@Nullable private Long rid;
	}
	
	interface MyEmployeeRepository extends OrientRepository<Employee> {

	}
	
	interface MyUserRepository extends OrientRepository<User> {

	}
	
	interface MyPersonRepository extends OrientRepository<Person> {
		
	}
}

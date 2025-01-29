/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.repository.query;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

import java.util.List;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.data.projection.ProjectionFactory;
import org.springframework.data.repository.core.NamedQueries;
import org.springframework.data.repository.query.QueryCreationException;
import org.springframework.data.repository.query.QueryLookupStrategy.Key;

import innowake.lib.core.api.lang.Nullable;
import innowake.spring.data.orientdb.commons.core.OrientOperations;
import innowake.spring.data.orientdb.integration.repository.domain.Employee;
import innowake.spring.data.orientdb.repository.OrientRepository;
import innowake.spring.data.orientdb.repository.config.EnableOrientRepositories;
import innowake.spring.data.orientdb.repository.support.OrientRepositoryFactory;

/**
 * Unit tests for {@link OrientQueryLookupStrategy} class.
 */
@RunWith(MockitoJUnitRunner.class)
public class OrientQueryLookupStrategyUnitTests {

	@Mock @Nullable private OrientOperations<Employee> operations;
	@Mock @Nullable private NamedQueries namedQueries;
	@Mock @Nullable private ProjectionFactory projectionFactory;

	/**
	 * Test case for invalid named query with declared strategy.
	 */
	@Test
	public void invalidNamedQueryExceptionWithStrategyDeclaredQuery() {
		final OrientRepositoryFactory<Employee> factory = new OrientRepositoryFactory<>(assertNotNull(operations));
		factory.setQueryLookupStrategyKey(Key.USE_DECLARED_QUERY);
		assertThatExceptionOfType(QueryCreationException.class)
		.isThrownBy(() -> factory.getRepository(UserRepository.class));
	}
	
	/**
	 * Test case for invalid named query.
	 */
	@Test
	public void invalidNamedQueryException() {
		final OrientRepositoryFactory<Employee> factory = new OrientRepositoryFactory<>(assertNotNull(operations));
		assertThatExceptionOfType(QueryCreationException.class)
		.isThrownBy(() -> factory.getRepository(UserRepository.class));
	}
	
	/**
	 * Test case for invalid named query with create strategy.
	 */
	@Test
	public void invalidNamedQueryExceptionWithStrategyCreateQuery() {
		final OrientRepositoryFactory<Employee> factory = new OrientRepositoryFactory<>(assertNotNull(operations));
		factory.setQueryLookupStrategyKey(Key.CREATE);
		assertThatExceptionOfType(QueryCreationException.class)
		.isThrownBy(() -> factory.getRepository(UserRepository.class));
	}
	
	@EnableOrientRepositories(considerNestedRepositories = true)
	static class Config {

	}

	interface UserRepository extends OrientRepository<Employee> {
		
		List<Employee> findByInvalidNamedQuery(final String param);
	}

}

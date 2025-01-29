/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.repository.support;

import java.util.Optional;

import org.springframework.data.repository.core.EntityInformation;
import org.springframework.data.repository.core.RepositoryInformation;
import org.springframework.data.repository.core.RepositoryMetadata;
import org.springframework.data.repository.core.support.RepositoryFactorySupport;
import org.springframework.data.repository.query.QueryLookupStrategy;
import org.springframework.data.repository.query.QueryLookupStrategy.Key;
import org.springframework.data.repository.query.QueryMethodEvaluationContextProvider;
import org.springframework.lang.Nullable;

import innowake.lib.core.lang.Assert;
import innowake.spring.data.orientdb.commons.core.OrientOperations;
import innowake.spring.data.orientdb.repository.query.OrientQueryLookupStrategy;

/**
 * Orient specific generic repository factory.
 * 
 * @param <T> domain type
 */
public class OrientRepositoryFactory<T> extends RepositoryFactorySupport {

	/**
	 * To perform orientdb operations
	 */
	protected final OrientOperations<T> operations;

	/**
	 * Instantiates a new {@link OrientRepositoryFactory}.
	 *
	 * @param operations the orient object template
	 */
	public OrientRepositoryFactory(final OrientOperations<T> operations) {
		super();
		this.operations = operations;
	}

	@SuppressWarnings("unchecked")
	@Override
	public <U, V> EntityInformation<U, V> getEntityInformation(final Class<U> domainClass) {
		final Class<?> domainClassRef = Assert.assertNotNull(domainClass, "Domain class must not be null!");
		return (EntityInformation<U, V>) new OrientEntityInformation<>(domainClassRef);
	}

	@Override
	protected Object getTargetRepository(final RepositoryInformation information) {
		return getTargetRepositoryViaReflection(information, Assert.assertNotNull(information).getDomainType(), operations);
	}

	@Override
	protected Class<?> getRepositoryBaseClass(final RepositoryMetadata metadata) {
		return SimpleOrientRepository.class;
	}

	@Override
	protected Optional<QueryLookupStrategy> getQueryLookupStrategy(@Nullable final Key key, final QueryMethodEvaluationContextProvider evaluationContextProvider) {
		return Optional.of(OrientQueryLookupStrategy.create(operations, key));
	}

}

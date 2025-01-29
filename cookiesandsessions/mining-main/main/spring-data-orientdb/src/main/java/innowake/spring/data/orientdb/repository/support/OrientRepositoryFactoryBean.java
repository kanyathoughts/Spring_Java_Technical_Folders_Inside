/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.repository.support;

import java.io.Serializable;

import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.repository.Repository;
import org.springframework.data.repository.core.support.RepositoryFactorySupport;
import org.springframework.data.repository.core.support.TransactionalRepositoryFactoryBeanSupport;

import innowake.spring.data.orientdb.commons.core.OrientOperations;

/**
 * Special adapter for spring's {@link FactoryBean} interface to allow easy setup of
 * repository factories via Spring configuration, to create repository proxy instances.
 * 
 * @param <T> the type of the repository
 * @param <U> the type of the entity to handle
 * @param <V> the type of the entity identifier to handle
 */
public class OrientRepositoryFactoryBean<T extends Repository<U, V>, U, V extends Serializable> extends TransactionalRepositoryFactoryBeanSupport<T, U, V> {

	@Autowired
	private OrientOperations<U> operations;

	/**
	 * Creates a new {@link OrientRepositoryFactoryBean} for the given repository interface.
	 * @param repositoryInterface must not be {@code null}.
	 */
	protected OrientRepositoryFactoryBean(final Class<? extends T> repositoryInterface) {
		super(repositoryInterface);
	}

	@Override
	protected RepositoryFactorySupport doCreateRepositoryFactory() {
		return new OrientRepositoryFactory<U>(operations);
	}

}

/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.repository.config;

import javax.sql.DataSource;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.cache.CacheManager;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cache.concurrent.ConcurrentMapCacheManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;

import innowake.spring.data.orientdb.commons.core.OrientOperations;
import innowake.spring.data.orientdb.commons.core.SessionManager;
import innowake.spring.data.orientdb.commons.core.SortingAndPaginationImpl;
import innowake.spring.data.orientdb.ogm.mapping.custom.CustomPropertyService;
import innowake.spring.data.orientdb.ogm.mapping.custom.CustomPropertyServiceImpl;
import innowake.spring.data.orientdb.repository.cache.OrientRequestCacheManager;
import innowake.spring.data.orientdb.transaction.OrientTransactionManager;

/**
 * Creates all the required application beans.
 */
@EnableCaching
@Configuration
public class OrientSpringConfiguration {
	
	/**
	 * Instantiates {@link SessionManager} class.
	 *
	 * @param dataSource instance set by application
	 * @return {@link SessionManager} instance
	 */
	@Bean
	@Autowired
	public SessionManager orientDbSessionFactory(final DataSource dataSource) {
		return new SessionManager(dataSource);
	}
	
	/**
	 * Instantiates orientdb transactions.
	 *
	 * @return instance of {@link OrientTransactionManager}
	 */
	@Qualifier("orientdb-transaction")
	@Bean
	public OrientTransactionManager transactionManager() {
		return new OrientTransactionManager();
	}

	/**
	 * Returns a single instance of {@link CustomPropertyService}.
	 *
	 * @return a single instance of {@link CustomPropertyService}
	 */
	@Bean
	@Scope("singleton")
	public CustomPropertyService getCustomProperty() {
		return new CustomPropertyServiceImpl();
	}
	
	/**
	 * Instantiates {@link OrientOperations} used to perform orientDB operations
	 *
	 * @param sessionManager instance of {@link SessionManager}
	 * @return instance of {@link SortingAndPaginationImpl}
	 */
	@Bean
	@Autowired 
	public <T> OrientOperations<T> orientOperations(final SessionManager sessionManager) {
		return new SortingAndPaginationImpl<>(sessionManager, getCustomProperty());
	}
	
	/**
	 * Create proxy to intercept the method call and handle the caching behavior.
	 * 
	 * Instantiates {@link CacheManager} class.
	 * @return {@link ConcurrentMapCacheManager} instance
	 */
	@Bean
	public CacheManager springDataCacheManager() {
		return new OrientRequestCacheManager();
	}
}

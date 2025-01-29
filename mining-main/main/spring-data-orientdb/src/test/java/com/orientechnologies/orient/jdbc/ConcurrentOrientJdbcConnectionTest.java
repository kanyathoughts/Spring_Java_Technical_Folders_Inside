/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package com.orientechnologies.orient.jdbc;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.sql.Connection;
import java.sql.SQLException;

import javax.sql.DataSource;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import com.orientechnologies.orient.core.db.ODatabaseRecordThreadLocal;
import com.orientechnologies.orient.core.db.document.ODatabaseDocumentAbstract;

import innowake.spring.data.orientdb.integration.repository.AbstractEmployeeRepositoryIntegrationTests;

public class ConcurrentOrientJdbcConnectionTest extends AbstractEmployeeRepositoryIntegrationTests {
	
	@Autowired
	private DataSource dataSource;
	
	@Test
	public void testIfProperInstancesAreCreated() throws SQLException {
		assertNotNull("dataSource is null", dataSource);
		try (Connection connection = dataSource.getConnection()) {
			assertTrue("proper connection is not created", connection.isWrapperFor(OrientJdbcConnection.class));
		}
	}
	
	/**
	 * 
	 * Checks that {@link IllegalStateException} from {@link ODatabaseDocumentAbstract#checkIfActive()} is not thrown when 2 different DB instances in the same thread commit.
	 *
	 * @throws SQLException thrown from getConnection of DataSource
	 */
	@Test
	public void testToCheckNotActiveInCurrentThreadIsThrownInCommit() throws SQLException {
		final Connection connection = getConnectionAndDeactivateInCurrentThread();
		connection.commit();
	}
	
	/**
	 * 
	 * Checks that  {@link IllegalStateException} from {@link ODatabaseDocumentAbstract#checkIfActive()} is not thrown when 2 different DB instances in the same thread rolls back.
	 *
	 * @throws SQLException thrown from getConnection of DataSource
	 */
	@Test
	public void testToCheckNotActiveInCurrentThreadIsThrownInRollback() throws SQLException {
		final Connection connection = getConnectionAndDeactivateInCurrentThread();
		connection.rollback();
	}
	
	/**
	 * 
	 * Checks that  {@link IllegalStateException} from {@link ODatabaseDocumentAbstract#checkIfActive()} is not thrown when 2 different DB instances in the same thread gets catalog.
	 *
	 * @throws SQLException thrown from getConnection of DataSource
	 */
	@Test
	public void testToCheckNotActiveInCurrentThreadIsThrownInGetCatalog() throws SQLException {
		final Connection connection = getConnectionAndDeactivateInCurrentThread();
		connection.getCatalog();
	}

	private Connection getConnectionAndDeactivateInCurrentThread() throws SQLException {
		final Connection connection = dataSource.getConnection();
		// this will remove the connection from current thread
		ODatabaseRecordThreadLocal.instance().set(null);
		return connection;
	}

}

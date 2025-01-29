/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.commons.core;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.concurrent.atomic.AtomicLong;

import javax.sql.DataSource;

import com.orientechnologies.orient.jdbc.ConcurrentOrientJdbcConnection;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.stereotype.Component;

import com.orientechnologies.orient.core.Orient;
import com.orientechnologies.orient.core.db.ODatabaseDocumentInternal;
import com.orientechnologies.orient.core.db.ODatabaseRecordThreadLocal;
import com.orientechnologies.orient.core.db.ODatabaseSession;
import com.orientechnologies.orient.core.db.ODatabaseThreadLocalFactory;
import com.orientechnologies.orient.core.db.document.ODatabaseDocument;
import com.orientechnologies.orient.jdbc.OrientJdbcConnection;

import innowake.lib.core.api.lang.NonNull;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;

/**
 * Gets a connection to database from the pool and returns an {@link ODatabaseSession}.
 */
@Component
public class SessionManager implements ODatabaseThreadLocalFactory, DisposableBean {
	
	private static final Logger LOG = LoggerFactory.getLogger(SessionManager.class);
	private static final ThreadLocal<Connection> THREAD_LOCAL_CONNECTION = new ThreadLocal<>();
	private static final ThreadLocal<OrientJdbcConnection> THREAD_LOCAL_ORIENT_CONNECTION = new ThreadLocal<>();
	
	private final DataSource dataSource;

	private final AtomicLong lastSchemaChange = new AtomicLong(-1);
	
	/**
	 * Constructor to initialize the session manager.
	 * 
	 * @param dataSource member of the session manager
	 */
	public SessionManager(final DataSource dataSource) {
		this.dataSource = dataSource;
		/* when no DB instance is registered in the current thread or the DB instance already registered is closed and if we 
		 * directly operate on vertex or edge, orient will not find any DB instance and will check if a DB instance factory
		 * such as this is registered. if not found -> throws an exception and if found -> uses it to get the new DB instance. 
		 * check ODatabaseRecordThreadLocal#get for more info */
		Orient.instance().registerThreadDatabaseFactory(this);
	}
	
	/**
	 * Closes the connection and removes it from the local member list.
	 */
	public void closeConnection() {
		try {
			final Connection connection = THREAD_LOCAL_CONNECTION.get();
			if (connection != null) {
				connection.close();
				THREAD_LOCAL_CONNECTION.remove();
				THREAD_LOCAL_ORIENT_CONNECTION.remove();
				ODatabaseRecordThreadLocal.instance().remove();
			}
		} catch (final SQLException e) {
			throw new IllegalStateException("Connection wasn't able to close", e);
		}
	}
	
	/**
	 * Closes the threadDatabase if there is no transaction active.
	 *
	 */
	public void closeNonTransactionalThreadDatabase() {
		if (hasThreadDatabase() && ! getThreadDatabase().getTransaction().isActive()) {
			closeConnection();
		}
	}
	
	/**
	 * Returns a {@link ODatabaseDocumentInternal} session.
	 * 
	 * @return the database session after checking if it is active on the current thread
	 * that is trying to get the session
	 */
	@Override
	public ODatabaseDocumentInternal getThreadDatabase() {
		if ( ! hasThreadDatabase()) {
			LOG.debug(() -> "creating new connection as connection not found for the thread : " + Thread.currentThread().getName());
			loadConnectionInToThreadLocal();
		}

		@NonNull
		final var orientJdbcConnection = THREAD_LOCAL_ORIENT_CONNECTION.get();
		try {
			if ( ! orientJdbcConnection.isValid(0)) {
				closeConnection();
				loadConnectionInToThreadLocal();
			}
		} catch (final SQLException e) {
			LOG.error("Error while checking if DB connection is valid", e);
			closeConnection();
			loadConnectionInToThreadLocal();
		}
		if (lastSchemaChange.get() != -1) {
			((ConcurrentOrientJdbcConnection) orientJdbcConnection).refreshSchemaIfRequired(lastSchemaChange.get());
		}

		final ODatabaseDocument database = orientJdbcConnection.getDatabase();
		if ( ! database.isActiveOnCurrentThread()) {
			database.activateOnCurrentThread();
		}
		return (ODatabaseDocumentInternal) database;
	}
	
	/**
	 * Needed so that the same connection is not used in the subsequent tests after the pool is closed.
	 */
	@Override
	public void destroy() throws Exception {
		closeConnection();
	}
	
	private boolean hasThreadDatabase() {
		try {
			return THREAD_LOCAL_CONNECTION.get() != null && ! THREAD_LOCAL_CONNECTION.get().isClosed();
		} catch (final SQLException e) {
			throw new IllegalStateException(e);
		}
	}
	
	private void loadConnectionInToThreadLocal() {
		try {
			final Connection connection = dataSource.getConnection();
			final ConcurrentOrientJdbcConnection orientConnection = connection.unwrap(ConcurrentOrientJdbcConnection.class);
			
			THREAD_LOCAL_CONNECTION.set(connection);
			THREAD_LOCAL_ORIENT_CONNECTION.set(orientConnection);
		} catch (final Exception e) {
			throw new IllegalStateException("Unable to retrieve Connection from " + dataSource, e);
		}
	}

	/**
	 * Set lastSchemaChange to the current time in milliseconds to mark that there was a schema change.
	 */
	public void markSchemaChanged() {
		lastSchemaChange.set(System.currentTimeMillis());
	}
}

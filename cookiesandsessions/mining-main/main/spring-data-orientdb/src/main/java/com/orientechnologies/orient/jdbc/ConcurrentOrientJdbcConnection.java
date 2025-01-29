/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.*/
package com.orientechnologies.orient.jdbc;

import java.sql.SQLException;
import java.util.Properties;

import com.orientechnologies.orient.core.db.document.ODatabaseDocument;
import com.orientechnologies.orient.core.db.document.ODatabaseDocumentRemote;

/**
 * Extends the original {@link OrientJdbcConnection} to be able to use the connection in a multithreaded environment.
 * @see <a href="https://orientdb.com/docs/3.1.x/java/Java-Multi-Threading.html">Orient DB Multi-Threading</a>
 */
public class ConcurrentOrientJdbcConnection extends OrientJdbcConnection {

	private static final String CONNECTION_TEST_QUERY = "SELECT eval(1)";
	public static final long CONNECTION_CHECK_INTERVAL = 500L;

	private long lastAccess;
	private long lastSchemaRefresh;

	/**
	 * Instantiate {@link ConcurrentOrientJdbcConnection}.
	 * @param url JDBC connection URL
	 * @param info properties configured through application.yml
	 */
	public ConcurrentOrientJdbcConnection(final String url, final Properties info) {
		super(url, info);
		lastAccess = System.currentTimeMillis();
		lastSchemaRefresh = lastAccess;
	}

	@Override
	public String getCatalog() throws SQLException {
		getDatabase().activateOnCurrentThread();
		return super.getCatalog();
	}

	@Override
	public void commit() throws SQLException {
		getDatabase().activateOnCurrentThread();
		super.commit();
	}

	@Override
	public void rollback() throws SQLException {
		getDatabase().activateOnCurrentThread();
		super.rollback();
	}
	
	@Override
	public boolean isValid(final int timeout) throws SQLException {
		final ODatabaseDocumentRemote db = (ODatabaseDocumentRemote) getDatabase();
		db.activateOnCurrentThread();
		if (db.isClosed() || db.getSessionMetadata().commandExecuting) {
			return false;
		}
		if (System.currentTimeMillis() - lastAccess >= CONNECTION_CHECK_INTERVAL) {
			try {
				db.query(CONNECTION_TEST_QUERY, new Object[0]).close();
			} catch (final Exception e) {
				return false;
			}
		}
		lastAccess = System.currentTimeMillis();
		return true;
	}

	/**
	 * Reloads schema information that is cached in the connection if the last refresh is older than the given timestamp.
	 *
	 * @param lastSchemaChange the timestamp of the last known schema change
	 */
	@SuppressWarnings("deprecation")
	public void refreshSchemaIfRequired(final long lastSchemaChange) {
		if (lastSchemaChange > lastSchemaRefresh) {
			final ODatabaseDocument db = getDatabase();
			db.activateOnCurrentThread();
			db.getMetadata().getSchema().reload();
			db.getMetadata().getIndexManager().reload();
			lastSchemaRefresh = System.currentTimeMillis();
		}
	}
}

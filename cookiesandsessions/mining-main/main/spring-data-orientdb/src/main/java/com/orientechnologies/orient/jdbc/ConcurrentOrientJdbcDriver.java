/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package com.orientechnologies.orient.jdbc;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Properties;

import com.orientechnologies.common.log.OLogManager;
import com.orientechnologies.orient.core.db.document.ODatabaseDocument;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;

/**
 * Extends the original {@link OrientJdbcDriver} to be able to create our own implementation of {@link OrientJdbcConnection} for multi-threading support
 * for both JDBC-template implementation using {@link OrientJdbcConnection} and spring-data implementation using {@link ODatabaseDocument} for DB access.
 */
public class ConcurrentOrientJdbcDriver extends OrientJdbcDriver {
	private static final Logger ORIENT_LOG = LoggerFactory.getLogger("com.orientechnologies");
	
	static {
		OLogManager.instance().setDebugEnabled(ORIENT_LOG.isDebugEnabled());
		try {
			DriverManager.registerDriver(new ConcurrentOrientJdbcDriver());
		} catch (final SQLException e) {
			OLogManager.instance().error(null, "Error while registering the JDBC Driver wrapper", e);
		}
	}
	
	/**
	 * Creates and returns connection of type {@link ConcurrentOrientJdbcConnection}.
	 * 
	 * @param url the URL of the database to which to connect
	 * @param info key/value pairs as connection arguments. 
	 * @return a <code>Connection</code> object that represents a
	 *         connection to the URL
	 * @exception SQLException if a database access error occurs or the url is
	 * {@code null}
	 */
	@Override
	public Connection connect(final String url, final Properties info) throws SQLException {
		if ( ! acceptsURL(url)) {
			return null;
		}
		return new ConcurrentOrientJdbcConnection(url, info);
	}
}

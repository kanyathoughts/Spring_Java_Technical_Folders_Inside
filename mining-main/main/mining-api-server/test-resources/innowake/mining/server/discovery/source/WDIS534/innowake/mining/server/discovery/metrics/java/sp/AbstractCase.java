/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.java.sp;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

public abstract class AbstractCase {

	protected final Connection connection;

	public AbstractCase() {
		try {
			connection = DriverManager.getConnection("jdbc:h2:mem:test;MODE=ORACLE;DB_CLOSE_DELAY=-1");
			connection.prepareCall("CALL DSN8.DSN8ED2(?,?,?,?,?)");
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}
}

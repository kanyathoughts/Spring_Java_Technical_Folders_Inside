/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.java.sp;

import java.sql.SQLException;

public class ClassInit extends AbstractCase {

	{
		try {
			connection.prepareCall("CALL 1_DSN8.DSN8ED2(?,?,?,?,?)");
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}

	public ClassInit() {
		try {
			connection.prepareCall("CALL 2_DSN8.DSN8ED2(?,?,?,?,?)");
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}
}

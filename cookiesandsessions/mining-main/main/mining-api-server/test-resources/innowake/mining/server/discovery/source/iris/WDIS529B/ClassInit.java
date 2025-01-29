/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package A;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

public class ClassInit {

	private final Connection connection;

	{
		try {
			connection = DriverManager.getConnection("jdbc:h2:mem:test;MODE=ORACLE;DB_CLOSE_DELAY=-1");
			connection.prepareCall("CALL GETPRML(?,?,?,?,?)");
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}

	public ClassInit() {
		try {
			connection.prepareCall("CALL SYSPROC.GETPRML(?,?,?,?,?)");
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}
}

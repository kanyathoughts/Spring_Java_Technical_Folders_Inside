/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package A;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

public class Fields {

	private static final String SP1 = "CALL SP1_DSN8.DSN8ED2(?,?,?,?,?)";
	private final String sp2 = "CALL SP2_DSN8.DSN8ED2(?,?,?,?,?)";

	private final OtherClass otherClass = new OtherClass();

	private final Connection connection;

	public Fields() {
		try {
			connection = DriverManager.getConnection("jdbc:h2:mem:test;MODE=ORACLE;DB_CLOSE_DELAY=-1");
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}

	public void a() {
		try {
			b();

			/* 
			 * Entry 1 in expected file
			 */
			connection.prepareCall(SP1);
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}

	public void b() {
		try {
			if (connection != null) {
				/* 
				 * Entry 2 in expected file
				 */
				connection.prepareCall(sp2);
			} else {
				c();
			}
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}

	public void c() {
		try {
			connection.prepareCall(OtherClass.O_SP1);
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}

	@SuppressWarnings("static-access")
	public void d() {
		try {
			connection.prepareCall(otherClass.O_SP2);
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}

	public void e() {
		try {
			connection.clearWarnings();
			connection.prepareCall(otherClass.o_sp4);
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}

	private static class OtherClass {

		public static final String O_SP1 = "CALL O_SP1_DSN8.DSN8ED2(?,?,?,?,?)";
		public static final String O_SP2 = "CALL O_SP2_DSN8.DSN8ED2(?,?,?,?,?)";

		public final String o_sp4 = "CALL o_sp4_DSN8.DSN8ED2(?,?,?,?,?)";

		public CallableStatement getCallable1() {
			try {
				final Connection connection = DriverManager.getConnection("jdbc:h2:mem:test;MODE=ORACLE;DB_CLOSE_DELAY=-1");
				return connection.prepareCall("CALL 1_DSN8.DSN8ED21(?,?,?,?,?)");
			} catch (SQLException e) {
				throw new IllegalStateException(e);
			}
		}
	}
}

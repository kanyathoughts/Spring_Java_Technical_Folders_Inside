/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.java.sp;

import java.sql.SQLException;

public class FakeMethods extends AbstractCase {

	public static interface CallableStatement { }

	public static class Connection {

		@SuppressWarnings("unused")
		public CallableStatement prepareCall(final String sql) throws SQLException {
			return new CallableStatement() {

				@Override
				public String toString() {
					return sql;
				}
			};
		}
	}

	private final Connection connection2 = new Connection();

	@SuppressWarnings("unused")
	public FakeMethods() {
		try {
			final CallableStatement statement = connection2.prepareCall("CALL FAKE.FAKE1(?,?,?,?,?)");
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}

	public CallableStatement getCallable1() {
		try {
			return connection2.prepareCall("CALL FAKE.FAKE2(?,?,?,?,?)");
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}

	public void prepareCall() {
		try {
			connection2.prepareCall("CALL FAKE.FAKE3(?,?,?,?,?)");
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}
}

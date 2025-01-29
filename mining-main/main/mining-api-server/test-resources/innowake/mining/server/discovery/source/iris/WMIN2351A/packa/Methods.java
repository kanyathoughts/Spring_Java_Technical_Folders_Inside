/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package WMIN2351A.packa;

import java.sql.CallableStatement;
import java.sql.SQLException;
import java.sql.Statement;

import WMIN2351A.AbstractCase;

public class Methods extends AbstractCase {

	public CallableStatement getCallable1(final String call) {
		System.out.println("sysout");
		try {
			/* 
			 * Entry 1 in expected file
			 */
			final CallableStatement lokalSP1 = connection.prepareCall("CALL 1_DSN8.M1_DSN8ED2(?,?,?,?,?)");
			if (lokalSP1.execute()) {
				connection.clearWarnings();
			}

			final Statement statement = connection.createStatement();
			statement.execute(call);

			/*
			 * Entry 2 in expected file
			 */
			return connection.prepareCall(call);
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}

	public void getCallable2() {
		try {
			/*
			 * Entry 3 in expected file
			 */
			final CallableStatement lokalSP1 = connection.prepareCall("CALL 2a_DSN8.M2_DSN8ED2(?,?,?,?,?)");
			if (lokalSP1.execute()) {

				/*
				 * Entry 4 in expected file
				 */
				final CallableStatement lokalSP2 = connection.prepareCall("CALL 2b_DSN8.M2_DSN8ED2(?,?,?,?,?)");
				lokalSP2.cancel();
			}
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}

	public CallableStatement getCallable3() {
		getCallable2();

		/*
		 * Entry 5 in expected file
		 */
		return getCallable1("CALL 3_DSN8.DSN8ED1(?,?,?,?,?)");
	}

	public void getCallable4() {
		getCallable2();
		try {
			/*
			 * Entry 6 in expected file
			 */
			getCallable1("CALL 4_DSN8.DSN8ED2(?,?,?,?,?)").execute();
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}

	public CallableStatement getCallable5(final String name) {
		try {
			/*
			 * Entry 7 in expected file
			 */
			return connection.prepareCall("CALL 5_DSN8." + name + "(?,?,?,?,?)");
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}

	public void getCallable6(final String name) {
		try {
			/*
			 * Entry 8 in expected file
			 */
			connection.prepareCall("CALL 6a_DSN8." + name + "(?,?,?,?,?)");

			/*
			 * Entry 9 in expected file
			 */
			connection.prepareCall("CALL 6b_DSN8." + name.trim() + "(?,?,?,?,?)");

			/*
			 * Entry 10 in expected file
			 */
			connection.prepareCall("CALL 6c_DSN8." + (name.length() == 0 ? "name" : name) + "(?,?,?,?,?)");

			/*
			 * Entry 11 in expected file
			 */
			connection.prepareCall(getCall());
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}

	private String getCall() {
		return "CALL 6c_DSN8.name(?,?,?,?,?)";
	}
	
	public CallableStatement getCallable7() {
		getCallable4();

		/*
		 * Entry 12 in expected file
		 */
		getCallable5("SP7a");

		getCallable6("SP7b");

		/*
		 * Entry 13 in expected file
		 */
		getCallable5("SP7c");

		return null;
	}

	public CallableStatement getCallable8() {
		/*
		 * Entry 14 in expected file
		 */
		return getCallable7();
	}
}

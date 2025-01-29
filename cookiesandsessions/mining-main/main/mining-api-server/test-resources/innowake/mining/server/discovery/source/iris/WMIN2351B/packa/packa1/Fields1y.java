/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package WMIN2351B.packa.packa1;

import static WMIN2351B.packc.OtherClass.O_SP1;

import java.sql.SQLException;

import WMIN2351B.AbstractCase;
import WMIN2351B.packc.OtherClass;

public class Fields1y extends AbstractCase {

	private static final String SP1 = "CALL SP1_DSN8.DSN8ED2(?,?,?,?,?)";
	private final String sp2 = "CALL SP2_DSN8.DSN8ED2(?,?,?,?,?)";

	private final OtherClass otherClass = new OtherClass();

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
			/* 
			 * Entry 3 in expected file
			 */
			connection.prepareCall(O_SP1);
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}

	public void d() {
		try {
			/* 
			 * Entry 4 in expected file
			 */
			connection.prepareCall(OtherClass.O_SP2);
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}

	@SuppressWarnings("static-access")
	public void e() {
		try {
			/* 
			 * Entry 5 in expected file
			 */
			connection.prepareCall(otherClass.O_SP3);
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}

	public void f() {
		try {
			connection.clearWarnings();

			/* 
			 * Entry 6 in expected file
			 */
			connection.prepareCall(otherClass.o_sp4);
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}
}

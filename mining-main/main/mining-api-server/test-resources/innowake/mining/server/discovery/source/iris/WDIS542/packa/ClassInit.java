/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package WDIS542.packa;

import java.sql.SQLException;

import WDIS542.AbstractCase;

public class ClassInit extends AbstractCase {

	{
		try {
			connection.prepareCall("CALL 1_DSN8.CI1_DSN8ED2(?,?,?,?,?)");
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}

	public ClassInit() {
		try {
			connection.prepareCall("CALL 2_DSN8.CI2_DSN8ED2(?,?,?,?,?)");
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}
}

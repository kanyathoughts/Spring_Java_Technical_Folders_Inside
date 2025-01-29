/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package WMIN6537;

import java.sql.SQLException;

import WDIS542.AbstractCase;

public class Aclass extends AbstractCase {

	{
		try {
			connection.prepareCall("CALL 1_DSN8.CI1_DSN8ED2(?,?,?,?,?)");
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}

	public Aclass() {
		try {
			connection.prepareCall("CALL 2_DSN8.CI2_DSN8ED2(?,?,?,?,?)");
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}
}

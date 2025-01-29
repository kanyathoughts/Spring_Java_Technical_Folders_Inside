/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data;

import java.sql.PreparedStatement;
import java.sql.SQLException;

/**
 * Initializes a {@link PreparedStatement} after it was created.
 */
@FunctionalInterface
public interface PreparedStatementInitializer {

	/**
	 * Initializes a {@link PreparedStatement} after it was created.
	 *
	 * @param statement the {@link PreparedStatement} to initialize
	 * @return the initialized {@link PreparedStatement}
	 * @throws SQLException if an error occurs
	 */
	PreparedStatement initPreparedStatement(PreparedStatement statement) throws SQLException;
}

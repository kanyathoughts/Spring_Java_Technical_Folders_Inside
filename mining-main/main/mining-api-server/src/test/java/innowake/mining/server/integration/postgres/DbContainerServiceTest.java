/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.postgres;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.function.Consumer;

import javax.sql.DataSource;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.EntityId;

/**
 * Test to verify status and connection to database container
 */
class DbContainerServiceTest extends DatabaseRelatedTest {

	@Autowired
	@Qualifier("postgres")
	private DataSource ds;
	
	private static final EntityId PROJECT_ID = EntityId.of(1l);

	/**
	 * Test to verify container status
	 */
	@Test
	void testContainerStatus() {
		getPostgreSQLContainer().ifPresent(container -> {
			assertTrue(container.isRunning());
		});
	}

	/**
	 * Test to verify connection
	 */
	@Test
	void testPostgresDBConnection() throws SQLException {
		performQuery("SELECT 1", resultSet -> {
			try {
				assertTrue(resultSet.next());
			} catch (SQLException e) {
				throw new IllegalStateException(e);
			}
		});
		assertNotNull(projectService.get(PROJECT_ID));
	}

	private void performQuery(final String sql, final Consumer<ResultSet> consumer) throws SQLException {
		try (
			final Connection db = ds.getConnection();
			final Statement statement = db.createStatement();
		) {
			statement.execute(sql);
			try (final ResultSet rs = statement.getResultSet()) {
				consumer.accept(rs);
			}
		}
	}

}
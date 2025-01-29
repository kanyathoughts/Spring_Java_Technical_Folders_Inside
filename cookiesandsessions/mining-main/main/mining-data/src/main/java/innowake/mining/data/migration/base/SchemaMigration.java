/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.data.migration.base;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import org.apache.commons.io.IOUtils;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;

/**
 * Runs the schema  migration script
 */
public abstract class SchemaMigration {
	
	protected static final Logger LOG = LoggerFactory.getLogger(innowake.mining.data.Logging.MIGRATION);
	
	protected final SchemaMigrationContext context;
	
	protected interface SqlRunnable extends Runnable {
		public void runSQL() throws SQLException;
		
		@Override
		public default void run() {
			try {
				runSQL();
			} catch (SQLException e) {
				throw new IllegalStateException(e);
			}
		}
	}
	
	protected SchemaMigration(SchemaMigrationContext context) {
		this.context = context;
	}
	
	/**
	 * Runs the migration.
	 * @throws SQLException If anything goes wrong.
	 */
	public abstract void migrate() throws SQLException;
	
	protected static String readResourceFile(final String resName) {
		final URL res = PostgresSchemaMigrations.class.getResource(resName);
		try {
			if (res == null) {
				throw new FileNotFoundException(resName);
			}
			return IOUtils.toString(res, StandardCharsets.UTF_8);
		} catch (IOException e) {
			throw new IllegalArgumentException("Error reading resource: " + resName);
		}
	}
	
	protected static void executeScript(final Connection db, final String path, final String name,
			final Function<String, List<String>> queryTransform) throws SQLException {
		final String resName = path + name + ".sql";
		LOG.info("Executing script " + resName);
		try {
			final var query = readResourceFile(resName);
			executeStatements(db, queryTransform != null ? queryTransform.apply(query) : Collections.singletonList(query));
		} catch (final Exception e) {
			throw new SQLException("Error executing script " + resName, e);
		}
	}

	/**
	 * Executes the given {@code statements}.
	 *
	 * @param db the DB connection, OrientDB or Postgres
	 * @param statements SQL statements to execute
	 * @throws SQLException in case of an error
	 */
	protected static void executeStatements(final Connection db, final List<String> statements) throws SQLException {
		try (final var st = db.createStatement()) {
			for (final var s : statements) {
				int limit = 10;
				while (limit > 0) {
					limit--;
					try {
						/* Statements where OrientDB needs to recursively delete schema objects sometimes fail,
						 * with the problem often resolving itself if the statement is executed again. */
						if ( ! s.trim().isEmpty()) {
							LOG.info(() -> "Executing statement [ " + s.replaceAll("\\s+", " ") + " ]");
							st.execute(s);
						}
						limit = 0;
					} catch (final Exception e) {
						if (limit == 0) {
							throw e;
						}
						LOG.warn("Statement [" + s.replaceAll("\\s+", " ") + "] executed with errors. Retrying " + limit + " more times.", e);
					}
				}
			}
		}
	}
	
	protected static void executeStatements(final Connection db, final String... statements) throws SQLException {
		executeStatements(db, Arrays.asList(statements));
	}
	
	/**
	 * Executes the given update {@code statement} batchwise. The caller is responsible for setting the LIMIT option and for ensuring that the query will not
	 * loop infinitely. The given {@code statement} is executed until the DB returns "0"
	 *
	 * @param db the DB connection, OrientDB or Postgres
	 * @param statement SQL statements to execute
	 * @throws SQLException in case of an error
	 */
	protected static void updateBatchwise(final Connection db, final String statement) throws SQLException {
		try (final var st = db.createStatement()) {
			var count = 0;
			var sum = 0;
			do {
				count = st.executeUpdate(statement);
				sum += count;
				LOG.info("Executing batchwise update: {}. Counter: {}, Processed: {}", statement, Integer.valueOf(count), Integer.valueOf(sum));
			} while (count != 0);
		}
	}
}

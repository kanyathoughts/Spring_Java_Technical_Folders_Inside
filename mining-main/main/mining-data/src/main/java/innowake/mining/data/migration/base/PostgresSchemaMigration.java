/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.data.migration.base;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;
import java.util.function.Function;

import javax.sql.DataSource;

import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.SingleConnectionDataSource;

/**
 * Base class for implementing Postgres schema migrations to a specific version.
 */
public abstract class PostgresSchemaMigration extends SchemaMigration {

	protected final Connection dbPostgres;
	
	/**
	 * Initializes the migration.
	 * @param context The specification of the environment in which the migration is to be run.
	 */
	protected PostgresSchemaMigration(final SchemaMigrationContext context) {
		super(context);
		this.dbPostgres = context.getPostgresConnection();
	}
	
	protected JdbcTemplate getPgTemplate() {
		return new JdbcTemplate(new SingleConnectionDataSource(dbPostgres, true));
	}
	
	/**
	 * Reset a number sequence to the value obtained from a query.
	 * @param name Name of the sequence definition.
	 * @param valueQuery Query returning the next free value for the sequence.
	 * @throws SQLException If a query fails.
	 */
	protected void updatePgSequence(final String name, final String valueQuery) throws SQLException {
		try (final PreparedStatement stValue = dbPostgres.prepareStatement(valueQuery)) {
			final ResultSet rsValue = stValue.executeQuery();
			rsValue.next();
			final var value = rsValue.getLong(1);
			LOG.info("Updating sequence " + name + " to " + value);
			try (final var st = dbPostgres.createStatement()) {
				st.execute("ALTER SEQUENCE \"" + name + "\" RESTART " + value);
			}
		} catch (final Exception e) {
			throw new SQLException("Error updating sequence " + name, e);
		}
	}
	
	protected String getVersionedResourcePath() {
		return String.format("/db/schema/postgres/%06d/", context.getCurrentVersion());
	}
	
	protected String readVersionedResourceFile(final String name) {
		return readResourceFile(getVersionedResourcePath() + name);
	}
	
	/**
	 * Load an SQL script from the resource folder and run it against the Postgres database.
	 * @param name Name of the script file, without extension.
	 * @param queryTransform Optional filter to modify/extend the script before execution.
	 * @throws SQLException If anything goes wrong.
	 */
	protected void executePgScript(final String name, final Function<String, List<String>> queryTransform) throws SQLException {
		executeScript(dbPostgres, getVersionedResourcePath(), name, queryTransform);
	}
	
	/**
	 * Load an SQL script from the resource folder and run it against the Postgres database.
	 * @param name Name of the script file, without extension.
	 * @throws SQLException If anything goes wrong.
	 */
	protected void executePgScript(final String name) throws SQLException {
		executePgScript(name, null);
	}
	
	public static void executePgScript(final DataSource ds, final String name, final Function<String, List<String>> queryTransform) throws SQLException {
		try (final var db = ds.getConnection()) {
			executeScript(db, "/db/data/postgres/", name, queryTransform);
		} 
	}
	
	public static void executePgScript(final DataSource db, final String name) throws SQLException {
		executePgScript(db, name, null);
	}

}

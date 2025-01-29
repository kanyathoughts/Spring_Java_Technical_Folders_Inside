/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.data.migration.base;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;

/**
 * Base class for implementing OrientDB schema migrations as part of the Postgres migration process.
 */
public abstract class OrientSchemaMigration extends SchemaMigration {
	
	protected final Connection dbOrient;
	
	/**
	 * Initializes a migration with connection OrientDB.
	 * @param context The specification of the environment in which the migration is to be run.
	 */
	protected OrientSchemaMigration(final SchemaMigrationContext context) {
		super(context);
		this.dbOrient = context.getOrientConnection();
	}
	
	public abstract void migrateOrient() throws SQLException;
	
	@Override
	public final void migrate() throws SQLException {
		if (dbOrient == null) {
			LOG.info("No OrientDB available, skipping migration.");
			return;
		}
		migrateOrient();
	}
	
	public final void ifDeletionRequested(final SqlRunnable deletion, final Supplier<String> skipMessage) {
		if (Boolean.getBoolean("mining.schema.migration.orient.deletion")) {
			deletion.run();
		} else {
			LOG.info(() -> "System property 'mining.schema.migration.orient.deletion' not set to true. " + skipMessage.get());
		}
	}

	/**
	 * Load an SQL script from the resource folder and run it against OrientDB.
	 * @param name Name of the script file, without extension.
	 * @throws SQLException If anything goes wrong.
	 */
	protected void executeOrientScript(final String name) throws SQLException {
		executeOrientScript(name, s -> Arrays.stream(s.split("\n"))
											.filter(line -> ! line.isBlank() && ! line.startsWith("--"))
											.collect(Collectors.toList()));
	}
	
	/**
	 * Load an SQL script from the resource folder and run it against OrientDB.
	 * @param name Name of the script file, without extension.
	 * @param queryTransform Optional filter to modify/extend the script before execution.
	 * @throws SQLException If anything goes wrong.
	 */
	protected void executeOrientScript(final String name, final Function<String, List<String>> queryTransform) throws SQLException {
		executeScript(dbOrient, String.format("/db/schema/orient/%06d/", context.getCurrentVersion()), name, queryTransform);
	}

	protected void dropIndexSafe(final String index) {
		LOG.debug("Droping Index: " + index);
		try {
			executeStatements(dbOrient, List.of("DROP INDEX " + index + " IF EXISTS"));
		} catch (final SQLException exc) {
			/* Orient reported an com.orientechnologies.orient.core.exception.NotEmptyComponentCanNotBeRemovedException: HasAnnotation_in_idx : 
			 * Not empty index can not be deleted. Index has 562 records but the index was removed from the schema. So we can ignore it here */
			LOG.warn(() -> "Ignoring error while deleting Orient index: " + index, exc);
		}
	}

}

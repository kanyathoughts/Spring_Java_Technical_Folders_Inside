/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package db.migration;

import java.util.Arrays;
import org.apache.commons.lang3.time.StopWatch;
import org.apache.logging.log4j.util.Strings;
import org.flywaydb.core.api.migration.BaseJavaMigration;
import org.flywaydb.core.api.migration.Context;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.SingleConnectionDataSource;
import org.springframework.stereotype.Component;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.Logging;

/**
 * Migration script for inlining {@code ModuleLocations} in {@code Module}, {@code Reference}, {@code ContainsModule} and {@code AstNode}.
 * <p>The script will unlike all {@code AstNode AstNodes} from {@code Module Modules} and delete all {@code AstNode}, {@code RefersTo}, {@code Redefines},
 * {@code ReturnPoint}, {@code HaltPoint}, {@code EntryPoint} vertices and edges.</p>
 */
@Component
public class V1_2_90__Inline_ModuleLocations extends BaseJavaMigration {
	
	private static final int BATCH_SIZE = 10_000;
	private static final Logger LOG = LoggerFactory.getLogger(Logging.MIGRATION);
	
	@Override
	public void migrate(@Nullable final Context context) throws Exception {
		final JdbcTemplate jdbcTemplate = new JdbcTemplate(new SingleConnectionDataSource(Assert.assertNotNull(context).getConnection(), true));
		
		final StopWatch stopWatch = new StopWatch();
		stopWatch.start();
		/* Migrate property location in Module */
		execute(jdbcTemplate, "CREATE PROPERTY Module.location_old IF NOT EXISTS LINK ModuleLocation UNSAFE");
		updateBatchwise(jdbcTemplate, "UPDATE Module SET location_old = location WHERE location IS NOT NULL AND location_old IS NULL");
		removeProperties(jdbcTemplate, "Module", "location");
		execute(jdbcTemplate, "CREATE PROPERTY Module.location IF NOT EXISTS  EMBEDDED ModuleLocation UNSAFE");
		updateBatchwise(jdbcTemplate, "UPDATE Module SET location = { offset: location_old.offset, length: location_old.length } "
									+ "WHERE location_old IS NOT NULL AND location IS NULL");

		/* Migrate property fromModuleLocationLink to fromModuleLocation in Reference */
		execute(jdbcTemplate, "CREATE PROPERTY Reference.fromModuleLocation IF NOT EXISTS EMBEDDED ModuleLocation UNSAFE");
		updateBatchwise(jdbcTemplate, "UPDATE Reference SET fromModuleLocation = { offset: fromModuleLocationLink.offset, length: fromModuleLocationLink.length } "
									+ "WHERE fromModuleLocationLink IS NOT NULL AND fromModuleLocation IS NULL");

		/* Migrate property toModuleLocationLink to toModuleLocation in Reference */
		execute(jdbcTemplate, "CREATE PROPERTY Reference.toModuleLocation IF NOT EXISTS EMBEDDED ModuleLocation UNSAFE");
		updateBatchwise(jdbcTemplate, "UPDATE Reference SET toModuleLocation = { offset: toModuleLocationLink.offset, length: toModuleLocationLink.length } "
									+ "WHERE toModuleLocationLink IS NOT NULL AND toModuleLocation IS NULL");

		/* Migrate property location in ContainsModule */
		execute(jdbcTemplate, "CREATE PROPERTY ContainsModule.location_old IF NOT EXISTS LINK ModuleLocation UNSAFE");
		updateBatchwise(jdbcTemplate, "UPDATE ContainsModule SET location_old = location WHERE location IS NOT NULL AND location_old IS NULL");
		removeProperties(jdbcTemplate, "ContainsModule", "location");
		execute(jdbcTemplate, "CREATE PROPERTY ContainsModule.location IF NOT EXISTS EMBEDDED ModuleLocation UNSAFE");
		updateBatchwise(jdbcTemplate, "UPDATE ContainsModule SET location = { offset: location_old.offset, length: location_old.length } "
									+ "WHERE location_old IS NOT NULL AND location IS NULL");

		/* Remove all AstNodes including their edges */
		execute(jdbcTemplate, "TRUNCATE CLASS RefersTo UNSAFE;");
		execute(jdbcTemplate, "TRUNCATE CLASS Redefines UNSAFE");
		execute(jdbcTemplate, "TRUNCATE CLASS ReturnPoint UNSAFE");
		execute(jdbcTemplate, "TRUNCATE CLASS EntryPoint UNSAFE");
		execute(jdbcTemplate, "TRUNCATE CLASS HaltPoint UNSAFE");
		execute(jdbcTemplate, "TRUNCATE CLASS HasAst UNSAFE");
		execute(jdbcTemplate, "TRUNCATE CLASS AstNode UNSAFE");

		/* Migrate property moduleLocation in AstNode */
		removeProperties(jdbcTemplate, "AstNode", "moduleLocation");
		execute(jdbcTemplate, "CREATE PROPERTY AstNode.moduleLocation IF NOT EXISTS EMBEDDED ModuleLocation");

		/* Recreate class ModuleLocation but not as vertex. In Orient you can't embed vertices */
		execute(jdbcTemplate, "TRUNCATE CLASS ModuleLocation UNSAFE;");
		execute(jdbcTemplate, "DROP CLASS ModuleLocation UNSAFE");
		execute(jdbcTemplate, "CREATE CLASS ModuleLocation");
		execute(jdbcTemplate, "CREATE PROPERTY ModuleLocation.offset INTEGER (NOTNULL, MANDATORY TRUE)");
		execute(jdbcTemplate, "CREATE PROPERTY ModuleLocation.length INTEGER (NOTNULL, MANDATORY TRUE)");

		/* Drop location_old property from Module */
		execute(jdbcTemplate, "UPDATE Module REMOVE location_old, out_HasAst, entryPointLink, returnPointLink, haltPointLink");
		execute(jdbcTemplate, "DROP PROPERTY Module.location_old IF EXISTS");

		/* Drop location_old property from ContainsModule */
		removeProperties(jdbcTemplate, "ContainsModule", "location_old");

		/* Drop fromModuleLocationLink and toModuleLocationLink properties from Reference */
		removeProperties(jdbcTemplate, "Reference", "fromModuleLocationLink", "toModuleLocationLink");

		stopWatch.stop();
		LOG.info(() -> String.format("Migration of ModuleLocations took %s (H:mm:ss.SSS)", stopWatch.toString()));
	}

	/**
	 * Executes the given {@code query}. The execution time is logged.
	 *
	 * @param jdbcTemplate The {@link JdbcTemplate}
	 * @param query The query to execute
	 */
	void execute(final JdbcTemplate jdbcTemplate, final String query) {
		final StopWatch stopWatch = new StopWatch();
		stopWatch.start();
		jdbcTemplate.execute(query);
		stopWatch.stop();
		LOG.info(() -> String.format("Query took %s (H:mm:ss.SSS) to execute update: %s", stopWatch.toString(), query));
	}

	/**
	 * Removes the given {@code properties} from the specified {@code entity}. The execution time is logged.
	 *
	 * @param jdbcTemplate The {@link JdbcTemplate}
	 * @param entity The entity the property belongs to
	 * @param properties The properties to remove
	 */
	void removeProperties(final JdbcTemplate jdbcTemplate, final String entity, final String...properties) {
		switch (properties.length) {
			case 0:
				throw new IllegalArgumentException("At least on property must be set");
			case 1:
				execute(jdbcTemplate, String.format("UPDATE %s REMOVE %s", entity, properties[0]));
				break;
			default:
				execute(jdbcTemplate, String.format("UPDATE %s REMOVE %s", entity, Strings.join(Arrays.asList(properties), ',')));
				break;
		}
		
		for (String property : properties) {
			execute(jdbcTemplate, String.format("DROP PROPERTY %s.%s IF EXISTS", entity, property));
		}
	}

	/**
	 * Executes the given update {@code query}. The execution is done batch wise in chunks of 10_000 updates. The execution time and logged.
	 *
	 * @param jdbcTemplate The {@link JdbcTemplate}
	 * @param query The query to execute
	 */
	void updateBatchwise(final JdbcTemplate jdbcTemplate, final String query) {
		int counter = 0;
		int updatedRows;
		
		final String batchQuery = query + " LIMIT " + BATCH_SIZE;
		
		final StopWatch stopWatch = new StopWatch();
		stopWatch.start();

		do {
			updatedRows = jdbcTemplate.update(batchQuery);
			counter += updatedRows;
			if (counter % 100_000 == 0) {
				final int cnt = counter;
				LOG.info(() -> String.format("Updated %d records. %s", Integer.valueOf(cnt), query));
			}
		} while (updatedRows == BATCH_SIZE);

		stopWatch.stop();
		final int cnt = counter;
		LOG.info(() -> String.format("Query took %s (H:mm:ss.SSS) to update %d records. %s", stopWatch.toString(), Integer.valueOf(cnt), query));
	}
}

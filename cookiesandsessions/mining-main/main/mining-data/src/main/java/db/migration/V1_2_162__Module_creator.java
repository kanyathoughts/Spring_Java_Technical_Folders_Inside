/* Copyright (c) 2022 Deloitte. All rights reserved. */
package db.migration;

import org.apache.commons.lang3.time.StopWatch;
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
 * Migration script that adds the 'creator' property to {@code Module} and sets it to 'DISCOVERY' in all modules.
 */
@Component
public class V1_2_162__Module_creator extends BaseJavaMigration {

	private static final int BATCH_SIZE = 100_000;
	private static final Logger LOG = LoggerFactory.getLogger(Logging.MIGRATION);

	@Override
	public void migrate(@Nullable final Context context) throws Exception {
		final JdbcTemplate jdbcTemplate = new JdbcTemplate(new SingleConnectionDataSource(Assert.assertNotNull(context).getConnection(), true));

		final StopWatch stopWatch = new StopWatch();
		stopWatch.start();
		execute(jdbcTemplate, "CREATE PROPERTY Module.creator IF NOT EXISTS STRING");

		updateBatchwise(jdbcTemplate, "UPDATE Module SET creator = 'DISCOVERY' WHERE creator IS NULL");

		execute(jdbcTemplate, "ALTER PROPERTY Module.creator MANDATORY TRUE");
		execute(jdbcTemplate, "ALTER PROPERTY Module.creator NOTNULL TRUE");
		execute(jdbcTemplate, "CREATE INDEX Module_creator_idx IF NOT EXISTS ON Module (projectLink, creator) NOTUNIQUE");

		stopWatch.stop();
		LOG.info(() -> String.format("Creation of Module.creator property and index took %s (H:mm:ss.SSS)", stopWatch.toString()));
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

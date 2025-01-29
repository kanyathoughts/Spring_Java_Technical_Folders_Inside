/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package db.migration;

import org.apache.commons.lang3.time.StopWatch;
import org.flywaydb.core.api.migration.BaseJavaMigration;
import org.flywaydb.core.api.migration.Context;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.SingleConnectionDataSource;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.Logging;

/**
 * Migration script for adding the {@code linkHash} to {@code Module}.
 */
public class V1_2_150__Create_Module_Link_Hash extends BaseJavaMigration{

	private static final int BATCH_SIZE = 100_000;

	private static final String UPDATE_MODULE_WITH_PATH = "update module set linkHash=\"Module:\".append(name).append(\",\").append(path).append(\",\").append(objectTypeLink.technologyLink.name).append(\",\").append(objectTypeLink.typeLink.name).hash(\"sha-256\") where linkHash IS NULL AND path is not null and path != \"\" LIMIT " + BATCH_SIZE;
	private static final String UPDATE_MODULE_WITH_CONTAINED_ID = "update module set linkHash=\"Module:\".append(name).append(\",\").append(\"ContainedIn:\").append(in_ContainsModule.out.path).append(\",\").append(objectTypeLink.technologyLink.name).append(\",\").append(objectTypeLink.typeLink.name).hash(\"sha-256\") where linkHash IS NULL AND (path is null or path = \"\" ) and in_ContainsModule.out.path is not null LIMIT " + BATCH_SIZE;
	private static final String UPDATE_MODULE_WITHOUT_PATH = "update module set linkHash=\"Module:\".append(name).append(\",\").append(\"\").append(\",\").append(objectTypeLink.technologyLink.name).append(\",\").append(objectTypeLink.typeLink.name).hash(\"sha-256\") where linkHash IS NULL AND path is null and in_ContainsModule.out.path is null LIMIT " + BATCH_SIZE;

	private static final Logger LOG = LoggerFactory.getLogger(Logging.MIGRATION);

	@Override
	public void migrate(@Nullable final Context context) throws Exception {
		final JdbcTemplate jdbcTemplate = new JdbcTemplate(new SingleConnectionDataSource(Assert.assertNotNull(context).getConnection(), true));

		LOG.info("Creating property linkHash in class Module");
		jdbcTemplate.execute("CREATE PROPERTY Module.linkHash IF NOT EXISTS STRING");

		final StopWatch stopWatch = new StopWatch();

		LOG.info("Creating linkHash in modules that have a path. Query: " + UPDATE_MODULE_WITH_PATH);
		update(jdbcTemplate, UPDATE_MODULE_WITH_PATH, stopWatch);
		LOG.info("Finished creating linkHash in modules that have a path");

		LOG.info("Creating linkHash in modules that have a contains path. Query: " + UPDATE_MODULE_WITH_CONTAINED_ID);
		update(jdbcTemplate, UPDATE_MODULE_WITH_CONTAINED_ID, stopWatch);
		LOG.info("Finished creating linkHash in modules that have a contains path");

		LOG.info("Creating linkHash in modules that have no path and no contains path. Query: " + UPDATE_MODULE_WITHOUT_PATH);
		update(jdbcTemplate, UPDATE_MODULE_WITHOUT_PATH, stopWatch);
		LOG.info("Finished creating linkHash in modules that have no path and no contains path");

		jdbcTemplate.execute("CREATE INDEX Module_linkHash IF NOT EXISTS ON Module (projectLink, linkHash) NOTUNIQUE METADATA { ignoreNullValues : true }");
	}

	private static void update(final JdbcTemplate jdbcTemplate, final String query, final StopWatch stopWatch) {
		int count;
		do {
			stopWatch.start();
			count = jdbcTemplate.update(query);
			stopWatch.stop();
			final int c = count;
			LOG.info(() -> String.format("Update of %d modules took %s (H:mm:ss.SSS)", Integer.valueOf(c), stopWatch.toString()));
			stopWatch.reset();
		} while (count >= BATCH_SIZE);
	}
}

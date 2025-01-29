/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package db.migration;

import org.springframework.jdbc.core.JdbcTemplate;

/**
 * Migration script for updating the link hash of the modules with link hash of its parent/containing module, if it's path or its parent path is null
 */
public class V1_2_192__Update_Contains_Module_Link_Hash extends AbstractMiningMigration {

	private static final Integer BATCH_SIZE = Integer.valueOf(100_000);

	private static final String UPDATE_CONTAINS_MODULE_LINK_HASH = "UPDATE (SELECT FROM Module WHERE (path is null or path = \"\" ) "
			+ "AND (in_ContainsModule.out.path is null OR in_ContainsModule.out.path = \"\") ORDER BY id LIMIT %d SKIP %d) "
			+ "SET linkHash=\"Module:\".append(name).append(\",\").append(\"ContainedIn:\") .append(in_ContainsModule.out.linkHash).append(\",\")"
			+ ".append(objectTypeLink.technologyLink.name).append(\",\") .append(objectTypeLink.typeLink.name).hash(\"MD5\")";

	@Override
	void migrate(final JdbcTemplate jdbcTemplate) {
		LOG.info("Updating of linkHash property for empty containingModuleLinkHash and path");
		var count = 0;
		var totalCount = 0;

		final var stopWatch = createStopWatch();
		do {
			count = jdbcTemplate.update(String.format(UPDATE_CONTAINS_MODULE_LINK_HASH, BATCH_SIZE, Integer.valueOf(totalCount)));
			totalCount += count;
		} while (count == BATCH_SIZE.intValue());
		stopWatch.stop();

		final long finalCount = totalCount;
		LOG.info(() -> String.format("Update of %d contains module link hash took %s (H:mm:ss.SSS)", Long.valueOf(finalCount), stopWatch.toString()));
	}
}
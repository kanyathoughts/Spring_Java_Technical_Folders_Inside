/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package db.migration;

import org.springframework.jdbc.core.JdbcTemplate;

/**
 * Migration script creates projectLink property in DataDictionaryEntry and indexs based on projectlink
 */
public class V1_2_189__CreateProjectlinkPropertyAndIndexesOnDataDictionaryEntry extends AbstractMiningMigration {

	private static final int BATCH_SIZE = 100_000;
	private static final int DELETE_BATCH_SIZE = 10_000;

	@Override
	void migrate(final JdbcTemplate jdbcTemplate) {
		final var stopWatch = createStopWatch();

		LOG.info("Cleaning DataDictionaryEntry which are not linked with any project");
		updateBatchwise(jdbcTemplate, "DELETE VERTEX FROM DataDictionaryEntry where in_HasDataDictionaryEntry = null or in_HasDataDictionaryEntry.size() = 0", 
				DELETE_BATCH_SIZE);
		LOG.info("DataDictionaryEntry which are not linked with any project is cleared");

		LOG.info("Create Property projectLink for DataDictionaryEntry.");
		jdbcTemplate.update("CREATE PROPERTY DataDictionaryEntry.projectLink IF NOT EXISTS LINK Project (NOTNULL, MANDATORY TRUE)");
		LOG.info("Property projectLink for DataDictionaryEntry is created.");

		LOG.info("Update of DataDictionaryEntry.projectLink property for all DataDictionaryEntries where projectLink is not assigned.");
		updateBatchwise(jdbcTemplate, "UPDATE DataDictionaryEntry SET projectLink = first(in('HasDataDictionaryEntry')).projectLink where projectLink = null", BATCH_SIZE);
		LOG.info("DataDictionaryEntry.projectLink property is updated for all DataDictionaryEntries where projectLink is not assigned.");
		
		LOG.info("Creating Indexes linked with projectLink for all DataDictionaryEntries.");
		jdbcTemplate.update("CREATE INDEX DataDictionaryEntry_projectLink_idx IF NOT EXISTS ON "
				+ "DataDictionaryEntry (projectLink) NOTUNIQUE");
		jdbcTemplate.update("CREATE INDEX DataDictionaryEntry_projectLink_dataElementName_idx IF NOT EXISTS ON "
				+ "DataDictionaryEntry (projectLink, dataElementName) NOTUNIQUE");
		jdbcTemplate.update("CREATE INDEX DataDictionaryEntry_projectLink_format_idx IF NOT EXISTS ON "
				+ "DataDictionaryEntry (projectLink, format) NOTUNIQUE");
		jdbcTemplate.update("CREATE INDEX DataDictionaryEntry_projectLink_groupPath_idx IF NOT EXISTS ON "
				+ "DataDictionaryEntry (projectLink, groupPath) NOTUNIQUE");
		jdbcTemplate.update("CREATE INDEX DataDictionaryEntry_projectLink_picClause_idx IF NOT EXISTS ON "
				+ "DataDictionaryEntry (projectLink, picClause) NOTUNIQUE");
		jdbcTemplate.update("CREATE INDEX DataDictionaryEntry_projectLink_isReferenced_idx IF NOT EXISTS ON "
				+ "DataDictionaryEntry (projectLink, isReferenced) NOTUNIQUE");
		jdbcTemplate.update("CREATE INDEX DataDictionaryEntry_projectLink_definedLocation_idx IF NOT EXISTS ON "
				+ "DataDictionaryEntry (projectLink, definedLocation) NOTUNIQUE");
		jdbcTemplate.update("CREATE INDEX DataDictionaryEntry_projectLink_isBusiness_idx IF NOT EXISTS ON "
				+ "DataDictionaryEntry (projectLink, isBusiness) NOTUNIQUE");
		jdbcTemplate.update("CREATE INDEX DataDictionaryEntry_projectLink_scopeLink_idx IF NOT EXISTS ON "
				+ "DataDictionaryEntry (projectLink, scopeLink) NOTUNIQUE");
		jdbcTemplate.update("CREATE INDEX DataDictionaryEntry_projectLink_stateLink_idx IF NOT EXISTS ON "
				+ "DataDictionaryEntry (projectLink, stateLink) NOTUNIQUE");
		LOG.info("Indexes linked with projectLink for all DataDictionaryEntries is created.");
		
		stopWatch.stop();
		LOG.info(() -> String.format("Creating and Updating data dictionary projectLinka and creating new indexes took %s (H:mm:ss.SSS)",
				stopWatch.toString()));
	}
}

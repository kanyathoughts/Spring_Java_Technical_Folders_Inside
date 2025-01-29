/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package db.migration;

import org.springframework.jdbc.core.JdbcTemplate;

/**
 * Migration script that updates the data dictionary default location
 */
public class V1_2_166__Update_DataDictionary_DefaultLocation extends AbstractMiningMigration {

	private static final int BATCH_SIZE = 100_000;

	@Override
	void migrate(final JdbcTemplate jdbcTemplate) {
		final var stopWatch = createStopWatch();

		LOG.info("Updating DataDictionaryEntry.definedLocation property for copies");
		updateBatchwise(jdbcTemplate, "UPDATE DataDictionaryEntry SET definedLocation='Copybook' WHERE definedLocation != 'Copybook' AND "
				+ "in_HasDataDictionaryEntry[0].out.objectTypeLink.typeLink.name IN ['CONTROLCARD','COPYBOOK','COPYCODE','COPYLIB','COPYPROC','GDA',"
				+ "'INCLUDE','INLINE_PROC','LDA','PDA','PROC']", BATCH_SIZE);
		LOG.info("Update of DataDictionaryEntry.definedLocation property for copies finished");

		LOG.info("Updating DataDictionaryEntry.definedLocation property for programs");
		updateBatchwise(jdbcTemplate, "UPDATE DataDictionaryEntry SET definedLocation='Program' WHERE definedLocation NOT in ['Copybook','Program']", BATCH_SIZE);
		LOG.info("Update of DataDictionaryEntry.definedLocation property for programs finished");

		stopWatch.stop();
		LOG.info(() -> String.format("Updating data dictionary definedLocation took %s (H:mm:ss.SSS)", stopWatch.toString()));
	}
}

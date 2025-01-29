/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package db.migration;

import org.springframework.jdbc.core.JdbcTemplate;

/**
 * Migration script for adding the Advanced Module Location
 */
public class V1_2_200__Create_Advanced_Module_Location extends AbstractMiningMigration {

	private static final int DELETE_BATCH_SIZE = 10_000;

	@Override
	void migrate(final JdbcTemplate jdbcTemplate) {
		final var stopWatch = createStopWatch();

		LOG.info("Deleting records for RefersTo");
		deleteBatchwise(jdbcTemplate, "DELETE FROM RefersTo LIMIT %d UNSAFE", DELETE_BATCH_SIZE);
		LOG.info("Deleting records for RefersTo finished");
		
		LOG.info("Deleting records for Redefines");
		deleteBatchwise(jdbcTemplate, "DELETE FROM Redefines LIMIT %d UNSAFE", DELETE_BATCH_SIZE);
		LOG.info("Deleting records for Redefines finished");
		
		LOG.info("Deleting records for ReturnPoint");
		updateBatchwise(jdbcTemplate, "DELETE VERTEX ReturnPoint", DELETE_BATCH_SIZE);
		LOG.info("Deleting records for ReturnPoint finished");
		
		LOG.info("Deleting records for EntryPoint");
		updateBatchwise(jdbcTemplate, "DELETE VERTEX EntryPoint", DELETE_BATCH_SIZE);
		LOG.info("Deleting records for EntryPoint finished");
		
		LOG.info("Deleting records for HaltPoint");
		updateBatchwise(jdbcTemplate, "DELETE VERTEX HaltPoint", DELETE_BATCH_SIZE);
		LOG.info("Deleting records for HaltPoint finished");
		
		LOG.info("Deleting records for HasAst");
		deleteBatchwise(jdbcTemplate, "DELETE FROM HasAst LIMIT %d UNSAFE", DELETE_BATCH_SIZE);
		LOG.info("Deleting records for HasAst finished");
		
		LOG.info("Deleting records for AstNode");
		updateBatchwise(jdbcTemplate, "DELETE VERTEX AstNode", DELETE_BATCH_SIZE);
		LOG.info("Deleting records for AstNode finished");
		
		LOG.info("Deleting records for DataFlowNode");
		deleteBatchwise(jdbcTemplate, "DELETE FROM DataFlowNode LIMIT %d UNSAFE", DELETE_BATCH_SIZE);
		LOG.info("Deleting records for DataFlowNode finished");
		
		LOG.info("Deleting records for ProxyContainer");
		deleteBatchwise(jdbcTemplate, "DELETE FROM ProxyContainer LIMIT %d UNSAFE", DELETE_BATCH_SIZE);
		LOG.info("Deleting records for ProxyContainer finished");
		
		LOG.info("Dropping the property moduleLocation");
		jdbcTemplate.update("DROP PROPERTY AstNode.moduleLocation IF EXISTS");
		
		LOG.info("Creating class AdvancedModuleLocation");
		jdbcTemplate.execute("CREATE CLASS AdvancedModuleLocation IF NOT EXISTS");
		jdbcTemplate.execute("CREATE PROPERTY AdvancedModuleLocation.retracedOffset IF NOT EXISTS INTEGER");
		jdbcTemplate.execute("CREATE PROPERTY AdvancedModuleLocation.retracedLength IF NOT EXISTS INTEGER");
		jdbcTemplate.execute("CREATE PROPERTY AdvancedModuleLocation.assembledOffset IF NOT EXISTS INTEGER");
		jdbcTemplate.execute("CREATE PROPERTY AdvancedModuleLocation.assembledLength IF NOT EXISTS INTEGER");
		jdbcTemplate.execute("CREATE PROPERTY AdvancedModuleLocation.rootRelativeOffset IF NOT EXISTS INTEGER");
		jdbcTemplate.execute("CREATE PROPERTY AdvancedModuleLocation.rootRelativeLength IF NOT EXISTS INTEGER");

		LOG.info("Creating property advancedModuleLocation in class AstNode");
		jdbcTemplate.execute("CREATE PROPERTY AstNode.advancedModuleLocation IF NOT EXISTS EMBEDDED AdvancedModuleLocation (NOTNULL)");
		
		stopWatch.stop();
	}

	void deleteBatchwise(final JdbcTemplate jdbcTemplate, final String query, final int batchSize) {
		var count = 0;
		var totalCount = 0;

		final String batchQuery = String.format(query, Integer.valueOf(batchSize));
		final var stopWatch = createStopWatch();
		do {
			count = jdbcTemplate.update(batchQuery);
			totalCount += count;
			LOG.info("Executed " + query + " and updated " + totalCount + " number of records");
		} while (count == batchSize);

		stopWatch.stop();
		final long cnt = totalCount;
		LOG.info(() -> String.format("Query took %s (H:mm:ss.SSS) to update %d records. %s", stopWatch.toString(), Long.valueOf(cnt), query));
	}
}

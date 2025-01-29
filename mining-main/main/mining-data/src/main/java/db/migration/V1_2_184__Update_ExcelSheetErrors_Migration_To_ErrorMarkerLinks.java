/*
 * Copyright (c) 2023 Deloitte. All rights reserved..
 */
package db.migration;

import java.util.List;

import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.ResultSetExtractor;

import innowake.mining.data.model.discovery.ErrorMarker;

/**
 * Flyway migration script that dump all records from {@code ExcelSheetErrors} to {@link ErrorMarker}.
 * It also links {@link Module} and {@link ErrorMarker} using HasErrorMarker edge. We delete ExcelSheetErrors table at the end.
 */
public class V1_2_184__Update_ExcelSheetErrors_Migration_To_ErrorMarkerLinks extends AbstractMiningMigration {
	
	@Override
	void migrate(final JdbcTemplate jdbcTemplate) {
		final var stopWatch = createStopWatch();

		LOG.info("Updating Module.errorMarkerLinks property.");
		updateModuleErrorMarkerLinks(jdbcTemplate);
		LOG.info("Update of Module.errorMarkerLinks property finished.");

		stopWatch.stop();
		LOG.info(() -> String.format("Migration of ExcelSheetErrors took %s (H:mm:ss.SSS)", stopWatch.toString()));
	}
	
	private void updateModuleErrorMarkerLinks(final JdbcTemplate jdbcTemplate) {
		final var stopWatch = createStopWatch();

		final var functionName = "updateModuleErrorMarkerLinksJsV_1_2_184";
		final String functionCode = "var db = orient.getDatabase();"
				+ "var rs = db.query('SELECT FROM (SELECT expand(moduleLink) FROM (SELECT DISTINCT moduleLink FROM ExcelSheetErrors where projectLink=?))"
				+ " WHERE errorMarkerLinks.size() = 0', project);"
				+ "for (var i = 0; i < rs.length; i++) {"
				+ "  db.command('UPDATE ? SET errorMarkerLinks=(SELECT FROM ExcelSheetErrors WHERE moduleLink=?)', rs[i], rs[i]);"
				+ "}"
				+ "return rs.length;";

		try {
			createJsOrientFunction(jdbcTemplate, functionName, functionCode, "project");

			final ResultSetExtractor<Integer> rsExtractor = createJsFunctionCountResultSetExtractor(functionName);
			final List<String> rids = jdbcTemplate.queryForList("SELECT @rid FROM Project where id != 0", String.class);
			for (final String rid : rids) {
				LOG.info(() -> String.format("Linking Modules for project: %s", rid));
				final Integer count = jdbcTemplate.query("SELECT " + functionName + "(" + rid + ")", rsExtractor);
				LOG.info(() -> String.format("Linked %d Modules for project: %s", count, rid));
			}

			stopWatch.stop();
			LOG.info(() -> String.format("Linking Modules and ExcelSheetErrors took %s (H:mm:ss.SSS)", stopWatch.toString()));
		} catch (final Exception e) {
			LOG.error("Error while linking Modules and ExcelSheetErrors", e);
		} finally {
			deleteOrientFunction(jdbcTemplate, functionName);
		}
	}

}

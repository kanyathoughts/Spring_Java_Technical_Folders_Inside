/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package db.migration;

import org.springframework.jdbc.core.JdbcTemplate;

/**
 * Creates a JS Orient function to update all the ErrorMarkers record ids which are associated with the Module into the Module table.
 */
public class V1_2_188__CreateModuleErrorMarkerLinksJsFunction extends AbstractMiningMigration {

	@Override
	void migrate(final JdbcTemplate jdbcTemplate) {
		final var stopWatch = createStopWatch();

		LOG.info("Creating updateModuleErrorMarkerLinksJs function.");

		final var functionName = "updateModuleErrorMarkerLinksJs";
		final String functionCode = "var db = orient.getDatabase();"
				+ "var rs = db.query('SELECT FROM (SELECT expand(moduleLink) FROM (SELECT DISTINCT moduleLink FROM ExcelSheetErrors where projectLink.id=?))'"
				+ ", project);"
				+ "for (var i = 0; i < rs.length; i++) {"
				+ "  db.command('UPDATE ? SET errorMarkerLinks=(SELECT FROM ExcelSheetErrors WHERE moduleLink=?)', rs[i], rs[i]);"
				+ "}"
				+ "return rs.length;";

		createJsOrientFunction(jdbcTemplate, functionName, functionCode, "project");

		stopWatch.stop();
		LOG.info(() -> String.format("Creation of updateModuleErrorMarkerLinksJs function took %s (H:mm:ss.SSS)", stopWatch.toString()));
	}
}


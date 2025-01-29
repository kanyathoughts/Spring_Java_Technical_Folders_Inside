/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package db.migration;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

import org.flywaydb.core.api.migration.BaseJavaMigration;
import org.flywaydb.core.api.migration.Context;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.ResultSetExtractor;
import org.springframework.jdbc.datasource.SingleConnectionDataSource;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import com.orientechnologies.orient.core.id.ORecordId;

/**
 * Flyway migration script that selects all ExcelSheetDeadCode records and inserts the sum of linesOfDeadCode into the Module class.
 */
public class V1_2_62__Module_DeadCodeLines extends BaseJavaMigration {

	@Override
	public void migrate(@Nullable final Context context) throws Exception {
		final JdbcTemplate jdbcTemplate = new JdbcTemplate(new SingleConnectionDataSource(Assert.assertNotNull(context).getConnection(), true));

		jdbcTemplate.execute("CREATE PROPERTY Module.linesOfDeadCode IF NOT EXISTS INTEGER (NOTNULL, MANDATORY TRUE);");
		jdbcTemplate.execute("ALTER PROPERTY Module.linesOfDeadCode DEFAULT -1");
		jdbcTemplate.update("UPDATE Module SET linesOfDeadCode=-1 WHERE objectTypeLink.storageLink.name <> 'FILE';");
		jdbcTemplate.update("UPDATE Module SET linesOfDeadCode=0 WHERE objectTypeLink.storageLink.name='FILE';");
		
		final DeadCodeLinesExtractor extractor = new DeadCodeLinesExtractor();
		jdbcTemplate.query("SELECT sum(numberOfLines), moduleLink FROM ExcelSheetDeadCode GROUP BY moduleLink", extractor);

		extractor.getDeadCodeModules().entrySet().forEach(deadCodeModule -> {
			jdbcTemplate.update("UPDATE ? SET linesOfDeadCode=?", deadCodeModule.getKey(), deadCodeModule.getValue());
		});

	}

	private static class DeadCodeLinesExtractor implements ResultSetExtractor<Void> {

		private final Map<ORecordId, Integer> deadCodeModules = new HashMap<>();

		@Override
		@Nullable
		public Void extractData(final ResultSet rs) throws SQLException {
			while (rs.next()) {
				final String moduleLink = rs.getString("moduleLink");
				if (moduleLink != null) {
					deadCodeModules.put(new ORecordId(moduleLink), Integer.valueOf(rs.getInt(1)));
				}
			}
			return null;
		}

		/**
		 * Retrieves the number of lines of dead code for each module as a {@link Map} where key being the ModuleLink and value being the lines of dead code.
		 *
		 * @return map containing number of lines of dead code for the modules.
		 */
		public Map<ORecordId, Integer> getDeadCodeModules() {
			return deadCodeModules;
		}
	}
}

/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package db.migration;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import org.flywaydb.core.api.migration.BaseJavaMigration;
import org.flywaydb.core.api.migration.Context;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.ResultSetExtractor;
import org.springframework.jdbc.datasource.SingleConnectionDataSource;

import com.orientechnologies.orient.core.id.ORecordId;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;

/**
 * Flyway migration script that selects all ExcelSheetModules records and inserts the properties into the Module class. Afterwards the ExcelSheetModules
 * class is being dropped.
 */
public class V1_2_58__Clusterable_Discover_Metrics_2 extends BaseJavaMigration {
	
	private static final Integer VALUE_UNKNOWN = Integer.valueOf(-1);
	
	@Override
	public void migrate(@Nullable final Context context) throws Exception {
		final JdbcTemplate jdbcTemplate = new JdbcTemplate(new SingleConnectionDataSource(Assert.assertNotNull(context).getConnection(), true));
		
		final ExcelSheetModuleExtractor extractor = new ExcelSheetModuleExtractor();
		jdbcTemplate.query("SELECT moduleLink, statements, sqlStatements, representation, errors, type FROM ExcelSheetModules", extractor);
		
		for (int i = 0; i < extractor.moduleLinks.size(); i++) {
			jdbcTemplate.update("UPDATE ? SET statements=?, sqlStatements=?, representation=?, errors=?, excelType=?", new ORecordId(extractor.moduleLinks.get(i)),
					extractor.statements.get(i), extractor.sqlStatements.get(i), extractor.representations.get(i), extractor.errors.get(i), extractor.types.get(i));
		}
		
		jdbcTemplate.update("DROP CLASS ExcelSheetModules IF EXISTS UNSAFE");
	}
	
	private class ExcelSheetModuleExtractor implements ResultSetExtractor<Void> {
		
		private final List<String> moduleLinks = new ArrayList<>();
		private final List<Integer> statements = new ArrayList<>();
		private final List<Integer> sqlStatements = new ArrayList<>();
		private final List<String> representations = new ArrayList<>();
		private final List<Integer> errors = new ArrayList<>();
		private final List<String> types = new ArrayList<>();
		
		@Override
		@Nullable
		public Void extractData(final ResultSet rs) throws SQLException {
			while (rs.next()) {
				final String moduleLink = rs.getString("moduleLink");
				if (moduleLink != null) {
					moduleLinks.add(moduleLink);
					final Integer statementCount = (Integer) rs.getObject("statements");
					statements.add(statementCount != null ? statementCount : VALUE_UNKNOWN);
					
					final Integer sqlStatementCount = (Integer) rs.getObject("sqlStatements");
					sqlStatements.add(sqlStatementCount != null ? sqlStatementCount : VALUE_UNKNOWN);
					
					final String representation = rs.getString("representation");
					representations.add(representation != null ? representation : "");
					
					final Integer errorCount = (Integer) rs.getObject("errors");
					errors.add(errorCount != null ? errorCount : VALUE_UNKNOWN);
					
					final String type = rs.getString("type");
					types.add(type != null ? type : "");
				}
			}
			return null;
		}
	}

}

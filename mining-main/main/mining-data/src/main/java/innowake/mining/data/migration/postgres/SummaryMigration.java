package innowake.mining.data.migration.postgres;

import innowake.mining.data.access.postgres.PgJSON;
import innowake.mining.data.migration.base.PostgresSchemaMigrationFromOrient;
import innowake.mining.data.migration.base.SchemaMigrationContext;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

@SuppressWarnings("unchecked")
public class SummaryMigration extends PostgresSchemaMigrationFromOrient {

	public SummaryMigration(final SchemaMigrationContext context) {
		super(context);
	}

	@Override
	public void migrate() throws SQLException {
		executePgScript("effort_summary_table_creation");

		migrateData("PricingSummary",
				"SELECT projectId, " +
						"pricingSummaries.totalBatchExecAsmPrograms, " +
						"pricingSummaries.totalBatchExecCobolPrograms, " +
						"pricingSummaries.totalBatchExecPgmStatements, " +
						"pricingSummaries.totalBatchExecUnknownMissingPrograms, " +
						"pricingSummaries.totalDataFiles, " +
						"pricingSummaries.totalErrors, " +
						"pricingSummaries.totalMissingDependencies, " +
						"pricingSummaries.totalModulesWithErrors, " +
						"pricingSummaries.totalScreens, " +
						"pricingSummaries.totalSQLStatements from EffortSummary",
				"INSERT INTO effort_summary (project, index, type, properties) " +
						"VALUES ((SELECT uid FROM project WHERE nid = ?), ?, ?::effort_summary_type, ?)",
				1000, (in, out, pass) -> {
					out.add(in.getLong(1));
					out.add(pass + 1);
					out.add("PRICING");
					final Map<String, Long> properties = new HashMap<>();
					properties.put("totalBatchExecAsmPrograms", ((ArrayList<Long>) in.getObject(2)).get(pass));
					properties.put("totalBatchExecCobolPrograms", ((ArrayList<Long>) in.getObject(3)).get(pass));
					properties.put("totalBatchExecPgmStatements", ((ArrayList<Long>) in.getObject(4)).get(pass));
					properties.put("totalBatchExecUnknownMissingPrograms", ((ArrayList<Long>) in.getObject(5)).get(pass));
					properties.put("totalDataFiles", ((ArrayList<Long>) in.getObject(6)).get(pass));
					properties.put("totalErrors", ((ArrayList<Long>) in.getObject(7)).get(pass));
					properties.put("totalMissingDependencies", ((ArrayList<Long>) in.getObject(8)).get(pass));
					properties.put("totalModulesWithErrors", ((ArrayList<Long>) in.getObject(9)).get(pass));
					properties.put("totalScreens", ((ArrayList<Long>) in.getObject(10)).get(pass));
					properties.put("totalSQLStatements", ((ArrayList<Long>) in.getObject(11)).get(pass));
					out.add(PgJSON.toPGobject(properties));
					return ((ArrayList<Long>) in.getObject(2)).size() - 1 != pass;
				});

		migrateData("TypeSummary",
				"SELECT projectId, " +
						"typeSummaries.complexCount, " +
						"typeSummaries.count, " +
						"typeSummaries.easyCount, " +
						"typeSummaries.errorCount, " +
						"typeSummaries.loc, " +
						"typeSummaries.locComment, " +
						"typeSummaries.technology, " +
						"typeSummaries.type, " +
						"typeSummaries.unmaintainableCount, " +
						"typeSummaries.veryComplexCount from EffortSummary",
				"INSERT INTO effort_summary (project, index, type, properties) " +
						"VALUES ((SELECT uid FROM project WHERE nid = ?), ?, ?::effort_summary_type, ?)",
				1000, (in, out, pass) -> {
					out.add(in.getLong(1));
					out.add(pass + 1);
					out.add("TYPE");
					final Map<String, Object> properties = new HashMap<>();
					properties.put("complexCount", ((ArrayList<Long>) in.getObject(2)).get(pass));
					properties.put("count", ((ArrayList<Long>) in.getObject(3)).get(pass));
					properties.put("easyCount", ((ArrayList<Long>) in.getObject(4)).get(pass));
					properties.put("errorCount", ((ArrayList<Long>) in.getObject(5)).get(pass));
					properties.put("loc", ((ArrayList<Long>) in.getObject(6)).get(pass));
					properties.put("locComment", ((ArrayList<Long>) in.getObject(7)).get(pass));
					properties.put("technology", ((ArrayList<String>) in.getObject(8)).get(pass));
					properties.put("type", ((ArrayList<String>) in.getObject(9)).get(pass));
					properties.put("unmaintainableCount", ((ArrayList<Long>) in.getObject(10)).get(pass));
					properties.put("veryComplexCount", ((ArrayList<Long>) in.getObject(11)).get(pass));
					out.add(PgJSON.toPGobject(properties));
					return ((ArrayList<Long>) in.getObject(2)).size() - 1 != pass;
				});
	}
}

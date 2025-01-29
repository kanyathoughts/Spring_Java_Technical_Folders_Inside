package innowake.mining.data.migration.postgres;

import innowake.mining.data.migration.base.PostgresSchemaMigrationFromOrient;
import innowake.mining.data.migration.base.SchemaMigrationContext;

import java.sql.SQLException;

public class ExcelSheetUndiscoveredMigration extends PostgresSchemaMigrationFromOrient {

	public ExcelSheetUndiscoveredMigration(SchemaMigrationContext context) {
		super(context);
	}

	@Override
	public void migrate() throws SQLException {
		executePgScript("module_undiscovered_table_creation");

		migrateData("ExcelSheetUndiscovered", "SELECT name, path, projectId FROM ExcelSheetUndiscovered",
				"INSERT INTO module_undiscovered VALUES((SELECT uid from project WHERE nid = ?), ?, ?)",
				1000, (in, out, pass) -> {
					out.add(in.getLong(3));
					out.add(in.getString(1));
					out.add(in.getString(2));
					return false;
				});
	}
}

package innowake.mining.data.migration.postgres;

import java.sql.SQLException;

import innowake.mining.data.access.postgres.PgJSON;
import innowake.mining.data.migration.base.PostgresSchemaMigrationFromOrient;
import innowake.mining.data.migration.base.SchemaMigrationContext;
import innowake.mining.shared.MiningEnitityNames;

public class FieldInfoMigration extends PostgresSchemaMigrationFromOrient {

	public FieldInfoMigration(final SchemaMigrationContext context) {
		super(context);
	}

	@Override
	public void migrate() throws SQLException {
		executePgScript("field_info_table_creation");

		migrateData("FieldInfo",
			"SELECT @rid, moduleId, ordinal, name, reference, comment, properties FROM FieldInfo",
			"INSERT INTO field_info values (?, (SELECT uid FROM module WHERE nid = ?), ?, ?, ?, ?, ?)",
			1000, (in, out, n) -> {
				out.add(genUUIDv5(MiningEnitityNames.FIELD_INFO, in.getString(1)));		/* id */
				out.add(in.getLong(2));													/* module */
				out.add(in.getInt(3));													/* ordinal */
				out.add(in.getString(4));												/* name */
				out.add(in.getString(5));												/* reference */
				out.add(in.getString(6));												/* comment */
				out.add(PgJSON.toPGobject(in.getObject(7)));							/* properties */
				return false;
			});

		
	}
}

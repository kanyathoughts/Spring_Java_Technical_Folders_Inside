package innowake.mining.data.migration.postgres;

import innowake.mining.data.migration.base.PostgresSchemaMigrationFromOrient;
import innowake.mining.data.migration.base.SchemaMigrationContext;

import java.sql.SQLException;

public class FF4JMigration extends PostgresSchemaMigrationFromOrient {

	public FF4JMigration(final SchemaMigrationContext context) {
		super(context);
	}

	@Override
	public void migrate() throws SQLException {
		executePgScript("ff4j_create_tables");

		migrateData("ff4j_FeatureMigration",
				"SELECT FEAT_UID, ENABLE, DESCRIPTION, `STRATEGY`, EXPRESSION, GROUPNAME FROM ff4j_Feature",
				"INSERT INTO FF4J_FEATURES VALUES(?, ?, ?, ?, ?, ?)",
				1000, (in, out, pass) -> {
					out.add(in.getString(1));
					out.add(in.getInt(2));
					out.add(in.getString(3));
					out.add(in.getString(4));
					out.add(in.getString(5));
					out.add(in.getString(6));
					return false;
				});

		migrateData("ff4j_FeaturePropertyMigration",
				"SELECT PROPERTY_ID, CLAZZ, CURRENTVALUE, FIXEDVALUES, DESCRIPTION, FEAT_UID FROM ff4j_FeatureProperty", 
				"INSERT INTO FF4J_CUSTOM_PROPERTIES VALUES(?, ?, ?, ?, ?, ?)",
				1000, (in, out, pass) -> {
					out.add(in.getString(1));
					out.add(in.getString(2));
					out.add(in.getString(3));
					out.add(in.getString(4));
					out.add(in.getString(5));
					out.add(in.getString(6));
					return false;
				});

		migrateData("ff4j_PropertyMigration",
				"SELECT PROPERTY_ID, CLAZZ, CURRENTVALUE, FIXEDVALUES, DESCRIPTION FROM ff4j_Property",
				"INSERT INTO FF4J_PROPERTIES VALUES(?, ?, ?, ?, ?)",
				1000, (in, out, pass) -> {
					out.add(in.getString(1));
					out.add(in.getString(2));
					out.add(in.getString(3));
					out.add(in.getString(4));
					out.add(in.getString(5));
					return false;
				});

		migrateData("ff4j_RoleMigration", "SELECT FEAT_UID, ROLE_NAME FROM ff4j_Role",
				"INSERT INTO FF4J_ROLES VALUES(?, ?)", 1000, (in, out, pass) -> {
					out.add(in.getString(1));
					out.add(in.getString(2));
					return false;
				});
	}
}

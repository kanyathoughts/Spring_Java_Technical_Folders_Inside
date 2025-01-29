/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.data.migration.postgres;

import java.sql.SQLException;

import innowake.mining.data.migration.base.PostgresSchemaMigrationFromOrient;
import innowake.mining.data.migration.base.SchemaMigrationContext;
import innowake.mining.shared.MiningEnitityNames;

/**
 * Migrates the Client entity
 */
public class ClientEntityMigration extends PostgresSchemaMigrationFromOrient {

	public ClientEntityMigration(final SchemaMigrationContext context) {
		super(context);
	}

	@Override
	public void migrate() throws SQLException {
		executePgScript("client_table_create");
		LOG.info("Copying Client entities from OrientDB to Postgres");
		migrateData("Clients",
			"select id, name, logo.mimetype.name, logo.content, toBeDeleted, @rid from Client",
			"insert into client values (?, ?, ?, ?, case when ? then (?, ?)::binary_attachment else null end, ?)",
			100, (in, out, n) -> {
				out.add(genUUIDv5(MiningEnitityNames.CLIENT, in.getString(6)));
				out.add(null);
				out.add(in.getLong(1));
				out.add(in.getString(2));
				out.add(in.getString(4) != null);
				out.add(in.getString(3));
				out.add(in.getBytes(4));
				out.add(in.getBoolean(5));
				return false;
			},
			() -> executeStatements(dbPostgres, "INSERT INTO client (uid, nid, name) VALUES (gen_random_uuid(), 0, 'SYSTEM')")
		);
		updatePgSequence("client_nid", "select coalesce(max(nid) + 1, 1) from client");
	}

}

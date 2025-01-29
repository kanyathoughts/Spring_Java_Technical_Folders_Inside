/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.migration.postgres;

import java.sql.SQLException;
import java.util.UUID;

import innowake.mining.data.migration.base.PostgresSchemaMigrationFromOrient;
import innowake.mining.data.migration.base.SchemaMigrationContext;

/**
 * Migrate OfflineToken entity.
 */
public class OfflineTokenMigration extends PostgresSchemaMigrationFromOrient {
	
	public OfflineTokenMigration(final SchemaMigrationContext context) {
		super(context);
	}
	
	@Override
	public void migrate() throws SQLException {
		executePgScript("offline_token_table_creation");
		migrateData("OfflineToken",
			"SELECT id, subject, username, description, bearerToken, refreshToken, created FROM OfflineToken",
			"INSERT INTO offline_token values (?, ?, ?, ?, ?, ?, ?)",
			1000, (in, out, n) -> {
				out.add(UUID.fromString(in.getString(1)));	/* id */
				out.add(in.getString(2));					/* subject */
				out.add(in.getString(3));					/* username */
				out.add(in.getString(4));					/* description */
				out.add(in.getString(5));					/* bearerToken */
				out.add(in.getString(6));					/* refreshToken */
				out.add(in.getTimestamp(7));				/* created */
				return false;
			});
	}
	
}

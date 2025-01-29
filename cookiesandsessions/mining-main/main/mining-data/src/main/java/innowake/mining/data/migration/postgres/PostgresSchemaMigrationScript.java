/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.migration.postgres;

import java.sql.SQLException;

import innowake.mining.data.migration.base.PostgresSchemaMigration;
import innowake.mining.data.migration.base.SchemaMigrationContext;

/**
 * Runs one or more SQL script against the Postgres database.
 */
public class PostgresSchemaMigrationScript extends PostgresSchemaMigration {

	private final String[] scriptNames;

	public PostgresSchemaMigrationScript(SchemaMigrationContext context, String... scriptNames) {
		super(context);
		this.scriptNames = scriptNames;
	}

	@Override
	public void migrate() throws SQLException {
		for (String scriptName : scriptNames) {
			executePgScript(scriptName);
		}
	}

}

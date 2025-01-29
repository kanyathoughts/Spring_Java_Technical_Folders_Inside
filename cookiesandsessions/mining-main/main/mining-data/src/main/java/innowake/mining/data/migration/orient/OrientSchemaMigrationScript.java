/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.migration.orient;

import java.sql.SQLException;

import innowake.mining.data.migration.base.OrientSchemaMigration;
import innowake.mining.data.migration.base.SchemaMigrationContext;

/**
 * Runs one or more SQL script against the Orient database.
 */
public class OrientSchemaMigrationScript extends OrientSchemaMigration {

	private final String[] scriptNames;

	public OrientSchemaMigrationScript(final SchemaMigrationContext context, final String... scriptNames) {
		super(context);
		this.scriptNames = scriptNames;
	}

	@Override
	public void migrateOrient() throws SQLException {
		for (final var scriptName : scriptNames) {
			executeOrientScript(scriptName);
		}
	}

}

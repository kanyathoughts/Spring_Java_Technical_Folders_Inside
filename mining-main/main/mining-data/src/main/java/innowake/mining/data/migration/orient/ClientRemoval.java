/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.migration.orient;

import java.sql.SQLException;
import java.util.List;

import innowake.mining.data.migration.base.OrientSchemaMigration;
import innowake.mining.data.migration.base.SchemaMigrationContext;

/**
 * <b>Repeatable</b> migration script that migrates all entities in OrientDB that are linked with Clients and deletes all Client entities from OrientDB.
 */
public class ClientRemoval extends OrientSchemaMigration {

	/**
	 * Constructor.
	 * 
	 * @param context the {@link SchemaMigrationContext}
	 */
	public ClientRemoval(final SchemaMigrationContext context) {
		super(context);
	}

	@Override
	public void migrateOrient() throws SQLException {
		LOG.info("Starting migration of Projects and SavedSearch for Clients in OrientDB");
		executeOrientScript("ClientVertexRemoval");
		ifDeletionRequested(this::deleteClientClass, () -> "Skipping Client class deletion.");
	}

	private void deleteClientClass() throws SQLException {
		LOG.info("Deleting Client vertex in OrientDB");

		/* If Client class was already deleted in a previous run then this statement will fail in the repeating run with an SQLException */
		try {
			executeStatements(dbOrient, List.of("DELETE VERTEX Client;"));
		} catch (final SQLException e) {
			LOG.warn("Query 'DELETE VERTEX Client' failed with error", e);
		}

		executeStatements(dbOrient, List.of("DROP CLASS Client IF EXISTS;"));
	}
}

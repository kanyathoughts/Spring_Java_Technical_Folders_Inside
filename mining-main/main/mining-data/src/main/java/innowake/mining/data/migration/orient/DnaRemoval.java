/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.migration.orient;

import java.sql.SQLException;
import java.util.List;

import innowake.mining.data.migration.base.OrientSchemaMigration;
import innowake.mining.data.migration.base.SchemaMigrationContext;

/**
 * <b>Repeatable</b> migration script that migrates all entities in OrientDB that are linked with Projects and deletes all Project entities from OrientDB.
 */
public class DnaRemoval extends OrientSchemaMigration {

	/**
	 * Constructor.
	 * 
	 * @param context the {@link SchemaMigrationContext}
	 */
	public DnaRemoval(final SchemaMigrationContext context) {
		super(context);
	}

	@Override
	public void migrateOrient() throws SQLException {
		LOG.info("Starting migration of ModuleUnits for DNA in OrientDB");

		ifDeletionRequested(() -> {
				executeStatements(dbOrient, List.of(
					"DROP PROPERTY ModuleUnit.out_BelongsToCluster IF EXISTS;",
					"UPDATE ModuleUnit REMOVE out_BelongsToCluster;"));
				deleteDnaClasses();
			}, () -> "Skipping deletion of DNA vertices and edges.");
	}

	private void deleteDnaClasses() throws SQLException {
		LOG.info("Deleting DNA vertices and edges in OrientDB");

		deleteSafe("DELETE EDGE BelongsToCluster;");
		deleteSafe("DELETE VERTEX DnaCommunity;");
		deleteSafe("DELETE VERTEX DnaSnapshot;");
		deleteSafe("DELETE VERTEX DnaSimilarity;");
		deleteSafe("DELETE VERTEX DnaStringElement;");
		deleteSafe("DELETE VERTEX DnaString;");

		executeStatements(dbOrient, List.of("DROP CLASS BelongsToCluster IF EXISTS;",
											"DROP CLASS DnaCommunity IF EXISTS;",
											"DROP CLASS DnaSnapshot IF EXISTS;",
											"DROP CLASS DnaSimilarity IF EXISTS;",
											"DROP CLASS DnaStringElement IF EXISTS;",
											"DROP CLASS DnaString IF EXISTS;" ));
	}

	private void deleteSafe(final String statement) {
		/* If Project class was already deleted in a previous run then this statement will fail in the repeating run with an SQLException */
		try {
			executeStatements(dbOrient, List.of(statement));
		} catch (final SQLException e) {
			LOG.warn("Query '" + statement + "' failed with error", e);
		}
	}
}

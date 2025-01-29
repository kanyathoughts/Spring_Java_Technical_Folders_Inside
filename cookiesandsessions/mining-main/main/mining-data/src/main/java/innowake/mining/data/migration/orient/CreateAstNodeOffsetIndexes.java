/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.data.migration.orient;

import java.sql.SQLException;

import innowake.mining.data.migration.base.OrientSchemaMigration;
import innowake.mining.data.migration.base.SchemaMigrationContext;

/**
 * Creates separate {@code assembledOffset} and {@code retracedOffset} columns on the {@code AstNode} table
 * and sets it to the value of {@code advancedModuleLocation.assembledOffset} and {@code advancedModuleLocation.retracedOffset} respectively.
 * Also creates indexes on the new columns.
 * <p>
 * Reason is that we often (Data Lineage in particular) query by offset, but it is not possible to create an index on the embedded property.
 */
public class CreateAstNodeOffsetIndexes extends OrientSchemaMigration {

	private static final int BATCH_SIZE = 100_000;

	public CreateAstNodeOffsetIndexes(final SchemaMigrationContext context) {
		super(context);
	}

	@Override
	public void migrateOrient() throws SQLException {
		try (final var st = dbOrient.createStatement()) {
			st.executeUpdate("CREATE PROPERTY AstNode.assembledOffset IF NOT EXISTS INTEGER");
			st.executeUpdate("CREATE PROPERTY AstNode.retracedOffset IF NOT EXISTS INTEGER");
		}

		updateBatchwise(dbOrient, "UPDATE AstNode" +
				" SET assembledOffset=advancedModuleLocation.assembledOffset, " +
				"     retracedOffset=advancedModuleLocation.retracedOffset" +
				" WHERE (assembledOffset IS NULL AND advancedModuleLocation.assembledOffset IS NOT NULL) " +
				" OR (retracedOffset IS NULL AND advancedModuleLocation.retracedOffset IS NOT NULL)" +
				" LIMIT " + BATCH_SIZE);

		try (final var st = dbOrient.createStatement()) {
			st.executeUpdate("CREATE INDEX AstNode_module_assembledOffset_idx IF NOT EXISTS ON AstNode (moduleId, assembledOffset) NOTUNIQUE");
			st.executeUpdate("CREATE INDEX AstNode_module_retracedOffset_idx IF NOT EXISTS ON AstNode (moduleId, retracedOffset) NOTUNIQUE");
		}
	}
}

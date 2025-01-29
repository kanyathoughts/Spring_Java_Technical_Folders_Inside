/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.data.migration.orient;

import innowake.mining.data.migration.base.OrientSchemaMigration;
import innowake.mining.data.migration.base.SchemaMigrationContext;

import java.sql.SQLException;
import java.sql.Statement;

import static innowake.lib.core.api.profiling.ProfilingFactory.createStopWatch;

/**
 * Migration script creates projectId property in Annotations and indexs based on projectId
 */
public class CreateIndexesOnAnnotation extends OrientSchemaMigration {

	public CreateIndexesOnAnnotation(final SchemaMigrationContext context) {
		super(context);
	}

	@Override
	public void migrateOrient() throws SQLException {
		final var stopWatch = createStopWatch();

		LOG.info("Cleaning Annotations which are not linked with any project");
		updateBatchwise(dbOrient, "DELETE VERTEX FROM Annotation where in_HasAnnotation = null or in_HasAnnotation.size() = 0 LIMIT 10000");
		LOG.info("Annotation which are not linked with any project is cleared");
		LOG.info("Creating Indexes linked with projectId for all Annotations");
		try (final Statement st = dbOrient.createStatement()) {
			
			/* Dropping indexes with projectLink directly created in 07/28 release. */
			st.executeUpdate("DROP INDEX Annotation_projectLink_name_idx IF EXISTS");
			st.executeUpdate("DROP INDEX Annotation_projectLink_englishTranslation_idx IF EXISTS");
			st.executeUpdate("DROP INDEX Annotation_projectLink_createdByUserId_idx IF EXISTS");
			st.executeUpdate("DROP INDEX Annotation_projectLink_updatedByUserId_idx IF EXISTS");
			st.executeUpdate("DROP INDEX Annotation_projectLink_typeLink_idx IF EXISTS");
			st.executeUpdate("DROP INDEX Annotation_projectLink_categoryLink_idx IF EXISTS");
			st.executeUpdate("DROP INDEX Annotation_projectLink_stateLink_idx IF EXISTS");
			st.executeUpdate("DROP INDEX Annotation_projectLink_metaData_idx IF EXISTS");
			
			/* Dropping indexes created without the projectId*/
			st.executeUpdate("DROP INDEX Annotation_in_HasAnnotation_idx IF EXISTS;");
			st.executeUpdate("DROP INDEX Annotation_typeLink_idx IF EXISTS");
			st.executeUpdate("DROP INDEX Annotation_categoryLink_idx IF EXISTS");
			st.executeUpdate("DROP INDEX Annotation_sourceAttachmentLink_idx IF EXISTS");
			st.executeUpdate("DROP INDEX Annotation_stateLink_idx IF EXISTS");
			st.executeUpdate("DROP INDEX Annotation_projectId_englishTranslation_idx IF EXISTS");
			st.executeUpdate("DROP INDEX Annotation_projectId_createdByUserId_idx IF EXISTS");
			st.executeUpdate("DROP INDEX Annotation_projectId_updatedByUserId_idx IF EXISTS");
			st.executeUpdate("DROP INDEX Annotation_out_HasAdditionalInfo_idx IF EXISTS");
			st.executeUpdate("DROP INDEX Annotation_metaData_idx IF EXISTS");
			
			/* Creating indexes with projectId */

/*			Don't create indexes as next mig script will remove Annotation from OrientDB anyway
			st.executeUpdate("CREATE INDEX Annotation_projectId_typeLink_idx IF NOT EXISTS ON "
					+ "Annotation (projectId, typeLink) NOTUNIQUE");
			st.executeUpdate("CREATE INDEX Annotation_projectId_categoryLink_idx IF NOT EXISTS ON "
					+ "Annotation (projectId, categoryLink) NOTUNIQUE");
			st.executeUpdate("CREATE INDEX Annotation_projectId_stateLink_idx IF NOT EXISTS ON "
					+ "Annotation (projectId, stateLink) NOTUNIQUE");
			st.executeUpdate("CREATE INDEX Annotation_projectId_name_idx IF NOT EXISTS ON "
					+ "Annotation (projectId, name) NOTUNIQUE");
			st.executeUpdate("CREATE INDEX Annotation_projectId_metaData_idx IF NOT EXISTS ON "
					+ "Annotation (projectId, metaData) NOTUNIQUE");
					*/
		}
		LOG.info("Indexes linked with projectId for all Annotations is created.");
		stopWatch.stop();
		LOG.info(() -> String.format("Creating new indexes took %s (H:mm:ss.SSS)",
				stopWatch.toString()));
	}

}

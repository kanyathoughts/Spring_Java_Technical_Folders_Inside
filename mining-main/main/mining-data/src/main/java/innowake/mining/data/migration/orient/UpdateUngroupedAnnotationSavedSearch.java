/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.data.migration.orient;

import static innowake.lib.core.api.profiling.ProfilingFactory.createStopWatch;

import java.sql.SQLException;
import java.sql.Statement;

import innowake.mining.data.migration.base.OrientSchemaMigration;
import innowake.mining.data.migration.base.SchemaMigrationContext;

public class UpdateUngroupedAnnotationSavedSearch extends OrientSchemaMigration {

	public UpdateUngroupedAnnotationSavedSearch(final SchemaMigrationContext context) {
		super(context);
	}

	@Override
	public void migrateOrient() throws SQLException {
		final var stopWatch = createStopWatch();

		LOG.info("Updating Ungrouped Annotations Saved Search");
		
		try (final Statement st = dbOrient.createStatement()) {
			/* Updating Ungrouped Annotations saved search */
			st.executeUpdate("UPDATE SavedSearch SET savedSearch='columns=Module.name&columns=Annotation.type&"
					+ "columns=Annotation.categoryName&columns=Annotation.state&columns=Annotation.name&"
					+ "columns=Annotation.sourceAttachment&columns=Annotation.englishTranslation&"
					+ "columns=AnnotationFunctionalGroup.name&columns=Annotation.updatedByUserName&page=1&"
					+ "&sort={content_module_name: ASC}&filter=[{\"key\":\"functionalGroups.name\","
					+ "\"value\":[{\"operator\":\"is\",\"value\":null}]}]' WHERE usage=\"miningUi.annotationsTable\""
					+ " AND name=\"Ungrouped Annotations\";");
		}
		
		LOG.info("Ungrouped Annotations saved search updated");
		stopWatch.stop();
		LOG.info(() -> String.format("Migration of Updating Ungrouped Annotations Saved Search in OrientDB completed in %s",
				stopWatch.toString()));
	}
}

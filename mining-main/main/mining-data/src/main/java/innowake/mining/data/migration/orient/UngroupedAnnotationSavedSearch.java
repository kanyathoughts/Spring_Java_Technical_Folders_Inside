/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.data.migration.orient;

import innowake.mining.data.migration.base.OrientSchemaMigration;
import innowake.mining.data.migration.base.SchemaMigrationContext;

import java.sql.SQLException;
import java.sql.Statement;

import static innowake.lib.core.api.profiling.ProfilingFactory.createStopWatch;

public class UngroupedAnnotationSavedSearch extends OrientSchemaMigration {

	public UngroupedAnnotationSavedSearch(final SchemaMigrationContext context) {
		super(context);
	}

	@Override
	public void migrateOrient() throws SQLException {
		final var stopWatch = createStopWatch();

		LOG.info("Creating Ungrouped Annotations saved search");
		try (final Statement st = dbOrient.createStatement()) {
			/* Creating Ungrouped Annotations saved search */
			st.executeUpdate("INSERT INTO SavedSearch SET name=\"Ungrouped Annotations\", usage=\"miningUi.annotationsTable\", projectId=0,"
					+ "scope = (SELECT FROM ScopeEnum WHERE name = 'GLOBAL'),savedSearch='columns=Module.name&"
					+ "columns=Annotation.typeLink&columns=AnnotationCategory.name&columns=Annotation.stateLink&columns=Annotation.name&"
					+ "columns=Annotation.sourceAttachment&columns=Annotation.englishTranslation&columns=AnnotationFunctionalGroup.name&"
					+ "columns=Annotation.updatedByUserName&page=1&&sort={content_inHasAnnotation_out_name: ASC}"
					+ "&filter=[{\"key\":\"functionalGroups.name\",\"value\":[\"\"]}]';");
		}
		LOG.info("Ungrouped Annotations saved search created");
		stopWatch.stop();
		LOG.info(() -> String.format("Migration of Ungrouped Annotations saved search in OrientDB completed in %s",
				stopWatch.toString()));
	}

}

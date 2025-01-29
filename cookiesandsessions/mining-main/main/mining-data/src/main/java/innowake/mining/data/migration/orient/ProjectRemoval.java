/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.migration.orient;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import innowake.mining.data.migration.base.OrientSchemaMigration;
import innowake.mining.data.migration.base.SchemaMigrationContext;

/**
 * <b>Repeatable</b> migration script that migrates all entities in OrientDB that are linked with Projects and deletes all Project entities from OrientDB.
 */
public class ProjectRemoval extends OrientSchemaMigration {

	/**
	 * Constructor.
	 * 
	 * @param context the {@link SchemaMigrationContext}
	 */
	public ProjectRemoval(final SchemaMigrationContext context) {
		super(context);
	}

	@Override
	public void migrateOrient() throws SQLException {
		migrateModules();
		migrateAnnotations();
		migrateTaxonomies();
		migrateStatements();
		migrateExcelSheetDeadCode();
		migrateExcelSheetErrors();
		migrateSourceObjects();
		migrateDataDictionary();
		migrateEffortSummary();
		migrateSavedSearch();
		migrateExcelSheetUndiscovered();
		migrateMiningJobInfo();
		migrateDependencyDefinition();

		createUpdateModuleErrorMarkerLinksJs();

		ifDeletionRequested(this::deleteProjectClass, () -> "Skipping Project class deletion.");
	}

	private void migrateModules() throws SQLException {
		LOG.info("Starting migration of Modules for Projects in OrientDB");
		executeStatements(dbOrient, List.of("CREATE PROPERTY module.projectId IF NOT EXISTS LONG;"));

		updateBatchwise(dbOrient, "UPDATE Module SET projectId = projectLink.id WHERE projectId is Null LIMIT 10000;");

		dropIndexSafe("Module_creator_idx");
		dropIndexSafe("Module_linkHash");
		dropIndexSafe("Module_name_idx");
		dropIndexSafe("Module_path_idx");
		dropIndexSafe("Module_projectLink_requiresReview_idx");
		dropIndexSafe("Module_projectLink_idx");
		dropIndexSafe("Module_projectLinkAndobjectTypeLink_idx");

		final List<String> statements = new ArrayList<>();
		statements.add("ALTER PROPERTY Module.projectId MANDATORY TRUE;");
		statements.add("ALTER PROPERTY Module.projectId NOTNULL TRUE;");
		statements.add("DROP PROPERTY Module.projectLink IF EXISTS;");
		statements.add("UPDATE Module REMOVE projectLink;");
		statements.add("CREATE INDEX Module_creator_idx IF NOT EXISTS ON Module(projectId, creator) NOTUNIQUE;");
		statements.add("CREATE INDEX Module_linkHash IF NOT EXISTS ON Module(projectId, linkHash) NOTUNIQUE;");
		statements.add("CREATE INDEX Module_name_idx IF NOT EXISTS ON Module(projectId, name) NOTUNIQUE;");
		statements.add("CREATE INDEX Module_path_idx IF NOT EXISTS ON Module (projectId, path) UNIQUE_HASH_INDEX METADATA {ignoreNullValues: true};");
		statements.add("CREATE INDEX Module_projectId_requiresReview_idx IF NOT EXISTS ON Module(projectId, requiresReview) NOTUNIQUE;");
		statements.add("CREATE INDEX Module_projectId_idx IF NOT EXISTS ON Module(projectId) NOTUNIQUE;");
		statements.add("CREATE INDEX Module_projectIdAndobjectTypeLink_idx IF NOT EXISTS ON Module(projectId, objectTypeLink) NOTUNIQUE;");

		executeStatements(dbOrient, statements);
	}

	private void migrateAnnotations() throws SQLException {
		LOG.info("Starting migration of Annotations for Projects in OrientDB");

		/* Annotation */
		executeStatements(dbOrient, List.of("CREATE PROPERTY Annotation.projectId IF NOT EXISTS LONG;"));

		updateBatchwise(dbOrient, "UPDATE Annotation SET projectId = projectLink.id WHERE projectId is Null LIMIT 10000;");

		/* Suppressed: com.orientechnologies.orient.core.exception.OCommandExecutionException: Property used in indexes 
		 * (Annotation_projectLink_metaData_idx, Annotation_projectLink_categoryLink_idx, 
		 *  Annotation_projectLink_stateLink_idx, Annotation_projectLink_typeLink_idx). */
		dropIndexSafe("Annotation_projectLink_name_idx");
		dropIndexSafe("Annotation_projectLink_englishTranslation_idx");
		dropIndexSafe("Annotation_projectLink_createdByUserId_idx");
		dropIndexSafe("Annotation_projectLink_updatedByUserId_idx");
		dropIndexSafe("Annotation_projectLink_typeLink_idx");
		dropIndexSafe("Annotation_projectLink_stateLink_idx");
		dropIndexSafe("Annotation_projectLink_categoryLink_idx");
		dropIndexSafe("Annotation_projectLink_metaData_idx");
		dropIndexSafe("Annotation_projectLink_idx");
		dropIndexSafe("AnnotationCategory_name_idx");
		dropIndexSafe("AnnotationMetaData_project_reason_idx");

		final List<String> statements = new ArrayList<>();

		statements.add("ALTER PROPERTY Annotation.projectId MANDATORY TRUE;");
		statements.add("ALTER PROPERTY Annotation.projectId NOTNULL TRUE;");
		statements.add("DROP PROPERTY Annotation.projectLink IF EXISTS;");
		statements.add("UPDATE Annotation remove projectLink;");
		statements.add("CREATE INDEX Annotation_projectId_idx IF NOT EXISTS ON Annotation(projectId) NOTUNIQUE;");

		/* AnnotationCategory */
		statements.add("CREATE PROPERTY AnnotationCategory.projectId IF NOT EXISTS LONG;");
		statements.add("UPDATE AnnotationCategory SET projectId = projectLink.id WHERE projectId is Null;");
		statements.add("ALTER PROPERTY AnnotationCategory.projectId MANDATORY TRUE;");
		statements.add("ALTER PROPERTY AnnotationCategory.projectId NOTNULL TRUE;");
		statements.add("DROP PROPERTY AnnotationCategory.projectLink IF EXISTS;");
		statements.add("UPDATE  AnnotationCategory REMOVE projectLink;");
		statements.add("CREATE INDEX AnnotationCategory_name_idx IF NOT EXISTS ON AnnotationCategory(name, projectId, typeLink) UNIQUE;");

		/* AnnotationMetaData */
		statements.add("CREATE PROPERTY AnnotationMetaData.projectId IF NOT EXISTS LONG;");
		statements.add("UPDATE AnnotationMetaData SET projectId = projectLink.id WHERE projectId is Null;");
		statements.add("ALTER PROPERTY AnnotationMetaData.projectId MANDATORY TRUE;");
		statements.add("ALTER PROPERTY AnnotationMetaData.projectId NOTNULL TRUE;");
		statements.add("DROP PROPERTY AnnotationMetaData.projectLink IF EXISTS;");
		statements.add("UPDATE  AnnotationMetaData REMOVE projectLink;");
		statements.add("CREATE INDEX AnnotationMetaData_project_reason_idx IF NOT EXISTS ON AnnotationMetaData(projectId, reason) NOTUNIQUE;");

		executeStatements(dbOrient, statements);
	}

	private void migrateTaxonomies() throws SQLException {
		LOG.info("Starting migration of Taxonomies for Projects in OrientDB");

		dropIndexSafe("Taxonomy_name_idx");
		dropIndexSafe("TaxonomyEnum_name_idx");
		dropIndexSafe("TaxonomyCategory_project_name_idx");

		/* Taxonomy */
		executeStatements(dbOrient, List.of("CREATE PROPERTY Taxonomy.projectId IF NOT EXISTS LONG;"));

		updateBatchwise(dbOrient, "UPDATE Taxonomy SET projectId = projectLink.id WHERE projectId is Null LIMIT 10000;");

		final List<String> statements = new ArrayList<>();
		statements.add("ALTER PROPERTY Taxonomy.projectId MANDATORY TRUE;");
		statements.add("ALTER PROPERTY Taxonomy.projectId NOTNULL TRUE;");
		statements.add("DROP PROPERTY Taxonomy.projectLink IF EXISTS;");
		statements.add("UPDATE Taxonomy REMOVE projectLink;");
		statements.add("CREATE INDEX Taxonomy_name_idx IF NOT EXISTS ON Taxonomy(projectId, typeLink, name) UNIQUE;");

		/* TaxonomyEnum */
		statements.add("CREATE PROPERTY TaxonomyEnum.projectId IF NOT EXISTS LONG;");
		statements.add("UPDATE TaxonomyEnum SET projectId = projectLink.id WHERE projectId is Null;");
		statements.add("ALTER PROPERTY TaxonomyEnum.projectId MANDATORY TRUE;");
		statements.add("ALTER PROPERTY TaxonomyEnum.projectId NOTNULL TRUE;");
		statements.add("DROP PROPERTY TaxonomyEnum.projectLink IF EXISTS;");
		statements.add("UPDATE TaxonomyEnum REMOVE projectLink;");
		statements.add("CREATE INDEX TaxonomyEnum_name_idx IF NOT EXISTS ON TaxonomyEnum(name, projectId) UNIQUE;");

		/* TaxonomyCategory */
		statements.add("CREATE PROPERTY TaxonomyCategory.projectId IF NOT EXISTS LONG;");
		statements.add("UPDATE TaxonomyCategory SET projectId = projectLink.id WHERE projectId is Null;");
		statements.add("ALTER PROPERTY TaxonomyCategory.projectId MANDATORY TRUE;");
		statements.add("ALTER PROPERTY TaxonomyCategory.projectId NOTNULL TRUE;");
		statements.add("DROP PROPERTY TaxonomyCategory.projectLink IF EXISTS;");
		statements.add("UPDATE  TaxonomyCategory REMOVE projectLink;");
		statements.add("CREATE INDEX TaxonomyCategory_project_name_idx IF NOT EXISTS ON TaxonomyCategory(projectId, name) UNIQUE;");

		executeStatements(dbOrient, statements);
	}

	private void migrateStatements() throws SQLException {
		LOG.info("Starting migration of Statements for Projects in OrientDB");

		dropIndexSafe("Statement_projectLink_idx");
		dropIndexSafe("Statement_technology_type_idx");
		dropIndexSafe("ExcelSheetConditionalOutlines_projectLink_idx");

		executeStatements(dbOrient, List.of("CREATE PROPERTY Statement.projectId IF NOT EXISTS LONG;"));

		updateBatchwise(dbOrient, "UPDATE Statement SET projectId = projectLink.id WHERE projectId is Null LIMIT 10000;");

		final List<String> statements = new ArrayList<>();
		statements.add("ALTER PROPERTY Statement.projectId MANDATORY TRUE;");
		statements.add("ALTER PROPERTY Statement.projectId NOTNULL TRUE;");
		statements.add("DROP PROPERTY Statement.projectLink IF EXISTS;");
		statements.add("UPDATE  Statement REMOVE projectLink;");
		statements.add("CREATE INDEX Statement_projectId_idx IF NOT EXISTS ON Statement(projectId) NOTUNIQUE;");
		statements.add("CREATE INDEX Statement_technology_type_idx IF NOT EXISTS ON Statement(projectId, technologyLink, statementTypeLink) NOTUNIQUE;");

		executeStatements(dbOrient, statements);
	}

	private void migrateExcelSheetDeadCode() throws SQLException {
		LOG.info("Starting migration of ExcelSheetDeadCodes for Projects in OrientDB");

		dropIndexSafe("ExcelSheetDeadCode_projectLink_idx");
		dropIndexSafe("ExcelSheetErrors_projectLink_idx");
		dropIndexSafe("ExcelSheetErrors_projectLink_key_idx");

		executeStatements(dbOrient, List.of("CREATE PROPERTY ExcelSheetDeadCode.projectId IF NOT EXISTS LONG;"));

		updateBatchwise(dbOrient, "UPDATE ExcelSheetDeadCode SET projectId = projectLink.id WHERE projectId is Null LIMIT 10000;");

		final List<String> statements = new ArrayList<>();
		statements.add("ALTER PROPERTY ExcelSheetDeadCode.projectId MANDATORY TRUE;");
		statements.add("ALTER PROPERTY ExcelSheetDeadCode.projectId NOTNULL TRUE;");
		statements.add("DROP PROPERTY ExcelSheetDeadCode.projectLink IF EXISTS;");
		statements.add("UPDATE  ExcelSheetDeadCode REMOVE projectLink;");
		statements.add("CREATE INDEX ExcelSheetDeadCode_projectId_idx IF NOT EXISTS ON ExcelSheetDeadCode(projectId) NOTUNIQUE;");

		executeStatements(dbOrient, statements);
	}

	private void migrateExcelSheetErrors() throws SQLException {
		LOG.info("Starting migration of ExcelSheetErrors for Projects in OrientDB");

		executeStatements(dbOrient, List.of("CREATE PROPERTY ExcelSheetErrors.projectId IF NOT EXISTS LONG;"));

		updateBatchwise(dbOrient, "UPDATE ExcelSheetErrors SET projectId = projectLink.id WHERE projectId is Null LIMIT 10000;");

		final List<String> statements = new ArrayList<>();
		statements.add("ALTER PROPERTY ExcelSheetErrors.projectId MANDATORY TRUE;");
		statements.add("ALTER PROPERTY ExcelSheetErrors.projectId NOTNULL TRUE;");
		statements.add("DROP PROPERTY ExcelSheetErrors.projectLink IF EXISTS;");
		statements.add("UPDATE  ExcelSheetErrors REMOVE projectLink;");
		statements.add("CREATE INDEX ExcelSheetErrors_projectId_idx IF NOT EXISTS ON ExcelSheetErrors(projectId) NOTUNIQUE;");
		statements.add("CREATE INDEX ExcelSheetErrors_projectId_key_idx IF NOT EXISTS ON ExcelSheetErrors(projectId,key) NOTUNIQUE;");

		executeStatements(dbOrient, statements);
	}

	private void migrateSourceObjects() throws SQLException {
		LOG.info("Starting migration of SourceObjects for Projects in OrientDB");

		dropIndexSafe("projectLink_name_idx");
		dropIndexSafe("SourceObject_path_idx");
		dropIndexSafe("SourceObject_project_technology_idx");
		dropIndexSafe("SourceObject_projectLink_idx");
		dropIndexSafe("DataDictionaryOtherScopeEnum_idx");

		executeStatements(dbOrient, List.of("CREATE PROPERTY SourceObject.projectId IF NOT EXISTS LONG;"));

		updateBatchwise(dbOrient, "UPDATE SourceObject SET projectId = projectLink.id WHERE projectId is Null LIMIT 10000;");

		final List<String> statements = new ArrayList<>();
		statements.add("ALTER PROPERTY SourceObject.projectId MANDATORY TRUE;");
		statements.add("ALTER PROPERTY SourceObject.projectId NOTNULL TRUE;");
		statements.add("DROP PROPERTY SourceObject.projectLink IF EXISTS;");
		statements.add("UPDATE SourceObject REMOVE projectLink;");
		statements.add("CREATE INDEX projectId_name_idx IF NOT EXISTS ON SourceObject(projectId, name) NOTUNIQUE_HASH_INDEX;");
		statements.add("CREATE INDEX SourceObject_path_idx IF NOT EXISTS ON SourceObject(projectId, path) UNIQUE;");
		statements.add("CREATE INDEX SourceObject_project_technology_idx IF NOT EXISTS ON SourceObject(projectId ,technologyLink) NOTUNIQUE");
		statements.add("CREATE INDEX SourceObject_projectId_idx IF NOT EXISTS ON SourceObject(projectId) NOTUNIQUE;");

		executeStatements(dbOrient, statements);
	}

	private void migrateDataDictionary() throws SQLException {
		LOG.info("Starting migration of DataDictionaryOtherScopeEnums for Projects in OrientDB");

		dropIndexSafe("DataDictionaryEntry_projectLink_idx");
		dropIndexSafe("DataDictionaryEntry_projectLink_dataElementName_idx");
		dropIndexSafe("DataDictionaryEntry_projectLink_format_idx");
		dropIndexSafe("DataDictionaryEntry_projectLink_groupPath_idx");
		dropIndexSafe("DataDictionaryEntry_projectLink_picClause_idx");
		dropIndexSafe("DataDictionaryEntry_projectLink_isReferenced_idx");
		dropIndexSafe("DataDictionaryEntry_projectLink_definedLocation_idx");
		dropIndexSafe("DataDictionaryEntry_projectLink_isBusiness_idx");
		dropIndexSafe("DataDictionaryEntry_projectLink_scopeLink_idx");
		dropIndexSafe("DataDictionaryEntry_projectLink_stateLink_idx");

		/* DataDictionaryOtherScopeEnum */
		final List<String> statements = new ArrayList<>();
		statements.add("CREATE PROPERTY DataDictionaryOtherScopeEnum.projectId IF NOT EXISTS LONG;");
		statements.add("UPDATE DataDictionaryOtherScopeEnum SET projectId = projectLink.id WHERE projectId is Null;");
		statements.add("ALTER PROPERTY DataDictionaryOtherScopeEnum.projectId MANDATORY TRUE;");
		statements.add("ALTER PROPERTY DataDictionaryOtherScopeEnum.projectId NOTNULL TRUE;");
		statements.add("DROP PROPERTY DataDictionaryOtherScopeEnum.projectLink IF EXISTS;");
		statements.add("UPDATE  DataDictionaryOtherScopeEnum REMOVE projectLink;");
		statements.add("CREATE INDEX DataDictionaryOtherScopeEnum_idx IF NOT EXISTS ON DataDictionaryOtherScopeEnum(name, projectId) UNIQUE;");

		LOG.info("Starting migration of DataDictionaryEntries for Projects in OrientDB");

		/* DataDictionaryEntry */
		statements.add("CREATE PROPERTY DataDictionaryEntry.projectId IF NOT EXISTS LONG;");

		executeStatements(dbOrient, statements);

		updateBatchwise(dbOrient, "UPDATE DataDictionaryEntry SET projectId = projectLink.id WHERE projectId is Null LIMIT 10000;");

		statements.clear();
		statements.add("ALTER PROPERTY DataDictionaryEntry.projectId MANDATORY TRUE;");
		statements.add("ALTER PROPERTY DataDictionaryEntry.projectId NOTNULL TRUE;");
		statements.add("DROP PROPERTY DataDictionaryEntry.projectLink IF EXISTS;");
		statements.add("UPDATE DataDictionaryEntry REMOVE projectLink;");
		statements.add("CREATE INDEX DataDictionaryEntry_projectId_idx IF NOT EXISTS ON DataDictionaryEntry (projectId) NOTUNIQUE;");
		statements.add("CREATE INDEX DataDictionaryEntry_projectId_dataElementName_idx IF NOT EXISTS ON DataDictionaryEntry (projectId, dataElementName) NOTUNIQUE;");
		statements.add("CREATE INDEX DataDictionaryEntry_projectId_format_idx IF NOT EXISTS ON DataDictionaryEntry (projectId, format) NOTUNIQUE;");
		statements.add("CREATE INDEX DataDictionaryEntry_projectId_groupPath_idx IF NOT EXISTS ON DataDictionaryEntry (projectId, groupPath) NOTUNIQUE;");
		statements.add("CREATE INDEX DataDictionaryEntry_projectId_picClause_idx IF NOT EXISTS ON DataDictionaryEntry (projectId, picClause) NOTUNIQUE;");
		statements.add("CREATE INDEX DataDictionaryEntry_projectId_isReferenced_idx IF NOT EXISTS ON DataDictionaryEntry (projectId, isReferenced) NOTUNIQUE;");
		statements.add("CREATE INDEX DataDictionaryEntry_projectId_definedLocation_idx IF NOT EXISTS ON DataDictionaryEntry (projectId, definedLocation) NOTUNIQUE;");
		statements.add("CREATE INDEX DataDictionaryEntry_projectId_isBusiness_idx IF NOT EXISTS ON DataDictionaryEntry (projectId, isBusiness) NOTUNIQUE;");
		statements.add("CREATE INDEX DataDictionaryEntry_projectId_scopeLink_idx IF NOT EXISTS ON DataDictionaryEntry (projectId, scopeLink) NOTUNIQUE;");
		statements.add("CREATE INDEX DataDictionaryEntry_projectId_stateLink_idx IF NOT EXISTS ON DataDictionaryEntry (projectId, stateLink) NOTUNIQUE;");

		executeStatements(dbOrient, statements);
	}

	private void migrateEffortSummary() throws SQLException {
		LOG.info("Starting migration of EffortSummaries for Projects in OrientDB");

		dropIndexSafe("EffortSummary_projectLink_idx");

		final List<String> statements = new ArrayList<>();
		statements.add("CREATE PROPERTY EffortSummary.projectId IF NOT EXISTS LONG;");
		statements.add("UPDATE EffortSummary SET projectId = projectLink.id WHERE projectId is Null;");
		statements.add("ALTER PROPERTY EffortSummary.projectId MANDATORY TRUE;");
		statements.add("ALTER PROPERTY EffortSummary.projectId NOTNULL TRUE;");
		statements.add("DROP PROPERTY EffortSummary.projectLink IF EXISTS;");
		statements.add("UPDATE  EffortSummary REMOVE projectLink;");
		statements.add("CREATE INDEX EffortSummary_projectId_idx IF NOT EXISTS ON EffortSummary(projectId) UNIQUE;");

		executeStatements(dbOrient, statements);
	}

	private void migrateSavedSearch() throws SQLException {
		LOG.info("Starting migration of SavedSearch for Projects in OrientDB");

		dropIndexSafe("SavedSearch_idx");
		dropIndexSafe("SavedSearch_usage_idx");

		final List<String> statements = new ArrayList<>();
		statements.add("CREATE PROPERTY SavedSearch.projectId IF NOT EXISTS LONG;");
		statements.add("UPDATE SavedSearch SET projectId = projectLink.id WHERE projectId is Null;");
		statements.add("DROP PROPERTY SavedSearch.projectLink IF EXISTS;");
		statements.add("UPDATE SavedSearch REMOVE projectLink;");
		statements.add("CREATE INDEX SavedSearch_idx IF NOT EXISTS ON SavedSearch(name, usage, projectId,clientId, createdByUserId) UNIQUE;");
		statements.add("CREATE INDEX SavedSearch_usage_idx IF NOT EXISTS ON SavedSearch(usage, projectId,clientId, createdByUserId) NOTUNIQUE;");

		executeStatements(dbOrient, statements);
	}

	private void migrateExcelSheetUndiscovered() throws SQLException {
		LOG.info("Starting migration of ExcelSheetUndiscovered for Projects in OrientDB");

		dropIndexSafe("ExcelSheetUndiscovered_project_name_path");
		dropIndexSafe("ExcelSheetUndiscovered_projectLink_idx");

		final List<String> statements = new ArrayList<>();
		statements.add("CREATE PROPERTY ExcelSheetUndiscovered.projectId IF NOT EXISTS LONG;");
		statements.add("UPDATE ExcelSheetUndiscovered SET projectId = projectLink.id WHERE projectId is Null;");
		statements.add("ALTER PROPERTY ExcelSheetUndiscovered.projectId MANDATORY TRUE;");
		statements.add("ALTER PROPERTY ExcelSheetUndiscovered.projectId NOTNULL TRUE;");
		statements.add("DROP PROPERTY ExcelSheetUndiscovered.projectLink IF EXISTS;");
		statements.add("UPDATE ExcelSheetUndiscovered REMOVE projectLink;");
		statements.add("CREATE INDEX ExcelSheetUndiscovered_project_name_path IF NOT EXISTS ON ExcelSheetUndiscovered(projectId,name,path) UNIQUE;");
		statements.add("CREATE INDEX ExcelSheetUndiscovered_projectId_idx IF NOT EXISTS ON ExcelSheetUndiscovered(projectId) NOTUNIQUE;");

		executeStatements(dbOrient, statements);
	}

	private void migrateMiningJobInfo() throws SQLException {
		LOG.info("Starting migration of MiningJobInfos for Projects in OrientDB");

		dropIndexSafe("MiningJobInfo_projectLink_moduleLink_idx");

		executeStatements(dbOrient, List.of("CREATE PROPERTY MiningJobInfo.projectId IF NOT EXISTS LONG;"));

		updateBatchwise(dbOrient, "UPDATE MiningJobInfo SET projectId = projectLink.id WHERE projectId is Null AND projectLink is NOT Null LIMIT 10000;");

		final List<String> statements = new ArrayList<>();
		statements.add("DROP PROPERTY MiningJobInfo.projectLink IF EXISTS;");
		statements.add("UPDATE MiningJobInfo REMOVE projectLink;");
		statements.add("CREATE INDEX MiningJobInfo_projectId_moduleLink_idx IF NOT EXISTS ON MiningJobInfo(projectId,moduleLink) NOTUNIQUE;");

		executeStatements(dbOrient, statements);
	}

	private void migrateDependencyDefinition() throws SQLException {
		LOG.info("Starting migration of DependencyDefinitions for Projects in OrientDB");

		executeStatements(dbOrient, List.of("CREATE PROPERTY DependencyDefinition.projectId IF NOT EXISTS LONG;"));

		updateBatchwise(dbOrient, "UPDATE DependencyDefinition SET projectId = projectLink.id WHERE projectId is Null LIMIT 10000;");

		final List<String> statements = new ArrayList<>();
		statements.add("ALTER PROPERTY DependencyDefinition.projectId MANDATORY TRUE;");
		statements.add("ALTER PROPERTY DependencyDefinition.projectId NOTNULL TRUE;");
		statements.add("DROP PROPERTY DependencyDefinition.projectLink IF EXISTS;");
		statements.add("UPDATE DependencyDefinition REMOVE projectLink;");

		executeStatements(dbOrient, statements);
	}

	private void createUpdateModuleErrorMarkerLinksJs() throws SQLException {
		LOG.info("Starting migration of Functions for Projects in OrientDB");

		executeStatements(dbOrient, List.of("DELETE FROM OFunction WHERE name = 'updateModuleErrorMarkerLinksJs';"));

		final String function = "CREATE FUNCTION updateModuleErrorMarkerLinksJs \""
							  + "var db = orient.getDatabase();"
							  + "var rs = db.query('SELECT FROM (SELECT expand(moduleLink) FROM (SELECT DISTINCT moduleLink FROM ExcelSheetErrors where projectId=?)) ', project);"
							  + "for (var i = 0; i < rs.length; i++) {"
							  + "  db.command('UPDATE ? SET errorMarkerLinks=(SELECT FROM ExcelSheetErrors WHERE moduleLink=?)', rs[i], rs[i]);"
							  + "}"
							  + "return rs.length;"
							  + "\" PARAMETERS[project] LANGUAGE javascript;";

		/* If function was not yet deleted in a previous run then this statement will fail in the repeating run with an SQLException */
		try {
			executeStatements(dbOrient, List.of(function));
		} catch (final SQLException e) {
			LOG.warn("Query 'CREATE FUNCTION updateModuleErrorMarkerLinksJs' failed with error", e);
		}
	}

	private void deleteProjectClass() throws SQLException {
		LOG.info("Deleting Project vertex in OrientDB");

		/* If Project class was already deleted in a previous run then this statement will fail in the repeating run with an SQLException */
		try {
			executeStatements(dbOrient, List.of("DELETE VERTEX Project;"));
		} catch (final SQLException e) {
			LOG.warn("Query 'DELETE VERTEX Project' failed with error", e);
		}

		executeStatements(dbOrient, List.of("DROP CLASS Project IF EXISTS;"));
	}
}

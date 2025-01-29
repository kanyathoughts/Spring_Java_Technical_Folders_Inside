/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.data.migration;

import innowake.mining.data.migration.base.PostgresSchemaMigration;
import innowake.mining.data.migration.base.PostgresSchemaMigrations;
import innowake.mining.data.migration.base.SchemaMigrationContext;
import innowake.mining.data.migration.orient.ClientRemoval;
import innowake.mining.data.migration.orient.CreateAstNodeOffsetIndexes;
import innowake.mining.data.migration.orient.CreateIndexesOnAnnotation;
import innowake.mining.data.migration.orient.DnaRemoval;
import innowake.mining.data.migration.orient.ModuleRemoval;
import innowake.mining.data.migration.orient.OrientSchemaMigrationScript;
import innowake.mining.data.migration.orient.ProjectRemoval;
import innowake.mining.data.migration.orient.SourceAttachmentRemoval;
import innowake.mining.data.migration.orient.UngroupedAnnotationSavedSearch;
import innowake.mining.data.migration.orient.UpdateUngroupedAnnotationSavedSearch;
import innowake.mining.data.migration.postgres.AnnotationMigration;
import innowake.mining.data.migration.postgres.AstMigration;
import innowake.mining.data.migration.postgres.ClientEntityMigration;
import innowake.mining.data.migration.postgres.CustomPropertyMetaMigration;
import innowake.mining.data.migration.postgres.CustomPropertyMigration;
import innowake.mining.data.migration.postgres.DataDictionaryMigration;
import innowake.mining.data.migration.postgres.DataFlowMigration;
import innowake.mining.data.migration.postgres.FieldInfoMigration;
import innowake.mining.data.migration.postgres.DnaMigration;
import innowake.mining.data.migration.postgres.ExcelSheetUndiscoveredMigration;
import innowake.mining.data.migration.postgres.FF4JMigration;
import innowake.mining.data.migration.postgres.JobInfoMigration;
import innowake.mining.data.migration.postgres.ModuleMigration;
import innowake.mining.data.migration.postgres.OfflineTokenMigration;
import innowake.mining.data.migration.postgres.PostgresSchemaMigrationScript;
import innowake.mining.data.migration.postgres.ProjectEntityMigration;
import innowake.mining.data.migration.postgres.SavedSearchMigration;
import innowake.mining.data.migration.postgres.SourceObjectMigration;
import innowake.mining.data.migration.postgres.SummaryMigration;
import innowake.mining.data.migration.postgres.TaxonomyMigration;

/**
 * Migrations for the Mining Postgres schema.
 */
public class PostgresMiningSchemaMigrations extends PostgresSchemaMigrations {
	
	@Override
	protected PostgresSchemaMigration supplyInitial(final SchemaMigrationContext context) {
		return new PostgresSchemaMigrationScript(context, 
				"properties_create",
				"mining_entity_create",
				"binary_attachment_create",
				"timestamp_zoned_milli_create"
			);
	}
	
	@Override
	protected void supplyMigrations(final SchemaMigrationContext context) {
		migration(1, () -> new ClientEntityMigration(context));
		migration(2, () -> new ClientRemoval(context));
		migration(3, () -> new ProjectEntityMigration(context));
		migration(4, () -> new PostgresSchemaMigrationScript(context,
				"module_location_create",
				"module_table_create_dummy"));
		migration(5, () -> new DnaMigration(context));
		migration(6, () -> new DnaRemoval(context));
		migration(7, () -> new ProjectRemoval(context));
		migration(8, () -> new CreateIndexesOnAnnotation(context));
		migration(9, () -> new PostgresSchemaMigrationScript(context,
				"functional_block_create"));
		migration(10, () -> new OrientSchemaMigrationScript(context,
				"Create_InitialValueOnDataDictionaryEntry"));
		migration(11, () -> new PostgresSchemaMigrationScript(context,
				"functional_block_children_add_ordinal"));
		migration(12, () -> new PostgresSchemaMigrationScript(context,
				"functional_block_flags_and_generated_from_create"));
		migration(13, () -> new PostgresSchemaMigrationScript(context,
				"dna_community_add_description_property"));
		migration(14, () -> new OrientSchemaMigrationScript(context,
				"SavedSearch_sortBy_fieldName"));
		migration(15, () -> new PostgresSchemaMigrationScript(context,
				"functional_block_link_create"));
		migration(16, () -> new PostgresSchemaMigrationScript(context,
				"functional_block_reachability_data_create"));
		migration(17, () -> new PostgresSchemaMigrationScript(context,
				"functional_block_link_create_index"));
		migration(18, () -> new PostgresSchemaMigrationScript(context,
				"functional_block_children_deep_create"));
		migration(19, () -> new PostgresSchemaMigrationScript(context,
				"functional_block_type_create_index"));
		migration(20, () -> new PostgresSchemaMigrationScript(context,
				"reachability_data_functional_block_index_create"));
		migration(21, () -> new PostgresSchemaMigrationScript(context,
				"functional_block_status_index_create"));
		migration(22, () -> new UngroupedAnnotationSavedSearch(context));
		migration(23, () -> new PostgresSchemaMigrationScript(context,
				"update_functional_block_reachability_data_schema"));
		migration(24, () -> new PostgresSchemaMigrationScript(context,
				"create_table_reachability_data_intermediate_modules"));
		migration(25, () -> new SourceObjectMigration(context));
		migration(26, () -> new ModuleMigration(context));
		migration(27, () -> new AnnotationMigration(context));
		migration(28, () -> new TaxonomyMigration(context));
		migration(29, () -> new DataDictionaryMigration(context));
		migration(30, () -> new CustomPropertyMigration(context));
		migration(31, () -> new ModuleRemoval(context));
		migration(32, () -> new SourceAttachmentRemoval(context));
		migration(33, () -> new OrientSchemaMigrationScript(context,
				"SavedSearch_migration"));
		migration(34, () -> new PostgresSchemaMigrationScript(context,
				"project_client_nonnull"));
		migration(35, () -> new PostgresSchemaMigrationScript(context,
				"functional_block_link_condition_replace_trigger"));
		migration(36, () -> new PostgresSchemaMigrationScript(context,
				"foreign_key_indexes"));
		migration(37, () -> new OrientSchemaMigrationScript(context,
				"SavedSearch_migration"));
		migration(38, () -> new PostgresSchemaMigrationScript(context,
				"alter_requires_review_in_module"));
		migration(39, () -> new OrientSchemaMigrationScript(context,
				"DataFlowNode_DataFlowId"));
		migration(40, () -> new CreateAstNodeOffsetIndexes(context));
		migration(41, () -> new UpdateUngroupedAnnotationSavedSearch(context));
		migration(42, () -> new PostgresSchemaMigrationScript(context,
				"functional_block_generated_from_add_timestamps"));
		migration(43, () -> new PostgresSchemaMigrationScript(context,
				"add_dependency_hash_column_to_module"));
		migration(44, () -> new PostgresSchemaMigrationScript(context,
				"alter_module_relations"));
		migration(45, () -> new PostgresSchemaMigrationScript(context,
				"functional_block_referenced_data_dictionary"));
		migration(46, () -> new PostgresSchemaMigrationScript(context,
				"module_relationship_add_dependency_definition"));
		migration(47, () -> new PostgresSchemaMigrationScript(context,
				"set_requires_review_not_null_in_module"));
		migration(48, () -> new PostgresSchemaMigrationScript(context,
				"functional_block_generated_from_module_dependency"));
		migration(49, () -> new PostgresSchemaMigrationScript(context,
				"translated_field_data_dictionary"));
		migration(50, () -> new PostgresSchemaMigrationScript(context,
				"taxonomy_add_utility"));
		migration(51, () -> new PostgresSchemaMigrationScript(context,
				"scheduler_info_create"));
		migration(52, () -> new PostgresSchemaMigrationScript(context,
				"module_relationship_type_new_succeeds"));
		migration(53, () -> new PostgresSchemaMigrationScript(context,
				"module_relationship_type_rename_succeeds"));
		migration(54, () -> new PostgresSchemaMigrationScript(context,
				"create_metadata_backup_column"));
		migration(55, () -> new PostgresSchemaMigrationScript(context,
				"insert_Functional_type_into_annotation_category"));
		migration(56, () -> new PostgresSchemaMigrationScript(context, "module_relationship_from_dead_code"));
		migration(57, () -> new CustomPropertyMetaMigration(context));
		migration(58, () -> new FF4JMigration(context));
		migration(59, () -> new AstMigration(context));
		migration(60, () -> new DataFlowMigration(context));
		migration(61, () -> new SummaryMigration(context));
		migration(62, () -> new ExcelSheetUndiscoveredMigration(context));
		migration(63, () -> new SavedSearchMigration(context));
		migration(64, () -> new JobInfoMigration(context));
		migration(65, () -> new FieldInfoMigration(context));
		migration(66, () -> new OfflineTokenMigration(context));
		migration(67, () -> new PostgresSchemaMigrationScript(context, "create_properties_column_in_scheduler_import"));
		migration(68, () -> new PostgresSchemaMigrationScript(context, "change_project_configuration_value_column_to_jsonb"));
		migration(69, () -> new PostgresSchemaMigrationScript(context, "insert_default_configuration_for_reachability_analysis"));
		migration(70, () -> new PostgresSchemaMigrationScript(context, "functional_block_generated_from_indexes"));
		migration(71, () -> new PostgresSchemaMigrationScript(context, "modify_error_marker_location"));
		migration(72, () -> new PostgresSchemaMigrationScript(context, "field_Level_Mining_Candidates_Saved_Search"));
	}
	
}

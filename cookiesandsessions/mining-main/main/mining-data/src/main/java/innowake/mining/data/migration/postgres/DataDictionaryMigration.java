/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.migration.postgres;

import java.sql.SQLException;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.orientechnologies.orient.core.sql.executor.OResult;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.access.postgres.PgJSON;
import innowake.mining.data.access.postgres.PgUtil;
import innowake.mining.data.migration.base.PostgresSchemaMigrationFromOrient;
import innowake.mining.data.migration.base.SchemaMigrationContext;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.model.DefinedLocation;

/**
 * Migrates Data Dictionary entries to Postgres.
 */
public class DataDictionaryMigration extends PostgresSchemaMigrationFromOrient {

	public DataDictionaryMigration(final SchemaMigrationContext context) {
		super(context);
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public void migrate() throws SQLException {
		executePgScript("data_dictionary_tables_create");
		migrateData("DataDictionaryOtherScopeEnum ", 
			"select projectId, name from DataDictionaryOtherScopeEnum", 
			"insert into data_dictionary_other_scope values ((select uid from project where nid = ?), ?)",
			1000, (in, out, pass) -> {
				out.add(in.getLong(1));
				out.add(in.getString(2));
				return false;
			});
		migrateData("DataDictionaryEntries", 
			"select @rid, id, first(in_HasDataDictionaryEntry).out.id, first(in_HasDataDictionaryEntry).fromModuleLocation.offset,"
			+ " first(in_HasDataDictionaryEntry).fromModuleLocation.length, dataElementName, description, format, scopeLink.name, scopeAttributes, length,"
			+ " createdByUserId, updatedByUserId, otherScopeLink.name, otherScopeSource, picClause, definedLocation, stateLink.name, isBusiness,"
			+ " fieldTransformation, sourceInput, targetOutput, isReferenced, usage, isCandidate, fieldLevel, parentGroup, groupPath, indentation, initialValue"
			+ " from DataDictionaryEntry",
			"insert into data_dictionary values (?, null, ?, (select uid from module where nid = ?), (?,?), ?, ?,"
			+ " ?, ?, ?, ?, ?, ?, ?, ?::working_state, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
			1000, (in, out, pass) -> {
				final List<String> scopes = (List<String>) in.getObject(9);
				Map<String, Object> scopeAttr = (Map<String, Object>) in.getObject(10);
				final String otherScope = in.getString(14);
				final String otherScopeSource = in.getString(15);
				if (scopeAttr == null) {
					scopeAttr = new LinkedHashMap<>();
				} else {
					for (final Map.Entry<String, Object> attr : scopeAttr.entrySet()) {
						final OResult value = (OResult) attr.getValue();
						attr.setValue(value != null ? value.getProperty("attributes") : Collections.emptyMap());
					}
				}
				if (scopes != null && ! scopes.isEmpty()) {
					for (String scope : scopes) {
						if (! scopeAttr.containsKey(scope)) {
							scopeAttr.put(scope, Collections.emptyMap());
						}
					}
				}
				if (otherScope != null || otherScopeSource != null) {
					final Map<String, String> other = new LinkedHashMap<>();
					if (otherScope != null) {
						other.put("scope", otherScope);
					}
					if (otherScopeSource != null) {
						other.put("source", otherScopeSource);
					}
					scopeAttr.put("OTHER", other);
				}
				out.add(uuid5.generate(MiningEnitityNames.DATA_DICTIONARY, in.getString(1))); /* uid */
				out.add(in.getLong(2)); /* nid */
				out.add(in.getLong(3)); /* module */
				out.add(in.getInt(4)); /* location.offset */
				out.add(in.getInt(5)); /* location.length */
				out.add(replaceNullBytes(in.getString(6))); /* name */
				out.add(replaceNullBytes(in.getString(7))); /* description */
				out.add(replaceNullBytes(in.getString(8))); /* format */
				out.add(PgJSON.toPGobject(scopeAttr)); /* scopes */
				out.add(in.getLong(11)); /* length */
				out.add(in.getString(12)); /* created_by */
				out.add(in.getString(13)); /* updated_by */
				out.add(replaceNullBytes(in.getString(16))); /* pic_clause */
				out.add(PgUtil.mapNullable(in.getString(17), s -> DefinedLocation.fromName(s).name())); /* defined_location */
				out.add(in.getString(18)); /* state */
				out.add(in.getObject(19)); /* is_business */
				out.add(replaceNullBytes(in.getString(20))); /* field_transformation */
				out.add(replaceNullBytes(in.getString(21))); /* source_input */
				out.add(replaceNullBytes(in.getString(22))); /* target_output */
				out.add(in.getObject(23)); /* is_referenced */
				out.add(in.getString(24)); /* usage */
				out.add(in.getObject(25)); /* is_candidate */
				out.add(in.getLong(26)); /* field_level */
				out.add(replaceNullBytes(in.getString(27))); /* parent_group */
				out.add(replaceNullBytes(in.getString(28))); /* group_path */
				out.add(in.getLong(29)); /* indentation */
				out.add(replaceNullBytes(in.getString(30))); /* initial_value */
				return false;
			});
		migrateData("HasBusinessRules", 
			"select out.@rid, in.@rid from HasBusinessRule",
			"insert into data_dictionary_annotations values (?, ?)",
			1000, (in, out, pass) -> {
				out.add(uuid5.generate(MiningEnitityNames.DATA_DICTIONARY, in.getString(1)));
				out.add(uuid5.generate(MiningEnitityNames.ANNOTATION, in.getString(2)));
				return false;
			});

		updatePgSequence("data_dictionary_nid", "select coalesce(max(nid) + 1, 1) from data_dictionary");
	}
	
	@Nullable
	private String replaceNullBytes(@Nullable final String cause) {
		return cause == null ? null : cause.replace('\0', ' ');
	}
}

/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.migration.postgres;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Stream;

import innowake.mining.data.access.postgres.PgJSON;
import innowake.mining.data.migration.base.PostgresSchemaMigrationFromOrient;
import innowake.mining.data.migration.base.SchemaMigrationContext;
import innowake.mining.shared.MiningEnitityNames;

/**
 * Migrates the Project entity
 */
public class ProjectEntityMigration extends PostgresSchemaMigrationFromOrient {
	
	public ProjectEntityMigration(final SchemaMigrationContext context) {
		super(context);
	}
	
	@Override
	public void migrate() throws SQLException {
		LOG.info("Creating project tables Postgres");
		executePgScript("project_tables_create");
		executePgScript("custom_enum_table_create");
		migrateProjects();
		migrateProjectConfigs();
		migrateCustomEnmus();
		migrateCustomEnumValues();
	}
	
	private UUID genProjectId(String rid) {
		return genUUIDv5(MiningEnitityNames.PROJECT, rid);
	}
	
	@SuppressWarnings("unchecked")
	private void migrateProjects() throws SQLException {
		LOG.info("Copying Project entities from OrientDB to Postgres");

		migrateData("Projects",
			"select clientId, id, name, toBeDeleted, sourceCodeRevision, metricsBaseRevision, metricsVersion, metricsDate,"
			+ " searchOrders, defaultTaxonomyCategoryLink.id, technicalTaxonomyCategoryLink.id,"
			+ " customPropertyClasses, @rid from Project",
			"insert into project values (?, ?, (select uid from client where nid = ?), ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
			100, (in, out, n) -> {
				out.add(genProjectId(in.getString(13)));
				out.add(null);
				out.add(in.getLong(1));
				out.add(in.getLong(2));
				out.add(in.getString(3));
				out.add(in.getBoolean(4));
				out.add(in.getLong(5));
				out.add(in.getLong(6));
				out.add(in.getString(7));
				out.add(in.getTimestamp(8));
				out.add(PgJSON.toPGobjectsFromStrings((ArrayList<String>) in.getObject(9)).toJdbcArray(dbPostgres));
				out.add(in.getLong(10));
				out.add(in.getLong(11));
				out.add(PgJSON.toPGobject(in.getObject(12)));
				return false;
			}, () -> executeStatements(dbPostgres, 
				"INSERT INTO project (uid, nid, client, name) VALUES (gen_random_uuid(), 0, (SELECT uid FROM client WHERE nid = 0), 'SYSTEM')")
		);
		updatePgSequence("project_nid", "select coalesce(max(nid) + 1, 1) from project");
	}
	
	@SuppressWarnings("unchecked")
	private void migrateProjectConfigs() {
		LOG.info("Copying Project configurations from OrientDB to Postgres");

		final AtomicReference<Iterator<Map.Entry<String, String>>> configs = new AtomicReference<>();
		migrateData("ProjectConfigs",
			"select @rid, configurations from Project",
			"insert into project_configuration values (?, ?, ?)",
			10, (in, out, n) -> {
				if (n == 0) {
					configs.set(((Map<String, String>) in.getObject(2)).entrySet().iterator());
				}
				if (configs.get().hasNext()) {
					final Map.Entry<String, String> config = configs.get().next();
					out.add(genProjectId(in.getString(1)));
					out.add(config.getKey());
					out.add(config.getValue());
				}
				return configs.get().hasNext();
			}
		);
	}
	
	@SuppressWarnings("unchecked")
	private void migrateCustomEnmus() {
		LOG.info("Copying custom enums from OrientDB to Postgres");

		final AtomicReference<Iterator<String>> tagNames = new AtomicReference<>();
		migrateData("ProjectAutoCompletionMapKeys",
			"select @rid, autoCompletionMap.keys() from Project",
			"insert into custom_enum (id, project, name) values (gen_random_uuid(), ?, ?)",
			1000, (in, out, n) -> {
				if (in.getObject(2) == null) {
					return false;
				}
				if (n == 0) {
					tagNames.set(((Set<String>) in.getObject(2)).iterator());
				}
				if (tagNames.get().hasNext()) {
					out.add(genProjectId(in.getString(1)));
					out.add(tagNames.get().next());
				}
				return tagNames.get().hasNext();
			}
		);
	}
	
	@SuppressWarnings("unchecked")
	private void migrateCustomEnumValues() {
		LOG.info("Copying custom enum values from OrientDB to Postgres");

		final AtomicReference<Iterator<Object[]>> tagRecords = new AtomicReference<>();
		migrateData("ProjectAutoCompletionMapValues",
			"select @rid, autoCompletionMap from Project",
			"insert into custom_enum_values (enum, value) values ((select id from custom_enum where project = ? and name = ?), ?)",
			1000, (in, out, n) -> {
				if (n == 0) {
					final Stream.Builder<Object[]> tagRecBuilder = Stream.builder();
					final UUID projectId = genProjectId(in.getString(1));
					final Map<String, List<String>> tagMap = ((Map<String, List<String>>) in.getObject(2));
					if (tagMap != null) {
						for (final Map.Entry<String, List<String>> tagEntry : tagMap.entrySet()) {
							for (final String tag : tagEntry.getValue()) {
								tagRecBuilder.accept(new Object[] {
									projectId,
									tagEntry.getKey(),
									tag
								});
							}
						}
					}
					tagRecords.set(tagRecBuilder.build().iterator());
				}
				if (tagRecords.get().hasNext()) {
					out.addAll(Arrays.asList(tagRecords.get().next()));
				}
				return tagRecords.get().hasNext();
			}
		);
	}
	
}

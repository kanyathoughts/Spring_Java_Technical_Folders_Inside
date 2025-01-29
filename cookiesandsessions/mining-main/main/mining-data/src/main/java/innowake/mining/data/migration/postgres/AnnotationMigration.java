/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.migration.postgres;

import java.nio.charset.StandardCharsets;
import java.sql.SQLException;

import innowake.mining.data.access.postgres.PgArray;
import innowake.mining.data.access.postgres.PgType;
import innowake.mining.data.access.postgres.PgUtil;
import innowake.mining.data.migration.base.PostgresSchemaMigrationFromOrient;
import innowake.mining.data.migration.base.SchemaMigrationContext;
import innowake.mining.shared.MiningEnitityNames;

/**
 * Migrates Annotations and Annotation type definitions to Postgres.
 */
public class AnnotationMigration extends PostgresSchemaMigrationFromOrient {
	
	public AnnotationMigration(final SchemaMigrationContext context) {
		super(context);
	}
	
	@Override
	public void migrate() throws SQLException {
		executePgScript("working_state_enum_create");
		executePgScript("annotation_tables_create");
		migrateData("AnnotationCategories", 
			"select id, name, projectId, typeLink.name from AnnotationCategory", 
			"insert into annotation_category values (?, (select uid from project where nid = ?), ?, ?)",
			1000, (in, out, pass) -> {
				out.add(in.getLong(1));
				out.add(in.getLong(3));
				out.add(in.getString(2));
				out.add(PgUtil.arrayFromCollectionNonNull(PgType.STRING, PgUtil.collection(in.getObject(4))).toJdbcArray(dbPostgres));
				return false;
			}, () -> executePgScript("annotation_defaults")
		);
		updatePgSequence("annotation_category_id", "select coalesce(max(id) + 1, 1) from annotation_category");
		migrateData("Annotations", 
			"select @rid, id, name, stateLink.name, typeLink.name, categoryLink.id, createdByUserId, updatedByUserId, first(in_HasAnnotation).out.@rid,"
			+ " first(in_HasAnnotation).fromModuleLocation.offset, first(in_HasAnnotation).fromModuleLocation.length,"
			+ " sourceAttachmentLink.content, englishTranslation, metaData.reason.name from Annotation", 
			"insert into annotation values (?, null, ?, ?, (?,?), ?, ?::working_state, ?, ?, ?, ?, ?, ?, ?)",
			1000, (in, out, pass) -> {
				out.add(uuid5.generate(MiningEnitityNames.ANNOTATION, in.getString(1)));
				out.add(in.getLong(2));
				out.add(uuid5.generate(MiningEnitityNames.MODULE, in.getString(9)));
				out.add(in.getInt(10));
				out.add(in.getInt(11));
				out.add(in.getString(3));
				out.add(in.getString(4));
				out.add(in.getString(5));
				out.add(in.getObject(6) == null ? null : in.getLong(6));
				out.add(in.getString(7));
				out.add(in.getString(8));
				out.add(in.getString(12).getBytes(StandardCharsets.UTF_8));
				final PgArray reasons = PgUtil.arrayFromCollectionNonEmpty(PgType.STRING, PgUtil.collection(in.getObject(14)));
				out.add(reasons != null ? reasons.toJdbcArray(dbPostgres) : reasons);
				out.add(in.getString(13));
				return false;
			});
		/* start sequence at the number following the present maximum NID or 1 if no records exist */
		updatePgSequence("annotation_nid", "select coalesce(max(nid) + 1, 1) from annotation");
		executePgScript("jsonb_merge_create");
		executePgScript("array_set_merge_create");
	}
	
}

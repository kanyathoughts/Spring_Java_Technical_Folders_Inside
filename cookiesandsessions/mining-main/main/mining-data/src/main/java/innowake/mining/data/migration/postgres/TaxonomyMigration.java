/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.migration.postgres;

import java.sql.SQLException;

import innowake.mining.data.access.postgres.PgJSON;
import innowake.mining.data.migration.base.PostgresSchemaMigrationFromOrient;
import innowake.mining.data.migration.base.SchemaMigrationContext;
import innowake.mining.shared.MiningEnitityNames;


/**
 * Migrates Taxonomies to Postgres
 */
public class TaxonomyMigration extends PostgresSchemaMigrationFromOrient {
	
	public TaxonomyMigration(SchemaMigrationContext context) {
		super(context);
	}
	
	@Override
	public void migrate() throws SQLException {
		executePgScript("taxonomy_tables_create");
		migrateData("TaxonomyCategories",
			"select id, projectId, name from TaxonomyCategory",
			"insert into taxonomy_category values (?, (select uid from project where nid = ?), ?)",
			1000, (in, out, n) -> {
				out.add(in.getLong(1));
				out.add(in.getLong(2));
				out.add(in.getString(3));
				return false;
			});
		migrateData("TaxonomyTypes",
			"select @rid, projectId, categoryLink.id, name from TaxonomyEnum",
			"insert into taxonomy_type values (?, (select uid from project where nid = ?), ?, ?)",
			1000, (in, out, n) -> {
				out.add(uuid5.generate(MiningEnitityNames.TAXONOMY_TYPE, in.getString(1)));
				out.add(in.getLong(2));
				out.add(in.getLong(3));
				out.add(in.getString(4));
				return false;
			});
		migrateData("Taxonomies",
			"select @rid, id, projectId, typeLink.@rid, name from Taxonomy",
			"insert into taxonomy values (?, null, ?, (select uid from project where nid = ?), ?, ?)",
			1000, (in, out, n) -> {
				out.add(uuid5.generate(MiningEnitityNames.TAXONOMY, in.getString(1)));
				out.add(in.getLong(2));
				out.add(in.getLong(3));
				out.add(uuid5.generate(MiningEnitityNames.TAXONOMY_TYPE, in.getString(4)));
				out.add(in.getString(5));
				return false;
			});
		migrateData("ModuleTaxonomies",
			"select out.@rid, in.@rid, properties from HasTaxonomy",
			"insert into module_taxonomies values (?, ?, ?)",
			1000, (in, out, n) -> {
				out.add(uuid5.generate(MiningEnitityNames.MODULE, in.getString(1)));
				out.add(uuid5.generate(MiningEnitityNames.TAXONOMY, in.getString(2)));
				out.add(PgJSON.toPGobject(in.getObject(3)));
				return false;
			});

		updatePgSequence("taxonomy_category_id", "select coalesce(max(id) + 1, 1) from taxonomy_category");
		updatePgSequence("taxonomy_nid", "select coalesce(max(nid) + 1, 1) from taxonomy");
	}
	
}

/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.migration.postgres;

import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

import org.bouncycastle.util.encoders.Hex;

import innowake.mining.data.access.postgres.PgJSON;
import innowake.mining.data.migration.base.PostgresSchemaMigrationFromOrient;
import innowake.mining.data.migration.base.SchemaMigrationContext;
import innowake.mining.shared.MiningEnitityNames;

/**
 * Migrates all DNA related entities
 */
public class DnaMigration extends PostgresSchemaMigrationFromOrient {

	public DnaMigration(final SchemaMigrationContext context) {
		super(context);
	}

	@Override
	public void migrate() throws SQLException {
		LOG.info("Creating DNA tables Postgres");
		executePgScript("dna_tables_create");

		LOG.info("Copying DnaString entities from OrientDB to Postgres");
		migrateData("DnaString",
				"select moduleUnitLink.id, moduleUnitLink.@rid, moduleUnitLink.projectLink.@rid, sequencerId, generatedOn, contentHash from DnaString",
				/* the nonsense with the CTEs in the following is necessary because JDBC cannot handle multiple results from batch statements */
				"WITH b AS (insert into module values (?, null, ?, ?) on conflict (uid) do nothing returning 1)"
				+ " , c AS (insert into dna_string values (?, ?, ?, ?) returning 1)"
				+ " SELECT * FROM b, c",
				100, (in, out, n) -> {
					final var moduleId = genUUIDv5(MiningEnitityNames.MODULE, in.getString(2));
					/* Module */
					out.add(moduleId);
					out.add(in.getLong(1));
					out.add(genUUIDv5(MiningEnitityNames.PROJECT, in.getString(3)));
					/* DnaString */
					out.add(moduleId);
					out.add(in.getString(4));
					out.add(in.getTimestamp(5));
					out.add(Hex.decode(in.getString(6)));
					return false;
				}
			);

		LOG.info("Copying DnaStringElement entities from OrientDB to Postgres");
		migrateData("DnaStringElement",
				"select stringLink.moduleUnitLink.@rid, stringLink.sequencerId, index, location.offset, location.length, value from DnaStringElement",
				"insert into dna_string_element values (?, ?, ?, (?, ?), ?)",
				100, (in, out, n) -> {
					out.add(genUUIDv5(MiningEnitityNames.MODULE, in.getString(1)));
					out.add(in.getString(2));
					out.add(in.getInt(3));
					out.add(in.getInt(4));
					out.add(in.getInt(5));
					out.add(in.getString(6));
					return false;
				}
			);

		LOG.info("Copying DnaSimilarity entities from OrientDB to Postgres");
		migrateData("DnaSimilarity",
				"select fromDnaString.moduleUnitLink.@rid, toDnaString.moduleUnitLink.@rid, sequencerId, similarityId, similarity from DnaSimilarity",
				"insert into dna_similarity values (?, ?, ?, ?, ?)",
				100, (in, out, n) -> {
					out.add(genUUIDv5(MiningEnitityNames.MODULE, in.getString(1)));
					out.add(genUUIDv5(MiningEnitityNames.MODULE, in.getString(2)));
					out.add(in.getString(3));
					out.add(in.getString(4));
					out.add(in.getDouble(5));
					return false;
				}
			);

		LOG.info("Copying DnaSnapshot entities from OrientDB to Postgres");
		migrateData("DnaSnapshot",
				"select projectLink.@rid, @rid, name, updatedOn, totalModuleCount,"
				+ " maxLevels, maxIterations, defaultTolerance, minDNALength, similarityThreshold from DnaSnapshot",
				"insert into dna_snapshot values (?, ?, ?, ?, ?, ?)",
				100, (in, out, n) -> {
					out.add(genUUIDv5(MiningEnitityNames.DNA_SNAPSHOT, in.getString(2)));
					out.add(genUUIDv5(MiningEnitityNames.PROJECT, in.getString(1)));
					out.add(in.getString(3));
					out.add(in.getTimestamp(4));
					out.add(in.getInt(5));
					final Map<String, Object> conf = new HashMap<>();
					conf.put("maxLevels", in.getInt(6));
					conf.put("maxIterations", in.getInt(7));
					conf.put("defaultTolerance", in.getDouble(8));
					conf.put("minDNALength", in.getInt(9));
					conf.put("similarityThreshold", in.getDouble(10));
					out.add(PgJSON.toPGobject(conf));
					return false;
				}
			);

		LOG.info("Copying DnaCommunity entities from OrientDB to Postgres");
		migrateData("DnaCommunity",
				"select @rid, snapshotLink.@rid, sequencerId, similarityId, clusterAlgorithmId, clusterIndex, title from DnaCommunity",
				"insert into dna_community values (?, ?, ?, ?, ?, ?, ?)",
				100, (in, out, n) -> {
					out.add(genUUIDv5(MiningEnitityNames.DNA_COMMUNITY, in.getString(1)));
					out.add(genUUIDv5(MiningEnitityNames.DNA_SNAPSHOT, in.getString(2)));
					out.add(in.getString(3));
					out.add(in.getString(4));
					out.add(in.getString(5));
					out.add(in.getInt(6));
					out.add(in.getString(7));
					return false;
				}
			);

		LOG.info("Copying DnaCommunity Module edges from OrientDB to Postgres");
		migrateData("DnaCommunity Modules",
				"select in.@rid, out.@rid from (select expand(in_BelongsToCluster) from DnaCommunity)",
				"insert into dna_community_modules values (?, ?)",
				100, (in, out, n) -> {
					out.add(genUUIDv5(MiningEnitityNames.DNA_COMMUNITY, in.getString(1)));
					out.add(genUUIDv5(MiningEnitityNames.MODULE, in.getString(2)));
					return false;
				}
			);
	}

}

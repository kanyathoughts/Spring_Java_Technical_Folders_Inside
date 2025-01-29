/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.migration.postgres;

import java.nio.charset.StandardCharsets;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

import org.apache.commons.codec.DecoderException;
import org.apache.commons.codec.binary.Hex;

import innowake.mining.data.migration.base.PostgresSchemaMigrationFromOrient;
import innowake.mining.data.migration.base.SchemaMigrationContext;
import innowake.mining.shared.MiningEnitityNames;

/**
 * Migrates the SourceAttachment/SourceObject entities
 */
public class SourceObjectMigration extends PostgresSchemaMigrationFromOrient {

	public SourceObjectMigration(final SchemaMigrationContext context) {
		super(context);
	}

	@Override
	public void migrate() throws SQLException {
		final Set<Long> invalidOrientEntries = findMissingProjects();
		if ( ! invalidOrientEntries.isEmpty()) {
			throw new SQLException("The Postgres migration can not be continued, there are sources in your OrientDB which have a link to project(s) "
					+ "which don't exist anymore. The missing Postgres projects nid's are: " + invalidOrientEntries + ". Please reach out to PE for more assistance");
		}

		executePgScript("source_tables_create");
		migrateSourceAttachments("SourceAttachments@SourceObjects", "select projectId, @rid, content from SourceObject");
		migrateSourceAttachments("SourceAttachments@Modules", "select projectId, sourceAttachmentLink.@rid, sourceAttachmentLink.content from Module where sourceAttachmentLink.@class = 'SourceAttachment'");
		migrateSourceObjects();
		migrateSourceReferences();
	}
	
	private UUID genSourceId(final String rid) {
		return genUUIDv5(MiningEnitityNames.SOURCE, rid);
	}
	
	private void migrateSourceAttachments(final String title, final String srcQuery) {
		migrateData(title, srcQuery,
			"insert into source values (?, (select uid from project where nid = ?), ?)",
			100, (in, out, n) -> {
				out.add(genSourceId(in.getString(2)));
				out.add(in.getLong(1));
				out.add(in.getString(3).getBytes(StandardCharsets.UTF_8));
				return false;
			}
		);
	}
	
	private void migrateSourceObjects() throws SQLException {
		migrateData("SourceObjects",
			"select @rid, id, path, name, technologyLink.name, typeLink.name, metaDataRevision, contentRevision, contentHash from SourceObject",
			"insert into source_info values (?, null, ?, ?, ?, ?, ?, ?, ?, ?)",
			100, (in, out, n) -> {
				out.add(genSourceId(in.getString(1)));
				out.add(in.getLong(2));
				out.add(in.getString(3));
				out.add(in.getString(4));
				out.add(in.getString(5));
				out.add(in.getString(6));
				out.add(in.getLong(7));
				out.add(in.getLong(8));
				try {
					out.add(Hex.decodeHex(in.getString(9)));
				} catch (final DecoderException e) {
					throw new NumberFormatException(e.getMessage());
				}
				return false;
			}
		);
		updatePgSequence("source_nid", "select coalesce(max(nid) + 1, 1) from source_info");
	}
	
	private void migrateSourceReferences() {
		migrateData("SourceReferences",
			"select in.@rid, out.@rid from ReferencesSourceObject",
			"insert into source_references (src, dst) values (?, ?)",
			1000, (in, out, n) -> {
				out.add(genSourceId(in.getString(1)));
				out.add(genSourceId(in.getString(2)));
				return false;
			}
		);
	}

	private Set<Long> findMissingProjects() {
		if (! isOrientAvailable()) {
			return Collections.emptySet();
		}

		final String orientQuery1 = "SELECT distinct projectId FROM SourceObject;";
		final String orientQuery2 = "SELECT distinct projectId FROM Module;";
		final String postgresQuery = "SELECT nid FROM project;";

		final Set<Long> uniqueIdsInOrient = new HashSet<>();
		final Set<Long> uniqueIdsInPostgres = new HashSet<>();

		try (final ResultSet rs1 = getOrientDB().createStatement().executeQuery(orientQuery1);
			 final ResultSet rs2 = getOrientDB().createStatement().executeQuery(orientQuery2);
			 final ResultSet rsP = dbPostgres.createStatement().executeQuery(postgresQuery)) {
			while (rs1.next()) {
				uniqueIdsInOrient.add(Long.valueOf(rs1.getLong(1)));
			}
			while (rs2.next()) {
				uniqueIdsInOrient.add(Long.valueOf(rs2.getLong(1)));
			}
			while (rsP.next()) {
				uniqueIdsInPostgres.add(Long.valueOf(rsP.getLong(1)));
			}
		} catch (final SQLException e) {
			throw new IllegalStateException("Error while fetching project ids from OrientDB or PostgresDB", e);
		}

		uniqueIdsInOrient.removeAll(uniqueIdsInPostgres);
		return uniqueIdsInOrient;
	}
}

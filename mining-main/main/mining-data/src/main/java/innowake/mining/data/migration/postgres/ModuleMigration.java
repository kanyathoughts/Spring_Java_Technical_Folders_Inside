/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.migration.postgres;

import java.io.Serializable;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.Instant;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import org.bouncycastle.util.encoders.Hex;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSetter;
import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.access.postgres.PgArray;
import innowake.mining.data.access.postgres.PgJSON;
import innowake.mining.data.access.postgres.PgType;
import innowake.mining.data.migration.base.PostgresSchemaMigrationFromOrient;
import innowake.mining.data.migration.base.SchemaMigrationContext;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.RelationshipType;

/**
 * Migrates all Module entities to Postgres.
 */
public class ModuleMigration extends PostgresSchemaMigrationFromOrient {
	
	private static final int MIGRATION_BATCH_SIZE = 10000;
	
	private static final String MODULE_LOCATION_PARAMETERS = "CASE WHEN ? THEN (?,?)::module_location ELSE null END";
	
	public ModuleMigration(final SchemaMigrationContext context) {
		super(context);
	}
	
	@Override
	public void migrate() throws SQLException {
		final Set<Long> invalidOrientEntries = findMissingProjects();
		if ( ! invalidOrientEntries.isEmpty()) {
			throw new SQLException("The Postgres migration can not be continued, there are modules in your OrientDB which have a link to project(s) "
					+ "which don't exist anymore. The missing Postgres projects nid's are: " + invalidOrientEntries + ". Please reach out to PE for more assistance");
		}
		
		/* alter existing tables, add sequences and indexes */
		executePgScript("mining_job_info_create");
		executePgScript("ci_collation_create");
		executePgScript("module_table_extend_dummy");
		executePgScript("metrics_tables_create");
		executePgScript("statement_table_create");
		executePgScript("module_relations_create");
		
		/* migrate and merge modules */
		migrateModules();
		
		/* migrate source metrics */
		migrateSourceMetrics();

		/* migrate Statements and SqlStatements */
		migrateStatements();

		/* migrate ExcelSheetErrors */
		migrateErrorMarkers();

		/* ExcelSheetDeadCode */
		migrateModelDeadCodes();

		migrateMiningJobInfo();

		/* set module sequence to latest */
		updatePgSequence("module_nid", "select coalesce(max(nid) + 1, 1) from module");

		/* migrate references between modules */
		migrateReferences();

		/* Migrate dawn module dependency definitions */
		migrateDependencyDefinitions();

		/* set mandatory module columns to NOT NULL */
		executePgScript("module_table_post_migration");
	}
	
	private void migrateModules() {
		migrateData("Modules",
			"SELECT @rid, contentHash, creator, description, id, identificationLink.name as identification, "
			+ "info, linkHash, location.offset as locationOffset, location.length as locationLength, metricsDate, modifiedDate, name, "
			+ "objectTypeLink.typeLink.name as typeName, objectTypeLink.technologyLink.name as technologyName, objectTypeLink.storageLink.name as storageName, "
			+ "objectTypeLink.originLink.name as originName, path, projectId, representation, requiresReview, sourceAttachmentLink.id as sourceId "
			+ " FROM Module",
			"INSERT INTO module "
			+ "(nid, name, path, technology, type, storage,"
			+ " origin, creator, identified, info, description, source, "
			+ " content_hash, location, representation, requires_review, modified_date, metrics_date,"
			+ " link_hash, project, uid) VALUES ("
				+ "?, ?, ?, ?, ?, ?, "
				+ "?, ?, ?, ?, ?, (SELECT uid FROM source_info WHERE nid = ?), "
				+ "?, " + MODULE_LOCATION_PARAMETERS + ", ?, ?, ?, ?, "
				+ "?, (SELECT uid FROM project WHERE nid = ?), ?)"
			+ "ON CONFLICT (nid) DO UPDATE SET "
				+ "name = EXCLUDED.name,"
				+ "path = EXCLUDED.path,"
				+ "technology = EXCLUDED.technology,"
				+ "type = EXCLUDED.type,"
				+ "storage = EXCLUDED.storage,"
				+ "origin = EXCLUDED.origin,"
				+ "creator = EXCLUDED.creator,"
				+ "identified = EXCLUDED.identified,"
				+ "info = EXCLUDED.info,"
				+ "description = EXCLUDED.description,"
				+ "source = EXCLUDED.source,"
				+ "content_hash = EXCLUDED.content_hash,"
				+ "location = EXCLUDED.location,"
				+ "representation = EXCLUDED.representation,"
				+ "requires_review = EXCLUDED.requires_review,"
				+ "modified_date = EXCLUDED.modified_date,"
				+ "metrics_date = EXCLUDED.metrics_date,"
				+ "link_hash = EXCLUDED.link_hash",
			1000, (in, out, n) -> {
				final var moduleUId = genUUIDv5(MiningEnitityNames.MODULE, in.getString("@rid"));

				/* module */
				out.add(Long.valueOf(in.getLong("id")));
				out.add(in.getString("name"));
				out.add(in.getString("path"));
				out.add(in.getString("technologyName"));
				out.add(in.getString("typeName"));
				out.add(in.getString("storageName"));
				out.add(in.getString("originName"));
				out.add(in.getString("creator"));
				out.add(Boolean.valueOf("IDENTIFIED".equals(in.getString("identification"))));
				out.add(PgJSON.toPGobject(in.getObject("info")));
				out.add(in.getString("description"));
				out.add(Long.valueOf(in.getLong("sourceId")));
				final var contentHash = in.getString("contentHash");
				out.add(contentHash == null ? null : Hex.decode(contentHash));
				migrateLocation(in, out);
				out.add(in.getString("representation"));
				out.add(Boolean.valueOf(in.getBoolean("requiresReview")));
				out.add(in.getTimestamp("modifiedDate"));
				out.add(in.getTimestamp("metricsDate"));
				out.add(in.getString("linkHash"));
				out.add(Long.valueOf(in.getLong("projectId")));
				out.add(moduleUId);
				return false;
			});
	}

	private void migrateSourceMetrics() {
		migrateData("SourceMetrics",
			"select in_HasAdditionalInfo[0].out, physicalLines, codeLines, commentLines, complexityMcCabe, deadCodeLines from SourceMetrics",
			"INSERT INTO source_metrics (module, physical_lines, code_lines, comment_lines, complexity_mc_cabe, dead_code_lines) VALUES (?, ?, ?, ?, ?, ?)",
			10000, (in, out, n) -> {
				out.add(genUUIDv5(MiningEnitityNames.MODULE, in.getString(1)));
				out.add(in.getObject(2) == null ? null : Integer.valueOf(in.getInt(2)));
				out.add(in.getObject(3) == null ? null : Integer.valueOf(in.getInt(3)));
				out.add(in.getObject(4) == null ? null : Integer.valueOf(in.getInt(4)));
				out.add(in.getObject(5) == null ? null : Integer.valueOf(in.getInt(5)));
				out.add(in.getObject(6) == null ? null : Integer.valueOf(in.getInt(6)));
				return false;
			});
	}
	
	private String mapReferenceType(final String orientEdge) {
		switch (orientEdge) {
			case "None":
				return "NONE";
			case "Includes":
				return "INCLUDES";
			case "References":
				return "REFERENCES";
			case "Calls":
				return "CALLS";
			case "ReadsWrites":
				return "ACCESSES";
			default:
				throw new IllegalArgumentException("Unknown Module reference " + orientEdge);
		}
	}
	
	public void migrateReferences() {
		migrateData("ModuleReferences",
			"select @rid, @class, in, fromModuleLocation.offset, fromModuleLocation.length,"
			+ " out, toModuleLocation.offset, toModuleLocation.length, properties,"
			+ " excelSheetDependencies.binding, excelSheetDependencies.attributes from Reference where not @class like 'Has%'", 
			"INSERT INTO module_relationship VALUES (?, ?, " + MODULE_LOCATION_PARAMETERS + ","
			+ " ?, " + MODULE_LOCATION_PARAMETERS + ", ?::module_relationship_type, ?, ?, ?)",
			10000, (in, out, n) -> {
				out.add(genUUIDv5(MiningEnitityNames.MODULE_RELATIONSHIP, in.getString(1)));
				out.add(genUUIDv5(MiningEnitityNames.MODULE, in.getString(6)));
				out.add(Boolean.valueOf(in.getObject(4) != null));
				out.add(in.getObject(4));
				out.add(in.getObject(5));
				out.add(genUUIDv5(MiningEnitityNames.MODULE, in.getString(3)));
				out.add(Boolean.valueOf(in.getObject(7) != null));
				out.add(in.getObject(7));
				out.add(in.getObject(8));
				out.add(mapReferenceType(in.getString(2)));
				out.add(PgJSON.toPGobject(in.getObject(9)));
				out.add(in.getString(10));
				out.add(in.getString(11));
				return false;
			});

		migrateData("Conditional Dependencies of ModuleReferences",
				"SELECT @rid, conditionalDependency.ifReachedFrom as ifReachedFrom from Reference WHERE conditionalDependency is not Null UNWIND ifReachedFrom", 
				"INSERT INTO module_conditional_relationship VALUES (?, ?)",
				10000, (in, out, n) -> {
					out.add(genUUIDv5(MiningEnitityNames.MODULE_RELATIONSHIP, in.getString(1)));
					out.add(genUUIDv5(MiningEnitityNames.MODULE, in.getString(2)));
					return false;
				});

		migrateData("ModuleContains",
			"select @rid, out, in, location.offset, location.length from ContainsModule", 
			"INSERT INTO module_relationship VALUES (?, ?, " + MODULE_LOCATION_PARAMETERS + ","
					+ " ?, ?, ?::module_relationship_type, ?, ?, ?)",
			10000, (in, out, n) -> {
				out.add(genUUIDv5(MiningEnitityNames.MODULE_RELATIONSHIP, in.getString(1)));	/* id: UUID from @rid */
				out.add(genUUIDv5(MiningEnitityNames.MODULE, in.getString(2)));					/* src: UUID from out module @rid */
				out.add(Boolean.valueOf(in.getObject(4) != null));								/* src_location exists from location */
				out.add(in.getObject(4));														/* src_location.offset from location.offset */
				out.add(in.getObject(5));														/* src_location.length from location.length */
				out.add(genUUIDv5(MiningEnitityNames.MODULE, in.getString(3)));					/* dst: UUID from in module @rid */
				out.add(null);																	/* dst_location is null */
				out.add(RelationshipType.CONTAINS.name());											/* type */
				out.add(null);																	/* ContainsModule edges have no properties */
				out.add(null);																	/* ContainsModule edges have no dependency_binding */
				out.add(null);																	/* ContainsModule edges have no dependency_attributes */
				return false;
			});
	}

	private void migrateStatements() throws SQLException {
		migrateData("Statements and SqlStatements",
				"SELECT @rid, moduleLink, id, technologyLink.name as tech, statementTypeLink.name as type, text, "
					 + "customComplexity, distinctTables, halsteadComplexity, halsteadDifficulty, sqlLength, tables FROM Statement",
				"INSERT INTO statement (uid, module, nid, technology, type, text, properties) VALUES (?, ?, ?, ?, ?, ?, ?)",
					10000, (in, out, n) -> {
						out.add(genUUIDv5(MiningEnitityNames.STATEMENT, in.getString("@rid")));
						out.add(genUUIDv5(MiningEnitityNames.MODULE, in.getString("moduleLink")));
						out.add(Long.valueOf(in.getLong("id")));
						out.add(in.getString("tech"));
						out.add(in.getString("type"));
						out.add(in.getString("text").replace('\0', ' '));

						final Map<String, Object> properties = createProperties(in, "customComplexity", "distinctTables", "halsteadComplexity",
																					"halsteadDifficulty", "sqlLength", "tables");
						out.add(properties.isEmpty() ? null : PgJSON.toPGobject(properties));
						return false;
					});

		updatePgSequence("statement_nid", "select coalesce(max(nid) + 1, 1) from statement");
	}

	private void migrateErrorMarkers() {
		migrateData("ExcelSheetErrors",
			"SELECT projectId, moduleLink, severity, key, cause, moduleLocation.offset as locationOffset, "
					+ "moduleLocation.length as locationLength, line FROM ExcelSheetErrors",
			"INSERT INTO error_marker VALUES ((SELECT uid FROM project WHERE nid = ?), ?, ?, ?, ?, " + MODULE_LOCATION_PARAMETERS + ", ?)",
				10000, (in, out, n) -> {
					out.add(in.getObject("projectId") == null ? null : Long.valueOf(in.getLong("projectId")));
					final var module = in.getString("moduleLink");
					out.add(module == null ? null : genUUIDv5(MiningEnitityNames.MODULE, module));
					out.add(in.getString("severity"));
					out.add(in.getString("key"));
					out.add(replaceNullBytes(in.getString("cause")));

					migrateLocation(in, out);

					out.add(in.getObject("line") == null ? Integer.valueOf(-1) : Integer.valueOf(in.getInt("line")));
					return false;
				});
	}

	private String replaceNullBytes(final String cause) {
		return cause.replace('\0', ' ');
	}

	private void migrateModelDeadCodes() {
		migrateData("ExcelSheetDeadCode",
				"SELECT moduleLink, startingLine, numberOfLines, deadCode FROM ExcelSheetDeadCode",
						"INSERT INTO module_dead_code VALUES (?, ?, ?, ?)",
						MIGRATION_BATCH_SIZE, (in, out, n) -> {
							out.add(genUUIDv5(MiningEnitityNames.MODULE, in.getString("moduleLink")));
							out.add(Integer.valueOf(in.getInt("startingLine")));
							out.add(Integer.valueOf(in.getInt("numberOfLines")));
							out.add(in.getString("deadCode"));
							return false;
						});
	}

	private void migrateDependencyDefinitions() {
		migrateData("DependencyDefinition",
				"SELECT @rid, attributes, bindingType, id, location.offset as locationOffset, location, location.length as locationLength, moduleFilter, moduleLink,"
					 + "relationshipType, resolutionFlags, resolved, conditionalDependency FROM DependencyDefinition",
				"INSERT INTO dependency_definition (module, id, attributes, binding_type, location, module_filters, type, resolution_flags, resolved, reached_from_modules)"
										 + " VALUES (?, ?, ?, ?, " + MODULE_LOCATION_PARAMETERS + ", ?::jsonb[], ?::module_relationship_type, ?, ?, ?)",
				MIGRATION_BATCH_SIZE, (in, out, n) -> {
					out.add(genUUIDv5(MiningEnitityNames.MODULE, in.getString("moduleLink")));
					out.add(genUUIDv5(MiningEnitityNames.DEPENDENCY_DEFINITION, in.getString("@rid")));
					out.add(PgJSON.toPGobject(in.getObject("attributes")));
					out.add(in.getString("bindingType"));
					migrateLocation(in, out);

					final var moduleFilters = getModuleFilters(in.getString("moduleFilter"));
					out.add(PgJSON.toPGobjects(moduleFilters).toJdbcArray(dbPostgres));
					final var type = in.getString("relationshipType");
					out.add("READS_WRITES".equals(type) ? "ACCESSES" : type);
					@SuppressWarnings("unchecked")
					final var flags = (Set<String>) in.getObject("resolutionFlags");
					out.add(new PgArray(PgType.STRING.toString(), flags.toArray(new String[0])).toJdbcArray(dbPostgres));
					out.add(Boolean.valueOf(in.getBoolean("resolved")));
					final var conditionalDependencies = getConditionalDependencies(in.getString("conditionalDependency"));
					out.add(PgJSON.toPGobjects(conditionalDependencies).toJdbcArray(dbPostgres));

					return false;
				});
	}

	private static List<ModuleFilter> getModuleFilters(final String json) {
		if (json != null && ! json.isBlank()) {
			try {
				return List.of(PgJSON.fromJSON(json, ModuleFilter.class));
			} catch (final Exception e) {
				return PgJSON.fromJSON(json, new TypeReference<List<ModuleFilter>>() {} );
			}
		}

		return Collections.emptyList();
	}

	private List<EntityId> getConditionalDependencies(final String json) {
		if (json != null && ! json.isBlank()) {
			ConditionalDependency cd;
			try {
				cd = PgJSON.fromJSON(json, ConditionalDependency.class);
			} catch (final Exception e) {
				cd = PgJSON.fromJSON(json, new TypeReference<ConditionalDependency>() {} );
			}

			if ( ! cd.ifReachedFromModules.isEmpty()) {
				final boolean[] error = { false };
				final var result = cd.ifReachedFromModules.stream().map(module -> {
					final UUID uid = module.recordId != null ? genUUIDv5(MiningEnitityNames.MODULE, module.recordId) : null;
					final Long nid = module.id != null ? module.id : null;

					if (uid == null && nid == null) {
						error[0] = true;
					}

					return EntityId.of(uid, nid);
				}).collect(Collectors.toList());

				if (error[0]) {
					throw new IllegalStateException("Either the uid or the nid of conditional dependency modules must be present in json: " + json);
				}

				return result;
			}
		}

		return Collections.emptyList();
	}

	private void migrateMiningJobInfo() {
		migrateData("MiningJobInfo",
				"SELECT jobId, moduleLink, projectId FROM MiningJobInfo",
				"INSERT INTO mining_job_info (project, module, job_id) VALUES ((SELECT uid FROM project WHERE nid = ?), ?, ?)",
				MIGRATION_BATCH_SIZE, (in, out, n) -> {
					out.add(in.getObject("projectId") == null ? null : Long.valueOf(in.getLong("projectId")));
					final var moduleRid = in.getString("moduleLink");
					out.add(moduleRid == null ? null : genUUIDv5(MiningEnitityNames.MODULE, moduleRid));
					out.add(UUID.fromString(in.getString("jobId")));
					return false;
				});
	}

	private static Map<String, Object> createProperties(final ResultSet in, final String... columns) throws SQLException {
		final Map<String, Object> properties = new HashMap<>();
		for (final var column : columns) {
			final var val = in.getObject(column);
			if (val != null) {
				properties.put(column, val);
			}
		}

		return properties;
	}

	private static void migrateLocation(final ResultSet in, final List<Object> out) throws SQLException {
		out.add(in.getObject("locationOffset") != null);
		out.add(in.getInt("locationOffset"));
		out.add(in.getInt("locationLength"));
	}
	
	private Set<Long> findMissingProjects() {
		if (! isOrientAvailable()) {
			return Collections.emptySet();
		}

		final String orientQuery = "SELECT distinct projectId FROM Module;";
		final String postgresQuery = "SELECT nid FROM project;";

		final Set<Long> uniqueIdsInOrient = new HashSet<>();
		final Set<Long> uniqueIdsInPostgres = new HashSet<>();

		try (
				final ResultSet rsO = getOrientDB().createStatement().executeQuery(orientQuery);
				final ResultSet rsP = dbPostgres.createStatement().executeQuery(postgresQuery)
				) {
			while (rsO.next()) {
				uniqueIdsInOrient.add(Long.valueOf(rsO.getLong(1)));
			}
			while (rsP.next()) {
				uniqueIdsInPostgres.add(Long.valueOf(rsP.getLong(1)));
			}
		} catch (final SQLException e) {
			throw new IllegalStateException("Something went wrong during the ModuleMigration pre-check. Your db has corrupted entries most likely");
		}

		uniqueIdsInOrient.removeAll(uniqueIdsInPostgres);
		return uniqueIdsInOrient;
	} 

	@SuppressWarnings("serial")
	public static class ConditionalDependency implements Serializable {

		private List<LightWeightModule> ifReachedFromModules = Collections.emptyList();

		public ConditionalDependency() { /* nothing to be done here */ }

		@JsonSetter
		public void setIfReachedFromModules(final List<LightWeightModule> modules) {
			this.ifReachedFromModules = modules;
		}
	}

	@SuppressWarnings("serial")
	public static class LightWeightModule implements Serializable {
		@Nullable
		private final String recordId;
		@Nullable
		private final Long id;
		@JsonCreator
		public LightWeightModule(@JsonProperty("recordId") final String recordId,
				@JsonProperty("id") final Long id,
				@JsonProperty("name") final String name) {
			this.recordId = recordId;
			this.id = id;
			/* ignore name */
		}
	}

	@SuppressWarnings("serial")
	@JsonInclude(Include.NON_NULL)
	public static class ModuleFilter implements Serializable {

		public Set<EntityId> moduleIds = Collections.emptySet();
		public Set<String> names = Collections.emptySet();
		public Set<String> types = Collections.emptySet();
		public Set<String> subtypes = Collections.emptySet();
		public Set<String> paths = Collections.emptySet();
		public Set<String> pathPatterns = Collections.emptySet();
		@Nullable
		public ModuleFilter containedIn;
		@Nullable
		public ModuleFilter not;
		@Nullable
		public Instant metricsDate;
		
		public ModuleFilter() { /* nothing to be done here */ }

		public void setModuleIds(final Collection<EntityId> ids) {
			if (ids instanceof Set) {
				this.moduleIds = (Set<EntityId>) ids;
			} else {
				this.moduleIds = new HashSet<>(ids);
			}
		}

		public void setNames(final Collection<String> names) {
			this.names = new HashSet<>(names);
		}

		public void setTypes(final Collection<String> types) {
			if( ! types.isEmpty()) {
				this.types = new HashSet<>(types);
			}
		}

		public void setSubtypes(final Collection<String> subtypes) {
			if( ! subtypes.isEmpty()) {
				this.subtypes = new HashSet<>(subtypes);
			}
		}

		public void setPaths(final Collection<String> paths) {
			this.paths = new HashSet<>(paths);
		}

		public void setPathPatterns(final Collection<String> patterns) {
			this.pathPatterns = new HashSet<>(patterns);
		}

		public void setContainedIn(final ModuleFilter otherModule) {
			this.containedIn = otherModule;
		}
		
		public void setNot(final ModuleFilter not) {
			this.not = not;
		}
		
		public void setMetricsDate(final Instant metricsDate) {
			this.metricsDate = metricsDate;
		}
	}
}

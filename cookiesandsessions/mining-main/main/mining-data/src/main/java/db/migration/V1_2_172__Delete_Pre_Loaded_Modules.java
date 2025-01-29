/* Copyright (c) 2023 Deloitte. All rights reserved. */
package db.migration;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.flywaydb.core.api.migration.BaseJavaMigration;
import org.flywaydb.core.api.migration.Context;
import org.flywaydb.core.internal.jdbc.JdbcUtils;
import org.springframework.jdbc.UncategorizedSQLException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.PreparedStatementCallback;
import org.springframework.jdbc.core.PreparedStatementCreator;
import org.springframework.jdbc.core.ResultSetExtractor;
import org.springframework.jdbc.datasource.SingleConnectionDataSource;

import com.orientechnologies.orient.core.id.ORecordId;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.Logging;
import innowake.mining.data.StatementBuilder;
import innowake.mining.data.error.ConstraintViolationException;
import innowake.mining.shared.model.Creator;

/**
 * FlyWay migration class for deleting PreLoded Utilities.
 */
public class V1_2_172__Delete_Pre_Loaded_Modules extends BaseJavaMigration {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.MIGRATION);

	private static final String UPDATE_REFERENCES_IN_MODULE_QUERY = "UPDATE %s set in_%s=%s";
	private static final String UPDATE_MODULE_IN_REFERENCE_QUERY = "UPDATE %s set in=%s";
	private static final String REMOVE_REFERENCES_FROM_PRE_LOADED_MODULES_QUERY = "UPDATE Module "
			+ "SET in_None=[], in_Calls=[], in_Includes=[], in_References=[], in_ReadsWrites=[] WHERE projectLink.id = 0";
	private static final String DELETE_PRE_LOADED_MODULES = "DELETE FROM Module where projectLink.id=0 UNSAFE";
	private static final String DELETE_SOURCE_METRICS_OF_MODULE_QUERY = "DELETE FROM ("
		+ "SELECT expand(rid) FROM ("
			+ "SELECT expand($d) LET "
				/* Fetch HasAdditionalInfo edges between Module and SourceMetrics vertices */
				+ "$a = ( SELECT expand(out_HasAdditionalInfo) FROM Module WHERE projectLink.id=0 and out_HasAdditionalInfo.in.@class=\"SourceMetrics\"),"
				/* Select SourceMetrics record ids */
				+ "$b = ( SELECT in as rid FROM $a),"
				/* Select HasAdditionalInfo record ids */
				+ "$c = ( SELECT @rid as rid FROM $a),"
				/* Union SourceMetrics and HasAdditionalInfo record ids */
				+ "$d = unionAll( $b, $c ) "
		+ ")"
	+ ") UNSAFE";

	private static final String SOURCE_METRICS_OF_MODULE = "first(out('HasAdditionalInfo')[@class=\"SourceMetrics\"]).";
	private static final String SOURCE_METRICS_FIELDS = SOURCE_METRICS_OF_MODULE + "commentLines AS linesOfComment, "
													  + SOURCE_METRICS_OF_MODULE + "codeLines AS linesOfCode, "
													  + SOURCE_METRICS_OF_MODULE + "complexityMcCabe AS complexity, "
													  + SOURCE_METRICS_OF_MODULE + "deadCodeLines AS linesOfDeadCode, "
													  + SOURCE_METRICS_OF_MODULE + "@rid AS sourceMetricsRid, "
													  + SOURCE_METRICS_OF_MODULE + "physicalLines AS physicalLines ";

	private static final String COLLECT_MODULES_QUERY = "SELECT @rid, contentHash, creator, description, errors, excelType, identificationLink, "
													  + "in_None, in_None.out.projectLink as in_NoneProjectLink, "
													  + "in_Calls, in_Calls.out.projectLink as in_CallsProjectLink, "
													  + "in_Includes, in_Includes.out.projectLink as in_IncludesProjectLink, "
													  + "in_References, in_References.out.projectLink as in_ReferencesProjectLink, "
													  + "in_ReadsWrites, in_ReadsWrites.out.projectLink as in_ReadsWritesProjectLink, "
													  + "info, linkHash, uid, "
													  + "location.offset, location.length, metricsDate, modifiedDate, name, objectTypeLink, "
													  + "path, representation, requiresReview, sqlStatements, statements, "
													  + SOURCE_METRICS_FIELDS
													  + "FROM Module where projectLink.id = 0 AND "
													  + "(in_Calls.size() > 0 OR in_Includes.size() > 0 OR in_None.size() > 0 OR "
													  + "in_ReadsWrites.size() > 0 OR in_References.size() > 0)";

	@Override
	public void migrate(@Nullable final Context context) throws Exception {
		final var jdbcTemplate = new JdbcTemplate(new SingleConnectionDataSource(assertNotNull(context).getConnection(), true));
		LOG.info("Starting to Migrate Utility Modules in project 0");

		final List<ModulePojo> modules = assertNotNull(jdbcTemplate.query(COLLECT_MODULES_QUERY, new ModuleExtractor()), "Modules list must not be null");
		LOG.info(() -> String.format("Found %d Utility Modules in project 0 that must be migrated", Integer.valueOf(modules.size())));

		/* Mapping of project rid to Maps of preloaded module to cloned module rids */
		final Map<ORecordId, Map<String, String>> projectModuleMapping = new HashMap<>();

		for (final ModulePojo module : modules) {
			migrateReferences(jdbcTemplate, projectModuleMapping, module, "None", module.inNone, module.inNoneProjects);
			migrateReferences(jdbcTemplate, projectModuleMapping, module, "Calls", module.inCalls, module.inCallsProjects);
			migrateReferences(jdbcTemplate, projectModuleMapping, module, "Includes", module.inIncludes, module.inIncludesProjects);
			migrateReferences(jdbcTemplate, projectModuleMapping, module, "References", module.inReferences, module.inReferencesProjects);
			migrateReferences(jdbcTemplate, projectModuleMapping, module, "ReadsWrites", module.inReadsWrites, module.inReadsWritesProjects);
		}

		/* Delete preloaded modules including their SourceMetrics if present  */
		LOG.info("Deleting SourceMetrics of Utility Modules in project 0");
		jdbcTemplate.update(DELETE_SOURCE_METRICS_OF_MODULE_QUERY);
		LOG.info("Removing references from Utility Modules in project 0");
		jdbcTemplate.update(REMOVE_REFERENCES_FROM_PRE_LOADED_MODULES_QUERY);
		LOG.info("Deleting Utility Modules in project 0");
		jdbcTemplate.update(DELETE_PRE_LOADED_MODULES);
	}

	private void migrateReferences(final JdbcTemplate jdbcTemplate, final Map<ORecordId, Map<String, String>> projectModuleMapping,
			final ModulePojo module, final String refType, final List<ORecordId> refRids, final List<ORecordId> refProjectRids) {

		if ( ! refRids.isEmpty()) {
			if (refRids.size() != refProjectRids.size()) {
				throw new IllegalStateException("Number of " + refType + " rids must match number of call project rids ");
			}

			LOG.info(() -> String.format("Found %d %s in module '%s' that must be migrated", Integer.valueOf(refRids.size()), refType, module.name));

			/* Mapping of cloned module rids to reference rids, e.g. Calls rid */
			final Map<String, List<ORecordId>> referenceMap = new HashMap<>();

			/* For every reference rid clone the module if required and collect all reference edges that must now be linked to the cloned modules */
			for (var i = 0; i < refRids.size(); i++) {
				final ORecordId referenceRid = refRids.get(i);
				final ORecordId referenceProjectRid = refProjectRids.get(i);
				/* In the CF DB we have broken Calls edges which we must skip here */
				if ( ! (referenceRid == null || referenceProjectRid == null)) {

					/* get the map (preloaded module <-> cloned module) of the client project: reference.out.projectLink */
					final Map<String, String> moduleMapping = projectModuleMapping.computeIfAbsent(referenceProjectRid, key -> new HashMap<>());

					/* clone the preloaded module if no cloned module exists yet */
					final String clonedModuleRid = moduleMapping.computeIfAbsent(module.rid, rid -> cloneModule(module, referenceProjectRid, jdbcTemplate));

					/* add the reference to the list of references that must be switched from the preloaded to the cloned module */
					referenceMap.computeIfAbsent(clonedModuleRid, key -> new ArrayList<>()).add(referenceRid);
				}
			}

			/* Sets the IN module link in every reference edge from the preloaded to the cloned module and
			 * sets the list of in_${References} into the cloned module */
			for (final Entry<String, List<ORecordId>> entry : referenceMap.entrySet()) {
				final String clonedModuleRid = entry.getKey();
				final List<ORecordId> referenceRids = entry.getValue();
				final StringBuilder strb = updateInModuleLinkInReferences(jdbcTemplate, clonedModuleRid, referenceRids);

				/* set the list of in_${References} into the cloned module */
				jdbcTemplate.update(String.format(UPDATE_REFERENCES_IN_MODULE_QUERY, clonedModuleRid, refType, strb.toString()));
			}
		}
	}

	private static StringBuilder updateInModuleLinkInReferences(final JdbcTemplate jdbcTemplate, final String clonedModuleRid,
			final List<ORecordId> referenceRids) {
		final StringBuilder strb = new StringBuilder(referenceRids.size() * 8).append('[');
		for (final Iterator<ORecordId> it = referenceRids.iterator(); it.hasNext(); ) {
			final ORecordId referenceRid = it.next();
			strb.append(referenceRid);
			if (it.hasNext()) {
				strb.append(',');
			}

			/* Set the IN module link in the reference edge from the preloaded to the cloned module */
			jdbcTemplate.update(String.format(UPDATE_MODULE_IN_REFERENCE_QUERY, referenceRid, clonedModuleRid));
		}

		return strb.append(']');
	}

	private String cloneModule(final ModulePojo module, final ORecordId projectRid, final JdbcTemplate jdbcTemplate) {
		try {
			final var builder = new StatementBuilder();
			if (module.uid == null) {
				builder.addAttribute("id=sequence('Module_Sequence').next(),uid=-1");
			} else {
				builder.addAttribute("id=sequence('Module_Sequence').next(),uid=?", module.uid);
			}
			builder.addAttribute("projectLink=?", projectRid);
			builder.addAttribute("contentHash=?", module.contentHash);
			builder.addAttribute("creator=?", module.creator);
			builder.addAttribute("description=?", module.description);
			builder.addAttribute("errors=?", module.errors);
			builder.addAttribute("excelType=?", module.excelType);
			builder.addAttribute("identificationLink=?", module.identificationLink);
			builder.addAttribute("info=?", module.info);
			builder.addAttribute("linkHash=?", module.linkHash);
			if (module.locationLength != null && module.locationOffset != null) {
				builder.addAttribute(String.format("location={\"offset\":\"%d\", \"length\": \"%d\"}", module.locationOffset, module.locationLength));
			}
			builder.addAttribute("linkHash=?", module.linkHash);
			builder.addAttribute("metricsDate=?", module.metricsDate);
			builder.addAttribute("modifiedDate=?", module.modifiedDate);
			builder.addAttribute("name=?", module.name);
			builder.addAttribute("objectTypeLink=?", module.objectTypeLink);
			builder.addAttribute("path=?", module.path);
			builder.addAttribute("representation=?", module.representation);
			builder.addAttribute("requiresReview=?", Boolean.valueOf(module.requiresReview));
			builder.addAttribute("sqlStatements=?", module.sqlStatements);
			builder.addAttribute("statements=?", module.statements);

			final String moduleRid = insert(jdbcTemplate, builder, "Module");
			if (module.sourceMetricsRid != null) {
				/* Create the cloned SourceMetrics and link it with the cloned Module */
				insertSourceMetrics(module, moduleRid, jdbcTemplate);
			}

			return moduleRid;
		} catch (final UncategorizedSQLException e) {
			LOG.error(String.format("An error occured while creating module %s in project %s ", module.name, projectRid), e);
			throw new ConstraintViolationException(module, ExceptionUtils.getRootCauseMessage(e), e);
		}
	}

	private void insertSourceMetrics(final ModulePojo module, final String moduleRid, final JdbcTemplate jdbcTemplate) {
		final StatementBuilder builder = new StatementBuilder()
				.addAttribute("id=sequence('SourceMetrics_Sequence').next()")
				.addAttribute("physicalLines=?", module.physicalLines)
				.addAttribute("deadCodeLines=?", module.linesOfDeadCode)
				.addAttribute("codeLines=?", module.linesOfCode)
				.addAttribute("commentLines=?", module.linesOfComment)
				.addAttribute("complexityMcCabe=?", module.complexity);

		final String smRid = insert(jdbcTemplate, builder, "SourceMetrics");
		jdbcTemplate.update(String.format("CREATE EDGE HasAdditionalInfo FROM %s TO %s", moduleRid, smRid));
	}

	private static String insert(final JdbcTemplate jdbcTemplate, final StatementBuilder builder, final String entity) {
		final PreparedStatementCreator creator = connection -> {
			final PreparedStatement statement = connection.prepareStatement(String.format( "INSERT INTO %s SET %s RETURN @rid", entity, builder.attributes()));
			builder.init(statement);
			return statement;
		};

		final PreparedStatementCallback<String> callback = statement -> {
			ResultSet resultSet = null;
			try {
				if (statement.execute()) {
					resultSet = statement.getResultSet();
					if (resultSet.getMetaData().getColumnCount() == 0) {
						throw new IllegalStateException("ResultSet column count must not be 0");
					}

					return resultSet.getString("@rid");
				} else {
					throw new IllegalStateException("There must be a ResultSet when inserting a " + entity);
				}
			} finally {
				JdbcUtils.closeResultSet(resultSet);
			}
		};

		return assertNotNull(jdbcTemplate.execute(creator, callback), entity + " rid must not be null");
	}

	private class ModuleExtractor implements ResultSetExtractor<List<ModulePojo>> {

		@SuppressWarnings("unchecked")
		@Override
		@Nullable
		public List<ModulePojo> extractData(final ResultSet resultSet) throws SQLException {
			final List<ModulePojo> modules = new ArrayList<>(256);
			while (resultSet.next()) {
				final var module = new ModulePojo();
				modules.add(module);

				module.rid = resultSet.getString("@rid");
				module.contentHash = resultSet.getString("contentHash");
				module.creator = Creator.DISCOVERY.toString();
				module.description = resultSet.getString("description");
				module.errors = (Integer) resultSet.getObject("errors");
				module.excelType = resultSet.getString("excelType");
				module.identificationLink = resultSet.getString("identificationLink");
				module.inCalls = ListUtils.emptyIfNull((List<ORecordId>) resultSet.getObject("in_Calls"));
				module.inCallsProjects = ListUtils.emptyIfNull((List<ORecordId>) resultSet.getObject("in_CallsProjectLink"));
				module.inIncludes = ListUtils.emptyIfNull((List<ORecordId>) resultSet.getObject("in_Includes"));
				module.inIncludesProjects = ListUtils.emptyIfNull((List<ORecordId>) resultSet.getObject("in_IncludesProjectLink"));
				module.inNone = ListUtils.emptyIfNull((List<ORecordId>) resultSet.getObject("in_None"));
				module.inNoneProjects = ListUtils.emptyIfNull((List<ORecordId>) resultSet.getObject("in_NoneProjectLink"));
				module.inReadsWrites = ListUtils.emptyIfNull((List<ORecordId>) resultSet.getObject("in_ReadsWrites"));
				module.inReadsWritesProjects = ListUtils.emptyIfNull((List<ORecordId>) resultSet.getObject("in_ReadsWritesProjectLink"));
				module.inReferences = ListUtils.emptyIfNull((List<ORecordId>) resultSet.getObject("in_References"));
				module.inReferencesProjects = ListUtils.emptyIfNull((List<ORecordId>) resultSet.getObject("in_ReferencesProjectLink"));
				module.info = (Map<String, String>) resultSet.getObject("info");
				module.linkHash = resultSet.getString("linkHash");
				module.locationOffset = (Integer) resultSet.getObject("location.offset");
				module.locationLength = (Integer) resultSet.getObject("location.length");
				module.metricsDate = resultSet.getTimestamp("metricsDate");
				module.modifiedDate = resultSet.getTimestamp("modifiedDate");
				module.name = resultSet.getString("name");
				module.objectTypeLink = resultSet.getString("objectTypeLink");
				module.path = resultSet.getString("path");
				module.representation = resultSet.getString("representation");
				module.requiresReview = resultSet.getBoolean("requiresReview");
				module.sqlStatements = (Integer) resultSet.getObject("sqlStatements");
				module.statements = (Integer) resultSet.getObject("statements");
				module.uid = (Long) resultSet.getObject("uid");
				module.linesOfComment = (Integer) resultSet.getObject("linesOfComment");
				module.linesOfCode = (Integer) resultSet.getObject("linesOfCode");
				module.complexity = (Integer) resultSet.getObject("complexity");
				module.linesOfDeadCode = (Integer) resultSet.getObject("linesOfDeadCode");
				module.physicalLines = (Integer) resultSet.getObject("physicalLines");
				module.sourceMetricsRid = resultSet.getString("sourceMetricsRid");
			}

			return modules;
		}
	}

	private static class ModulePojo {

		@Nullable
		private String rid;
		@Nullable
		private String contentHash;
		private String creator = Creator.DISCOVERY.toString();
		@Nullable
		private String description;
		@Nullable
		private Integer errors;
		@Nullable
		private String excelType;
		@Nullable
		private String identificationLink;
		private List<ORecordId> inCalls = Collections.emptyList();
		private List<ORecordId> inCallsProjects = Collections.emptyList();
		private List<ORecordId> inIncludes = Collections.emptyList();
		private List<ORecordId> inIncludesProjects = Collections.emptyList();
		private List<ORecordId> inNone = Collections.emptyList();
		private List<ORecordId> inNoneProjects = Collections.emptyList();
		private List<ORecordId> inReadsWrites = Collections.emptyList();
		private List<ORecordId> inReadsWritesProjects = Collections.emptyList();
		private List<ORecordId> inReferences = Collections.emptyList();
		private List<ORecordId> inReferencesProjects = Collections.emptyList();
		@Nullable
		private Map<String, String> info;
		@Nullable
		private String linkHash;
		@Nullable
		private Integer locationOffset;
		@Nullable
		private Integer locationLength;
		@Nullable
		private Date metricsDate;
		@Nullable
		private Date modifiedDate;
		@Nullable
		private String name;
		@Nullable
		private String objectTypeLink;
		@Nullable
		private String path;
		@Nullable
		private String representation;
		private boolean requiresReview;
		@Nullable
		private Integer sqlStatements;
		@Nullable
		private Integer statements;
		@Nullable
		private Long uid;
		@Nullable
		private Integer linesOfComment;
		@Nullable
		private Integer linesOfCode;
		@Nullable
		private Integer complexity;
		@Nullable
		private Integer linesOfDeadCode;
		@Nullable
		private Integer physicalLines;
		@Nullable
		private String sourceMetricsRid;
	}

}

/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.access.postgres;

import static innowake.mining.shared.access.FilterUtil.toBoolean;
import static innowake.mining.shared.access.FilterUtil.toEntityId;
import static innowake.mining.shared.access.FilterUtil.toEntityIds;
import static innowake.mining.shared.access.FilterUtil.toNumber;
import static innowake.mining.shared.access.FilterUtil.toStrings;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_EQ;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_GT;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_GTE;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_IN;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_IS_FALSE;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_IS_TRUE;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_LT;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_LTE;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_NOT_EQ;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_NOT_IN;

import java.nio.charset.StandardCharsets;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.persistence.PersistenceException;

import innowake.mining.shared.model.AstNodeLocation;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.jdbc.core.JdbcTemplate;

import com.google.common.collect.Sets;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.error.ConstraintViolationException;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.BinaryValue;
import innowake.mining.shared.access.Comperator;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ModuleService.DeadCodeInquiryBuilder;
import innowake.mining.shared.access.ModuleService.DependencyDefinitionInquiryBuilder;
import innowake.mining.shared.access.ModuleService.LinkedModuleInquiryBuilder;
import innowake.mining.shared.access.ModuleService.MetricField;
import innowake.mining.shared.access.ModuleService.ModuleAggregationInquiryBuilder;
import innowake.mining.shared.access.ModuleService.ModuleInquiryBuilder;
import innowake.mining.shared.access.ModuleService.ModuleLightInquiryBuilder;
import innowake.mining.shared.access.ModuleService.ModuleRelationshipInquiryBuilder;
import innowake.mining.shared.access.ModuleService.ModuleUndiscoveredInquiryBuilder;
import innowake.mining.shared.access.ModuleService.SourceMetricsInquiryBuilder;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Paged.Builder;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.access.Table;
import innowake.mining.shared.access.Table.FieldConverter;
import innowake.mining.shared.entities.DependencyDefinitionPojo;
import innowake.mining.shared.entities.DependencyDefinitionPojoPrototype;
import innowake.mining.shared.entities.ModuleDeadCodePojo;
import innowake.mining.shared.entities.ModuleDeadCodePojoPrototype;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipBasePojo;
import innowake.mining.shared.entities.ModuleUndiscoveredPojo;
import innowake.mining.shared.entities.ModuleUndiscoveredPojoPrototype;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SchemaInfoPojo;
import innowake.mining.shared.entities.SourceMetricsPojo;
import innowake.mining.shared.entities.SourceMetricsPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.lang.ConditionalConsumer;
import innowake.mining.shared.lang.EnumMapBuilder;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.HotSpot;
import innowake.mining.shared.model.HotSpot.FilterType;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.LinkedModule;
import innowake.mining.shared.model.ModuleFieldName;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;

/**
 * Postgres specific access methods for the {@code module} entity.
 */
public class ModulePgDao extends PgDao {

	private static final int INSERT_BATCH_SIZE = 1_000;
	
	/** Logic to reconstruct the legacy "uid" field used in Discovery, see {@link innowake.mining.data.io.DiscoveryUidUtils} */
	protected static final String LEGACY_UID_COLUMN = "CASE WHEN m.origin = '" + Origin.ENVIRONMENT.name() 
													+ "' THEN 0 WHEN NOT m.identified THEN -1 ELSE m.nid END";

	private static final Map<MetricField, String> metricsFields = EnumMapBuilder.of(MetricField.class).<String>create()
			.put(MetricField.PHYSICAL_LINES, "physical_lines")
			.put(MetricField.CODE_LINES, "code_lines")
			.put(MetricField.COMMENT_LINES, "comment_lines")
			.put(MetricField.COMPLEXITY_MCCABE, "complexity_mc_cabe")
			.put(MetricField.DEAD_CODE_LINES, "dead_code_lines")
		.build();

	public ModulePgDao(final JdbcTemplate jdbcTemplate) {
		super(jdbcTemplate);
	}

	protected static Consumer<PgDao.QueryBuilder> referenceUidOrNid(@Nullable final EntityId moduleId) {
		return q -> q.appendId(moduleId, "?", "(SELECT uid FROM module WHERE nid = ?)");
	}

	protected static Consumer<PgDao.QueryBuilder> appendLocation(@Nullable final ModuleLocation location) {
		return location == null ? q -> q.append("null") : q -> q.append("(?,?)::module_location", location.getOffset(), location.getLength());
	}

	protected static Consumer<PgDao.QueryBuilder> appendAstNodeLocation(@Nullable final AstNodeLocation location) {
		return location == null ? q -> q.append("null") : q -> q.append("(?,?,?,?,?,?,?,?)::ast_node_location",
				location.getRetracedOffset().orElse(null),
				location.getRetracedLength().orElse(null),
				location.getAssembledOffset().orElse(null),
				location.getAssembledLength().orElse(null),
				location.getRootRelativeOffset().orElse(null),
				location.getRootRelativeLength().orElse(null),
				location.getRootRelativeStartLineNumber().orElse(null),
				location.getRootRelativeEndLineNumber().orElse(null));
	}

	public class ModuleQueryBuilder extends ModuleAggregationQueryBuilder 
			implements ModuleService.ModuleInquiryBuilder, CustomPropertiesPgDao.CustomPropertiesInquiryBuilderStub {
		
		protected boolean joinModuleRelationship;
		protected boolean joinConditionalRelationships;

		protected final DynamicSelection dynamic = new DynamicSelection();
		protected final FilterStreamBuilder dynamicFilters = new FilterStreamBuilder();
		private boolean dynamicSort;

		private boolean includeContent;
		private int limit = -1;

		@Override
		protected void buildJoins(final QueryBuilder qb) {
			super.buildJoins(qb);

			qb.when(joinModuleRelationship, q -> q.append(" INNER JOIN module_relationship mr ON mr.src = module.uid OR mr.dst = module.uid "));
			qb.when(joinConditionalRelationships, q -> q.append(" INNER JOIN module_conditional_relationship cr on module.uid = cr.reached_from_module "));
		}

		@Override
		protected boolean hasJoins() {
			return super.hasJoins() || joinModuleRelationship || joinConditionalRelationships;
		}
		
		protected QueryBuilder delete() {
			if (hasJoins()) {
				return query("DELETE FROM module WHERE uid IN (SELECT uid FROM module")
						.with(this::buildJoins)
						.with(filters::build)
						.when(limit > 0, q -> q.limit(limit))
						.append(")");
			}

			return query("DELETE FROM module")
					.with(filters::build)
					.when(limit > 0, q -> q.limit(limit));
		}

		protected Paged.Builder<UUID> buildUid(@Nullable final Pagination paging) {
			return query("SELECT uid FROM module")
					.with(this::buildJoins)
					.with(filters::build)
					.with(orderBys::build)
					.when(limit > 0, q -> q.limit(limit))
					.toPageable(paging, (rs, row) -> (UUID) rs.getObject(1));
		}

		protected Paged.Builder<EntityId> buildId(@Nullable final Pagination paging) {
			if (dynamicFilters.isEmpty() && ! dynamicSort) {
				return query("SELECT module.uid, module.nid")
						.when(rbBlocksSortDirection != null, q -> q.append(", array_agg(fb.name ORDER BY fb.name) FILTER (WHERE fb.name IS NOT NULL) as rbName"))
						.append(" FROM module ")
						.with(this::buildJoins)
						.with(filters::build)
						.when(rbBlocksSortDirection != null, q -> q.append(" GROUP BY module.uid"))
						.with(orderBys::build)
						.when(limit > 0, q -> q.limit(limit))
						.toPageable(paging, (rs, row) -> EntityId.of((UUID) rs.getObject(1), Long.valueOf(rs.getLong(2))));
			}

			return query("SELECT * FROM ( SELECT module.uid, module.nid, ")
					.with(dynamic::apply)
					.when(rbBlocksSortDirection != null, q -> q.append(", array_agg(fb.name ORDER BY fb.name) FILTER (WHERE fb.name IS NOT NULL) as rbName"))
					.append(" FROM module")
					.with(this::buildJoins)
					.with(filters::build)
					.when(rbBlocksSortDirection != null, q -> q.append(" GROUP BY module.uid"))
					.with(orderBys::build)
					.append(") _moduleIds")
					.with(dynamicFilters::build)
					.when(limit > 0, q -> q.limit(limit))
					.toPageable(paging, (rs, row) -> EntityId.of((UUID) rs.getObject(1), Long.valueOf(rs.getLong(2))));
		}

		protected Paged.Builder<Pair<EntityId, String>> buildIdAndLinkHash(@Nullable final Pagination paging) {
			return query("SELECT module.uid, module.nid, module.link_hash FROM module")
					.with(this::buildJoins)
					.with(filters::build)
					.with(orderBys::build)
					.when(limit > 0, q -> q.limit(limit))
					.toPageable(paging, (rs, row) ->
							Pair.of(EntityId.of((UUID) rs.getObject(1), Long.valueOf(rs.getLong(2))), rs.getString(3)));
		}
		
		protected Paged.Builder<Pair<UUID, String>> buildUidAndDependencyHash(@Nullable final Pagination paging) {
			return query("SELECT module.uid, dependency_hash FROM module")
					.with(this::buildJoins)
					.with(filters::build)
					.with(orderBys::build)
					.when(limit > 0, q -> q.limit(limit))
					.toPageable(paging, (rs, row) ->
							Pair.of((UUID) rs.getObject(1), rs.getString(2)));
		}

		protected Paged.Builder<String> buildNames(@Nullable final Pagination paging) {
			return query("SELECT module.name FROM module")
					.with(this::buildJoins)
					.with(filters::build)
					.with(orderBys::build)
					.when(limit > 0, q -> q.limit(limit))
					.toPageable(paging, (rs, row) -> rs.getString(1));
		}

		@SuppressWarnings("unchecked")
		protected Paged.Builder<ModulePojo> build(@Nullable final Pagination paging) {
			joinContainingModule = true;
			joinSourceMetrics = true;
			final var resultModuleId = new ResultReference<EntityId>();
			final var resultMetrics = new ResultReference<SourceMetricsPojo>();
			final var resultContent = new ResultReference<String>();
			final var resultRelationships = new ResultReference<List<ModuleRelationshipBasePojo>>();
			return query("SELECT * FROM (SELECT ")
				.appendColumns(resultModuleId, n -> rs -> EntityId.of((UUID) rs.getObject(n), rs.getLong(n + 1)),
					"module.uid" /* 1 */,
					"module.nid" /* 2 */)
				.with(q -> q.appendColumns(
					"(SELECT to_jsonb(project_ids) FROM (SELECT uid, nid FROM project WHERE uid = module.project) project_ids) as projectIds", /* 3 */
					"module.name", /* 4 */
					"module.path", /* 5 */
					"module.technology", /* 6 */
					"module.type", /* 7 */
					"module.storage", /* 8 */
					"module.origin", /* 9 */
					"module.creator", /* 10 */
					"module.identified", /* 11 */
					"module.info", /* 12 */
					"module.description", /* 13 */
					"module.content_hash", /* 14 */
					"module.link_hash", /* 15 */
					"to_jsonb(module.location)", /* 16 */
					"module.representation", /* 17 */
					"module.requires_review", /* 18 */
					"module.modified_date", /* 19 */
					"module.metrics_date", /* 20 */
					"module.custom_properties", /* 21 */
					"module.source as sourceUid", /* 22 */
					/* Containing Module */
					"p.uid as containingUid", /* 23 */
					"p.nid as containingNid", /* 24 */
					"p.path as containingPath", /* 25 */
					/* sourceCodeAvailable boolean */
					"CASE WHEN module.source IS NOT NULL OR p.source IS NOT NULL THEN true ELSE false END sourceCodeAvailable", /* 26 */
					/* ErrorMarker count */
					"(SELECT count(*) FROM error_marker em WHERE em.module=module.uid) as errors", /* 27 */
					/* Statement count (non SQL) */
					"(SELECT count(*) FROM statement s1 WHERE s1.module=module.uid AND s1.technology != 'SQL') as statements", /* 28 */
					/* SQL Statement count */
					"(SELECT count(*) FROM statement s2 WHERE s2.module=module.uid AND s2.technology = 'SQL') as sqlStatements", /* 29 */
					"module.dependency_hash" /* 30 */
				))
				.when(rbBlocksSortDirection != null, q -> q.appendColumns(" array_agg(fb.name ORDER BY fb.name) FILTER (WHERE fb.name IS NOT NULL) as rbName"))
				.appendColumns(resultMetrics, n -> rs -> rs.getBoolean(n) ? new SourceMetricsPojo(
							resultModuleId.get(rs), null, null,	/* module */
							(Integer) rs.getObject(n + 1),	/* physical_lines */
							(Integer) rs.getObject(n + 2),	/* code_lines */
							(Integer) rs.getObject(n + 3),	/* comment_lines */
							(Integer) rs.getObject(n + 4),	/* complexity_mc_cabe */
							(Integer) rs.getObject(n + 5)	/* dead_code_lines */
						) : null,
					"sm.module IS NOT NULL as sm_present", /* +0 */
					"sm.physical_lines", /* +1 */
					"sm.code_lines", /* +2 */
					"sm.comment_lines", /* +3 */
					"sm.complexity_mc_cabe", /* +4 */
					"sm.dead_code_lines" /* +5 */)
				.when(includeContent, q -> q.appendColumns(resultContent,
					n -> rs -> mapNullable(rs.getBytes(n), bytes -> new String(bytes, StandardCharsets.UTF_8)),
					"(SELECT content FROM source WHERE id = module.source)"))
				.when(joinModuleRelationship, q -> q.appendColumns(resultRelationships,
					n -> rs -> unpivotRange(rs, n, 7).map(o -> new ModuleRelationshipBasePojo(
							(UUID) o[0], (UUID) o[1], PgJSON.fromPGobjectOrNull(o[2], ModuleLocation.class),
							(UUID) o[3], PgJSON.fromPGobjectOrNull(o[4], ModuleLocation.class),
							RelationshipType.valueOf((String) o[5]), PgJSON.fromPGobject(o[6])
						)).collect(Collectors.toList()),
					"array_agg(mr.id)", "array_agg(mr.src)", "array_agg(to_jsonb(mr.src_location))",
					"array_agg(mr.dst)", "array_agg(to_jsonb(mr.dst_location))",
					"array_agg(mr.type)", "array_agg(mr.properties)"))
				.with(dynamic::apply)
				.append(" FROM module")
				.with(this::buildJoins)
				.with(filters::build)
				.append(" GROUP BY module.uid, sm.module, p.uid) module")
				.with(dynamicFilters::build)
				.when(limit > 0, q -> q.limit(limit))
				/* order bys in orders can be module.name, module.path, etc. */
				.toPageable(paging, orderBys, (rs, row) -> {
					final var containingUid = (UUID) rs.getObject(23);
					final var m = new ModulePojo(
							(UUID) rs.getObject(1), 
							(Long) rs.getObject(2), 
							new CustomPropertiesMap(PgJSON.fromPGobject(rs.getObject(21))),
							PgJSON.fromPGobject(rs.getObject(3), EntityId.class), null, null,						/* project */
							rs.getString(4),																		/* name */
							rs.getString(5),																		/* path */
							map(rs.getString(6), Technology::valueOf),
							map(rs.getString(7), Type::valueOf),
							map(rs.getString(8), Storage::valueOf),
							map(rs.getString(9), Origin::valueOf),
							map(rs.getString(10), Creator::valueOf),
							rs.getBoolean(11) ? Identification.IDENTIFIED : Identification.MISSING,
							PgJSON.fromPGobjectOrNull(rs.getObject(12), Map.class),									/* info */
							rs.getString(13),																		/* description */
							(UUID) rs.getObject(22),																/* source */
							mapNullable(rs.getBytes(14), BinaryValue::new),											/* content_hash */
							rs.getString(15),																		/* link_hash */
							PgJSON.fromPGobjectOrNull(rs.getObject(16), ModuleLocation.class),
							mapNullable(rs.getString(17), Representation::valueOf),
							rs.getBoolean(18),																		/* requires_review */
							mapNullable(rs.getTimestamp(19), Timestamp::toInstant),									/* modified_date */
							mapNullable(rs.getTimestamp(20), Timestamp::toInstant), 								/* metrics_date */
							resultMetrics.get(rs),
							resultContent.get(rs),
							rs.getInt(27),																			/* errors count */
							rs.getInt(28),																			/* statements count */
							rs.getInt(29),																			/* SQL statements count */
							rs.getBoolean(26), 																		/* sourceCodeAvailable */
							containingUid == null ? null : EntityId.of(containingUid, (Long) rs.getObject(24)), null, null,	/* containingNid */
							rs.getString(25),
							rs.getString(30));																		/* dependency hash */
					resultRelationships.ifPresent(rs, m::dynamicRelations);
					m.defineDynamics(dynamic.extract(rs));
					return m;
				});
		}

		protected List<HotSpot> buildHotspots(final FilterType filterType) {
			final boolean filterZero;
			final String hotSpotFields;
			switch (filterType) {
				case CALLS:
					filterZero = false;
					hotSpotFields = "CASE WHEN module.source IS NOT NULL OR p.source IS NOT NULL THEN true ELSE false END AS sourceCodeAvailable, " /* 26 */
							+ "(SELECT count(*) from ast_node an WHERE an.module = module.uid AND an.type='CobolFileDefinition') AS hotspots "; /* 27 */
					break;
				case REFERENCES:
					filterZero = false;
					hotSpotFields = "CASE WHEN module.source IS NOT NULL OR p.source IS NOT NULL THEN true ELSE false END AS sourceCodeAvailable, " /* 26 */
								  + "(SELECT count(*) from module_relationship WHERE dst = module.uid AND type IN ('ACCESSES','CALLS','INCLUDES','REFERENCES')) AS hotspots "; /* 27 */
					break;
				case CANDIDATE_RULE:
					filterZero = true;
					hotSpotFields = "CASE WHEN module.source IS NOT NULL OR p.source IS NOT NULL THEN true ELSE false END AS sourceCodeAvailable, " /* 26 */
								  + "(SELECT count(*) from annotation WHERE module = module.uid AND state = 'CANDIDATE' AND type = 'RULE' AND " /* 27 */
								  + "category = (SELECT id FROM annotation_category WHERE name = 'Business Rule' AND project = (SELECT uid FROM project WHERE nid = 0))) AS hotspots ";
					break;
				case DATA_SETS:
					filterZero = true;
					hotSpotFields = "CASE WHEN module.source IS NOT NULL OR p.source IS NOT NULL THEN true ELSE false END AS sourceCodeAvailable, " /* 26 */
								  + "(SELECT count(*) from module_relationship WHERE dst = module.uid AND type = 'ACCESSES') AS hotspots "; /* 27 */
					break;
				case DATABASE_TABLES:
					filterZero = false;
					hotSpotFields = "false as sourceCodeAvailable,"																			/* 26 */
								  + "(SELECT count(*) from module_relationship WHERE dst = module.uid AND type = 'ACCESSES') AS hotspots "; /* 27 */
					break;
				default:
					throw new UnsupportedOperationException("FilterType '" + filterType + "' is not supported.");
			}

			joinContainingModule = true;

			return query("SELECT * FROM ("
						+ "SELECT module.uid, " /* 1 */
							   + "module.nid, " /* 2 */
							   + "(SELECT to_jsonb(project_ids) FROM (SELECT uid, nid FROM project WHERE uid = module.project) project_ids) AS projectIds," /* 3 */
							   + "module.name, " /* 4 */
							   + "module.path, " /* 5 */
							   + "module.technology, " /* 6 */
							   + "module.type, " /* 7 */
							   + "module.storage, " /* 8 */
							   + "module.origin, " /* 9 */
							   + "module.creator, " /* 10 */
							   + "module.identified, " /* 11 */
							   + "module.info, " /* 12 */
							   + "module.description, " /* 13 */
							   + "module.content_hash, " /* 14 */
							   + "module.link_hash," /* 15 */
							   + "to_jsonb(module.location), " /* 16 */
							   + "module.representation, " /* 17 */
							   + "module.requires_review, " /* 18 */
							   + "module.modified_date, " /* 19 */
							   + "module.metrics_date, " /* 20 */
							   + "module.custom_properties, " /* 21 */
							   + "module.source as sourceUid," /* 22 */
							   /* Containing module */
							   + "p.uid as containingUid, " /* 23 */
							   + "p.nid as containingNid, " /* 24 */
							   + "p.path as containingPath, ") /* 25 */
					.append(hotSpotFields) /* 26, 27 */
					.append("FROM module ")
					.with(this::buildJoins)
					.with(filters::build)
					.append(") data")
					.when(filterZero, q -> q.append(" WHERE hotspots > 0"))
					.append(" ORDER BY hotspots DESC")
					.when(limit > 0, q -> q.limit(limit))
					.toList((rs, row) -> {
						final var containingUid = (UUID) rs.getObject(23);			/* containingUid */
						@SuppressWarnings("unchecked")
						final var module = new ModulePojo(
								(UUID) rs.getObject(1),
								(Long) rs.getObject(2),
								new CustomPropertiesMap(PgJSON.fromPGobject(rs.getObject(21))),
								PgJSON.fromPGobject(rs.getObject(3), EntityId.class), null, null,			/* project */
								rs.getString(4),															/* name */
								rs.getString(5),															/* path */
								map(rs.getString(6), Technology::valueOf),
								map(rs.getString(7), Type::valueOf),
								map(rs.getString(8), Storage::valueOf),
								map(rs.getString(9), Origin::valueOf),
								map(rs.getString(10), Creator::valueOf),
								rs.getBoolean(11) ? Identification.IDENTIFIED : Identification.MISSING,
								PgJSON.fromPGobjectOrNull(rs.getObject(12), Map.class),						/* info */
								rs.getString(13),															/* description */
								(UUID) rs.getObject(22),													/* source */
								mapNullable(rs.getBytes(14), BinaryValue::new),								/* content_hash */
								rs.getString(15),															/* link_hash */
								PgJSON.fromPGobjectOrNull(rs.getObject(16), ModuleLocation.class),
								mapNullable(rs.getString(17), Representation::valueOf),
								rs.getBoolean(18),															/* requires_review */
								mapNullable(rs.getTimestamp(19), Timestamp::toInstant),						/* modified_date */
								mapNullable(rs.getTimestamp(20), Timestamp::toInstant),						/* metrics_date */
								null,
								null,
								-1,
								-1,
								-1,
								rs.getBoolean(26),																		/* sourceCodeAvailable */
								containingUid == null ? null : EntityId.of(containingUid, (Long) rs.getObject(24)), null, null,	/* containingNid */
								rs.getString(25),
								null);																		/* containingPath */
						return new HotSpot(module, rs.getInt(27));														/* hotspot count */
					});
		}

		protected QueryBuilder buildComplexities() {
			return query("SELECT module.technology, module.type, sm.complexity_mc_cabe FROM module LEFT JOIN source_metrics sm ON sm.module = module.uid")
					.with(filters::build);
		}

		protected QueryBuilder buildLightweight() {
			joinContainingModule = true;

			return query("SELECT module.uid, module.nid, project.uid prj_uid, project.nid prj_nid, module.name, module.path, module.technology, module.type,"
					+ " module.identified, module.link_hash, module.representation, p.uid as containingUid, p.nid as containingNid, p.path as containingPath"
					+ " FROM module INNER JOIN project ON project.uid = module.project")
				.with(this::buildJoins)
				.with(filters::build)
				.with(orderBys::build)
				.when(limit > 0, q -> q.limit(limit));
		}

		protected QueryBuilder buildCount() {
			if (dynamic.isEmpty()) {
				return query("SELECT count(*) FROM module")
						.with(filters::build);
			}

			return query("SELECT count(*) FROM (SELECT ")
					.with(dynamic::apply)
					.append(" FROM module")
					.with(filters::build)
					.append(") modules ")
					.with(dynamicFilters::build);
		}

		@Override
		public ModuleQueryBuilder ofProject(final EntityId project) {
			filters.accept(q -> q.append("module.project = ").with(ProjectPgDao.referenceUidOrNid(project)));
			return this;
		}

		@Override
		public ModuleQueryBuilder byIds(final Collection<EntityId> ids) {
			final var uids = EntityId.allUids(ids);
			if (uids.size() == ids.size()) {
				return byUids(uids);
			}

			final var nids = EntityId.allNids(ids);
			if (nids.size() == ids.size()) {
				return byNids(nids);
			}

			filters.accept(q -> q.append("(module.uid = any(?) OR module.nid = any(?))", 
					arrayFromCollection(PgType.UUID, uids), arrayFromCollection(PgType.LONG, nids)));

			return this;
		}

		@Override
		public ModuleQueryBuilder byUids(final Collection<UUID> uids) {
			filters.accept(q -> q.append("module.uid = any(?)", arrayFromCollection(PgType.UUID, uids)));
			return this;
		}

		@Override
		public ModuleQueryBuilder byNids(final Collection<Long> nids) {
			filters.accept(q -> q.append("module.nid = any(?)", arrayFromCollection(PgType.LONG, nids)));
			return this;
		}

		@Override
		public ModuleQueryBuilder byUid(final UUID uid) {
			filters.accept(q -> q.append("module.uid = ?", uid));
			return this;
		}

		@Override
		public ModuleQueryBuilder byNid(final Long nid) {
			filters.accept(q -> q.append("module.nid = ?", nid));
			return this;
		}

		@Override
		public ModuleQueryBuilder notByIds(final Collection<EntityId> ids) {
			final var nids = EntityId.allNids(ids);
			if ( ! nids.isEmpty()) {
				filters.accept(q -> q.append("not(module.nid = any(?))", arrayFromCollection(PgType.LONG, nids)));
			}

			final var uids = EntityId.allUids(ids);
			if ( ! uids.isEmpty()) {
				filters.accept(q -> q.append("not(module.uid = any(?))", arrayFromCollection(PgType.UUID, uids)));
			}

			return this;
		}

		@Override
		public ModuleQueryBuilder byId(final EntityId id) {
			id.perform(this::byUid, this::byNid);
			return this;
		}

		@Override
		public ModuleQueryBuilder ofSources(final Collection<UUID> sourceIds) {
			filters.accept(q -> q.append("module.source = any(?)", arrayFromCollection(PgType.UUID, sourceIds)));
			return this;
		}

		@Override
		public ModuleQueryBuilder filterHasSource(final boolean hasSource) {
			if (hasSource) {
				filters.accept(q -> q.append("module.source IS NOT Null"));
			} else {
				filters.accept(q -> q.append("module.source IS Null"));
			}
			return this;
		}

		@Override
		public ModuleQueryBuilder withLinkHash(final String linkHash) {
			filters.accept(q -> q.append("module.link_hash = ?", linkHash));
			return this;
		}

		@Override
		public ModuleQueryBuilder withLinkHashes(final Collection<String> linkHashes) {
			filters.accept(q -> q.append("module.link_hash = any(?)", arrayFromCollection(PgType.STRING, linkHashes)));
			return this;
		}

		@Override
		public ModuleQueryBuilder withTechnology(final Technology technology) {
			filters.accept(q -> q.append("module.technology = ?", technology.name()));
			return this;
		}
		
		@Override
		public ModuleQueryBuilder withTechnologies(final Collection<Technology> technologies) {
			final List<String> technologyNames = technologies.stream().map(Technology::name).collect(Collectors.toList());
			filters.accept(q -> q.append("module.technology = any(?)", arrayFromCollection(PgType.STRING, technologyNames)));
			return this;
		}

		@Override
		public ModuleQueryBuilder withType(final Type type) {
			filters.accept(q -> q.append("module.type = ?", type.name()));
			return this;
		}
		
		@Override
		public ModuleQueryBuilder withTechnologiesAndTypes(final Collection<Tuple2<Technology, Type>> technologiesAndTypes) {
			switch(technologiesAndTypes.size()) {
				case 0:
					/* empty, nothing to filter */
					break;
				case 1:
					final var tuple = technologiesAndTypes.iterator().next();
					filters.accept(q -> q.append("module.technology = ? AND module.type = ?", tuple.a.name(), tuple.b.name()));
					break;
				default:
					final var it = technologiesAndTypes.iterator();
					filters.accept(q -> {
						while (it.hasNext()) {
							final var t = it.next();
							if (it.hasNext()) {
								q.append("module.technology = ? AND module.type = ? OR ", t.a.name(), t.b.name());
							} else {
								q.append("module.technology = ? AND module.type = ?", t.a.name(), t.b.name());
							}
						}
					});
					break;
			}

			return this;
		}

		@Override
		public ModuleQueryBuilder notWithTechnologiesAndTypes(final Collection<Tuple2<Technology, Type>> technologiesAndTypes) {
			switch(technologiesAndTypes.size()) {
				case 0:
					/* empty, nothing to filter */
					break;
				case 1:
					final var tuple = technologiesAndTypes.iterator().next();
					filters.accept(q -> q.append("NOT (module.technology = ? AND module.type = ?)", tuple.a.name(), tuple.b.name()));
					break;
				default:
					final var it = technologiesAndTypes.iterator();
					filters.accept(q -> {
						q.append("NOT (");
						while (it.hasNext()) {
							final var t = it.next();
							if (it.hasNext()) {
								q.append("module.technology = ? AND module.type = ? OR ", t.a.name(), t.b.name());
							} else {
								q.append("module.technology = ? AND module.type = ?", t.a.name(), t.b.name());
							}
						}
						q.append(")");
					});
					break;
			}

			return this;
		}

		@Override
		public ModuleQueryBuilder withTypes(final Collection<Type> types) {
			filters.accept(q -> q.append("module.type = any(?)", arrayFromCollection(PgType.STRING, types.stream().map(Type::name).collect(Collectors.toList()))));
			return this;
		}

		@Override
		public ModuleQueryBuilder withSourceRelationships(final RelationshipType... types) {
			filters.accept(q -> q.append("module.uid IN (SELECT src FROM module_relationship WHERE ")
								 .with(referenceRelationshipTypes(types))
								 .append(")"));
			return this;
		}

		private Consumer<PgDao.QueryBuilder> referenceRelationshipTypes(final RelationshipType... types) {
			joinModuleRelationship = true;
			if (types.length == 1) {
				return q -> q.append("mr.type = ?::module_relationship_type", types[0].name());
			} else {
				return q -> q.append("mr.type = any(?::module_relationship_type[])",
									 new PgArray(PgType.STRING.toString(), Arrays.stream(types).map(RelationshipType::name).toArray(Object[]::new)));
			}
		}
		
		@Override
		public ModuleQueryBuilder withSourceRelationshipsFrom(final EntityId moduleId, final RelationshipType... types) {
			joinModuleRelationship = true;
			filters.accept(q -> {
				q.append("module.uid = mr.dst AND mr.src = ")
						.with(referenceUidOrNid(moduleId));
				if (types.length != 0) {
					q.append(" AND ")
					 .with(referenceRelationshipTypes(types));
				}
			});
			return this;
		}

		@Override
		public ModuleQueryBuilder withTransitiveSourceRelationshipsFrom(final EntityId moduleId, final RelationshipType... types) {
			filters.accept(q -> {
				final RelationshipType[] typeArr = types.length != 0 ? types : RelationshipType.DEPENDENCY_TYPES_ARR;
				q.append("module.uid IN ("
							+ "WITH RECURSIVE recursive_references AS ( "
							+ "SELECT mr.dst FROM module_relationship mr "
							+ "WHERE mr.src = ")
							.with(referenceUidOrNid(moduleId))
						.append(" AND ")
							.with(referenceRelationshipTypes(typeArr))
						.append(" UNION "
							+ "SELECT mr.dst FROM module_relationship mr "
							+ "JOIN recursive_references ri ON mr.src = ri.dst "
							+ "WHERE ")
							.with(referenceRelationshipTypes(typeArr))
						.append(") SELECT * FROM recursive_references)");
			});
			return this;
		}

		@Override
		public ModuleQueryBuilder withSourceRelationshipsFrom(final BuildingConsumer<ModuleLightInquiryBuilder<?>> other, final RelationshipType... types) {
			joinModuleRelationship = true;
			filters.accept(q -> {
				q.append("module.uid = mr.dst AND mr.src IN (SELECT uid FROM module");
				other.prepare(new ModuleQueryBuilder()).filters.build(q);
				if (types.length != 0) {
					q.append(") AND ")
					 .with(referenceRelationshipTypes(types));
				} else {
					q.append(")");
				}
			});
			return this;
		}

		@Override
		public ModuleQueryBuilder withDestinationRelationships(final RelationshipType... types) {
			joinModuleRelationship = true;
			filters.accept(q -> q.append("module.uid = mr.dst AND ")
								 .with(referenceRelationshipTypes(types)));
			return this;
		}
		
		@Override
		public ModuleQueryBuilder withDestinationRelationshipsTo(final EntityId moduleId, final RelationshipType... types) {
			joinModuleRelationship = true;
			filters.accept(q -> {
				q.append("module.uid = mr.src AND mr.dst = ")
						 .with(referenceUidOrNid(moduleId));
				if (types.length != 0) {
					q.append(" AND ")
					 .with(referenceRelationshipTypes(types));
				}
			});
			return this;
		}
		
		@Override
		public ModuleQueryBuilder withDestinationRelationshipsTo(final BuildingConsumer<ModuleLightInquiryBuilder<?>> other, final RelationshipType... types) {
			joinModuleRelationship = true;
			filters.accept(q -> {
				q.append("module.uid = mr.src AND mr.dst IN (SELECT uid FROM module");
				other.prepare(new ModuleQueryBuilder()).filters.build(q);
				if (types.length != 0) {
					q.append(") AND ")
					 .with(referenceRelationshipTypes(types));
				} else {
					q.append(")");
				}
			});
			return this;
		}
		
		@Override
		public ModuleQueryBuilder withRelationshipProperty(final String property, final String value, final boolean contain) {
			joinModuleRelationship = true;
			filters.accept(q -> q.append("mr.properties ->> ? ", property).append(contain ? "ILIKE" : "=")
					.append(" ?", contain ? "%" + value + "%" : value));
			return this;
		}
		
		@Override
		public ModuleQueryBuilder withRelationshipPropertyAny(final String property, final Collection<String> values) {
			joinModuleRelationship = true;
			filters.accept(q -> q.append("mr.properties ->> ? = any(?)", property).addArg(PgType.STRING, values));
			return this;
		}

		@Override
		public ModuleInquiryBuilder withRelationshipProperties(final Collection<String> properties, final boolean present) {
			joinModuleRelationship = true;
			filters.accept(q -> q.when(! present, qq -> qq.append("mr.properties IS null OR NOT ")).append("mr.properties ?? "
					+ (present ? "all" : "any") + "(?)").addArg(PgType.STRING, properties));
			return this;
		}
		
		@Override
		public ModuleQueryBuilder withRelationship(final EntityId module,
				final RelationshipDirection direction, @Nullable final Collection<RelationshipType> types) {
			joinModuleRelationship = true;
			filters.accept(q -> {
				q.append("(");
				if (direction == RelationshipDirection.IN || direction == RelationshipDirection.BOTH) {
					q.append("mr.dst = ").with(referenceUidOrNid(module));
				}
				if (direction == RelationshipDirection.BOTH) {
					q.append(" OR ");
				}
				if (direction == RelationshipDirection.OUT || direction == RelationshipDirection.BOTH) {
					q.append("mr.src = ").with(referenceUidOrNid(module));
				}
				q.append(")");
				if (types != null) {
					q.append(" AND mr.type = any(?::module_relationship_type[])").addArg(PgType.STRING, types);
				}
				q.append(" AND NOT module.uid = ").with(referenceUidOrNid(module));
			});
			return this;
		}

		@Override
		public ModuleQueryBuilder notWithSourceRelationships(final Collection<RelationshipType> types) {
			filters.accept(q -> q.append("module.uid NOT IN (SELECT DISTINCT(src) FROM module_relationship WHERE type = any(?::module_relationship_type[]))",
					arrayFromCollection(PgType.STRING, types)));
			return this;
		}

		@Override
		public ModuleQueryBuilder notWithDestinationRelationships(final Collection<RelationshipType> types) {
			filters.accept(q -> q.append("module.uid NOT IN (SELECT DISTINCT(dst) FROM module_relationship WHERE type = any(?::module_relationship_type[]))",
					arrayFromCollection(PgType.STRING, types)));
			return this;
		}

		@Override
		public ModuleQueryBuilder notWithSourceOrDestinationRelationships(final Collection<RelationshipType> types) {
			filters.accept(q -> {
				final PgArray typeArray = arrayFromCollection(PgType.STRING, types);
				q.append("module.uid NOT IN (SELECT DISTINCT(src) FROM module_relationship WHERE type = any(?::module_relationship_type[])) OR "
						+ "module.uid NOT IN (SELECT DISTINCT(dst) FROM module_relationship WHERE type = any(?::module_relationship_type[]))",
						typeArray, typeArray);
			});
			return this;
		}

		@Override
		public ModuleQueryBuilder withPath(final String path) {
			filters.accept(q -> q.append("module.path ILIKE ?", path));
			return this;
		}

		@Override
		public ModuleQueryBuilder withPathsSelfOrContaining(final EntityId project, final Collection<String> paths) {
			final var pgPaths = arrayFromCollection(PgType.STRING, paths);
			filters.accept(q -> q.append("module.path = any(?) OR module.uid IN ("
																		+ "SELECT dst FROM module_relationship mrs "
																		+ "INNER JOIN module pm ON mrs.src = pm.uid "
																		+ "WHERE mrs.type = 'CONTAINS' AND mrs.dst = module.uid AND pm.project = ", pgPaths)
																			.with(ProjectPgDao.referenceUidOrNid(project))
																			.append(" AND pm.path = any(?))", pgPaths));
			return this;
		}

		@Override
		public ModuleQueryBuilder withPathPatternsSelfOrContaining(final EntityId project, final Collection<String> patterns) {
			final var pgPaths = arrayFromCollection(PgType.STRING, patterns);
			filters.accept(q -> q.append("module.path ~ any(?) OR module.uid IN ("
																		+ "SELECT dst FROM module_relationship mrs "
																		+ "INNER JOIN module pm ON mrs.src = pm.uid "
																		+ "WHERE mrs.type = 'CONTAINS' AND mrs.dst = module.uid AND pm.project = ", pgPaths)
																			.with(ProjectPgDao.referenceUidOrNid(project))
																			.append(" AND pm.path ~ any(?))", pgPaths));
			return this;
		}

		@Override
		public ModuleQueryBuilder withName(final String name) {
			filters.accept(q -> q.append("module.name = ?", name));
			return this;
		}

		@Override
		public ModuleQueryBuilder withName(final String name, final boolean caseInsensitive) {
			if (caseInsensitive) {
				filters.accept(q -> q.append("module.name ilike ?", name));
				return this;
			}

			return withName(name);
		}

		@Override
		public ModuleQueryBuilder withNames(final Collection<String> names) {
			return withNames(names, false);
		}

		@Override
		public ModuleQueryBuilder withNames(final Collection<String> names, final boolean caseInsensitive) {
			if (caseInsensitive) {
				filters.accept(q -> q.append("module.name ilike any(?)", arrayFromCollection(PgType.STRING, names)));
			} else {
				filters.accept(q -> q.append("module.name = any(?)", arrayFromCollection(PgType.STRING, names)));
			}

			return this;
		}

		@Override
		public ModuleQueryBuilder notWithNameLike(final String name) {
			filters.accept(q -> q.append("module.name NOT LIKE ?", name));
			return this;
		}

		@Override
		public ModuleQueryBuilder withDescription(final String description) {
			filters.accept(q -> q.append("module.description = ?", description));
			return this;
		}

		@Override
		public ModuleQueryBuilder withIdentified(final boolean identified) {
			filters.accept(q -> q.append("module.identified = ?", Boolean.valueOf(identified)));
			return this;
		}

		@Override
		public ModuleQueryBuilder withRepresentation(final Representation representation) {
			filters.accept(q -> q.append("module.representation = ?", representation.toString()));
			return this;
		}

		@Override
		public ModuleQueryBuilder withCreator(final Creator creator) {
			filters.accept(q -> q.append("module.creator = ?", creator.name()));
			return this;
		}

		@Override
		public ModuleQueryBuilder withCreators(final Collection<Creator> creator) {
			filters.accept(q -> q.append("module.creator = any(?)").addArg(PgType.STRING, creator));
			return this;
		}

		@Override
		public ModuleQueryBuilder withOrigin(final Origin origin) {
			filters.accept(q -> q.append("module.origin = ?", origin.name()));
			return this;
		}

		@Override
		public ModuleQueryBuilder filterHasPathNoSource() {
			filters.accept(q -> q.append("module.path IS NOT Null AND module.source is Null"));
			return this;
		}

		@Override
		public ModuleQueryBuilder withSourceMatch() {
			filters.accept(q -> q.append("EXISTS (SELECT true FROM source_info WHERE name = module.name AND technology = module.technology AND type = module.type)"));
			return this;
		}

		@Override
		public ModuleQueryBuilder withConditionalRelationship(final UUID reference) {
			joinConditionalRelationships = true;
			filters.accept(q -> q.append("cr.module_relationship = ?", reference));
			return this;
		}

		@Override
		public ModuleQueryBuilder withStorage(final Storage storage) {
			filters.accept(q -> q.append("module.storage = ?", storage.name()));
			return this;
		}

		@Override
		public ModuleQueryBuilder withRequiresReview(final boolean requiresReview) {
			filters.accept(q -> q.append("module.requires_review = ?", requiresReview));
			return this;
		}

		@Override
		public ModuleQueryBuilder withMetricsDate(final Instant metricsDate) {
			filters.accept(q -> q.append("module.metrics_date = ?::timestamp_zoned_milli", Timestamp.from(metricsDate)));
			return this;
		}

		@Override
		public ModuleQueryBuilder withMetricsDateAfter(final Instant metricsDate) {
			filters.accept(q -> q.append("module.metrics_date > ?::timestamp_zoned_milli", Timestamp.from(metricsDate)));
			return this;
		}

		@Override
		public ModuleQueryBuilder withErrors() {
			filters.accept(q -> q.append("(SELECT count(*) FROM error_marker e where e.module = module.uid) >= 1"));
			return this;
		}

		@Override
		public ModuleQueryBuilder includeContent(final boolean include) {
			includeContent = include;
			return this;
		}

		@Override
		public ModuleQueryBuilder sortMetricsDate(final SortDirection sortDirection) {
			orderBys.accept(q -> q.appendOrder("metrics_date", sortDirection));
			return this;
		}

		@Override
		public ModuleQueryBuilder sortId(final SortDirection sortDirection) {
			orderBys.accept(q -> q.appendOrder("nid", sortDirection));
			return this;
		}

		@Override
		public ModuleQueryBuilder sortName(final SortDirection sortDirection) {
			orderBys.accept(q -> q.appendOrder("name COLLATE ci", sortDirection));
			return this;
		}

		@Override
		public ModuleQueryBuilder sortPath(final SortDirection sortDirection) {
			orderBys.accept(q -> q.appendOrder("path", sortDirection));
			return this;
		}

		@Override
		public ModuleQueryBuilder sortTechnology(final SortDirection sortDirection) {
			orderBys.accept(q -> q.appendOrder("technology", sortDirection));
			return this;
		}

		@Override
		public ModuleQueryBuilder sortCreator(final SortDirection sortDirection) {
			orderBys.accept(q -> q.appendOrder("creator", sortDirection));
			return this;
		}

		@Override
		public ModuleQueryBuilder sortType(final SortDirection sortDirection) {
			orderBys.accept(q -> q.appendOrder("type", sortDirection));
			return this;
		}

		@Override
		public ModuleInquiryBuilder sortRequiresReview(final SortDirection sortDirection) {
			orderBys.accept(q -> q.appendOrder("requires_review", sortDirection));
			return this;
		}

		@Override
		public ModuleInquiryBuilder sortModified(final SortDirection sortDirection) {
			orderBys.accept(q -> q.appendOrder("modified_date", sortDirection));
			return this;
		}

		@Override
		public ModuleInquiryBuilder sortErrorCount(final SortDirection sortDirection) {
			orderBys.accept(q -> q.appendOrder("(SELECT count(*) FROM error_marker e where e.module = uid)", sortDirection));
			return this;
		}

		@Override
		public ModuleInquiryBuilder sortMetric(final MetricField field, final SortDirection sortDirection) {
			joinSourceMetrics = true;
			orderBys.accept(q -> q.appendOrder(metricsFields.get(field), sortDirection));
			return this;
		}
		
		@Override
		public ModuleInquiryBuilder sortCustomProperties(final SortDirection sortDirection) {
			orderBys.accept(q -> q.appendOrder("custom_properties", sortDirection));
			return this;
		}


		@Override
		public ModuleQueryBuilder withAnnotation(final EntityId annotation) {
			filters.accept(q -> q.append("module.uid IN (SELECT module FROM annotation WHERE ").appendId(annotation).append(")"));
			return this;
		}

		@Override
		public ModuleQueryBuilder limit(final int limit) {
			if (limit < 1) {
				throw new IllegalArgumentException("Query limit must be greater than 0 but is: " + limit);
			}
			
			this.limit = limit;
			return this;
		}
		
		@Override
		public ModuleQueryBuilder includeDependencyCount(final String dynamicAlias,
				@Nullable final RelationshipDirection direction, @Nullable final RelationshipType type, final boolean distinctModuleRelationships) {
			dynamic.putColumn(dynamicAlias, q -> {
					if (distinctModuleRelationships) {
						q.append("SELECT count(distinct(src, dst, type)) FROM module_relationship WHERE ");
					} else {
						q.append("SELECT count(*) FROM module_relationship WHERE ");
					}
					if (direction == null || direction == RelationshipDirection.BOTH) {
						q.append("(src = module.uid OR dst = module.uid)");
					} else {
						q.append(direction == RelationshipDirection.OUT ? "src" : "dst");
						q.append(" = module.uid");
					}
					if (type != null) {
						q.append(" AND type = ?::module_relationship_type", type.name());
					} else {
						q.append(" AND type = any(?::module_relationship_type[])").addArg(PgType.STRING, RelationshipType.DEPENDENCY_TYPES);
					}
				}, (rs, n) -> rs.getLong(n));
			return this;
		}
		
		@Override
		public ModuleQueryBuilder includeAnnotationCount(final String dynamicAlias,
				@Nullable final AnnotationType type, @Nullable final WorkingState state, @Nullable final String categoryName) {
			dynamic.putColumn(dynamicAlias, q -> {
					q.append("SELECT count(*) FROM annotation WHERE module = module.uid");
					if (type != null) {
						q.append(" AND type = ?", type.name());
					}
					if (state != null) {
						q.append(" AND state = ?::working_state", state.name());
					}
					if (categoryName != null) {
						q.append(" AND category = (SELECT id FROM annotation_category"
								+ " WHERE (project = module.project OR project = (SELECT uid FROM project WHERE nid = 0)) AND name = ?)", categoryName);
					}
				}, (rs, n) -> rs.getLong(n));
			return this;
		}
		
		private ModuleQueryBuilder includeTaxonomies(final String dynamicAlias, @Nullable final UUID typeId, final boolean count) {
			dynamic.putColumn(dynamicAlias,
					q -> q.append("SELECT ").append(count ? "count" : "array_agg").append("(name) FROM taxonomy a"
							+ " INNER JOIN module_taxonomies b ON b.taxonomy = a.uid"
							+ " WHERE b.module = module.uid")
						.when(typeId, (qq, t) -> qq.append(" AND a.type = ?", typeId)),
					(rs, n) -> count ? rs.getLong(n) : streamArray(rs.getArray(n)).collect(Collectors.toList()));
			return this;
		}

		@Override
		public ModuleInquiryBuilder withTaxonomies(final Collection<EntityId> taxonomyIds) {
			final Collection<UUID> uids = EntityId.allUids(taxonomyIds);
			if (uids.size() == taxonomyIds.size()) {
				filters.accept(q -> q.append("module.uid IN (SELECT module FROM module_taxonomies WHERE taxonomy = any(?))",
											 arrayFromCollection(PgType.UUID, uids)));
			} else {
				filters.accept(q -> q.append("module.uid IN (SELECT module FROM module_taxonomies mt "
										   + "JOIN taxonomy t ON mt.taxonomy = t.uid WHERE ")
									 .appendIds(taxonomyIds,  "t.uid = any(?)", "t.nid = any(?)").append(")"));
			}
			return this;
		}

		@Override
		public ModuleInquiryBuilder notWithTaxonomies(final Collection<EntityId> taxonomyIds) {
			final Set<UUID> uids = EntityId.allUids(taxonomyIds);
			if (uids.size() == taxonomyIds.size()) {
				filters.accept(q -> q.append("module.uid NOT IN (SELECT module FROM module_taxonomies WHERE taxonomy = any(?))",
						arrayFromCollection(PgType.UUID, uids)));
			} else {
				filters.accept(q -> q.append("module.uid NOT IN (SELECT module FROM module_taxonomies mt "
								+ "JOIN taxonomy t ON mt.taxonomy = t.uid WHERE ")
						.appendIds(taxonomyIds,  "t.uid = any(?)", "t.nid = any(?)").append(")"));
			}
			return this;
		}

		@Override
		public ModuleQueryBuilder includeTaxonomies(final String dynamicAlias, @Nullable final UUID typeId) {
			return includeTaxonomies(dynamicAlias, typeId, false);
		}
		
		@Override
		public ModuleQueryBuilder includeTaxonomyCount(final String dynamicAlias, @Nullable final UUID typeId) {
			return includeTaxonomies(dynamicAlias, typeId, true);
		}
		
		@Override
		public ModuleQueryBuilder sortBy(final String dynamicAlias, final SortDirection sortDirection) {
			dynamicSort = true;
			orderBys.accept(q -> q.appendOrder(dynamic.getReference(dynamicAlias), sortDirection));
			return this;
		}
		
		@Override
		public ModuleQueryBuilder filterBy(final String dynamicAlias, final Comperator comperator, final Object value) {
			dynamicFilters.accept(q -> q.append(dynamic.getReference(dynamicAlias)).append(comperator.operator()).append("?", value));
			return this;
		}
		
		@Override
		public ModuleInquiryBuilder filterArray(final String dynamicAlias, final Collection<String> values) {
			dynamicFilters.accept(q -> {
				final String fieldRef = dynamic.getReference(dynamicAlias);
				if (values.contains(null)) {
					q.append(fieldRef).append(" IS null OR ");
				}
				q.append(fieldRef).append(" && ?").addArg(PgType.STRING, values);
			});
			return this;
		}
		
		@Override
		public ModuleInquiryBuilder filterArrayLength(final String dynamicAlias, final Comperator comperator, final Long value) {
			dynamicFilters.accept(q -> q.append("array_length(").append(dynamic.getReference(dynamicAlias)).append(", 1)")
					.append(comperator.operator()).append("?", value));
			return this;
		}
		
		@Override
		public ModuleInquiryBuilder filterNull(final String dynamicAlias, final boolean isNull) {
			dynamicFilters.accept(q -> q.append(dynamic.getReference(dynamicAlias)).append(" IS").when(! isNull, qq -> qq.append(" NOT")).append(" NULL"));
			return this;
		}
		
		@Override
		public void appendCustomPropertiesField(final QueryBuilder q) {
			q.append("module.custom_properties");
		}
		
		@Override
		public void acceptCustomPropetiesFilter(final ConditionalConsumer<QueryBuilder> clause) {
			filters.accept(clause);
		}

		@Override
		public ModuleInquiryBuilder sortByReachabilityBlockName(final SortDirection sortDirection) {
			rbBlocksSortDirection = sortDirection;
			orderBys.accept(q -> q.appendOrder("rbName", sortDirection));
			return this;
		}

		@Override
		public ModuleInquiryBuilder withReachabilityBlockName(final String reachabilityBlockName) {
			filters.accept(q -> q.append("module.uid IN ("
				+ " SELECT rm.module from functional_block fb"
				+ " LEFT JOIN functional_block_resolved_module_part rm ON fb.uid = rm.functional_block"
				+ " WHERE fb.flags->'TYPE' ?? 'REACHABILITY' AND fb.name ILIKE ?)", PgUtil.pgPattern(reachabilityBlockName)));
			return this;
		}
	}

	public class ModuleAggregationQueryBuilder extends AbstractAggregationQueryBuilder<ModuleFieldName, ModuleAggregationQueryBuilder> 
					implements ModuleAggregationInquiryBuilder<ModuleAggregationQueryBuilder> {

		protected boolean joinModuleRelationshipDst;
		protected boolean joinSourceMetrics;
		protected boolean joinContainingModule;

		@Nullable
		protected SortDirection rbBlocksSortDirection = null;

		@Override
		protected String getFromClause() {
			return "module";
		}

		@Override
		protected void buildJoins(final QueryBuilder qb) {
			qb.when(joinContainingModule, q -> q.append(" LEFT JOIN module_relationship cont ON module.uid = cont.dst and cont.type = 'CONTAINS'"
													  + " LEFT JOIN module p ON p.uid = cont.src"));
			qb.when(joinSourceMetrics, q -> q.append(" LEFT JOIN source_metrics sm ON sm.module = module.uid"));
			qb.when(joinModuleRelationshipDst, q -> q.append(" INNER JOIN module_relationship rdst ON rdst.dst = module.uid"));
			qb.when(rbBlocksSortDirection != null, q -> q.append(" LEFT JOIN functional_block_resolved_module_part rm ON rm.module = module.uid"
														+ " LEFT JOIN functional_block fb ON (rm.functional_block = fb.uid"
														+ " AND fb.flags->'TYPE' ?? 'REACHABILITY')"));
		}
		
		protected boolean hasJoins() {
			return joinModuleRelationshipDst || joinSourceMetrics || joinContainingModule;
		}

		/**
		 * Creates a {@code QueryBuilder} for querying utility module invocations.
		 *
		 * @return the {@code QueryBuilder}
		 */
		protected QueryBuilder buildRelationshipInvocations() {
			joinModuleRelationshipDst = true;
			filters.accept(q -> q.append("rdst.type = any(?::module_relationship_type[])").addArg(PgType.STRING, RelationshipType.DEPENDENCY_TYPES));
			return query("SELECT module.uid, module.nid, module.name, rdst.properties FROM module")
					.with(this::buildJoins)
					.with(filters::build);
		}

		@Override
		protected String getFieldQueryFragment(final ModuleFieldName field) {
			switch (field) {
				case COMPLEXITY:
				case LINES_OF_CODE:
				case LINES_OF_COMMENT:
				case LINES_OF_DEAD_CODE:
				case PHYSICAL_LINES_OF_CODE:
					joinSourceMetrics = true;
					break;
				case CONTAINING_MODULE_ID:
				case CONTAINING_MODULE_NAME:
					joinContainingModule = true;
					break;
				default:
					/* nothing to join */
					break;
			}

			switch (field) {
				case COMPLEXITY:
					return "sm.complexity_mc_cabe AS " + field.name().toLowerCase();
				case LINES_OF_CODE:
					return "sm.code_lines AS " + field.name().toLowerCase();
				case LINES_OF_COMMENT:
					return "sm.comment_lines AS " + field.name().toLowerCase();
				case LINES_OF_DEAD_CODE:
					return "sm.dead_code_lines AS " + field.name().toLowerCase();
				case PHYSICAL_LINES_OF_CODE:
					return "sm.physical_lines AS " + field.name().toLowerCase();
				case CONTAINING_MODULE_ID:
					return "p.nid AS " + field.name().toLowerCase();
				case CONTAINING_MODULE_NAME:
					return "p.name AS " + field.name().toLowerCase();
				case ERRORS:
					return "(SELECT COUNT(*) from error_marker WHERE module = module.uid) AS " + field.name().toLowerCase();
				case SQL_STATEMENTS:
					return "(SELECT COUNT(*) from statement stm WHERE module = module.uid AND stm.technology = 'SQL') AS " + field.name().toLowerCase();
				case ID:
					return "module.nid AS " + field.name().toLowerCase();
				case PROJECT_ID:
					return "module.project AS " + field.name().toLowerCase();
				case STORAGE:
					return "module.storage AS " + field.name().toLowerCase();
				case TECHNOLOGY:
					return "module.technology AS " + field.name().toLowerCase();
				case TYPE:
					return "module.type AS " + field.name().toLowerCase();
				case UID:
					return "module.uid AS " + field.name().toLowerCase();
				case CREATOR:
					return "module.creator AS " + field.name().toLowerCase();
				case REQUIRES_REVIEW:
					return "module.requires_review AS " + field.name().toLowerCase();
				case IDENTIFICATION:
					return "CASE WHEN module.identified THEN '" + Identification.IDENTIFIED + "' ELSE '" + Identification.MISSING + "' END AS " + field.name().toLowerCase();
				case NAME:
					return "module.name AS " + field.name().toLowerCase();
				case REPRESENTATION:
					return "module.representation AS " + field.name().toLowerCase();
				case TAXONOMY_ID:
					return "mtx.taxonomy AS " + field.name().toLowerCase();
				default:
					throw new UnsupportedOperationException("The field is not supported yet: " + field.name());
			}
		}

		@Override
		public ModuleAggregationQueryBuilder byId(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					var entity = toEntityId(value, "Module id");
					filters.accept(q -> q.append("module.").appendId(entity));
					break;
				case OPERATOR_NOT_EQ:
					entity = toEntityId(value, "Module id");
					filters.accept(q -> q.append("NOT module.").appendId(entity));
					break;
				case OPERATOR_IN:
					var values = toEntityIds(value, "Module ids");
					filters.accept(q -> q.appendIds(values, "module.uid = any(?)", "module.nid = any(?)"));
					break;
				case OPERATOR_NOT_IN:
					values = toEntityIds(value, "Module ids");
					filters.accept(q -> q.append("NOT (")
										 .appendIds(values, "module.uid = any(?)", "module.nid = any(?)")
										 .append(")"));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for TECHNOLOGY", operator));
			}
			return this;
		}

		@Override
		public ModuleAggregationQueryBuilder withName(final String operator, final Object value) {
			if (OPERATOR_EQ.equals(operator)) {
				filters.accept(q -> q.append("module.name = ?", value.toString()));
			} else {
				throw new UnsupportedOperationException(String.format("The operator %s is not supported for NAME", operator));
			}
			return this;
		}

		@Override
		public ModuleAggregationQueryBuilder ofProject(final String operator, final Object value) {
			if (OPERATOR_EQ.equals(operator)) {
				final EntityId entity = toEntityId(value, "Project id");
				filters.accept(q -> q.append("module.project = ").with(ProjectPgDao.referenceUidOrNid(entity)));
			} else {
				throw new UnsupportedOperationException(String.format("The operator %s is not supported for PROJECT", operator));
			}
			return this;
		}

		@Override
		public ModuleAggregationQueryBuilder withTechnology(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("module.technology = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("NOT module.technology = ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("module.technology = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT module.technology = any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for TECHNOLOGY", operator));
			}
			return this;
		}

		@Override
		public ModuleAggregationQueryBuilder withType(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("module.type = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("NOT module.type = ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("module.type = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT module.type = any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for TYPE", operator));
			}
			return this;
		}

		@Override
		public ModuleAggregationQueryBuilder withRequiresReview(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					var bool = toBoolean(value);
					filters.accept(q -> q.append("module.type = ?", bool));
					break;
				case OPERATOR_NOT_EQ:
					bool = ! toBoolean(value);
					filters.accept(q -> q.append("module.type = ?", bool));
					break;
				case OPERATOR_IS_TRUE:
					filters.accept(q -> q.append("module.requires_review = true"));
					break;
				case OPERATOR_IS_FALSE:
					filters.accept(q -> q.append("module.requires_review = false"));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for REQUIRES_REVIEW", operator));
			}
			return this;
		}

		@Override
		public ModuleAggregationQueryBuilder withStorage(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("module.storage = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("NOT module.storage = ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("module.storage = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT module.storage = any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for STORAGE", operator));
			}
			return this;
		}

		@Override
		public ModuleAggregationQueryBuilder withCreator(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("module.creator = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("NOT module.creator = ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("module.creator = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT module.creator = any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for CREATOR", operator));
			}
			return this;
		}

		@Override
		public ModuleAggregationQueryBuilder withIdentification(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					var bool = Identification.IDENTIFIED == value || Identification.IDENTIFIED.name().equals(value) || toBoolean(value);
					filters.accept(q -> q.append("module.identified = ?", bool));
					break;
				case OPERATOR_NOT_EQ:
					bool = ! (Identification.IDENTIFIED == value || Identification.IDENTIFIED.name().equals(value) || toBoolean(value));
					filters.accept(q -> q.append("module.identified = ?", bool));
					break;
				case OPERATOR_IS_TRUE:
					filters.accept(q -> q.append("module.identified = true"));
					break;
				case OPERATOR_IS_FALSE:
					filters.accept(q -> q.append("module.identified = false"));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for IDENTIFICATION", operator));
			}
			return this;
		}

		@Override
		public ModuleAggregationQueryBuilder withPhysicalLines(final String operator, final Object value) {
			final var num = toNumber(value, "Physical lines");
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("sm.physical_lines = ?", num));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("sm.physical_lines != ?", num));
					break;
				case OPERATOR_GTE:
					filters.accept(q -> q.append("sm.physical_lines >= ?", num));
					break;
				case OPERATOR_LTE:
					filters.accept(q -> q.append("sm.physical_lines <= ?", num));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for PHYSICAL_LINES_OF_CODE", operator));
			}
			joinSourceMetrics = true;
			return this;
		}

		@Override
		public ModuleAggregationQueryBuilder withCodeLines(final String operator, final Object value) {
			final var num = toNumber(value, "Lines of code");
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("sm.code_lines = ?", num));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("sm.code_lines != ?", num));
					break;
				case OPERATOR_GTE:
					filters.accept(q -> q.append("sm.code_lines >= ?", num));
					break;
				case OPERATOR_LTE:
					filters.accept(q -> q.append("sm.code_lines <= ?", num));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for LINES_OF_CODE", operator));
			}
			joinSourceMetrics = true;
			return this;
		}

		@Override
		public ModuleAggregationQueryBuilder withCommentLines(final String operator, final Object value) {
			final var num = toNumber(value, "Lines of comment");
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("sm.comment_lines = ?", num));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("sm.comment_lines != ?", num));
					break;
				case OPERATOR_GTE:
					filters.accept(q -> q.append("sm.comment_lines >= ?", num));
					break;
				case OPERATOR_LTE:
					filters.accept(q -> q.append("sm.comment_lines <= ?", num));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for LINES_OF_COMMENT", operator));
			}
			joinSourceMetrics = true;
			return this;
		}

		@Override
		public ModuleAggregationQueryBuilder withDeadCodeLines(final String operator, final Object value) {
			final var num = toNumber(value, "Lines of dead code");
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("sm.dead_code_lines = ?", num));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("sm.dead_code_lines != ?", num));
					break;
				case OPERATOR_GTE:
					filters.accept(q -> q.append("sm.dead_code_lines >= ?", num));
					break;
				case OPERATOR_LTE:
					filters.accept(q -> q.append("sm.dead_code_lines <= ?", num));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for LINES_OF_DEAD_CODE", operator));
			}
			joinSourceMetrics = true;
			return this;
		}

		@Override
		public ModuleAggregationQueryBuilder withComplexity(final String operator, final Object value) {
			final var num = toNumber(value, "Complexity mc cabe");
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("sm.complexity_mc_cabe = ?", num));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("sm.complexity_mc_cabe != ?", num));
					break;
				case OPERATOR_GTE:
					filters.accept(q -> q.append("sm.complexity_mc_cabe >= ?", num));
					break;
				case OPERATOR_GT:
					filters.accept(q -> q.append("sm.complexity_mc_cabe > ?", num));
					break;
				case OPERATOR_LTE:
					filters.accept(q -> q.append("sm.complexity_mc_cabe <= ?", num));
					break;
				case OPERATOR_LT:
					filters.accept(q -> q.append("sm.complexity_mc_cabe < ?", num));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for COMPLEXITY", operator));
			}
			joinSourceMetrics = true;
			return this;
		}

		@Override
		public ModuleAggregationQueryBuilder withErrors(final String operator, final Object value) {
			final var num = toNumber(value, "Error count");
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("(SELECT count(*) FROM error_marker e where e.module = module.uid) = ?", num));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("(SELECT count(*) FROM error_marker e where e.module = module.uid) != ?", num));
					break;
				case OPERATOR_GTE:
					filters.accept(q -> q.append("(SELECT count(*) FROM error_marker e where e.module = module.uid) >= ?", num));
					break;
				case OPERATOR_LTE:
					filters.accept(q -> q.append("(SELECT count(*) FROM error_marker e where e.module = module.uid) <= ?", num));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for ERRORS", operator));
			}
			return this;
		}

		@Override
		public ModuleAggregationQueryBuilder withSqlStatements(final String operator, final Object value) {
			final var num = toNumber(value, "SQL statement count");
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("(SELECT COUNT(*) from statement stm WHERE module = module.uid AND stm.technology = 'SQL') = ?", num));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("(SELECT COUNT(*) from statement stm WHERE module = module.uid AND stm.technology = 'SQL') != ?", num));
					break;
				case OPERATOR_GTE:
					filters.accept(q -> q.append("(SELECT COUNT(*) from statement stm WHERE module = module.uid AND stm.technology = 'SQL') >= ?", num));
					break;
				case OPERATOR_LTE:
					filters.accept(q -> q.append("(SELECT COUNT(*) from statement stm WHERE module = module.uid AND stm.technology = 'SQL') <= ?", num));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for SQL_STATEMENTS", operator));
			}
			return this;
		}

		@Override
		public ModuleAggregationQueryBuilder withContainingModuleId(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					var entity = toEntityId(value, "Containing id");
					filters.accept(q -> q.append("p.").appendId(entity));
					break;
				case OPERATOR_NOT_EQ:
					entity = toEntityId(value, "Containing id");
					filters.accept(q -> q.append("NOT p.").appendId(entity));
					break;
				case OPERATOR_IN:
					var values = toEntityIds(value, "Containing ids");
					filters.accept(q -> q.appendIds(values, "p.uid = any(?)", "p.nid = any(?)"));
					break;
				case OPERATOR_NOT_IN:
					values = toEntityIds(value, "Containing ids");
					filters.accept(q -> q.append("NOT (")
							 .appendIds(values, "p.uid = any(?)", "p.nid = any(?)")
							 .append(")"));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for CONTAINING_MODULE_ID", operator));
			}
			joinContainingModule = true;
			return this;
		}

		@Override
		public ModuleAggregationQueryBuilder withContainingModuleName(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("p.name = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("NOT p.name = ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("p.name = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT p.name = any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for CONTAINING_MODULE_NAME", operator));
			}
			joinContainingModule = true;
			return this;
		}

		@Override
		public ModuleAggregationQueryBuilder withRepresentation(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("module.representation = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("NOT module.representation = ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("module.representation = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT module.representation = any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for REPRESENTATION", operator));
			}
			return this;
		}

		@Override
		public ModuleAggregationQueryBuilder withTaxonomy(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					var entity = toEntityId(value, "Taxonomy id");
					filters.accept(q -> q.append("module.uid IN (SELECT module FROM module_taxonomies WHERE taxonomy = ")
										 .with(TaxonomyPgDao.referenceUidOrNid(entity))
										 .append(")"));
					break;
				case OPERATOR_NOT_EQ:
					entity = toEntityId(value, "Taxonomy id");
					filters.accept(q -> q.append("NOT module.uid IN (SELECT module FROM module_taxonomies WHERE taxonomy = ")
										 .with(TaxonomyPgDao.referenceUidOrNid(entity))
										 .append(")"));
					break;
				case OPERATOR_IN:
					var values = toEntityIds(value, "Taxonomy ids");
					filters.accept(q -> q.append("module.uid IN (SELECT module FROM module_taxonomies WHERE ")
										 .appendIds(values, "taxonomy = any(?)", "taxonomy IN (SELECT uid FROM taxonomy WHERE nid = any(?))")
										 .append(")"));
					break;
				case OPERATOR_NOT_IN:
					values = toEntityIds(value, "Taxonomy ids");
					filters.accept(q -> q.append("NOT module.uid IN (SELECT module FROM module_taxonomies WHERE ")
										 .appendIds(values, "taxonomy = any(?)", "taxonomy IN (SELECT uid FROM taxonomy WHERE nid = any(?))")
										 .append(")"));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for TAXONOMY_ID", operator));
			}
			return this;
		}

		@Override
		public ModuleAggregationQueryBuilder withTaxonomyName(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("module.uid IN (SELECT module FROM module_taxonomies WHERE taxonomy = (SELECT uid FROM taxonomy WHERE name = ?))", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("NOT module.uid IN (SELECT module FROM module_taxonomies WHERE taxonomy = (SELECT uid FROM taxonomy WHERE name = ?))", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("module.uid IN (SELECT module FROM module_taxonomies WHERE taxonomy IN (SELECT uid FROM taxonomy WHERE name = any(?)))")
										 .addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT module.uid IN (SELECT module FROM module_taxonomies WHERE taxonomy = (SELECT uid FROM taxonomy WHERE name = any(?)))")
										 .addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for TAXONOMY_ID", operator));
			}
			return this;
		}

		@Override
		public ModuleAggregationQueryBuilder applyFilters(final Map<ModuleFieldName, Map<String, Object>> filterObject) {
			filterObject.forEach((fieldName, filter) -> filter.forEach((operator, value) -> {
				switch (fieldName) {
					case ID:
						byId(operator, value);
						break;
					case NAME:
						withName(operator, value);
						break;
					case PROJECT_ID:
						ofProject(operator, value);
						break;
					case TECHNOLOGY:
						withTechnology(operator, value);
						break;
					case TYPE:
						withType(operator, value);
						break;
					case REQUIRES_REVIEW:
						withRequiresReview(operator, value);
						break;
					case STORAGE:
						withStorage(operator, value);
						break;
					case CREATOR:
						withCreator(operator, value);
						break;
					case IDENTIFICATION:
						withIdentification(operator, value);
						break;
					case LINES_OF_CODE:
						withCodeLines(operator, value);
						break;
					case LINES_OF_COMMENT:
						withCommentLines(operator, value);
						break;
					case LINES_OF_DEAD_CODE:
						withDeadCodeLines(operator, value);
						break;
					case COMPLEXITY:
						withComplexity(operator, value);
						break;
					case ERRORS:
						withErrors(operator, value);
						break;
					case SQL_STATEMENTS:
						withSqlStatements(operator, value);
						break;
					case CONTAINING_MODULE_ID:
						withContainingModuleId(operator, value);
						break;
					case CONTAINING_MODULE_NAME:
						withContainingModuleName(operator, value);
						break;
					case PHYSICAL_LINES_OF_CODE:
						withPhysicalLines(operator, value);
						break;
					case REPRESENTATION:
						withRepresentation(operator, value);
						break;
					case TAXONOMY_ID:
						withTaxonomy(operator, value);
						break;
					case ORIGIN:
						withOrigin(operator, value);
						break;
					default:
						throw new UnsupportedOperationException(String.format("Module field %s not supported", fieldName));
				}
			}));
			return this;
		}
		
		@Override
		public ModuleAggregationQueryBuilder withSourceRelationshipsFrom(final BuildingConsumer<ModuleLightInquiryBuilder<?>> other, final RelationshipType... types) {
			filters.accept(q -> {
				q.append("module.uid IN (SELECT dst FROM module_relationship WHERE src IN (SELECT uid FROM module");
				other.prepare(new ModuleQueryBuilder()).filters.build(q);
				q.append(") AND ")
				 .when(types.length == 1, q2 -> q2.append("type = ?::module_relationship_type", types[0].name()))
				 .when(types.length > 1, q2 -> q2.append("type = any(?::module_relationship_type[])", 
						 									new PgArray(PgType.STRING.toString(), Arrays.stream(types).map(RelationshipType::name).toArray(Object[]::new))))
				 .append(")");
			});

			return this;
		}

		@Override
		public ModuleAggregationQueryBuilder withOrigin(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("module.origin = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("module.origin != ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("module.origin = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT module.origin = any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for REPRESENTATION", operator));
			}
			return this;
		}
	}

	public class LinkedModuleQueryBuilder implements LinkedModuleInquiryBuilder {

		protected final FilterStreamBuilder filters = new FilterStreamBuilder();

		protected List<LinkedModule> build() {
			/* filter module_relationships having no source location and destination modules having no parent/containing module path */
			filters.accept(q -> q.append("rel.src_location IS NOT Null AND src.path IS NOT Null"));

			//Module location contains the Target module, it's path and the sources path.
			//Because the Target can be a virtual module, e.g. a subroutine, we need to join
			//the physical module to get the path.

			return query("SELECT COALESCE(virtual_dst.path, physical_dst.path) as TargetModulePath, "
						+ "to_jsonb(COALESCE(rel.dst_location, (virtual_dst.location))) as ToLocation, "
						+ "to_jsonb(rel.src_location) as FromLocation "
					+ "FROM module_relationship rel "
					+ "JOIN module src ON rel.src = src.uid "
					+ "JOIN module virtual_dst ON rel.dst = virtual_dst.uid "
					/* This could be improved by adding paths to Virtual modules, until then we just fetch the physical module extra.
					* Also, the relationship would need to contain the target location, which it doesn't currently. */
					+ "LEFT JOIN module_relationship vdst_to_physical ON vdst_to_physical.dst = virtual_dst.uid AND vdst_to_physical.type = 'CONTAINS' "
					+ "LEFT JOIN module physical_dst ON vdst_to_physical.src = physical_dst.uid ")
					.with(filters::build)
					.toList((rs,i) -> {
						final String targetPath = rs.getString(1);
						final ModuleLocation toLocation = PgJSON.fromPGobjectOrNull(rs.getObject(2), ModuleLocation.class);
						final ModuleLocation fromLocation = Objects.requireNonNull(PgJSON.fromPGobjectOrNull(rs.getObject(3), ModuleLocation.class),
																					"From location must not be null");
						return new LinkedModule(targetPath, toLocation, fromLocation);
					});
		}

		@Override
		public LinkedModuleQueryBuilder ofProject(final EntityId project) {
			filters.accept(q -> q.append("src.project = ").with(ProjectPgDao.referenceUidOrNid(project)));
			return this;
		}

		@Override
		public LinkedModuleQueryBuilder withSourcePath(final String path) {
			filters.accept(q -> q.append("src.path = ?", path));
			return this;
		}

		@Override
		public LinkedModuleQueryBuilder withSourceStorage(final Storage storage) {
			filters.accept(q -> q.append("src.storage = ?", storage.name()));
			return this;
		}

		@Override
		public LinkedModuleQueryBuilder withRelationships(final Collection<RelationshipType> types) {
			if (types.size() == 1) {
				filters.accept(q -> q.append("rel.type = ?::module_relationship_type", types.iterator().next().name()));
			} else {
				filters.accept(q -> q.append("rel.type = any(?::module_relationship_type[])", arrayFromCollection(PgType.STRING, types)));
			}

			return this;
		}
	}

	public class DeadCodeQueryBuilder implements DeadCodeInquiryBuilder {

		protected final FilterStreamBuilder filters = new FilterStreamBuilder();

		protected Paged.Builder<ModuleDeadCodePojo> build(@Nullable final Pagination paging) {
			return query("SELECT (SELECT to_jsonb(modIds) FROM (SELECT uid, nid FROM module WHERE uid = d.module) modIds), starting_line, number_of_lines, dead_code")
					.append(" FROM module_dead_code d ")
					.with(filters::build)
					.toPageable(paging, (rs, row) -> new ModuleDeadCodePojo(PgJSON.fromPGobject(rs.getObject(1), EntityId.class), null, null,	/* module ids */
																		Integer.valueOf(rs.getInt(2)),
																		Integer.valueOf(rs.getInt(3)),
																		rs.getString(4)));
		}

		protected QueryBuilder buildCount() {
			return query("SELECT count(*) FROM module_dead_code ")
					.with(filters::build);
		}

		@Override
		public DeadCodeQueryBuilder ofProject(final EntityId project) {
			filters.accept(q -> q.append("module IN (SELECT uid from module WHERE project = ").with(ProjectPgDao.referenceUidOrNid(project)).append(")"));
			return this;
		}

		@Override
		public DeadCodeQueryBuilder ofModule(final EntityId module) {
			filters.accept(q -> q.append("module = ").with(ModulePgDao.referenceUidOrNid(module)));
			return this;
		}
	}

	public class ModuleUndiscoveredQueryBuilder implements ModuleUndiscoveredInquiryBuilder {

		protected final FilterStreamBuilder filters = new FilterStreamBuilder();
		protected final OrderStreamBuilder order = new OrderStreamBuilder();

		protected Paged.Builder<ModuleUndiscoveredPojo> build(@Nullable final Pagination paging) {
			return query("SELECT project, name, path FROM module_undiscovered")
					.with(filters::build)
					.with(order::build)
					.toPageable(paging, (rs, rowNum) -> new ModuleUndiscoveredPojo(
							(UUID) rs.getObject("project"),
							rs.getString("name"),
							rs.getString("path")
					));
		}

		protected QueryBuilder buildCount() {
			return query("SELECT count(*) FROM module_undiscovered")
					.with(filters::build);
		}

		protected QueryBuilder buildDelete() {
			return query("DELETE FROM module_undiscovered")
					.with(filters::build);
		}

		@Override
		public ModuleUndiscoveredQueryBuilder sortProject(final SortDirection sort) {
			order.accept(q -> q.appendOrder("project", sort));
			return this;
		}

		@Override
		public ModuleUndiscoveredQueryBuilder sortName(final SortDirection sort) {
			order.accept(q -> q.appendOrder("name", sort));
			return this;
		}

		@Override
		public ModuleUndiscoveredQueryBuilder sortPath(final SortDirection sortDirection) {
			order.accept(q -> q.appendOrder("path", sortDirection));
			return this;
		}

		@Override
		public ModuleUndiscoveredQueryBuilder ofProject(final EntityId project) {
			filters.accept(q -> q.append("project = ").with(ProjectPgDao.referenceUidOrNid(project)));
			return this;
		}

		@Override
		public ModuleUndiscoveredQueryBuilder withName(final String name) {
			filters.accept(q -> q.append("name = ?", name));
			return this;
		}

		@Override
		public ModuleUndiscoveredQueryBuilder withPath(final String path) {
			filters.accept(q -> q.append("path = ?", path));
			return this;
		}
	}

	public class DependencyDefinitionQueryBuilder implements DependencyDefinitionInquiryBuilder {

		protected final FilterStreamBuilder filters = new FilterStreamBuilder();
		private boolean joinModule;
		private int havingCount = 0;
		protected Builder<DependencyDefinitionPojo> build(@Nullable final Pagination paging) {
			return query("SELECT d.id, "						/* 1 */
								+ "d.module,"					/* 2 */
								+ "m.nid,"						/* 3 */
								+ "d.attributes,"				/* 4 */
								+ "d.binding_type,"				/* 5 */
								+ "to_jsonb(d.location),"		/* 6 */
								+ "d.module_filters,"			/* 7 */
								+ "d.type,"						/* 8 */
								+ "d.resolution_flags,"			/* 9 */
								+ "d.resolved,"					/* 10 */
								+ "d.reached_from_modules"		/* 11 */
							+ " FROM dependency_definition d INNER JOIN module m ON d.module = m.uid")
					.with(filters::build)
					.toPageable(paging, (rs, row) -> new DependencyDefinitionPojo(
														(UUID) rs.getObject(1),															/* dependency definition id */
														EntityId.of((UUID) rs.getObject(2), Long.valueOf(rs.getLong(3))), null, null,	/* module EntityId */
														PgJSON.fromPGobject(rs.getObject(4)),											/* attributes */
														Binding.fromName(rs.getString(5)),												/* binding_type */
														PgJSON.fromPGobjectOrNull(rs.getObject(6), ModuleLocation.class),				/* location */
														PgJSON.fromPGobjects(rs.getArray(7), ModuleFilter.class),						/* module_filters */
														RelationshipType.from(rs.getString(8)),												/* relationship type */
														Sets.newHashSet(PgUtil.<String>streamArray(rs.getArray(9))						/* resolution_flags */
																					.map(ResolutionFlag::valueOf)
																					.toArray(ResolutionFlag[]::new)),
														rs.getBoolean(10),																/* resolved */
														PgJSON.fromPGobjects(rs.getArray(11), ModuleFilter.class)							/* reached_from_modules */
							));
		}

		protected Paged.Builder<EntityId> buildModuleId(@Nullable final Pagination paging) {
			return query("SELECT distinct d.module, m.nid FROM dependency_definition d INNER JOIN module m ON d.module = m.uid")
					.with(filters::build)
					.when(havingCount > 0, q -> q.append("GROUP BY d.module, m.nid HAVING count(*) > ?", havingCount))
					.toPageable(paging, (rs, row) -> EntityId.of((UUID) rs.getObject(1), Long.valueOf(rs.getLong(2))));
		}

		protected QueryBuilder buildCount() {
			return query("SELECT count(*) FROM dependency_definition d ")
					.when(joinModule, q -> q.append("INNER JOIN module m ON d.module = m.uid "))
					.with(filters::build);
		}

		@Override
		public DependencyDefinitionQueryBuilder byId(final UUID id) {
			filters.accept(q -> q.append("d.id = ?", id));
			return this;
		}

		@Override
		public DependencyDefinitionQueryBuilder ofProject(final EntityId project) {
			joinModule = true;
			filters.accept(q -> q.append("m.project = ").with(ProjectPgDao.referenceUidOrNid(project)));
			return this;
		}

		@Override
		public DependencyDefinitionQueryBuilder ofModule(final EntityId module) {
			filters.accept(q -> q.append("d.module = ").with(ModulePgDao.referenceUidOrNid(module)));
			return this;
		}

		@Override
		public DependencyDefinitionQueryBuilder withResolved(final boolean resolved) {
			filters.accept(q -> q.append("d.resolved = ?", resolved));
			return this;
		}

		@Override
		public DependencyDefinitionInquiryBuilder withFlag(final ResolutionFlag resolutionFlag) {
			filters.accept(q -> q.append("d.resolution_flags @> ARRAY['" + resolutionFlag + "']"));
			return this;
		}

		@Override
		public DependencyDefinitionInquiryBuilder havingCountGreaterThan(final int count) {
			this.havingCount = count;
			return this;
		}

		@Override
		public DependencyDefinitionInquiryBuilder withBindingType(final Binding binding) {
			filters.accept(q -> q.append("d.binding_type = ?", binding.name()));
			return this;
		}

		@Override
		public DependencyDefinitionInquiryBuilder withRelationship(final RelationshipType relationshipType) {
			filters.accept(q -> q.append("d.type = ?::module_relationship_type", relationshipType.toString()));
			return this;
		}
	}

	public class SourceMetricsQueryBuilder implements SourceMetricsInquiryBuilder {

		protected final FilterStreamBuilder filters = new FilterStreamBuilder();

		public QueryBuilder buildCount() {
			return query("SELECT count(*) FROM source_metrics sm INNER JOIN module m ON sm.module = m.uid")
				.with(filters::build);
		}

		public QueryBuilder buildCountCodeLines() {
			return query("SELECT sum(code_lines) FROM source_metrics sm INNER JOIN module m ON sm.module = m.uid")
					.with(filters::build);
		}

		public QueryBuilder buildCountCodeLinesByTechnology() {
			return query("SELECT m.technology, sum(sm.code_lines) FROM source_metrics sm INNER JOIN module m ON sm.module = m.uid")
					.with(filters::build)
					.append(" GROUP BY m.technology");
		}

		@Override
		public SourceMetricsQueryBuilder ofProject(final EntityId project) {
			filters.accept(q -> q.append("m.project = ").with(ProjectPgDao.referenceUidOrNid(project)));
			return this;
		}

		@Override
		public SourceMetricsQueryBuilder withRepresentation(final Representation representation) {
			filters.accept(q -> q.append("m.representation = ?", representation.name()));
			return this;
		}

	}

	/**
	 * Creates a new {@code module} entity or updates an existing one.
	 *
	 * @param module the {@link ModulePojoPrototype} to create
	 * @param isNew {@code true} to create a new {@code module} entity. {@code false} to update an existing {@code module} entity
	 * @return the {@link EntityId} of the created {@code module}.
	 */
	public EntityId put(final ModulePojoPrototype module, final boolean isNew) {
		final var fields = createFieldBuilder(module, isNew);

		final UUID uid;
		final QueryBuilder query;
		if (isNew) {
			uid = module.uid.orElseNonNull(UUID::randomUUID);
			fields.add("uid", "?", uid);
			query = query("INSERT INTO module ");
			fields.buildInsert(query);
		} else {
			query = query("UPDATE module SET ");
			fields.buildUpdate(query);
			query.append(" WHERE uid = ").with(referenceUidOrNid(module.identityProvisional()));
		}

		return query.append(" RETURNING uid, nid")
				.first(rs -> EntityId.of((UUID) rs.getObject(1), rs.getLong(2)))
				.orElseThrow(() -> new PersistenceException("Failed to " + (isNew ? "create" : "update") + " module: '" + module));
	}

	private FieldBuilder createFieldBuilder(final ModulePojoPrototype module, final boolean isNew) {
		module.name.ifDefined(name -> {
			if (StringUtils.isBlank(name)) {
				throw new ConstraintViolationException(module, "Module name must not be empty.");
			}
		});
		
		final var fieldBuilder = new FieldBuilder()
				.add(module.project.required(isNew), "project", ProjectPgDao::referenceUidOrNid)
				.add(module.name.required(isNew), "name", "?")
				.add(module.path, "path", "?")
				.add(module.technology.required(isNew), "technology", "?", Technology::name)
				.add(module.type.required(isNew), "type", "?", Type::name)
				.add(module.storage.required(isNew), "storage", "?", Storage::name)
				.add(module.origin.required(isNew), "origin", "?", Origin::name)
				.add(module.creator.required(isNew), "creator", "?", Creator::name)
				.add(module.identification.required(isNew), "identified", "?", ident -> Boolean.valueOf(ident == Identification.IDENTIFIED))
				.add(module.info, "info", "?", PgJSON::toPGobject)
				.add(module.contentHash, "content_hash", "?", passNull(BinaryValue::get))
				.add(module.linkHash.required(isNew), "link_hash", "?")
				.add(module.location, "location", ModulePgDao::appendLocation)
				.add(module.representation, "representation", "?", passNull(Representation::name))
				.add(module.requiresReview, "requires_review", "?")
				.add(module.modifiedDate, "modified_date", "?::timestamp_zoned_milli", passNull(Timestamp::from))
				.add(module.metricsDate, "metrics_date", "?::timestamp_zoned_milli", passNull(Timestamp::from))
				.add(module.description, "description", "?")
				.and(CustomPropertiesPgDao.addField(module, isNew));
		
		if (module.source.isDefined() && module.source.get() != null) {
			fieldBuilder.add(module.source, "source", SourcePgDao::referenceUidOrNid);
		}
		
		return fieldBuilder;
	}

	/**
	 * Deletes all {@code module} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of deleted modules
	 */
	public int deleteModules(final BuildingConsumer<ModuleInquiryBuilder> builder) {
		return builder.prepare(new ModuleQueryBuilder())
						.delete()
						.update();
	}

	/**
	 * Returns paged subset of optionally filtered {@code module} entities that match with the filters in the given {@code builder}.
	 * 
	 * @param paging Pagination specification.
	  * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options.
	 * @return Paged subset of matching {@code module} entities.
	 */
	public Paged<ModulePojo> findModules(final Pagination paging, final BuildingConsumer<ModuleInquiryBuilder> builder) {
		return builder.prepare(new ModuleQueryBuilder())
						.build(paging)
						.page();
	}

	/**
	 * Returns all {@code module} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain ModulePojo ModulePojos}
	 */
	public List<ModulePojo> findModules(final BuildingConsumer<ModuleInquiryBuilder> builder) {
		return builder.prepare(new ModuleQueryBuilder())
						.build(null)
						.all();
	}

	/**
	 * Returns the {@link UUID UUIDs} of all {@code modules} that reference the given {@code module} with the specified reference {@code type} and 
	 * {@code direction}.
	 * 
	 * @param module the id of the starting module
	 * @param type the type of the module references
	 * @param direction the direction of the module references
	 * @return list of matching {@code module} {@link UUID UUIDs}.
	 */
	public List<UUID> findReferencingModules(final EntityId module, final RelationshipType type, final RelationshipDirection direction) {
		switch (direction) {
			case OUT:
				return query("WITH RECURSIVE ref_modules AS ("
							+ " SELECT m.uid FROM module m INNER JOIN module_relationship r ON r.dst = m.uid"
							+ " WHERE r.type = ?::module_relationship_type AND r.src = ")
						.addArg(type.name())
						.with(referenceUidOrNid(module))
						.append(" UNION SELECT r.dst FROM module_relationship r JOIN ref_modules ON r.src = ref_modules.uid ) "
							+ "SELECT * FROM ref_modules")
						.toList((rs, i) -> (UUID) rs.getObject(1));

			case IN:
				return query("WITH RECURSIVE ref_modules AS ("
							+ " SELECT m.uid FROM module m INNER JOIN module_relationship r ON r.src = m.uid"
							+ " WHERE r.type = ?::module_relationship_type AND r.dst = ")
						.addArg(type.name())
						.with(referenceUidOrNid(module))
						.append(" UNION SELECT r.src FROM module_relationship r JOIN ref_modules ON r.dst = ref_modules.uid) "
							+ "SELECT * FROM ref_modules")
						.toList((rs, i) -> (UUID) rs.getObject(1));

			case BOTH:
				return query("WITH RECURSIVE out_ref_modules AS ("
							+ " SELECT m.uid FROM module m INNER JOIN module_relationship r ON r.dst = m.uid"
							+ " WHERE r.type = ?::module_relationship_type AND r.src = ")
						.addArg(type.name())
						.with(referenceUidOrNid(module))
						.append(" UNION SELECT r.dst FROM module_relationship r JOIN out_ref_modules ON r.src = out_ref_modules.uid"
							+ "), in_ref_modules AS ("
							+ " SELECT m.uid FROM module m INNER JOIN module_relationship r ON r.src = m.uid"
							+ " WHERE r.type = ?::module_relationship_type AND r.dst = ")
						.addArg(type.name())
						.with(referenceUidOrNid(module))
						.append(" UNION SELECT r.src FROM module_relationship r JOIN in_ref_modules ON r.dst = in_ref_modules.uid)"
							+ "SELECT * FROM out_ref_modules UNION SELECT * FROM in_ref_modules")
						.toList((rs, i) -> (UUID) rs.getObject(1));
		}

		throw new UnsupportedOperationException("Reference direction is not supported: " + direction);
	}

	/**
	 * Returns a list of names of all {@code module} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain ModulePojo ModulePojos}
	 */
	public List<String> findNames(final BuildingConsumer<ModuleInquiryBuilder> builder) {
		return builder.prepare(new ModuleQueryBuilder())
				.buildNames(null)
				.all();
	}

	/**
	 * Returns all {@code module} entities that match with the filters in the given {@code builder}.
	 * <p>This method loads only the basic module properties for better performance.</p>
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain ModuleLightweightPojo ModuleLightweightPojos}
	 */
	public List<ModuleLightweightPojo> findModulesLightweight(final BuildingConsumer<ModuleInquiryBuilder> builder) {
		return builder.prepare(new ModuleQueryBuilder())
				.buildLightweight()
				.toList((rs, rowNum) -> mapLightweightModule(rs, true));
	}

	/**
	 * Returns the first {@code module} that matches with the filters in the given {@code builder}. If no match was found then
	 * an empty {@linkplain Optional} is returned.
	 * <p>This method loads only the basic module properties for better performance.</p>
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options. 
	 * @return first matching {@linkplain ModuleLightweightPojo} if available
	 */
	public Optional<ModuleLightweightPojo> findAnyModuleLightweight(final BuildingConsumer<ModuleInquiryBuilder> builder) {
		return builder.prepare(new ModuleQueryBuilder())
				.limit(1)
				.buildLightweight()
				.first(rs -> mapLightweightModule(rs, true));
	}

	private static ModuleLightweightPojo mapLightweightModule(final ResultSet rs, final boolean withContaining) throws SQLException {
		final var containingUid = withContaining ? (UUID) rs.getObject("containingUid") : null;
		final var containingId = containingUid == null ? null : EntityId.of(containingUid, (Long) rs.getObject("containingNid"));
		return new ModuleLightweightPojo(
				(UUID) rs.getObject("uid"),
				Long.valueOf(rs.getLong("nid")),
				null, withContaining ? (UUID) rs.getObject("prj_uid") : null, withContaining ? rs.getLong("prj_nid") : null, /* project */
				rs.getString("name"),
				rs.getString("path"),
				Technology.valueOf(rs.getString("technology")),
				Type.valueOf(rs.getString("type")),
				rs.getString("link_hash"),
				rs.getString("representation"),
				rs.getBoolean("identified"),
				containingId, null, null,										/* parent */
				withContaining ? rs.getString("containingPath") : null);
	}

	/**
	 * Find all orphaned {@code modules} that must be recollected in incremental discovery.
	 *
	 * @param projectId the ID of the project
	 * @param creator the {@link Creator} of the {@link ModuleLightweightPojo} to find
	 * @return list of {@linkplain ModuleLightweightPojo ModuleLightweightPojos}
	 */
	public List<ModuleLightweightPojo> findModuleLightweightOrphaned(final EntityId projectId, final Creator creator) {
		return query("SELECT m.uid, m.nid, m.name, m.path, m.technology, m.type, m.link_hash, m.representation, m.identified FROM module m "
				   + "LEFT JOIN module_relationship r ON m.uid = r.dst WHERE m.project = ")
				.with(ProjectPgDao.referenceUidOrNid(projectId))
				/* ignore java package modules */
				.append(" AND creator = ? AND "
					  + "(m.source IS NOT Null AND m.technology = 'JCL' AND m.type = 'PROC' OR m.source IS Null AND "
					  + "m.uid NOT IN (SELECT dst FROM module_relationship WHERE type = 'CONTAINS')) AND r.dst is Null")
				.addArgs(creator.name())
				.toList((rs, rowNum) -> mapLightweightModule(rs, false));
	}

	/**
	 * Returns all {@code module} entities that have no source or whose source has changed and which must be recollected in incremental discovery.
	 *
	 * @param projectId the ID of the project
	 * @param creator the {@link Creator} of the {@link ModuleLightweightPojo} to find
	 * @return list of {@linkplain ModuleLightweightPojo ModuleLightweightPojos}
	 */
	public List<ModuleLightweightPojo> findModuleLightweightMissingOrChangedSource(final UUID projectId, final Creator creator) {
		// TODO npr alternate query for the exists check. Delete if testing is ok
//		-- Fetch all Cobol COPYLIB SourceObject IDs that were all newly added or content has changed
//		SELECT 1 FROM source_info si
//		INNER JOIN source s ON s.id = si.uid
//		WHERE s.project = '45396433-5fdc-4ae6-8c8a-e79f9e2fd50c' AND si.type = 'COPYLIB' AND si.uid NOT IN
//		(
//		SELECT si.uid FROM source_info si
//		LEFT JOIN module m ON m.source = si.uid AND m.content_hash = si.content_hash
//		WHERE m.project = '45396433-5fdc-4ae6-8c8a-e79f9e2fd50c' AND m.creator = 'DISCOVERY' AND m.identified AND
//		 m.technology = 'COBOL' AND m.type = 'COPYLIB' AND m.content_hash = si.content_hash
//		) LIMIT 1

		/* Check for modules that do not have source code (source is NULL) AND where also one of the following conditions matches */
		return query("SELECT uid, nid, name, path, technology, type, link_hash, representation, identified "
					+ "FROM module WHERE project = ? AND creator = ? AND source is Null AND ("
						/* Checks if Technology of the module is UNKNOWN */
						+ "technology = 'UNKNOWN' AND ("
								/* UNKNOWN modules must not be of type UTILITY OR we must have a source with same name so it's a potential candidate for rescan */
								+ "type != 'UTILITY' OR name IN (SELECT name FROM source_info si INNER JOIN source s ON s.id = si.uid AND s.project = ?)"
						+ ") OR ("
							+ "identified = false AND ("
									/* COPYPROCs: At this stage we don't known if we have any source of type COPYLIB which contains it
									 * DDMs: The NaturalContributor has to recreate it for collecting the ModelDependency attributes properly */
									+ "type = 'COPYPROC' OR type = 'DDM' OR type = 'TYPE'"
								+ " OR "
									/* we should recollect MISSING Natural modules that use the language code.*/
									+ "(technology = 'NATURAL' AND name LIKE '%&%')"
								+ " OR "
									/* Checks if the Module is PL1_COPYBOOK and a source with the same name as the module exists and the
									 * type of the source is the same as the type of the module or the type of the source is PL1_PROGRAM*/
									+ "(technology = 'PL1' AND type = 'COPYBOOK' AND name IN "
										+ "(SELECT name FROM source_info si INNER JOIN source s ON s.id = si.uid WHERE s.project = ? AND si.technology = 'PL1' AND"
											+ " (si.type = 'PROGRAM' OR si.type = 'COPYBOOK')))"
								+ " OR "
									/* We have to rescan 'MISSING' Cobol COPYBOOKs if new Cobol COPYLIBs were added or if one COPYLIB has changed */
									+ "(technology = 'COBOL' AND type = 'COPYBOOK' AND ("
											+ "EXISTS(SELECT 1 FROM source_info si "
													+ "INNER JOIN source s ON s.id = si.uid "
													+ "LEFT JOIN module m ON m.source = si.uid "
													+ "WHERE s.project = ? AND si.type = 'COPYLIB' AND "
													/* m.source IS NULL with si LEFT JOIN m => new source for which no module exists */
													+ "(m.source IS NULL OR m.project = ? AND m.identified AND "
													/* m.content_hash != si.content_hash => COPYLIB has changed */
													+ " m.technology = 'COBOL' AND m.type = 'COPYLIB' AND m.content_hash != si.content_hash) LIMIT 1)"
										+ " OR "
											/* or if the module is a COBOL_COPYBOOK and a source with the same name as the module exists and the type
											 * of the source is the same as the type of the module or the type of the source is COBOL_COPYPROC */
											+ "name IN (SELECT name FROM source_info si INNER JOIN source s ON s.id = si.uid WHERE s.project = ? AND "
												+ "si.technology = 'COBOL' AND (si.type = 'COPYPROC' OR si.type = 'COPYBOOK'))"
										+ ")"
									+ ")"
							+ ")"
						+ ")"
					+ ")")
				.addArgs(projectId, creator.name(), projectId, projectId, projectId, projectId, projectId)
				.toList((rs, rowNum) -> mapLightweightModule(rs, false));
	}

	/**
	 * Returns all {@linkplain LinkedModule LinkedModules} that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options. 
	 * @return list of {@linkplain LinkedModule LinkedModules}
	 */
	public List<LinkedModule> findLinkedModules(final BuildingConsumer<LinkedModuleInquiryBuilder> builder) {
		return builder.prepare(new LinkedModuleQueryBuilder())
						.build();
	}

	/**
	 * Returns the first {@code module} that matches with the filters in the given {@code builder}. If no match was found then
	 * an empty {@linkplain Optional} is returned.
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options. 
	 * @return first matching {@linkplain ModulePojo} if available
	 */
	public Optional<ModulePojo> findAnyModule(final BuildingConsumer<ModuleInquiryBuilder> builder) {
		return builder.prepare(new ModuleQueryBuilder()
										.limit(1))
					.build(null)
					.first();
	}
	
	/**
	 * Returns a substring of the content of the module for the given {@code moduleId} if available
	 *
	 * @param moduleId the id of the {@code module} to find
	 * @param offset the offset in the content from where to start
	 * @param length the length of the content to read
	 * @return the substring if available
	 */
	public Optional<String> getContentSubstring(final EntityId moduleId, final Integer offset, final Integer length) {
		return query("SELECT module_location_substr((?,?),content) FROM source where id IN (SELECT source FROM module WHERE ")
					.addArgs(offset, length)
					.appendId(moduleId)
					.append(")")
					.first(rs -> new BinaryString(rs.getBytes(1)).toString());
	}

	public Map<EntityId, String> getContents(final Collection<EntityId> moduleIds) {
		return query("SELECT uid, nid, s.content FROM source s LEFT JOIN module m ON s.id = m.source WHERE")
				.appendIds(moduleIds, " m.uid = any(?)", " m.nid = any(?)")
				.toMap((rs, map) -> map.put(EntityId.of((UUID)rs.getObject(1), rs.getLong(2)),
						new BinaryString(rs.getBytes(3)).toString()));
	}
	
	/**
	 * Retrieves the Project ID for a Module.
	 * @param moduleId ID of the Module.
	 * @return ID of the Project.
	 */
	public Optional<EntityId> getProject(final EntityId moduleId) {
		return query("SELECT p.uid, p.nid FROM module m INNER JOIN project p ON p.uid = m.project WHERE m.").appendId(moduleId)
				.first(rs -> EntityId.of((UUID) rs.getObject(1), rs.getLong(2)));
	}
	
	/**
	 * Returns the {@link Type} of the module for the given {@code moduleId} if available
	 *
	 * @param moduleId the id of the {@code module} to find
	 * @return the {@link Type} if available
	 */
	public Optional<Type> getModuleType(final EntityId moduleId) {
		return query("SELECT type FROM module WHERE ")
					.appendId(moduleId)
					.first(rs -> Type.fromName(rs.getString(1)));
	}

	/**
	 * Returns the {@code path} of the module for the given {@code moduleId} if available
	 *
	 * @param moduleId the id of the {@code module} to find
	 * @return the path
	 */
	public Optional<String> getModulePath(final EntityId moduleId) {
		return query("SELECT path FROM module WHERE ")
						.appendId(moduleId)
						.first(rs -> rs.getString(1));
	}

	/**
	 * Returns paged subset of optionally filtered {@code module} nids that match with the filters in the given {@code builder}.
	 * 
	 * @param paging Pagination specification.
	  * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options
	 * @return Paged subset of matching {@code module} {@link EntityId EntityIds}
	 */
	public Paged<EntityId> findModuleIds(final Pagination paging, final BuildingConsumer<ModuleInquiryBuilder> builder) {
		return builder.prepare(new ModuleQueryBuilder())
						.buildId(paging)
						.page();
	}

	/**
	 * Returns a list of {@code <module id, module link hash> pairs} for the modules matching the given filters.
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria
	 * @return list of the ids and link hashes of the matching modules
	 */
	public List<Pair<EntityId, String>> findModuleIdsAndLinkHashes(final BuildingConsumer<ModuleInquiryBuilder> builder) {
		return builder.prepare(new ModuleQueryBuilder())
				.buildIdAndLinkHash(null)
				.all();
	}
	
	/**
	 * Returns a list of {@code <module uid, module dependency hash> pairs} for the modules matching the given filters.
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria
	 * @return list of the uids and dependency hashes of the matching modules
	 */
	public List<Pair<UUID, String>> findModuleUidsAndDependencyHashes(final BuildingConsumer<ModuleInquiryBuilder> builder) {
		return builder.prepare(new ModuleQueryBuilder())
				.buildUidAndDependencyHash(null)
				.all();
	}

	/**
	 * Returns the {@link UUID UUIDs} of all {@code module} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options
	 * @return list of matching {@code module} {@link EntityId EntityIds}.
	 */
	public List<UUID> findModuleUids(final BuildingConsumer<ModuleInquiryBuilder> builder) {
		return builder.prepare(new ModuleQueryBuilder())
				.buildUid(null)
				.all();
	}
	
	/**
	 * Returns the {@link EntityId EntityIds} of all {@code module} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options
	 * @return list of matching {@code module} {@link EntityId EntityIds}.
	 */
	public List<EntityId> findModuleIds(final BuildingConsumer<ModuleInquiryBuilder> builder) {
		return builder.prepare(new ModuleQueryBuilder())
						.buildId(null)
						.all();
	}

	/**
	 * Returns the {@code nid} of the first {@code module} that matches with the filters in the given {@code builder}. If no match was found then
	 * an empty {@linkplain Optional} is returned.
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options. 
	 * @return the {@link EntityId} of the first matching {@code module} if available
	 */
	public Optional<EntityId> findAnyModuleId(final BuildingConsumer<ModuleInquiryBuilder> builder) {
		return builder.prepare(new ModuleQueryBuilder())
						.buildId(Pagination.FIRST)
						.first();
	}

	/**
	 * Returns the number of {@code module} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of matching {@code module} entities
	 */
	public long countModules(final BuildingConsumer<ModuleInquiryBuilder> builder) {
		return builder.prepare(new ModuleQueryBuilder())
				.buildCount()
				.first(rs -> Long.valueOf(rs.getLong(1)))
				.orElse(Long.valueOf(0l));
	}

	/**
	 * Returns duplicate {@code module} names based on the given {@code project}, {@code technology} and {@code types}.
	 * 
	 * @param project the project the {@code module} entities must be filtered with
	 * @param technology the technology of the {@code module} 
	 * @param types types of {@code module}
	 * @return list of {@code module} names
	 */
	public List<String> findDuplicateModuleNames(final EntityId project, final Technology technology, final List<Type> types) {
		return query("SELECT name FROM module WHERE project = ")
				.with(ProjectPgDao.referenceUidOrNid(project))
				.append(" AND technology = ? AND type = any(?) GROUP BY name HAVING count(name) > 1", technology.toString(), arrayFromCollection(PgType.STRING, types))
				.toList((rm, row) -> rm.getString(1));
	}

	/**
	 * Returns the {@link EntityId EntityIds} of {@code module} duplicates with type {@code typeToDelete}.
	 * <p>The query first searches for all modules that have the same name for the given {@code project}, {@code technology} and types ({@code typeToKeep} or
	 * {@code typeToDelete}). The {@link EntityId EntityIds} of {@code module} entities with type {@code typeToDelete} are then fetched.</p>
	 *
	 * @param project the project the {@code module} entities must be filtered with
	 * @param technology the technology the {@code module} entities must be filtered with
	 * @param typeToKeep the type of module to retain
	 * @param typeToDelete the type of module to delete
	 * @return list of {@code module} {@link EntityId EntityIds} with type {@code typeToDelete}
	 */
	public List<EntityId> findDuplicateModuleIds(final EntityId project, final Technology technology, final Type typeToKeep, final Type typeToDelete) {
		return query("WITH module_cte1 as ( ")
				.append("SELECT name, array_agg(uid) as uids, array_agg(nid) as nids, array_agg(type) as types FROM module ")
				.append("WHERE project = ").with(ProjectPgDao.referenceUidOrNid(project))
				.append(" AND technology = ? AND type IN (?,?) ", technology.toString(), typeToKeep.toString(), typeToDelete.toString())
				.append("GROUP BY name HAVING count(name) > 1), ")
				.append("module_cte2 as (SELECT name, unnest(uids) as uid, unnest(nids) as nid, unnest(types) as mc_types FROM module_cte1) ")
				.append("SELECT uid, nid FROM module_cte2 WHERE mc_types = ? ", typeToDelete.toString())
				.toList((rs, row) -> EntityId.of((UUID) rs.getObject(1), Long.valueOf(rs.getLong(2))));
	}

	/**
	 * Returns the {@link EntityId EntityIds} of all {@code module} entities that have unresolved dependencies in the given {@code project}.
	 *
	 * @param project the {@code project} for which to find the modules
	 * @return list of matching {@code module} {@link UUID UUIDs}.
	 */
	public List<EntityId> findModulesWithUnresolvedDependencies(final EntityId project) {
		return query("SELECT uid, nid FROM module WHERE project = ")
				.with(ProjectPgDao.referenceUidOrNid(project))
				.append(" AND uid IN (SELECT distinct(module) FROM error_marker WHERE key='UNDISCOVERED_DEPENDENCY')")
				.toList((rs, rom) -> EntityId.of((UUID) rs.getObject(1), Long.valueOf(rs.getLong(2))));
	}

	/**
	 * Returns the {@link EntityId EntityIds} of all {@code module} entities of the given {@code project} for which meta data (description, annotations,
	 * taxonomies or data dictionary entries) have been collected.
	 *
	 * @param project the {@code project} for which to find the modules
	 * @return list of matching {@code module} {@link UUID UUIDs}.
	 */
	public List<EntityId> findModulesWithMetaData(final EntityId project) {
		return query("SELECT uid, nid FROM module WHERE project = ")
				.with(ProjectPgDao.referenceUidOrNid(project))
				.append(" AND (description IS NOT Null OR uid IN (SELECT module FROM annotation) "
												   + "OR uid IN (SELECT module FROM data_dictionary) "
												   + "OR uid IN (SELECT module FROM module_taxonomies))")
				.toList((rs, rom) -> EntityId.of((UUID) rs.getObject(1), Long.valueOf(rs.getLong(2))));
	}

	/**
	 * Updates the {@code content_hash} of all {@code module} entities and sets it to the {@code content_hash} of their sources.
	 *
	 * @param project the {@code project} for which to update the {@code module} entities
	 * @return number of updated {@code modules}
	 */
	public int updateModuleContentHashes(final EntityId project) {
		return query("UPDATE module m SET content_hash = s.content_hash FROM source_info s WHERE m.source = s.uid  AND m.content_hash != s.content_hash AND project = ")
				.with(ProjectPgDao.referenceUidOrNid(project))
				.update();
	}
	
	/**
	 * Updates the {@code dependency_hash} of all {@code module} entities and sets it to the {@code dependency_hash} of their sources.
	 *
	 * @param project the {@code project} for which to update the {@code module} entities
	 * @return number of updated {@code modules}
	 */
	public int updateModuleDependencyHashes(final EntityId project) {
		final String query = "UPDATE module m set dependency_hash = "
				+ "(select md5(format('%s,%s,%s,%s,%s,%s,%s',src_linkhash, src_offset, src_length, dst_linkhash, dst_offset, dst_length, relationship_type)) "
				+ "  from  (SELECT  "
				+ " array_agg(m_src.link_hash ORDER BY m_src.link_hash) as src_linkhash,"
				+ " array_agg((mr.src_location).offset ORDER BY (mr.src_location).offset) as src_offset,"
				+ " array_agg((mr.src_location).length ORDER BY (mr.src_location).length) as src_length,"
				+ " array_agg(m_dst.link_hash ORDER BY m_dst.link_hash) as dst_linkhash,"
				+ " array_agg((mr.dst_location).offset ORDER BY (mr.dst_location).offset) as dst_offset, "
				+ " array_agg((mr.dst_location).length ORDER BY (mr.dst_location).length) as dst_length,"
				+ " array_agg(mr.\"type\" ORDER BY mr.\"type\") as relationship_type"
				+ "   FROM "
				+ "        module_relationship mr "
				+ "    JOIN "
				+ "        module m_src ON m_src.uid = mr.src"
				+ "    JOIN "
				+ "        module m_dst ON m_dst.uid = mr.dst"
				+ "        where m.uid = mr.src or m.uid = mr.dst"
				+ "    ) as agg_values)"
				+ " where m.project = ";
		return query(query)
				.with(ProjectPgDao.referenceUidOrNid(project))
				.update();
	}

	/**
	 * This method allows you to update multiple modules with the same values.
	 * 
	 * <p>Updates all modules that match with the filters in the given {@code builder}. The defined (set) fields in the given {@code template} are used for
	 * building the update query.</p>
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria
	 * @param values the {@link ModulePojoPrototype} containing the to be updated fields
	 * @return the number of updated {@code modules}, 0 if no module matched with the filer
	 */
	public int updateModules(final BuildingConsumer<ModuleInquiryBuilder> builder, final ModulePojoPrototype values) {
		final var fields = createFieldBuilder(values, false);
		final var filter = builder.prepare(new ModuleQueryBuilder());

		return query("UPDATE module SET ")
			.with(fields::buildUpdate)
			.with(filter.filters::build)
			.update();
	}

	/**
	 * Removes the given {@code property} from the {@code info} field of all {@code module} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria
	 * @param property the property to remove
	 * @return number of updated {@code modules}
	 */
	public int removeModuleInfoProperty(final BuildingConsumer<ModuleInquiryBuilder> builder, final String property) {
		final var filter = builder.prepare(new ModuleQueryBuilder());
		return query("UPDATE module SET info = info - ?")
				.addArg(property)
				.with(filter.filters::build)
				.update();
	}

	/*
	 * Methods for source_metrics table
	 */

	/**
	 * Creates a new {@code source_metrics} entity if none exists yet for the {@code module} in the given {@code sourceMetrics} or updates the existing one.
	 *
	 * @param sourceMetrics the {@link SourceMetricsPojoPrototype} to create or update
	 */
	public void putSourceMetrics(final SourceMetricsPojoPrototype sourceMetrics) {
		final FieldBuilder fields = new FieldBuilder()
				.add(sourceMetrics.physicalLines, "physical_lines", "?")
				.add(sourceMetrics.codeLines, "code_lines", "?")
				.add(sourceMetrics.commentLines, "comment_lines", "?")
				.add(sourceMetrics.complexityMcCabe, "complexity_mc_cabe", "?")
				.add(sourceMetrics.deadCodeLines, "dead_code_lines", "?")
				.add("module", q -> q.with(ModulePgDao.referenceUidOrNid(sourceMetrics.module.getNonNull())));

		final QueryBuilder query = query("INSERT INTO source_metrics ")
									.with(t -> fields.buildUpsert(t, "module"));

		if (query.update() == 0) {
			throw new PersistenceException("Failed to create / update sourceMetrics: " + sourceMetrics);
		}
	}

	/**
	 * Returns the number of {@code source_matrics} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain SourceMetricsInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of matching {@code source_matrics} entities
	 */
	public long countSourceMetrics(final BuildingConsumer<SourceMetricsInquiryBuilder> builder) {
		return builder.prepare(new SourceMetricsQueryBuilder())
				.buildCount()
				.first(rs -> Long.valueOf(rs.getLong(1)))
				.orElse(Long.valueOf(0l));
	}

	/**
	 * Returns the number of {@code source_metrics} of all {@code source_matrics} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain SourceMetricsInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of {@code code_lines}
	 */
	public long countSourceMetricsCodeLines(final BuildingConsumer<SourceMetricsInquiryBuilder> builder) {
		return builder.prepare(new SourceMetricsQueryBuilder())
				.buildCountCodeLines()
				.first(rs -> Long.valueOf(rs.getLong(1)))
				.orElse(Long.valueOf(0l))
				.longValue();
	}

	/**
	 * Returns a map containing the sum of {@code source_metrics} {@code code_lines} per technology for all {@code source_matrics} entities that match with the
	 * filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain SourceMetricsInquiryBuilder} containing the filter criteria and sort options.
	 * @return map with sums
	 */
	public Map<String, Long> countSourceMetricsCodeLinesByTechnology(final BuildingConsumer<SourceMetricsInquiryBuilder> builder) {
		return builder.prepare(new SourceMetricsQueryBuilder())
				.buildCountCodeLinesByTechnology()
				.toMap((rs, map) -> map.put(rs.getString(1), Long.valueOf(rs.getLong(2))));
	}

	/**
	 * Selects the complexities for all modules mapped by their {@link Technology} and {@link Type} for all {@code source_matrics} entities that match with the
	 * filters in the given {@code builder}.
	 * 
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options.
	 * @return Map of complexities mapped by technology and type. Outer map contains the mapping for the {@link Technology Technologies},
	 * the inner map the mapping by {@link Type Types}
	 */
	public Map<String, Map<String, List<Integer>>> getComplexities(final BuildingConsumer<ModuleInquiryBuilder>  builder) {
		return builder.prepare(new ModuleQueryBuilder())
				.buildComplexities()
				.toMap((rs, map) -> map.computeIfAbsent(rs.getString(1), k -> new HashMap<>())
																					.computeIfAbsent(rs.getString(2), k -> new ArrayList<>())
																																.add(rs.getInt(3)));
	}

	/**
	 * Returns aggregation values for the filters and selected aggregations in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleAggregationInquiryBuilder} containing the aggregation operations and filter criteria
	 * @return container with the aggregation values
	 */
	public Optional<Table> getAggregations(final BuildingConsumer<ModuleAggregationInquiryBuilder<?>> builder) {
		return builder.prepare(new ModuleAggregationQueryBuilder())
						.buildAggregation(this);
	}

	/**
	 * Sets the {@code dead_code_lines} to zero in all {@code source_metrics} entities for the given {@code project}.
	 *
	 * @param project the {@code project} for which to reset the {@code dead_code_lines}.
	 * @return number of modified {@code source_metrics} entities
	 */
	public int resetSourceMetricsLinesOfDeadCode(final EntityId project) {
		return query("UPDATE source_metrics sm SET dead_code_lines = 0 FROM module m WHERE sm.module = m.uid AND m.project = ")
				.with(ProjectPgDao.referenceUidOrNid(project))
				.update();
	}

	/**
	 * Sets the {@code dead_code_lines} in all {@code source_metrics} entities to the sum of {@code module_dead_code.number_of_lines} for each {@code module} of
	 * the given {@code project}.
	 *
	 * @param project the {@code project} for which to update the {@code dead_code_lines}.
	 * @return number of modified {@code source_metrics} entities
	 */
	public int updateSourceMetricsLinesOfDeadCode(final EntityId project) {
		return query("WITH dead_code AS (")
				.append(" SELECT dc.module as dc_module, sum(dc.number_of_lines) as dc_lines FROM module_dead_code dc ")
				.append(" INNER JOIN module m ON dc.module = m.uid ")
				.append(" WHERE m.project = ").with(ProjectPgDao.referenceUidOrNid(project))
				.append(" GROUP BY dc.module ) ")
				.append(" UPDATE source_metrics SET dead_code_lines = dc_lines FROM dead_code WHERE module = dc_module ")
				.update();
	}

	/**
	 * Creates a new {@code module_dead_code} entity.
	 *
	 * @param deadCode the {@link ModuleDeadCodePojoPrototype} to create
	 */
	public void createDeadCode(final ModuleDeadCodePojoPrototype deadCode) {
		final var fields = new FieldBuilder()
				.add("module", q -> q.with(ModulePgDao.referenceUidOrNid(deadCode.module.getNonNull())))
				.add(deadCode.startingLine, "starting_line", "?")
				.add(deadCode.numberOfLines, "number_of_lines", "?")
				.add(deadCode.deadCode, "dead_code", "?");

		query("INSERT INTO module_dead_code ")
				.with(fields::buildInsert)
				.update();
	}

	/**
	 * Creates a new {@code module_dead_code} entity for each {@link ModuleDeadCodePojoPrototype} in {@code code errorMarkers}.
	 *
	 * @param deadCodes the {@link ModuleDeadCodePojoPrototype ModuleDeadCodePojoPrototypes} to create
	 */
	public void createDeadCodes(final Collection<ModuleDeadCodePojoPrototype> deadCodes) {
		/* create batch arguments for all pojos that have a module UID */
		final var batchArgs = deadCodes.stream()
									.filter(deadCode -> deadCode.module.getNonNull().hasUid())
									.map(deadCode -> toStream(deadCode, true))
									.collect(Collectors.toList());

		/* INSERT all pojos that have a module UID */
		query("INSERT INTO module_dead_code(module, starting_line, number_of_lines, dead_code) VALUES (?, ?, ?, ?)")
			.updateBatch(batchArgs.stream(), 1_000);

		/* check if there are pojos that have a module NID but no module UID */
		if (batchArgs.size() < deadCodes.size()) {
			/* create batch arguments for all pojos that have a module NID only */
			query("INSERT INTO module_dead_code(module, starting_line, number_of_lines, dead_code) VALUES ((SELECT uid FROM module WHERE nid = ?), ?, ?, ?)")
				.updateBatch(deadCodes.stream()
					.filter(deadCode -> ! deadCode.module.getNonNull().hasUid())
					.map(deadCode -> toStream(deadCode, false)), 1_000);
		}
	}

	private static Stream<Object> toStream(final ModuleDeadCodePojoPrototype deadCode, final boolean hasUid) {
		return Stream.<Object>of(
				hasUid ? deadCode.module.getNonNull().getUid() : deadCode.module.getNonNull().getNid(),
				deadCode.startingLine.get(),
				deadCode.numberOfLines.get(),
				deadCode.deadCode.get()
			);
	}
	
	/**
	 * Returns all {@code module_dead_code} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain DeadCodeInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain ModuleDeadCodePojo ModuleDeadCodePojos}
	 */
	public List<ModuleDeadCodePojo> findDeadCode(final BuildingConsumer<DeadCodeInquiryBuilder> builder) {
		return builder.prepare(new DeadCodeQueryBuilder())
				.build(null)
				.all();
	}

	/**
	 * Returns the number of {@code module_dead_code} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain DeadCodeInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of matching {@code statement} entities
	 */
	public long countDeadCode(final BuildingConsumer<DeadCodeInquiryBuilder> builder) {
		return builder.prepare(new DeadCodeQueryBuilder())
				.buildCount()
				.first(rs -> Long.valueOf(rs.getLong(1)))
				.orElse(Long.valueOf(0l)).longValue();
	}

	/**
	 * Queries for all {@code module} entities that match with the given list of {@code moduleUids} and puts for each match the: 
	 * <ul>
	 * <li>uids of contained modules and modules that have a relationship to a matching module as {@code relations}</li>
	 * </ul>
	 * into the returned {@link Table}. If {@code collectContaining} is {@code true} then additionally the:
	 * <ul>
	 * <li>module path as {@code path}</li>
	 * <li>containing module uid as {@code containing}</li>
	 * </ul>
	 * are put present into the returned {@link Table}.
	 *
	 * @param moduleUids {@link UUID UUIDs} of the modules to fetch
	 * @param collectContaining {@code true} if path and uid of the containing module must be fetched as well. Otherwise {@code false}
	 * @return {@link Table} containing the values for each module match
	 */
	public Optional<Table> findContainingAndReferencingModules(final Collection<UUID> moduleUids, final boolean collectContaining) {
		return Optional.ofNullable(query("SELECT ")
									.when(collectContaining, q -> q.append("m.path, (SELECT src from module_relationship WHERE type = 'CONTAINS' AND dst = m.uid) as containing, "))
									.append("array(SELECT r.dst from module_relationship r WHERE r.type = 'CONTAINS' AND r.src = m.uid UNION "
										  + "SELECT r.src FROM module_relationship r WHERE r.type != 'CONTAINS' AND r.dst = m.uid) as relations "
										  + "FROM module m WHERE m.uid = any(?)")
									.addArgs(arrayFromCollection(PgType.UUID, moduleUids))
									.build(TableBuilder::build));
	}

	/**
	 * Returns a container with a list of values of all {@code module} and {@code module_dead_code} entities of the given {@code project} for the discovery exporter.
	 *
	 * @param project the id of the project for which the data export is done
	 * @param sorted {@code true} if the values must be sorted. Otherwise {@code false}
	 * @return container with list of values
	 */
	public Optional<Table> getModuleDeadCode(final EntityId project, final boolean sorted) {
		final var query = query("SELECT m.nid, m.name, d.dead_code, d.starting_line, d.number_of_lines FROM module_dead_code d "
							  + "INNER JOIN module m ON d.module = m.uid WHERE m.project=")
							.with(ProjectPgDao.referenceUidOrNid(project));
		if (sorted) {
			query.append(" ORDER BY m.name, m.path, m.technology, m.type, d.starting_line");
		}

		final Map<String, FieldConverter> fieldConverters = new HashMap<>();
		fieldConverters.put("starting_line", ModulePgDao::defaultInteger);
		fieldConverters.put("number_of_lines", ModulePgDao::defaultInteger);

		return Optional.ofNullable(query.build(new Table.Builder(fieldConverters)::extract));
	}

	private static Integer defaultInteger(final Object val) {
		return val instanceof Integer ? (Integer) val : Integer.valueOf(-1);
	}

	/**
	 * Creates a new {@code dependency_definition} entity for each item in {@code code errorMarkers}.
	 *
	 * @param dependencyDefinitions the {@link DependencyDefinitionPojoPrototype DependencyDefinitionPojoPrototypes} to create
	 * @return the {@link UUID UUIDs} of the created dependency definitions
	 */
	public List<UUID> createDependencyDefinitions(final Collection<DependencyDefinitionPojoPrototype> dependencyDefinitions) {
		final List<UUID> uids = new ArrayList<>(dependencyDefinitions.size());
		query("INSERT INTO dependency_definition(id, module, attributes, binding_type, location, module_filters, type, resolution_flags, resolved, reached_from_modules) "
				+ "VALUES (?, coalesce(?, (SELECT uid FROM module WHERE nid = ?)), ?, ?, jsonb_populate_record(null::module_location, ?), ?, ?::module_relationship_type, ?, ?, ?)")
			.updateBatch(dependencyDefinitions.stream()
					.map(dependency -> {
						final UUID uid = dependency.id.orElseNonNull(UUID::randomUUID);
						uids.add(uid);
						return Stream.<Object>of(
								uid,
								dependency.module.optional().flatMap(EntityId::getUidOptional).orElse(null),
								dependency.module.optional().flatMap(EntityId::getNidOptional).orElse(null),
								PgJSON.toPGobject(dependency.attributes.orElse(Collections.emptyMap())),
								dependency.bindingType.getNonNull().name(),
								PgJSON.toPGobject(dependency.location.orElse(null)),
								PgJSON.toPGobjects(dependency.moduleFilters.get()),
								dependency.type.getNonNull().name(),
								PgUtil.arrayFromCollection(PgType.STRING, dependency.resolutionFlags.getNonNull().stream().map(ResolutionFlag::name).collect(Collectors.toList())),
								dependency.resolved.orElse(false),
								PgJSON.toPGobjects(dependency.reachedFromModules.orElse(null))
							);
					}), INSERT_BATCH_SIZE);
		return uids;
	}

	/**
	 * Sets the {@code resolved} property of the {@code dependency_definition} for the given {@code id}.
	 *
	 * @param id the id of the to be updated {@code dependency_definition}
	 * @param resolved the to be set resolved value
	 */
	public void setDependencyDefinitionResolved(final UUID id, final boolean resolved) {
		query("UPDATE dependency_definition SET resolved = ? WHERE id = ?")
			.addArgs(resolved, id)
			.update();
	}

	public int deleteDependencyDefinitions(final List<UUID> dependencyDefinitionIds) {
		return query("DELETE FROM dependency_definition WHERE id = any(?)")
				.addArgs(arrayFromCollection(PgType.UUID, dependencyDefinitionIds))
				.update();
	}

	/**
	 * Returns a list of module uids that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleRelationshipInquiryBuilder} containing the filter criteria and sort options
	 * @return number of deleted entities
	 */
	public List<EntityId> findDependencyDefinitionModuleIds(final BuildingConsumer<DependencyDefinitionInquiryBuilder> builder) {
		return builder.prepare(new DependencyDefinitionQueryBuilder())
				.buildModuleId(null)
				.all();
	}

	/**
	 * Returns all {@code dependency_definition} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain DependencyDefinitionInquiryBuilder} containing the filter criteria and sort options
	 * @return list of {@linkplain DependencyDefinitionPojo DependencyDefinitionPojos}
	 */
	public List<DependencyDefinitionPojo> findDependencyDefinitions(final BuildingConsumer<DependencyDefinitionInquiryBuilder> builder) {
		return builder.prepare(new DependencyDefinitionQueryBuilder())
				.build(null)
				.all();
	}

	/**
	 * Returns the number of {@code dependency_definition} entities that match with the filters in the given {@code builder}.
	 * @param builder the {@linkplain DependencyDefinitionInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of matching {@code dependency_definition} entities
	 */
	public long countDependencyDefinitions(final BuildingConsumer<DependencyDefinitionInquiryBuilder> builder) {
		return builder.prepare(new DependencyDefinitionQueryBuilder())
				.buildCount()
				.first(rs -> rs.getLong(1))
				.orElse(0l);
	}

	/**
	 * Returns the 'hot spots' containing the modules and their reference count for the given {@code filterType} and module filter in {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria
	 * @param filterType the filter type 
	 * @return list of module hot spots
	 */
	public List<HotSpot> findHotSpots(final BuildingConsumer<ModuleInquiryBuilder> builder, final FilterType filterType) {
		return builder.prepare(new ModuleQueryBuilder())
					  .buildHotspots(filterType);
	}

	/**
	 * Queries for all {@code module} invocation (destination modules in {@code module_relationship} entities) that match with the filters in the given
	 * {@code builder} and puts for each match the: 
	 * <ul>
	 * <li>unique IDs of the utility modules as {@code uid}</li>
	 * <li>numeric IDs of the utility modules as {@code nid}</li>
	 * <li>names of the utility modules as {@code name}</li>
	 * <li>relationship properties of the utility module invocations as {@code properties}</li>
	 * </ul>
	 * into the returned {@link Table}.
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options.
	 * @return {@link Table} containing the values for each utility module invocation
	 */
	public Optional<Table> findRelationshipInvocations(final BuildingConsumer<ModuleAggregationInquiryBuilder<?>> builder) {
		return Optional.ofNullable(builder.prepare(new ModuleAggregationQueryBuilder())
											.buildRelationshipInvocations()
											.build(TableBuilder::build));
	}

	/**
	 * Returns all {@code module} entities of type {@code 'SCHEMA'} for the given {@code project}.
	 *
	 * @param projectId the ID of the project
	 * @return list of {@linkplain SchemaInfoPojo SchemaInfoPojos}
	 */
	public List<SchemaInfoPojo> findSchemaInfos(final EntityId projectId) {
		return query("SELECT m.uid, m.nid, m.name, m.technology, "																		/* 1, 2, 3, 4 */
				/* fetch the count of all contained modules with type TABLE, VIEW and STORED_PROCEDURE */
				+ "(SELECT jsonb_object_agg(q.type, q.count) FROM (SELECT c.type, count(r.*) FROM module_relationship r "
				+ "INNER JOIN module c ON r.dst = c.uid "
				+ "WHERE r.src = m.uid AND r.type = 'CONTAINS' AND c.type = any('{TABLE, VIEW, STORED_PROCEDURE}') "
				+ "GROUP BY c.type) q) as child_counts, "																				/* 5 */
				
				/* fetch the count of all contained modules with type TRIGGER of all contained modules with type TABLE and VIEW */
				+ "(SELECT count(cr.*) FROM module_relationship pr "
				+ "INNER JOIN module pm ON pr.dst = pm.uid "
				+ "INNER JOIN module_relationship cr ON cr.src = pm.uid "
				+ "INNER JOIN module cm ON cr.dst = cm.uid "
				+ "WHERE pr.src = m.uid AND pr.type = 'CONTAINS' AND pm.type = any('{TABLE, VIEW}') AND cr.type = 'CONTAINS' AND cm.type = 'TRIGGER' "
				+ ") as triggers "																										/* 6 */
				
				+ "FROM module m WHERE m.type = 'SCHEMA' AND m.project = ").with(ProjectPgDao.referenceUidOrNid(projectId))
			.append(" ORDER BY m.name")
			.toList((rs, n) -> {
				@SuppressWarnings("unchecked")
				final Map<String, Integer> childCounts = PgJSON.fromJSON(rs.getString(5), Map.class);
				return new SchemaInfoPojo(
							(UUID) rs.getObject(1),											/* module uid */
							rs.getLong(2),													/* module nid */
							rs.getString(3),												/* module name */
							rs.getString(4),												/* module technology */
							childCounts.getOrDefault("TABLE", 0),							/* number of tables */
							childCounts.getOrDefault("VIEW", 0),							/* number of views */
							childCounts.getOrDefault("STORED_PROCEDURE", 0),				/* number of stored procedures */
							rs.getInt(6));													/* number of triggers */
			});
	}

	/**
	 * Creates a new {@code module_undiscovered} entity. The insert will not fail if the entity already exists.
	 *
	 * @param moduleUndiscovered the {@link ModuleUndiscoveredPojoPrototype} to create
	 */
	public void putUndiscovered(final ModuleUndiscoveredPojoPrototype moduleUndiscovered) {
		final FieldBuilder fields = new FieldBuilder()
			.add(moduleUndiscovered.project, "project", ProjectPgDao::referenceUidOrNid)
			.add(moduleUndiscovered.name, "name", "?")
			.add(moduleUndiscovered.path, "path", "?");

		query("INSERT INTO module_undiscovered ")
			.with(fields::buildUpsert)
			.updateOrThrow(() -> new PersistenceException("Failed to upsert module_undiscovered: " + moduleUndiscovered.toString()));
	}

	/**
	 * Returns all {@code module_undiscovered} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleUndiscoveredInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain ModuleUndiscoveredPojo ModuleUndiscoveredPojos}
	 */
	public List<ModuleUndiscoveredPojo> findUndiscovered(final BuildingConsumer<ModuleUndiscoveredInquiryBuilder> builder) {
		return builder.prepare(new ModuleUndiscoveredQueryBuilder())
				.build(null)
				.all();
	}

	/**
	 * Returns paged subset of optionally filtered {@code module_undiscovered} entities that match with the filters in the given {@code builder}.
	 * 
	 * @param paging Pagination specification.
	 * @param builder the {@linkplain ModuleUndiscoveredInquiryBuilder} containing the filter criteria and sort options.
	 * @return Paged subset of matching  {@linkplain ModuleUndiscoveredPojo ModuleUndiscoveredPojos}
	 */
	public Paged<ModuleUndiscoveredPojo> findUndiscovered(final Pagination paging, final BuildingConsumer<ModuleUndiscoveredInquiryBuilder> builder) {
		return builder.prepare(new ModuleUndiscoveredQueryBuilder())
				.build(paging)
				.page();
	}

	/**
	 * Returns the number of {@code module_undiscovered} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleUndiscoveredInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of matching {@code module_undiscovered} entities
	 */
	public long countUndiscovered(final BuildingConsumer<ModuleUndiscoveredInquiryBuilder> builder) {
		return builder.prepare(new ModuleUndiscoveredQueryBuilder())
				.buildCount()
				.first(rs -> Long.valueOf(rs.getLong(1)))
				.orElse(0l);
	}

	/**
	 * Deletes all {@code module_undiscovered} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleUndiscoveredInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of deleted modules
	 */
	public int deleteUndiscovered(final BuildingConsumer<ModuleUndiscoveredInquiryBuilder> builder) {
		return builder.prepare(new ModuleUndiscoveredQueryBuilder())
				.buildDelete()
				.update();
	}
}

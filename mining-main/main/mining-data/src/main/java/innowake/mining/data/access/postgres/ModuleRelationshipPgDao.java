/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.access.postgres;

import static innowake.mining.shared.access.FilterUtil.toEntityId;
import static innowake.mining.shared.access.FilterUtil.toEntityIds;
import static innowake.mining.shared.access.FilterUtil.toStrings;
import static innowake.mining.shared.access.FilterUtil.toUuid;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_EQ;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_IN;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_NOT_EQ;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_NOT_IN;

import java.sql.Array;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.persistence.PersistenceException;

import org.springframework.jdbc.core.JdbcTemplate;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService.ModuleRelationshipAggregationInquiryBuilder;
import innowake.mining.shared.access.ModuleService.ModuleRelationshipInquiryBuilder;
import innowake.mining.shared.access.ModuleService.RelationshipField;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.access.Table;
import innowake.mining.shared.access.Table.FieldConverter;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.entities.ModuleBasePojo;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.lang.EnumMapBuilder;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipFieldName;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Postgres specific access methods for the {@code module_relationship} entity.
 */
public class ModuleRelationshipPgDao extends PgDao {
	
	private static final Map<RelationshipField, String> relationshipFields = EnumMapBuilder.of(RelationshipField.class).<String>create()
			.put(RelationshipField.TYPE, "rel_type")
			.put(RelationshipField.DIRECTION, "is_src_or_dst")
			.put(RelationshipField.SOURCE, "src")
			.put(RelationshipField.DESTINATION, "dst")
			.put(RelationshipField.SOURCE_LOCATION, "src_location")
			.put(RelationshipField.DESTINATION_LOCATION, "dst_location")
		.build();
	public static final String DEAD_CODE = "dead_code";

	public class ModuleRelationshipQueryBuilder implements ModuleRelationshipInquiryBuilder {
		
		protected final FilterStreamBuilder filters = new FilterStreamBuilder();
		protected final OrderStreamBuilder order = new OrderStreamBuilder();
		@Nullable
		private Collection<RelationshipField> distinct;
		private boolean fetchSrcDetails;
		private boolean fetchDstDetails;
		@Nullable
		private EntityId baseModule;
		private int limit = -1;
		
		@SuppressWarnings("unchecked")
		private void fetchModuleDetails(final ResultReference<ModuleBasePojo> result, final String alias, final QueryBuilder q) {
			q.appendColumns(result, n -> rs -> new ModuleBasePojo(
					EntityId.of((UUID) rs.getObject(n), rs.getLong(n + 1)), rs.getString(n + 2),
					rs.getString(n + 3), Technology.valueOf(rs.getString(n + 4)), Type.valueOf(rs.getString(n + 5)),
					rs.getString(n + 6), mapNullable(rs.getString(n + 7), Representation::valueOf),
					rs.getBoolean(n + 8) ? Identification.IDENTIFIED : Identification.MISSING,
					PgJSON.fromPGobjectOrNull(rs.getObject(n + 9), Map.class), rs.getInt(n + 10)),
				alias + ".uid", alias + ".nid", alias + ".name",
				alias + ".path", alias + ".technology", alias + ".type",
				alias + ".link_hash", alias + ".representation", alias + ".identified", alias + ".info",
				"(SELECT count(*) FROM error_marker em WHERE em.module=" + alias + ".uid) as errors"
			);
		}
		
		@Nullable
		private String getDistinctFields() {
			final var fields = distinct;
			if (fields != null) {
				return fields.stream().map(relationshipFields::get).collect(Collectors.joining(", "));
			}
			return null;
		}
		
		protected Paged.Builder<ModuleRelationshipPojo> build(@Nullable final Pagination paging) {
			final var resultSrcMod = new ResultReference<ModuleBasePojo>();
			final var resultDstMod = new ResultReference<ModuleBasePojo>();
			return query("SELECT ")
				.when(getDistinctFields(), (q, f) -> q.append("DISTINCT ON (").append(f).append(") * FROM (SELECT "))
				.with(q -> q.appendColumns(
					"a.id",						/* 1 */
					"a.src",					/* 2 */
					"to_jsonb(a.src_location) src_location",	/* 3 */
					"a.dst",					/* 4 */
					"to_jsonb(a.dst_location) dst_location",	/* 5 */
					"a.type rel_type",			/* 6 */
					"a.properties",				/* 7 */
					"a.dependency_binding",		/* 8 */
					"a.dependency_attributes",	/* 9 */
					/* always fetch all assigned reached_from_module even when a filter for the reached_from_module was set */
					"array(SELECT reached_from_module FROM module_conditional_relationship WHERE module_relationship = a.id)",	/* 10 */
					"a.dependency_definition" 	/* 11 */
				))
				.with(q -> {
					final var module = baseModule;
					if (module != null) {
						q.appendColumn("CASE ");
						q.with(ModulePgDao.referenceUidOrNid(module));
						q.append(" WHEN src THEN true WHEN dst THEN false ELSE null END");	/* 12 */
					} else {
						q.appendColumn("null");
					}
					q.append(" is_src_or_dst");
				})
				.when(fetchSrcDetails, q -> fetchModuleDetails(resultSrcMod, "srcmod", q))
				.when(fetchDstDetails, q -> fetchModuleDetails(resultDstMod, "dstmod", q))
				.append(" FROM module_relationship a")
				/* JOINs on module AND dependency_definition.id, because we have an index on module only */
				.when(fetchSrcDetails, q -> q.append(" INNER JOIN module srcmod ON srcmod.uid = a.src"))
				.when(fetchDstDetails, q -> q.append(" INNER JOIN module dstmod ON dstmod.uid = a.dst"))
				.append(" LEFT JOIN module_conditional_relationship b ON b.module_relationship = a.id")
				.with(filters::build)
				.append(" GROUP BY a.id")
				.when(fetchSrcDetails, q -> q.append(", srcmod.uid"))
				.when(fetchDstDetails, q -> q.append(", dstmod.uid"))
				.with(order::build)
				.when(distinct != null, q -> q.append(") _distinct"))
				.when(limit > 0, q -> q.limit(limit))
				.toPageable(paging, (rs, row) -> new ModuleRelationshipPojo(
							(UUID) rs.getObject(1),													/* id UUID */
							(UUID) rs.getObject(2),													/* src module UUID */
							PgJSON.fromPGobjectOrNull(rs.getObject(3), ModuleLocation.class),		/* src location */
							(UUID) rs.getObject(4),													/* dst module UUID */
							PgJSON.fromPGobjectOrNull(rs.getObject(5), ModuleLocation.class),		/* dst location */
							RelationshipType.from(rs.getString(6)),									/* relationship type */
							mapNullable(rs.getObject("is_src_or_dst"), d -> (boolean) d ? RelationshipDirection.OUT : RelationshipDirection.IN),	/* relationship direction */
							PgJSON.fromPGobject(rs.getObject(7)),									/* relationship properties */
							mapNullable(rs.getString(8), Binding::fromName),						/* dependency binding */
							rs.getString(9),														/* dependency attributes */
							PgUtil.<UUID>streamArray(rs.getArray(10)).collect(Collectors.toList()),	/* conditional modules */
							resultSrcMod.get(rs),
							resultDstMod.get(rs),
							(UUID) rs.getObject(11)											/* dependency_definition UUID */
					));
		}

		protected List<UUID> buildIds() {
			return query("SELECT id FROM module_relationship a ")
					.with(filters::build)
					.when(limit > 0, q -> q.limit(limit))
					.toList((rs, row) -> (UUID) rs.getObject(1));
		}

		protected long buildCount() {
			return query("SELECT count(")
				.append(Optional.ofNullable(getDistinctFields()).map(s -> "DISTINCT " + s).orElse("*"))
				.append(") FROM module_relationship a ")
				.with(filters::build)
				.when(limit > 0, q -> q.limit(limit))
				.first(rs -> rs.getLong(1))
				.orElse(0l);
		}

		protected int buildDelete() {
			return query("DELETE FROM module_relationship a ")
					.with(filters::build)
					.when(limit > 0, q -> q.limit(limit))
					.update();
		}

		@Override
		public ModuleRelationshipQueryBuilder byId(final UUID id) {
			filters.accept(q -> q.append("id = ?", id));
			return this;
		}

		@Override
		public ModuleRelationshipQueryBuilder byIds(final Collection<UUID> ids) {
			filters.accept(q -> q.append("id = any(?)", PgUtil.arrayFromCollection(PgType.UUID, ids)));
			return this;
		}

		@Override
		public ModuleRelationshipInquiryBuilder ofProject(final EntityId project) {
			filters.accept(q -> q.append("src IN (SELECT uid FROM module WHERE project = ").with(ProjectPgDao.referenceUidOrNid(project)).append(")"));
			return this;
		}

		@Override
		public ModuleRelationshipQueryBuilder ofSource(final EntityId module) {
			filters.accept(q -> q.append("src = ").with(ModulePgDao.referenceUidOrNid(module)));
			return this;
		}

		@Override
		public ModuleRelationshipQueryBuilder withSourceType(final Type type) {
			filters.accept(q -> q.append("src IN (SELECT uid FROM module WHERE type = ?)", type.name()));
			return this;
		}

		@Override
		public ModuleRelationshipQueryBuilder ofSource(final UUID module) {
			filters.accept(q -> q.append("src = ?", module));
			return this;
		}

		@Override
		public ModuleRelationshipInquiryBuilder ofDestination(final UUID module) {
			filters.accept(q -> q.append("dst = ?", module));
			return this;
		}

		@Override
		public ModuleRelationshipQueryBuilder withDestinationType(final Type type) {
			filters.accept(q -> q.append("dst IN (SELECT uid FROM module WHERE type = ?)", type.name()));
			return this;
		}

		@Override
		public ModuleRelationshipQueryBuilder ofDestination(final EntityId module) {
			filters.accept(q -> q.append("dst = ").with(ModulePgDao.referenceUidOrNid(module)));
			return this;
		}

		@Override
		public ModuleRelationshipInquiryBuilder ofModuleInDirection(final UUID module, final RelationshipDirection direction) {
			switch (direction) {
				case IN:
					filters.accept(q -> q.append("dst = ?", module));
					break;
				case OUT:
					filters.accept(q -> q.append("src = ?", module));
					break;
				case BOTH:
					filters.accept(q -> q.append("(src = ? OR dst = ?)", module, module));
					break;
			}

			return this;
		}

		@Override
		public ModuleRelationshipInquiryBuilder ofModulesInDirection(final Collection<UUID> modules, final RelationshipDirection direction) {
			switch (direction) {
				case IN:
					filters.accept(q -> q.append("dst = any(?)", PgUtil.arrayFromCollection(PgType.UUID, modules)));
					break;
				case OUT:
					filters.accept(q -> q.append("src = any(?)", PgUtil.arrayFromCollection(PgType.UUID, modules)));
					break;
				case BOTH:
					final var uuids = PgUtil.arrayFromCollection(PgType.UUID, modules);
					filters.accept(q -> q.append("(src = any(?) OR dst = any(?))", uuids, uuids));
					break;
			}

			return this;
		}

		@Override
		public ModuleRelationshipInquiryBuilder ofModuleInDirection(final EntityId module, final RelationshipDirection direction) {
			switch (direction) {
				case IN:
					filters.accept(q -> q.append("dst = ").with(ModulePgDao.referenceUidOrNid(module)));
					break;
				case OUT:
					filters.accept(q -> q.append("src = ").with(ModulePgDao.referenceUidOrNid(module)));
					break;
				case BOTH:
					filters.accept(q -> q.append("(src = ").with(ModulePgDao.referenceUidOrNid(module))
							.append(" OR dst = ").with(ModulePgDao.referenceUidOrNid(module))
							.append(")"));
					break;
			}
			
			return this;
		}

		@Override
		public ModuleRelationshipInquiryBuilder ofDependencyDefinition(final UUID dependencyDefinitionId) {
			filters.accept(q -> q.append("dependency_definition = ?", dependencyDefinitionId));
			return this;
		}

		@Override
		public ModuleRelationshipQueryBuilder withSourceLocation(final ModuleLocation location) {
			filters.accept(q -> q.append("src_location = (?,?)", location.getOffset(), location.getLength()));
			return this;
		}
		
		@Override
		public ModuleRelationshipQueryBuilder withSourceLocationInRange(final ModuleLocation location) {
			if (location.getOffset() < 0) {
				throw new IllegalArgumentException("Location offset cannot be less than zero but found: " + location.getOffset());
			}
			
			if (location.getLength() < 0) {
				throw new IllegalArgumentException("Location length cannot be less than zero but found: " + location.getLength());
			}
			
			final Integer startOffset = location.getOffset();
			final Integer endOffset = startOffset + location.getLength() - 1;
			filters.accept(q -> q.append("((a.src_location).offset BETWEEN ? AND ? OR ((a.src_location).offset + (a.src_location).length) BETWEEN ? AND ?)",
					startOffset, endOffset, startOffset, endOffset));
			return this;
		}

		@Override
		public ModuleRelationshipQueryBuilder withDestinationLocation(final ModuleLocation location) {
			filters.accept(q -> q.append("dst_location = (?,?)", location.getOffset(), location.getLength()));
			return this;
		}
		
		@Override
		public ModuleRelationshipQueryBuilder withDestinationLocationInRange(final ModuleLocation location) {
			if (location.getOffset() < 0) {
				throw new IllegalArgumentException("Location offset cannot be less than zero but found: " + location.getOffset());
			}
			
			if (location.getLength() < 0) {
				throw new IllegalArgumentException("Location length cannot be less than zero but found: " + location.getLength());
			}
			
			final Integer startOffset = location.getOffset();
			final Integer endOffset = startOffset + location.getLength() - 1;
			filters.accept(q -> q.append("((a.dst_location).offset BETWEEN ? AND ? OR ((a.dst_location).offset + (a.dst_location).length) BETWEEN ? AND ?)",
					startOffset, endOffset, startOffset, endOffset));
			return this;
		}

		@Override
		public ModuleRelationshipQueryBuilder withDestinationOrigin(final Origin origin) {
			filters.accept(q -> q.append("dst IN (SELECT uid FROM module WHERE origin = ?)", origin.toString()));
			return this;
		}
		
		@Override
		public ModuleRelationshipQueryBuilder withType(final RelationshipType type) {
			filters.accept(q -> q.append("a.type = ?::module_relationship_type", type.toString()));
			return this;
		}

		@Override
		public ModuleRelationshipInquiryBuilder withTypes(final Collection<RelationshipType> types) {
			filters.accept(q -> q.append("a.type = any(?::module_relationship_type[])",
					arrayFromCollection(PgType.STRING, types.stream()
						.map(RelationshipType::name)
						.collect(Collectors.toList()))));
			return this;
		}

		@Override
		public ModuleRelationshipQueryBuilder withProperties(final String pattern) {
			filters.accept(q -> q.append("EXISTS (SELECT true FROM jsonb_object_keys(a.properties) x(a) WHERE a ILIKE ?)", pattern));
			return this;
		}

		@Override
		public ModuleRelationshipQueryBuilder withProperties(final Map<String, Object> properties) {
			filters.accept(q -> q.append("a.properties = ?", PgJSON.toPGobject(properties)));
			return this;
		}


		
		@Override
		public ModuleRelationshipQueryBuilder withProperties(final List<String> properties, final List<String> values, final boolean contain) {
			filters.accept(q -> {
				final var property = properties.iterator();
				while (property.hasNext()) {
					final var prop = property.next();
					final var valueIterator = values.iterator();
					while (valueIterator.hasNext()) {
						final var value = valueIterator.next();
						if (contain) {
							q.append("CAST (a.properties -> ? AS TEXT) ILIKE ?", prop, "%" + value + "%");
						} else {
							q.append("CAST (a.properties -> ? AS TEXT) = ?", prop, value);
						}
						if (valueIterator.hasNext()) {
							q.append(" OR ");
						}
					}
					if (property.hasNext()) {
						q.append(" OR ");
					}
				}
			});

			return this;
		}

		@Override
		public ModuleRelationshipQueryBuilder distinct(final Collection<RelationshipField> fields) {
			this.distinct = fields.isEmpty() ? null : fields;
			return this;
		}
		
		@Override
		public ModuleRelationshipQueryBuilder distinct(final RelationshipField... fields) {
			distinct(Arrays.asList(fields));
			return this;
		}

		@Override
		public ModuleRelationshipInquiryBuilder withConditionalDependencies(final Collection<EntityId> reachedFromModules) {
			final var uids = EntityId.allUids(reachedFromModules);
			if (uids.size() == reachedFromModules.size()) {
				filters.accept(q -> q.append("b.reached_from_module = any(?)", arrayFromCollection(PgType.UUID, uids)));
				return this;
			}
			final var nids = EntityId.allNids(reachedFromModules);
			if (nids.size() == reachedFromModules.size()) {
				filters.accept(q -> q.append("b.reached_from_module IN (SELECT uid FROM module WHERE nid = any(?))",
						arrayFromCollection(PgType.LONG, nids)));
				return this;
			}

			filters.accept(q -> q.append("(b.reached_from_module = any(?) OR b.reached_from_module IN (SELECT uid FROM module WHERE nid = any(?)))",
					arrayFromCollection(PgType.UUID, uids), arrayFromCollection(PgType.LONG, nids)));

			return this;
		}

		@Override
		public ModuleRelationshipQueryBuilder limit(final int limit) {
			if (limit < 1) {
				throw new IllegalArgumentException("Query limit must be greater than 0 but is: " + limit);
			}

			this.limit = limit;
			return this;
		}

		@Override
		public ModuleRelationshipInquiryBuilder includeModuleDetails(final boolean src, final boolean dst) {
			this.fetchSrcDetails = src;
			this.fetchDstDetails = dst;
			return this;
		}

		@Override
		public ModuleRelationshipInquiryBuilder sortNid(final SortDirection sortDirection) {
			fetchSrcDetails = true;
			order.accept(q -> q.appendOrder("srcmod.nid", sortDirection));
			return this;
		}

		@Override
		public ModuleRelationshipInquiryBuilder sortType(final SortDirection sortDirection) {
			order.accept(q -> q.appendOrder("a.type", sortDirection));
			return this;
		}

		@Override
		public ModuleRelationshipInquiryBuilder setBaseModule(@Nullable final EntityId module) {
			this.baseModule = module;
			return this;
		}

		@Override
		public ModuleRelationshipInquiryBuilder sortByRelationshipDirection(final SortDirection sortDirection) {
			order.accept(q -> q.appendOrder("is_src_or_dst", sortDirection));
			return this;
		}
	}

	public class ModuleRelationshipAggregationQueryBuilder extends AbstractAggregationQueryBuilder<RelationshipFieldName, ModuleRelationshipAggregationQueryBuilder> 
							implements ModuleRelationshipAggregationInquiryBuilder<ModuleRelationshipAggregationQueryBuilder> {

		protected boolean joinSrcModule;
		protected boolean joinDstModule;
		protected boolean joinModuleTaxonomies;
		
		@Override
		protected void buildJoins(final QueryBuilder qb) {
			qb.when(joinSrcModule || joinModuleTaxonomies, q -> q.append("INNER JOIN module src_mod ON src_mod.uid = rel.src "));
			qb.when(joinDstModule, q -> q.append("INNER JOIN module dst_mod ON dst_mod.uid = rel.dst "));
			qb.when(joinModuleTaxonomies, q -> q.append(" INNER JOIN module_taxonomies mtx on src_mod.uid = mtx.module"));
		}

		@Override
		protected String getFromClause() {
			return "module_relationship rel";
		}

		@Override
		protected String getFieldQueryFragment(final RelationshipFieldName field) {
			switch (field) {
				case ID:
					return "rel.id AS " + field.name().toLowerCase();
				case SRC_ID:
					return "rel.src AS " + field.name().toLowerCase();
				case DST_ID:
					return "rel.dst AS " + field.name().toLowerCase();
				case PROPERTY_DB_ACCESS_OPERATION:
					return "rel.properties -> 'DB_ACCESS_OPERATION' AS " + field.name().toLowerCase();
				case PROPERTY_DB_ACCESS_TYPE:
					return "rel.properties -> 'DB_ACCESS_TYPE' AS " + field.name().toLowerCase();
				case RELATIONSHIP:
					return "rel.type AS " + field.name().toLowerCase();
				case SRC_LINKHASH:
					joinSrcModule = true;
					return "src_mod.link_hash AS " + field.name().toLowerCase();
				case DST_LINKHASH:
					joinDstModule = true;
					return "dst_mod.link_hash AS " + field.name().toLowerCase();
				case SRC_STORAGE:
					joinSrcModule = true;
					return "src_mod.storage AS " + field.name().toLowerCase();
				case DST_STORAGE:
					joinDstModule = true;
					return "dst_mod.storage AS " + field.name().toLowerCase();
				case SRC_NAME:
					joinSrcModule = true;
					return "src_mod.name AS " + field.name().toLowerCase();
				case DST_NAME:
					joinDstModule = true;
					return "dst_mod.name AS " + field.name().toLowerCase();
				case SRC_PROJECT_ID:
					joinSrcModule = true;
					return "src_mod.project AS " + field.name().toLowerCase();
				case DST_PROJECT_ID:
					joinDstModule = true;
					return "dst_mod.project AS " + field.name().toLowerCase();
				case SRC_TECHNOLOGY:
					joinSrcModule = true;
					return "src_mod.technology AS " + field.name().toLowerCase();
				case DST_TECHNOLOGY:
					joinDstModule = true;
					return "dst_mod.technology AS " + field.name().toLowerCase();
				case SRC_TYPE:
					joinSrcModule = true;
					return "src_mod.type AS " + field.name().toLowerCase();
				case DST_TYPE:
					joinDstModule = true;
					return "dst_mod.type AS " + field.name().toLowerCase();
				default:
					throw new UnsupportedOperationException("The field is not supported yet: " + field.name());
			}
		}

		@Override
		public ModuleRelationshipAggregationQueryBuilder byId(final String operator, final Object value) {
			final var id = toUuid(value, "Id");
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("rel.id = ?", id));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("rel.id != ?", id));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for ID", operator));
			}
			return this;
		}

		@Override
		public ModuleRelationshipAggregationQueryBuilder withRelationship(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("rel.type = ?::module_relationship_type", value));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("rel.type != ?::module_relationship_type", value));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("rel.type = any(?::module_relationship_type[])", arrayFromCollection(PgType.STRING, values)));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT rel.type = any(?::module_relationship_type[])", values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for relationship", operator));
			}
			return this;
		}

		@Override
		public ModuleRelationshipAggregationQueryBuilder withPropertyDbType(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("rel.properties ->> 'DB_ACCESS_TYPE' = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("rel.properties ->> 'DB_ACCESS_TYPE' != ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("rel.properties ->> 'DB_ACCESS_TYPE' = any(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("rel.properties ->> 'DB_ACCESS_TYPE' != any(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for property db type", operator));
			}
			return this;
		}

		@Override
		public ModuleRelationshipAggregationQueryBuilder withPropertyDbOperation(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("rel.properties ->> 'DB_ACCESS_OPERATION' = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("rel.properties ->> 'DB_ACCESS_OPERATION' != ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("rel.properties ->> 'DB_ACCESS_OPERATION' = any(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("rel.properties ->> 'DB_ACCESS_OPERATION' != any(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for property db operation", operator));
			}
			return this;
		}

		@Override
		public ModuleRelationshipAggregationQueryBuilder withSrcId(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					var entity = toEntityId(value, "Source id");
					filters.accept(q -> q.append("rel.src = ", ModulePgDao.referenceUidOrNid(entity)));
					break;
				case OPERATOR_NOT_EQ:
					entity = toEntityId(value, "Source id");
					filters.accept(q -> q.append("NOT rel.src = ", ModulePgDao.referenceUidOrNid(entity)));
					break;
				case OPERATOR_IN:
					var values = toEntityIds(value, "Source ids");
					filters.accept(q -> q.appendIds(values, "rel.src = any(?)", "rel.src = any(SELECT uid FROM module WHERE nid = any(?))"));
					break;
				case OPERATOR_NOT_IN:
					values = toEntityIds(value, "Source ids");
					filters.accept(q -> q.append("NOT (")
										 .appendIds(values, "rel.src = any(?)", "rel.src = any(SELECT uid FROM module WHERE nid = any(?))")
										 .append(")"));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for src id", operator));
			}
			return this;
		}

		@Override
		public ModuleRelationshipAggregationQueryBuilder withDstId(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					var entity = toEntityId(value, "Destination id");
					filters.accept(q -> q.append("rel.dst = ", ModulePgDao.referenceUidOrNid(entity)));
					break;
				case OPERATOR_NOT_EQ:
					entity = toEntityId(value, "Destination id");
					filters.accept(q -> q.append("NOT rel.dst = ", ModulePgDao.referenceUidOrNid(entity)));
					break;
				case OPERATOR_IN:
					var values = toEntityIds(value, "Destination ids");
					filters.accept(q -> q.appendIds(values, "rel.dst = any(?)", "rel.dst = any(SELECT uid FROM module WHERE nid = any(?))"));
					break;
				case OPERATOR_NOT_IN:
					values = toEntityIds(value, "Destination ids");
					filters.accept(q -> q.append("NOT (")
										 .appendIds(values, "rel.dst = any(?)", "rel.dst = any(SELECT uid FROM module WHERE nid = any(?))")
										 .append(")"));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for dst id", operator));
			}
			return this;
		}

		@Override
		public ModuleRelationshipAggregationQueryBuilder withSrcName(final String operator, final Object value) {
			joinSrcModule = true;
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("src_mod.name = ?", value));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("NOT src_mod.name = ?", value));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("src_mod.name = any(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT src_mod.name = any(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for src name", operator));
			}
			return this;
		}

		@Override
		public ModuleRelationshipAggregationQueryBuilder withDstName(final String operator, final Object value) {
			joinDstModule = true;
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("dst_mod.name = ?", value));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("NOT dst_mod.name = ?", value));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("dst_mod.name = any(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("dst_mod.name = any(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for dst name", operator));
			}
			return this;
		}

		@Override
		public ModuleRelationshipAggregationQueryBuilder withSrcProjectId(final String operator, final Object value) {
			joinSrcModule = true;
			switch (operator) {
				case OPERATOR_EQ:
					var entity = toEntityId(value, "Source project id");
					filters.accept(q -> q.append("src_mod.project = ")
										 .with(ProjectPgDao.referenceUidOrNid(entity)));
					break;
				case OPERATOR_NOT_EQ:
					entity = toEntityId(value, "Source project id");
					filters.accept(q -> q.append("NOT src_mod.project = ")
										 .with(ProjectPgDao.referenceUidOrNid(entity)));
					break;
				case OPERATOR_IN:
					var values = toEntityIds(value, "Source project ids");
					filters.accept(q -> q.appendIds(values, "src_mod.project = any(?)", "src_mod.project IN (SELECT uid FROM project where nid = any(?))"));
					break;
				case OPERATOR_NOT_IN:
					values = toEntityIds(value, "Source project ids");
					filters.accept(q -> q.append("NOT (")
										 .appendIds(values, "src_mod.project = any(?))", "src_mod.project IN (SELECT uid FROM project where nid = any(?))")
										 .append(")"));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for src project id", operator));
			}
			return this;
		}

		@Override
		public ModuleRelationshipAggregationQueryBuilder withDstProjectId(final String operator, final Object value) {
			joinDstModule = true;
			switch (operator) {
				case OPERATOR_EQ:
					var entity = toEntityId(value, "Destination project id");
					filters.accept(q -> q.append("dst_mod.project = ")
										 .with(ProjectPgDao.referenceUidOrNid(entity)));
					break;
				case OPERATOR_NOT_EQ:
					entity = toEntityId(value, "Destination project id");
					filters.accept(q -> q.append("NOT dst_mod.project = ")
										 .with(ProjectPgDao.referenceUidOrNid(entity)));
					break;
				case OPERATOR_IN:
					var values = toEntityIds(value, "Destination project ids");
					filters.accept(q -> q.appendIds(values, "dst_mod.project = any(?)", "dst_mod.project IN (SELECT uid FROM project where nid = any(?))"));
					break;
				case OPERATOR_NOT_IN:
					values = toEntityIds(value, "Destination project ids");
					filters.accept(q -> q.append("NOT (")
										 .appendIds(values, "dst_mod.project = any(?)", "dst_mod.project IN (SELECT uid FROM project where nid = any(?))")
										 .append(")"));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for dst project id", operator));
			}
			return this;
		}

		@Override
		public ModuleRelationshipAggregationQueryBuilder withSrcTechnology(final String operator, final Object value) {
			joinSrcModule = true;
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("src_mod.technology = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("src_mod.technology != ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("src_mod.technology = any(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT src_mod.technology = any(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for src technology", operator));
			}
			return this;
		}

		@Override
		public ModuleRelationshipAggregationQueryBuilder withDstTechnology(final String operator, final Object value) {
			joinDstModule = true;
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("dst_mod.technology = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("dst_mod.technology != ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("dst_mod.technology = any(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT dst_mod.technology = any(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for dst technology", operator));
			}
			return this;
		}

		@Override
		public ModuleRelationshipAggregationQueryBuilder withSrcType(final String operator, final Object value) {
			joinSrcModule = true;
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("src_mod.type = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("src_mod.type != ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("src_mod.type = any(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT src_mod.type = any(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for src type", operator));
			}
			return this;
		}

		@Override
		public ModuleRelationshipAggregationQueryBuilder withDstType(final String operator, final Object value) {
			joinDstModule = true;
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("dst_mod.type = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("dst_mod.type != ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("dst_mod.type = any(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT dst_mod.type = any(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for dst type", operator));
			}
			return this;
		}

		@Override
		public ModuleRelationshipAggregationQueryBuilder withSrcLinkhash(final String operator, final Object value) {
			joinSrcModule = true;
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("src_mod.link_hash = ?", value));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("src_mod.link_hash != ?", value));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("src_mod.link_hash = any(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT src_mod.link_hash = any(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for src linkhash", operator));
			}
			return this;
		}

		@Override
		public ModuleRelationshipAggregationQueryBuilder withDstLinkhash(final String operator, final Object value) {
			joinDstModule = true;
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("dst_mod.link_hash = ?", value));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("dst_mod.link_hash != ?", value));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("dst_mod.link_hash = any(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT dst_mod.link_hash = any(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for dst linkhash", operator));
			}
			return this;
		}

		@Override
		public ModuleRelationshipAggregationQueryBuilder withSrcStorage(final String operator, final Object value) {
			joinSrcModule = true;
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("src_mod.storage = ?", value));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("src_mod.storage != ?", value));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("src_mod.storage = any(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT src_mod.storage = any(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for src storage", operator));
			}
			return this;
		}

		@Override
		public ModuleRelationshipAggregationQueryBuilder withDstStorage(final String operator, final Object value) {
			joinDstModule = true;
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("dst_mod.storage = ?", value));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("dst_mod.storage != ?", value));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("dst_mod.storage = any(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT dst_mod.storage = any(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for src storage", operator));
			}
			return this;
		}

		@Override
		public ModuleRelationshipAggregationQueryBuilder withTaxonomy(final String operator, final Object value) {
			joinModuleTaxonomies = true;

			switch (operator) {
				case OPERATOR_EQ:
					EntityId entity = toEntityId(value, "Taxonomy id");
					filters.accept(q -> q.append("mtx.taxonomy = ").with(TaxonomyPgDao.referenceUidOrNid(entity)));
					break;
				case OPERATOR_NOT_EQ:
					entity = toEntityId(value, "Taxonomy id");
					filters.accept(q -> q.append("NOT mtx.taxonomy = ").with(TaxonomyPgDao.referenceUidOrNid(entity)));
					break;
				case OPERATOR_IN:
					var values = toEntityIds(value, "Taxonomy ids");
					filters.accept(q -> q.appendIds(values, "mtx.taxonomy = ANY(?)", "mtx.taxonomy IN (SELECT taxonomy.uid FROM taxonomy WHERE taxonomy.nid = ANY(?))"));
					break;
				case OPERATOR_NOT_IN:
					values = toEntityIds(value, "Taxonomy ids");
					filters.accept(q -> q.append("NOT (")
										 .appendIds(values, "mtx.taxonomy = ANY(?)", "mtx.taxonomy IN (SELECT taxonomy.uid FROM taxonomy WHERE taxonomy.nid = ANY(?))")
										 .append(")"));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for module", operator));
			}

			return this;
		}
	}

	public ModuleRelationshipPgDao(final JdbcTemplate jdbcTemplate) {
		super(jdbcTemplate);
	}

	/**
	 * Creates a new {@code module_relationship} entity.
	 *
	 * @param moduleReference the {@link ModuleRelationshipPojoPrototype} to create
	 * @return the {@link UUID} of the created {@code module_relationship}.
	 */
	public UUID create(final ModuleRelationshipPojoPrototype moduleReference) {
		final UUID id = moduleReference.id.orElseNonNull(UUID::randomUUID);

		final var fields = new FieldBuilder()
			.add("id", "?", id)
			.add("src", q -> q.with(ModulePgDao.referenceUidOrNid(moduleReference.srcModule.getNonNull())))
			.add("dst", q -> q.with(ModulePgDao.referenceUidOrNid(moduleReference.dstModule.getNonNull())))
			.add(moduleReference.type.required(true) , "type", "?::module_relationship_type", RelationshipType::name)
			.add(moduleReference.srcLocation, "src_location", ModulePgDao::appendLocation)
			.add(moduleReference.dstLocation, "dst_location", ModulePgDao::appendLocation)
			.add(moduleReference.properties, "properties", "?", PgJSON::toPGobject)
			.add(moduleReference.dependencyBinding, "dependency_binding", "?", Binding::name)
			.add(moduleReference.dependencyAttributes, "dependency_attributes", "?")
			.add(moduleReference.dependencyDefinition, "dependency_definition", "?::uuid");

		final var fieldNames = new StringBuilder();
		fields.buildColumns(fieldNames);
		final QueryBuilder query = query("INSERT INTO module_relationship (");
		query.append(fieldNames.toString()).append(") SELECT v.* FROM (VALUES (");
		fields.buildFields(query);
		query.append(")) v(").append(fieldNames.toString())
			.append(") WHERE (SELECT project FROM module WHERE uid = src) = (SELECT project FROM module WHERE uid = dst)");

		return query.append(" RETURNING id")
					.first(rs -> (UUID) rs.getObject(1))
					.orElseThrow(PersistenceException::new);
	}
	
	/**
	 * Updates the from_dead_code flag for all {@code module_relationship} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleRelationshipInquiryBuilder} containing the filter criteria and sort options.
	 * @param value the value to set from dead code to.
	 * @return number of updated entities
	 */
	public int setFromDeadCode(final BuildingConsumer<ModuleRelationshipInquiryBuilder> builder, final boolean value) {
		final var filter = builder.prepare(new ModuleRelationshipQueryBuilder());
		final var query = String.format("UPDATE module_relationship a SET properties['%s'] = to_jsonb(?)", DEAD_CODE);
		return query(query)
				.addArg(value)
				.with(filter.filters::build)
				.update();
	}	

	/**
	 * Deletes all {@code module_relationship} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleRelationshipInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of deleted entities
	 */
	public int delete(final BuildingConsumer<ModuleRelationshipInquiryBuilder> builder) {
		return builder.prepare(new ModuleRelationshipQueryBuilder())
				.buildDelete();
	}

	/**
	 * Returns all {@code module_relationship} entities that match with the filters in the given {@code builder}.
	 *
	 * @param paging Pagination specification.
	 * @param builder the {@linkplain ModuleRelationshipInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain ModuleRelationshipPojo ModuleRelationshipPojos}
	 */
	public Paged<ModuleRelationshipPojo> find(final Pagination paging, final BuildingConsumer<ModuleRelationshipInquiryBuilder> builder) {
		return builder.prepare(new ModuleRelationshipQueryBuilder())
						.build(paging)
						.page();
	}

	/**
	 * Returns all {@code module_relationship} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleRelationshipInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain ModuleRelationshipPojo ModuleRelationshipPojos}
	 */
	public List<ModuleRelationshipPojo> find(final BuildingConsumer<ModuleRelationshipInquiryBuilder> builder) {
		return builder.prepare(new ModuleRelationshipQueryBuilder())
						.build(null)
						.all();
	}

	/**
	 * Returns the first {@code module_relationship} entity that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleRelationshipInquiryBuilder} containing the filter criteria and sort options.
	 * @return {@linkplain ModuleRelationshipPojo}
	 */
	public Optional<ModuleRelationshipPojo> findAnyRelationship(final BuildingConsumer<ModuleRelationshipInquiryBuilder> builder) {
		return builder.prepare(new ModuleRelationshipQueryBuilder()
										.limit(1))
				.build(null)
				.first();
	}

	/**
	 * Returns the {@linkplain UUID UUIDs} of all {@code module_relationship} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleRelationshipInquiryBuilder} containing the filter criteria and sort options
	 * @return list of {@linkplain UUID UUIDs}
	 */
	public List<UUID> findIds(final BuildingConsumer<ModuleRelationshipInquiryBuilder> builder) {
		return builder.prepare(new ModuleRelationshipQueryBuilder())
				.buildIds();
	}

	/**
	 * Returns the number of {@code module_relationship} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleRelationshipInquiryBuilder} containing the filter criteria and sort options
	 * @return number of matching {@code module_relationship} entities
	 */
	public long count(final BuildingConsumer<ModuleRelationshipInquiryBuilder> builder) {
		return builder.prepare(new ModuleRelationshipQueryBuilder())
				.buildCount();
	}

	/**
	 * Returns aggregation {@code module_relationship} values for the filters and selected aggregations in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleRelationshipAggregationInquiryBuilder} containing the aggregation operations and filter criteria
	 * @return container with the aggregation values
	 */
	public Optional<Table> getRelationshipAggregations(final BuildingConsumer<ModuleRelationshipAggregationInquiryBuilder<?>> builder) {
		return builder.prepare(new ModuleRelationshipAggregationQueryBuilder())
						.buildAggregation(this);
	}

	/**
	 * Queries for all {@code module_relationship} entities that match with the given {@code project} and puts for each match for the source module the: 
	 * <ul>
	 * <li>numeric id as {@code src_nid}</li>
	 * <li>name as {@code src_name}</li>
	 * <li>technology as {@code technology}</li>
	 * </ul>
	 * for the destination module the:
	 * <ul>
	 * <li>numeric id as {@code dst_nid}</li>
	 * <li>name as {@code dst_name}</li>
	 * </ul>
	 * and for the relationship the:
	 * <ul>
	 * <li>dependency binding as {@code dependency_binding}</li>
	 * <li>dependency attributes as {@code dependency_attributes}</li>
	 * <li>source location as {@code src_location}</li>
	 * <li>destination location as {@code dst_location}</li>
	 * <li>relationship type as {@code type}</li>
	 * <li>array of reaching module UUIDs as {@code reached_from_module}</li>
	 * </ul>
	 * into the returned {@link Table}.
	 *
	 * @param project the id of the project for which the data export is done
	 * @param sorted {@code true} if the values must be sorted. Otherwise {@code false}
	 * @return {@link Table} containing the values for each relationship match
	 */
	public Optional<Table> getModuleRelationshipExport(final EntityId project, final boolean sorted) {
		final var query = query("SELECT src.nid AS src_nid, src.name AS src_name, dst.nid AS dst_nid, dst.name AS dst_name, dst.technology AS dst_technology,"
								+ "r.dependency_binding, r.dependency_attributes, to_jsonb(r.src_location) src_location, to_jsonb(r.dst_location) dst_location, r.type, "
								/* reached_from_module nids */
								+ "array(SELECT nid FROM module WHERE uid IN (SELECT reached_from_module FROM module_conditional_relationship WHERE "
									+ "module_relationship = r.id)) as reached_from_module "
								+ "FROM module_relationship r "
								+ "INNER JOIN module src ON src.uid = r.src "
								+ "INNER JOIN module dst ON dst.uid = r.dst "
								+ "WHERE src.project = ")
							.with(ProjectPgDao.referenceUidOrNid(project))
							.append(" AND r.type = any(?::module_relationship_type[])")
							.addArg(PgType.STRING, RelationshipType.DEPENDENCY_TYPES);

		if (sorted) {
			query.append(" ORDER BY src.name, src.path, src.technology, src.type, dst.name");
		}

		final Map<String, FieldConverter> fieldConverters = new HashMap<>();
		fieldConverters.put("src_location", val -> PgJSON.fromPGobjectOrNull(val, ModuleLocation.class));
		fieldConverters.put("dst_location", val -> PgJSON.fromPGobjectOrNull(val, ModuleLocation.class));
		fieldConverters.put("reached_from_module", val -> {
			try {
				return PgUtil.<Long>streamArray((Array) val).collect(Collectors.toList());
			} catch (final SQLException e) {
				throw new IllegalStateException(e);
			}
		});

		return Optional.ofNullable(query.build(new Table.Builder(fieldConverters)::extract));
	}
	
	/**
	 * Return IDs of all the source Module which have given relationship with the input destination module
	 *
	 * @param type of Module relationship
	 * @param dstModuleId destination module ID
	 * @return List of Source module IDs
	 */
	public List<Long> getSrcModuleIdsByRelationshipTypeAndDstId(final RelationshipType type, final UUID dstModuleId) {
		return query("SELECT nid AS src_nid FROM module WHERE uid IN (SELECT src FROM module_relationship WHERE type = ?::module_relationship_type")
				.addArg(type.name())
				.append(" AND dst = ? )")
				.addArg(dstModuleId)
				.toList((rs, row) -> rs.getLong("src_nid"));
	}

	public List<Tuple2<ModuleRelationshipPojo, Long>> findRelationshipsWithDepthRecursive(final EntityId moduleId, final Optional<Integer> limit,
			final Long maxDepth, final boolean singleRelationBetweenModules, final boolean dontFilterLeafRelations) {
		//singleRelationBetweenModules: if moduleA references moduleB twice, still return only one result
		final String distinctString = singleRelationBetweenModules ? " ON (src, dst, type) " : "";
		//group by and select all except the depth, as we want the MIN(depth) to find the first occurence
		final String groupByAndSelect = "r.id, r.src, r.src_location, r.dst, r.dst_location, r.type, r.properties, r.dependency_binding," +
				" r.dependency_attributes, r.array, r.dependency_definition";

		//Select all module_relationships from a given module in both directions, limit by depth and size

		final String whereClause = dontFilterLeafRelations ? "" : "WHERE (r.depth = " + maxDepth
				+ " OR r.dst IN (SELECT src FROM relationships) "
				+ " OR r.src IN (SELECT dst FROM relationships))"
				+ " AND r.type IN ('INCLUDES', 'REFERENCES', 'CALLS', 'ACCESSES') ";
		return query("WITH RECURSIVE relationships AS ( "
					+ "SELECT DISTINCT " + distinctString + " id, "																			/* 1 */
						+ "src, "																											/* 2 */
						+ "to_jsonb(src_location) AS src_location, "																		/* 3 */
						+ "dst, "																											/* 4 */
						+ "to_jsonb(dst_location) AS dst_location, "																		/* 5 */
						+ "type, "																											/* 6 */
						+ "properties, "																									/* 7 */
						+ "dependency_binding, "																							/* 8 */
						+ "dependency_attributes, "																							/* 9 */
						/* always fetch all assigned reached_from_module even when a filter for the reached_from_module was set */
						+ "array(SELECT reached_from_module FROM module_conditional_relationship WHERE module_relationship = a.id), "		/* 10 */
						+ "dependency_definition, " 																						/* 11 */
						+ "1 as depth "
					+ " FROM module_relationship a "
					+ " WHERE src = ").with(ModulePgDao.referenceUidOrNid(moduleId))
				.append(" OR dst = ").with(ModulePgDao.referenceUidOrNid(moduleId))
				.append(" AND type IN ('INCLUDES', 'REFERENCES', 'CALLS', 'ACCESSES')")
				.append(" UNION "
							+ "SELECT DISTINCT " + distinctString + " m.id, "																/* 1 */
							+ "m.src, "																										/* 2 */
							+ "to_jsonb(m.src_location) AS src_location, "																	/* 3 */
							+ "m.dst, "																										/* 4 */
							+ "to_jsonb(m.dst_location) AS dst_location, "																	/* 5 */
							+ "m.type, "																									/* 6 */
							+ "m.properties, "																								/* 7 */
							+ "m.dependency_binding, "																						/* 8 */
							+ "m.dependency_attributes, "																					/* 9 */
							/* always fetch all assigned reached_from_module even when a filter for the reached_from_module was set */
							+ "array(SELECT reached_from_module FROM module_conditional_relationship WHERE module_relationship = m.id), "	/* 10 */
							+ "m.dependency_definition, " 																					/* 11 */
							+ "(u.depth + 1) "
						+ "FROM module_relationship m "
						+ "JOIN relationships u ON u.src IN (m.dst, m.src) OR u.dst IN (m.dst, m.src) "
						+ "WHERE u.depth < ?) "
					+ "SELECT DISTINCT " + distinctString + " "
						+ groupByAndSelect + ", MIN(r.depth) as depth FROM relationships r "
						+ whereClause
						+ "GROUP BY " + groupByAndSelect)
				.addArg(maxDepth)
				.when(limit.isPresent(), q -> q.append(" LIMIT ?", limit.get()))
				.toList((rs, row) -> {
					final var pojo = new ModuleRelationshipPojo(
							(UUID) rs.getObject(1),                                                  /* id UUID */
							(UUID) rs.getObject(2),                                                  /* src module UUID */
							PgJSON.fromPGobjectOrNull(rs.getObject(3), ModuleLocation.class),        /* src location */
							(UUID) rs.getObject(4),                                                  /* dst module UUID */
							PgJSON.fromPGobjectOrNull(rs.getObject(5), ModuleLocation.class),        /* dst location */
							RelationshipType.from(rs.getString(6)),                                  /* relationship type */
							null,                                                                    			 /* relationship direction */
							PgJSON.fromPGobject(rs.getObject(7)),                                    /* reference properties */
							mapNullable(rs.getString(8), Binding::fromName),                         /* dependency binding */
							rs.getString(9),                                                         /* dependency attributes */
							PgUtil.<UUID>streamArray(rs.getArray(10)).collect(Collectors.toList()),  	 /* conditional modules */
							null,
							null,
							(UUID) rs.getObject(11)                                                 /* dependency_definition UUID */
					);
					final long depth = rs.getLong("depth");
					return new Tuple2<>(pojo, depth);
				});
	}

	public void putConditionalModules(final UUID relation, final List<EntityId> fromModules) {
		query("INSERT INTO module_conditional_relationship (module_relationship, reached_from_module)"
				+ " VALUES (?, coalesce(?, (SELECT uid FROM module WHERE nid = ?))) ON CONFLICT DO NOTHING").addArg(relation)
			.updateBatch(fromModules.stream().map(m -> Stream.<Object>of(m.getUidOptional().orElse(null), m.getNidOptional().orElse(null))), 1000);
	}
	
	/**
	 * Updates the properties of {@code module_relationship} entity.
	 * 
	 * @param properties the properties of the module relationship
	 * @param dependencyAttributes the dependency attributes of the module relationship
	 * @param id the id of the to be updated {@code module_relationship}
	 * @return number of updated entities
	 */
	public int updateProperties(final Map<String, Object> properties, final String dependencyAttributes, final UUID id) {
		return query("UPDATE module_relationship SET properties = ?, dependency_attributes = ? WHERE id = ?")
				.addArgs(PgJSON.toPGobject(properties), dependencyAttributes, id)
				.update();
	}
}

/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.access.postgres;

import static innowake.mining.shared.access.FilterUtil.toEntityId;
import static innowake.mining.shared.access.FilterUtil.toEntityIds;
import static innowake.mining.shared.access.FilterUtil.toNumber;
import static innowake.mining.shared.access.FilterUtil.toStrings;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_EQ;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_GT;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_GTE;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_IN;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_LT;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_LTE;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_NOT_EQ;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_NOT_IN;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.springframework.jdbc.core.JdbcTemplate;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ModuleService.StatementAggregationInquiryBuilder;
import innowake.mining.shared.access.ModuleService.StatementInquiryBuilder;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.Table;
import innowake.mining.shared.entities.StatementPojo;
import innowake.mining.shared.entities.StatementPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.StatementFieldName;
import innowake.mining.shared.model.StatementType;
import innowake.mining.shared.model.Technology;

/**
 * Postgres specific access methods for the {@code statement} entity.
 */
public class StatementPgDao extends PgDao {

	private static String getSqlFragment(final StatementFieldName fieldName) {
		switch (fieldName) {
			case ID:
				return "stm.uid AS " + fieldName.name().toLowerCase();
			case PROJECT_ID:
				return "(SELECT project FROM module WHERE uid = module) AS " + fieldName.name().toLowerCase();
			case MODULE_ID:
				return "stm.module AS " + fieldName.name().toLowerCase();
			case TAXONOMY_ID:
				return "mtx.taxonomy AS " + fieldName.name().toLowerCase();
			case TECHNOLOGY:
				return "stm.technology AS " + fieldName.name().toLowerCase();
			case STATEMENT_TYPE:
				return "stm.type AS " + fieldName.name().toLowerCase();
			case TEXT:
				return "stm.text AS " + fieldName.name().toLowerCase();
			case TEXT_LENGTH:
				return "LENGTH(stm.text) AS " + fieldName.name().toLowerCase();
			case CUSTOM_COMPLEXITY:
				return "((stm.properties->>'customComplexity')::integer) AS " + fieldName.name().toLowerCase();
			case DISTINCT_TABLES:
				return "((stm.properties->>'distinctTables')::integer) AS " + fieldName.name().toLowerCase();
			case HALSTEAD_COMPLEXITY:
				return "((stm.properties->>'halsteadComplexity')::float) AS " + fieldName.name().toLowerCase();
			case HALSTEAD_DIFFICULTY:
				return "((stm.properties->>'halsteadDifficulty')::float) AS " + fieldName.name().toLowerCase();
			case SQL_LENGTH:
				return "((stm.properties->>'sqlLength')::integer) AS " + fieldName.name().toLowerCase();
			case TABLES:
				return "((stm.properties->>'tables')::integer) AS " + fieldName.name().toLowerCase();
			default:
				throw new UnsupportedOperationException("The field name is not supported yet: " + fieldName);
		}
	}

	public class StatementQueryBuilder extends StatementsAggregationQueryBuilder implements StatementInquiryBuilder {

		@SuppressWarnings("unchecked")
		protected Paged.Builder<StatementPojo> build(@Nullable final Pagination paging) {
			return query("SELECT stm.uid, stm.nid, stm.module, stm.technology, stm.type, stm.text, stm.properties FROM statement stm")
					.with(this::buildJoins)
					.with(filters::build)
					.with(orderBys::build)
					.toPageable(paging,
							(rs, row) -> new StatementPojo((UUID) rs.getObject(1),
														   Long.valueOf(rs.getLong(2)),
														   (UUID) rs.getObject(3),
														   rs.getString(4),
														   rs.getString(5),
														   rs.getString(6),
														   PgJSON.fromPGobjectOrNull(rs.getObject(7), Map.class)));
		}

		protected long buildCount() {
			return query("SELECT count(*) FROM statement stm")
					.with(this::buildJoins)
					.with(filters::build)
					.first(rs -> Long.valueOf(rs.getLong(1)))
					.orElse(Long.valueOf(0l))
					.longValue();
		}

		protected int buildDelete() {
			return query("DELETE FROM statement stm")
					.with(filters::build)
					.update();
		}

		@Override
		public StatementQueryBuilder ofProject(final EntityId project) {
			filters.accept(q -> q.append("stm.module IN (SELECT uid from module WHERE project = ").with(ProjectPgDao.referenceUidOrNid(project)).append(")"));
			return this;
		}

		@Override
		public StatementQueryBuilder ofModule(final EntityId module) {
			filters.accept(q -> q.append("stm.module = ").with(ModulePgDao.referenceUidOrNid(module)));
			return this;
		}

		@Override
		public StatementInquiryBuilder ofTaxonomy(final EntityId taxonomy) {
			joinModuleTaxonomies = true;
			filters.accept(q -> q.append("mtx.taxonomy = ").with(TaxonomyPgDao.referenceUidOrNid(taxonomy)));
			return this;
		}

		@Override
		public StatementInquiryBuilder ofTaxonomies(final Collection<EntityId> taxonomies) {
			joinModuleTaxonomies = true;
			final var uids = EntityId.allUids(taxonomies);
			if (uids.size() == taxonomies.size()) {
				filters.accept(q -> q.append("mtx.taxonomy = any(?)", arrayFromCollection(PgType.UUID, uids)));
				return this;
			}
			final var nids = EntityId.allNids(taxonomies);
			if (nids.size() == taxonomies.size()) {
				filters.accept(q -> q.append("mtx.taxonomy IN (SELECT uid FROM taxonomy WHERE nid = any(?))",
								arrayFromCollection(PgType.LONG, nids)));
				return this;
			}

			filters.accept(q -> q.append("mtx.taxonomy = any(?) OR mtx.taxonomy IN (SELECT uid FROM taxonomy WHERE nid = any(?))",
								arrayFromCollection(PgType.UUID, uids), arrayFromCollection(PgType.LONG, nids)));
			return this;
		}

		@Override
		public StatementInquiryBuilder withTechnology(final Technology technology) {
			filters.accept(q -> q.append("stm.technology = ?", technology.toString()));
			return this;
		}

		@Override
		public StatementInquiryBuilder notWithTechnology(final Technology technology) {
			filters.accept(q -> q.append("stm.technology != ?", technology.toString()));
			return this;
		}

		@Override
		public StatementInquiryBuilder withType(final StatementType type) {
			filters.accept(q -> q.append("stm.type = ?", type.toString()));
			return this;
		}

		@Override
		public StatementInquiryBuilder withTypes(final Collection<StatementType> types) {
			filters.accept(q -> q.append("stm.type = any(?)",
					arrayFromCollection(PgType.STRING, types.stream()
							.collect(Collectors.toList()))));
			return this;
		}

		@Override
		public StatementInquiryBuilder notWithTypes(final Collection<StatementType> types) {
			filters.accept(q -> q.append("stm.type != all(?)",
					arrayFromCollection(PgType.STRING, types.stream()
							.collect(Collectors.toList()))));
			return this;
		}

		@Override
		public StatementInquiryBuilder withText(final String text) {
			filters.accept(q -> q.append("stm.text = ?", text));
			return this;
		}

		@Override
		public StatementInquiryBuilder withModuleName(final String name) {
			filters.accept(q -> q.append("stm.module IN (SELECT uid FROM module WHERE name ILIKE ?)", name));
			return this;
		}

		@Override
		public StatementInquiryBuilder withModulePath(final String path) {
			filters.accept(q -> q.append("stm.module IN (SELECT uid FROM module WHERE path = ?)", path));
			return this;
		}
	}
	
	public class StatementsAggregationQueryBuilder extends AbstractAggregationQueryBuilder<StatementFieldName, StatementsAggregationQueryBuilder> 
			implements ModuleService.StatementAggregationInquiryBuilder<StatementsAggregationQueryBuilder> {

		protected boolean joinModuleTaxonomies;

		@Override
		protected String getFromClause() {
			return "statement stm";
		}
		
		@Override
		protected void buildJoins(final QueryBuilder qb) {
			qb.when(joinModuleTaxonomies, q -> q.append(" INNER JOIN module_taxonomies mtx on stm.module = mtx.module"));
		}

		@Override
		public StatementsAggregationQueryBuilder byId(final String operator, final Object value) {
			final EntityId entity = value instanceof EntityId ? (EntityId) value : EntityId.of(Objects.toString(value));
			if (operator.equals(OPERATOR_EQ)) {
				filters.accept(q -> q.appendId(entity));
			} else {
				throw new UnsupportedOperationException(String.format("The operator %s is not supported for ID", operator));
			}
			return this;
		}

		@Override
		public StatementsAggregationQueryBuilder ofProject(final String operator, final Object value) {
			if (operator.equals(OPERATOR_EQ)) {
				final EntityId entity = toEntityId(value, "Project id");
				filters.accept(q -> q.append("stm.module IN (SELECT uid from module WHERE project = ").with(ProjectPgDao.referenceUidOrNid(entity)).append(")"));
			} else {
				throw new UnsupportedOperationException(String.format("The operator %s is not supported for project", operator));
			}
			return this;
		}

		@Override
		public StatementsAggregationQueryBuilder ofModule(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					EntityId entity = toEntityId(value, "Module id");
					filters.accept(q -> q.append("stm.module = ").with(ModulePgDao.referenceUidOrNid(entity)));
					break;
				case OPERATOR_NOT_EQ:
					entity = toEntityId(value, "Module id");
					filters.accept(q -> q.append("stm.module != ").with(ModulePgDao.referenceUidOrNid(entity)));
					break;
				case OPERATOR_IN:
					var values = toEntityIds(value, "Module ids");
					filters.accept(q -> q.appendIds(values, "stm.module = ANY(?)", "stm.module IN (SELECT uid FROM module WHERE nid = ANY(?))"));
					break;
				case OPERATOR_NOT_IN:
					values = toEntityIds(value, "Module ids");
					filters.accept(q -> q.append("NOT (")
										 .appendIds(values, "stm.module = ANY(?)", "stm.module IN (SELECT uid FROM module WHERE nid = ANY(?))")
										 .append(")"));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for module", operator));
			}
			return this;
		}

		@Override
		public StatementsAggregationQueryBuilder withTaxonomy(final String operator, final Object value) {
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

		@Override
		public StatementsAggregationQueryBuilder withTechnology(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("stm.technology = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("stm.technology != ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("stm.technology = ANY(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("stm.technology != ANY(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for technology", operator));
			}
			return this;
		}

		@Override
		public StatementsAggregationQueryBuilder withType(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("stm.type LIKE ?", PgUtil.pgPattern(value.toString())));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("NOT stm.type LIKE ?", PgUtil.pgPattern(value.toString())));
					break;
				case OPERATOR_IN:
					var values = toStrings(value).stream().map(s -> PgUtil.pgPattern(s)).collect(Collectors.toList());
					filters.accept(q -> q.append("stm.type LIKE ANY(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value).stream().map(s -> PgUtil.pgPattern(s)).collect(Collectors.toList());
					filters.accept(q -> q.append("NOT stm.type LIKE ANY(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for type", operator));
			}
			return this;
		}

		@Override
		public StatementsAggregationQueryBuilder withText(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("stm.text = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("stm.text != ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("stm.text = ANY(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("stm.text != ANY(?)", arrayFromCollection(PgType.STRING, values)));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for text", operator));
			}
			return this;
		}

		@Override
		public StatementsAggregationQueryBuilder withTextLength(final String operator, final Object value) {
			final var length = toNumber(value, "Text length");
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("LENGTH(stm.text) = ?", length));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("LENGTH(stm.text) != ?", length));
					break;
				case OPERATOR_GTE:
					filters.accept(q -> q.append("LENGTH(stm.text) >= ?", length));
					break;
				case OPERATOR_LTE:
					filters.accept(q -> q.append("LENGTH(stm.text) <= ?", length));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for text_length", operator));
			}
			return this;
		}

		@Override
		public StatementsAggregationQueryBuilder withTables(final String operator, final Object value) {
			final var length = toNumber(value, "Table count");
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("(stm.properties->>'tables')::integer = ?", length));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("(stm.properties->>'tables')::integer != ?", length));
					break;
				case OPERATOR_GTE:
					filters.accept(q -> q.append("(stm.properties->>'tables')::integer >= ?", length));
					break;
				case OPERATOR_LTE:
					filters.accept(q -> q.append("(stm.properties->>'tables')::integer <= ?", length));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for tables", operator));
			}
			return this;
		}

		@Override
		public StatementsAggregationQueryBuilder withDistinctTables(final String operator, final Object value) {
			final var length = toNumber(value, "Distinct table count");
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("(stm.properties->>'distinctTables')::integer = ?", length));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("(stm.properties->>'distinctTables')::integer != ?", length));
					break;
				case OPERATOR_GTE:
					filters.accept(q -> q.append("(stm.properties->>'distinctTables')::integer >= ?", length));
					break;
				case OPERATOR_LTE:
					filters.accept(q -> q.append("(stm.properties->>'distinctTables')::integer <= ?", length));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for distinct_tables", operator));
			}
			return this;
		}

		@Override
		public StatementsAggregationQueryBuilder withSqlLength(final String operator, final Object value) {
			final var length = toNumber(value, "SQL length");
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("(stm.properties->>'sqlLength')::integer = ?", length));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("(stm.properties->>'sqlLength')::integer != ?", length));
					break;
				case OPERATOR_GTE:
					filters.accept(q -> q.append("(stm.properties->>'sqlLength')::integer >= ?", length));
					break;
				case OPERATOR_LTE:
					filters.accept(q -> q.append("(stm.properties->>'sqlLength')::integer <= ?", length));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for sql_length", operator));
			}
			return this;
		}

		@Override
		public StatementsAggregationQueryBuilder withHalsteadDifficulty(final String operator, final Object value) {
			final var length = toNumber(value, "HalsteadDifficulty");
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("(stm.properties->>'halsteadDifficulty')::float = ?", length));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("(stm.properties->>'halsteadDifficulty')::float != ?", length));
					break;
				case OPERATOR_GTE:
					filters.accept(q -> q.append("(stm.properties->>'halsteadDifficulty')::float >= ?", length));
					break;
				case OPERATOR_GT:
					filters.accept(q -> q.append("(stm.properties->>'halsteadDifficulty')::float > ?", length));
					break;
				case OPERATOR_LTE:
					filters.accept(q -> q.append("(stm.properties->>'halsteadDifficulty')::float <= ?", length));
					break;
				case OPERATOR_LT:
					filters.accept(q -> q.append("(stm.properties->>'halsteadDifficulty')::float < ?", length));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for halstead_difficulty", operator));
			}
			return this;
		}

		@Override
		public StatementsAggregationQueryBuilder withHalsteadComplexity(final String operator, final Object value) {
			final var length = toNumber(value, "HalsteadComplexity");
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("(stm.properties->>'halsteadComplexity')::float = ?", length));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("(stm.properties->>'halsteadComplexity')::float != ?", length));
					break;
				case OPERATOR_GTE:
					filters.accept(q -> q.append("(stm.properties->>'halsteadComplexity')::float >= ?", length));
					break;
				case OPERATOR_GT:
					filters.accept(q -> q.append("(stm.properties->>'halsteadComplexity')::float > ?", length));
					break;
				case OPERATOR_LTE:
					filters.accept(q -> q.append("(stm.properties->>'halsteadComplexity')::float <= ?", length));
					break;
				case OPERATOR_LT:
					filters.accept(q -> q.append("(stm.properties->>'halsteadComplexity')::float < ?", length));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for halstead_complexity", operator));
			}
			return this;
		}

		@Override
		public StatementsAggregationQueryBuilder withCustomComplexity(final String operator, final Object value) {
			final var length = toNumber(value, "CustomComplexity");
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("(stm.properties->>'customComplexity')::integer = ?", length));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("(stm.properties->>'customComplexity')::integer != ?", length));
					break;
				case OPERATOR_GTE:
					filters.accept(q -> q.append("(stm.properties->>'customComplexity')::integer >= ?", length));
					break;
				case OPERATOR_GT:
					filters.accept(q -> q.append("(stm.properties->>'customComplexity')::integer > ?", length));
					break;
				case OPERATOR_LTE:
					filters.accept(q -> q.append("(stm.properties->>'customComplexity')::integer <= ?", length));
					break;
				case OPERATOR_LT:
					filters.accept(q -> q.append("(stm.properties->>'customComplexity')::integer < ?", length));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for custom_complexity", operator));
			}
			return this;
		}

		@Override
		public String getFieldQueryFragment(final StatementFieldName field) {
			return getSqlFragment(field);
		}
	}

	/**
	 * Creates a new {@code statement} data access for Postgres.
	 * @param jdbcTemplate Access to the Postgres database.
	 */
	public StatementPgDao(final JdbcTemplate jdbcTemplate) {
		super(jdbcTemplate);
	}

	/**
	 * Creates a new {@code statement} entity.
	 *
	 * @param statement the {@link StatementPojoPrototype} to create
	 * @return the {@link EntityId} of the created {@code statement}.
	 */
	public EntityId create(final StatementPojoPrototype statement) {
		final UUID uid = statement.uid.orElseNonNull(UUID::randomUUID);
		final var fields = new FieldBuilder()
				.add("uid", "?", uid)
				.add("module", q -> q.with(ModulePgDao.referenceUidOrNid(statement.module.getNonNull())))
				.add(statement.technology.required(true), "technology", "?", Technology::name)
				.add(statement.type.required(true), "type", "?", StatementType::name)
				.add(statement.text.required(true), "text", "?")
				.add(statement.properties, "properties", "?", PgJSON::toPGobject);

		return query("INSERT INTO statement ")
				.with(fields::buildInsert)
				.append(" RETURNING uid, nid")
				.first(rs -> EntityId.of((UUID) rs.getObject(1), Long.valueOf(rs.getLong(2))))
				.orElseThrow(() -> new MiningEntityNotFoundException(StatementPojo.class, uid.toString()));
	}

	/**
	 * Creates new {@code statement} entity for each {@link StatementPojoPrototype} in {@code code statements}.
	 *
	 * @param statements the {@link StatementPojoPrototype StatementPojoPrototypes} to create
	 */
	public void create(final Collection<StatementPojoPrototype> statements) {
		/* create batch arguments for all statements that have a module UID */
		final var batchArgs = statements.stream()
									.filter(statement -> statement.module.getNonNull().hasUid())
									.map(statement -> toStream(statement, true))
									.collect(Collectors.toList());

		/* INSERT all statements that have a module UID */
		query("INSERT INTO statement(uid, module, technology, type, text, properties) VALUES (?, ?, ?, ?, ?, ?)")
			.updateBatch(batchArgs.stream(), 1_000);

		/* check if there are statements that have a module NID but no module UID */
		if (batchArgs.size() < statements.size()) {
			/* create batch arguments for all statements that have a module NID only */
			query("INSERT INTO statement(uid, module, technology, type, text, properties) VALUES (?, (SELECT uid FROM module WHERE nid = ?), ?, ?, ?, ?)")
				.updateBatch(statements.stream()
					.filter(statement -> {
						final var id = statement.module.getNonNull();
						return ! id.hasUid() && id.hasNid();
					}).map(statement -> toStream(statement, false)), 1_000);
		}
	}

	private static Stream<Object> toStream(final StatementPojoPrototype statement, final boolean hasUid) {
		return Stream.<Object>of(
				statement.uid.orElseNonNull(UUID::randomUUID),
				hasUid ? statement.module.getNonNull().getUid() : statement.module.getNonNull().getNid(),
				statement.technology.optional().map(Technology::name).orElse(null),
				statement.type.optional().map(StatementType::name).orElse(null),
				statement.text.get(),
				PgJSON.toPGobject(statement.properties.orElse(null))
			);
	}

	/**
	 * Deletes all {@code statement} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain StatementInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of deleted {@code statement} entities
	 */
	public int delete(final BuildingConsumer<StatementInquiryBuilder> builder) {
		return builder.prepare(new StatementQueryBuilder())
						.buildDelete();
	}

	/**
	 * Returns all {@code statement} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain StatementInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain StatementPojo StatementPojos}
	 */
	public List<StatementPojo> find(final BuildingConsumer<StatementInquiryBuilder> builder) {
		return builder.prepare(new StatementQueryBuilder())
						.build(null)
						.all();
	}

	/**
	 * Returns aggregation values for the filters and selected aggregations in the given {@code builder}.
	 *
	 * @param builder the {@linkplain StatementAggregationInquiryBuilder} containing the aggregation operations and filter criteria
	 * @return container with the aggregation values
	 */
	public Optional<Table> getAggregations(final BuildingConsumer<ModuleService.StatementAggregationInquiryBuilder<?>> builder) {
		return builder.prepare(new StatementsAggregationQueryBuilder())
						.buildAggregation(this);
	}

	/**
	 * Returns all {@code statement} entities that match with the filters in the given {@code builder}.
	 * @param paging the paging to apply to the result, or null
	 *
	 * @param builder the {@linkplain StatementInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain StatementPojo StatementPojos}
	 */
	public Paged<StatementPojo> find(@Nullable final Pagination paging, final BuildingConsumer<StatementInquiryBuilder> builder) {
		return builder.prepare(new StatementQueryBuilder())
						.build(paging)
						.page();
	}

	/**
	 * Returns the number of {@code statement} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain StatementInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of matching {@code statement} entities
	 */
	public long count(final BuildingConsumer<StatementInquiryBuilder> builder) {
		return builder.prepare(new StatementQueryBuilder())
				.buildCount();
	}

	/**
	 * Returns a container with a list of values of all {@code module} and {@code statement} entities that match with the filters in the given {@code builder}
	 * for the discovery exporter.
	 *
	 * @param project the id of the project for which the data export is done
	 * @param isSql {@code true} if only SQL statements must be exported. {@code false} if all non SQL statements must be exported
	 * @param sorted {@code true} if the values must be sorted. Otherwise {@code false}
	 * @return container with a list of values
	 */
	public Optional<Table> getModuleStatementExport(final EntityId project, final boolean isSql, final boolean sorted) {
		return Optional.ofNullable(query("SELECT (").append(ModulePgDao.LEGACY_UID_COLUMN).append(") as nid, m.name, s.type, s.text")
										.when(isSql, q -> q.append(", s.properties"))
										.append(" FROM statement s LEFT JOIN module m ON s.module = m.uid WHERE m.project=")
										.with(ProjectPgDao.referenceUidOrNid(project))
										.append(isSql ? " AND s.technology = ?" : 
														" AND s.technology != ?", "SQL")
										.when(sorted, q -> q.append( isSql ? " ORDER BY m.name, m.path, m.technology, m.type, s.text, m.nid" : 
																			 " ORDER BY m.name, s.type, s.text, m.nid"))
										.build(TableBuilder::build));
	}
	
}

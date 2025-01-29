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

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import javax.persistence.PersistenceException;

import org.springframework.jdbc.core.JdbcTemplate;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService.ModuleInquiryBuilder;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.access.Table;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.access.TaxonomyService.TaxonomyAggregationInquiryBuilder;
import innowake.mining.shared.access.TaxonomyService.TaxonomyCategoryInquiryBuilder;
import innowake.mining.shared.access.TaxonomyService.TaxonomyInquiryBuilder;
import innowake.mining.shared.access.TaxonomyService.TaxonomyOrderBuilder;
import innowake.mining.shared.access.TaxonomyService.TaxonomyTypeInquiryBuilder;
import innowake.mining.shared.entities.TaxonomyCategoryPojo;
import innowake.mining.shared.entities.TaxonomyCategoryPojoPrototype;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojo;
import innowake.mining.shared.entities.TaxonomyTypePojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.TaxonomyFieldName;

/**
 * Postgres specific access methods for Taxonomy.
 */
public class TaxonomyPgDao extends PgDao {
	
	public TaxonomyPgDao(final JdbcTemplate jdbcTemplate) {
		super(jdbcTemplate);
	}

	static Consumer<QueryBuilder> referenceUidOrNid(final EntityId entityId) {
		return q -> q.appendId(entityId, "?", "(SELECT uid FROM taxonomy WHERE nid = ?)");
	}

	public class BaseTaxonomyQueryBuilder {
		protected final OrderStreamBuilder orders = new OrderStreamBuilder();
		protected final FilterStreamBuilder filters = new FilterStreamBuilder();
		protected boolean joinModuleTaxonomies = false;
		protected boolean joinModules = false;
		protected boolean joinSourceMetrics = false;
		protected boolean joinTaxonomyCategories = false;
		protected boolean joinTaxonomyType = false;

		/**
		 * Adds all registered joins to given query builder
		 * @param qb to add joins
		 * @param leftJoin {@code true} when a {@code LEFT JOIN} must be used for {@code module_taxonomies}. Otherwise an {@code INNER JOIN} is used.
		 */
		protected void buildJoins(final QueryBuilder qb, final boolean leftJoin) {
			qb.when(joinModuleTaxonomies || joinModules || joinSourceMetrics, q -> q.append(leftJoin ? " LEFT" : " INNER")
					.append(" JOIN module_taxonomies mod_tax ON mod_tax.taxonomy = tax.uid"));
			qb.when(joinModules || joinSourceMetrics, q -> q.append(" INNER JOIN module mod ON mod.uid = mod_tax.module AND mod.project = tax.project"));
			qb.when(joinSourceMetrics, q -> q.append(" LEFT JOIN source_metrics sm ON sm.module = mod.uid"));
			qb.when(joinTaxonomyType || joinTaxonomyCategories, q -> q.append(" INNER JOIN taxonomy_type typ ON typ.id = tax.type"));
			qb.when(joinTaxonomyCategories, q -> q.append(" INNER JOIN taxonomy_category cat ON cat.id = typ.category"));
		}
	}

	/**
	 * Query builder for filtering {@code taxonomy} entities.
	 */
	public class TaxonomyQueryBuilder extends BaseTaxonomyQueryBuilder implements TaxonomyService.TaxonomyInquiryBuilder {
		@Nullable
		private Collection<EntityId> moduleCountsReferencingTaxonomies = null;
		protected final FilterStreamBuilder moduleCountBuilder = new FilterStreamBuilder();

		@Override
		public TaxonomyInquiryBuilder withModuleCountsReferencingTaxonomies(final Collection<EntityId> taxonomyIds) {
			moduleCountsReferencingTaxonomies = taxonomyIds;
			return this;
		}


		public Paged.Builder<TaxonomyPojo> buildWithModuleCountReferencingTaxonomies(@Nullable final Pagination paging) {
			return query("WITH taxes (arr) AS (SELECT ARRAY(SELECT uid::UUID FROM taxonomy WHERE nid = ANY(?::BIGINT[])))"
					+ "SELECT "
					+ "    ( "
					+ "        CASE "
					+ "            WHEN cardinality(taxes.arr) > 0 THEN ( "
					+ "                WITH t AS ( "
					+ "                    SELECT "
					+ "                        mt.module, "
					+ "                        ARRAY_AGG(mt.taxonomy) AS taxonomies "
					+ "                    FROM "
					+ "                        module_taxonomies mt "
					+ "                        INNER JOIN taxonomy tax on mt.taxonomy = tax.uid "
					+ "                        INNER JOIN taxonomy_type typ ON typ.id = tax.type "
					+ "                    GROUP BY  "
					+ "                        mt.module "
					+ "                ) "
					+ "                SELECT "
					+ "                    COUNT(*) "
					+ "                FROM t "
					+ "                WHERE "
					+ "                    ARRAY(SELECT DISTINCT unnest(taxes.arr))::uuid[] <@ t.taxonomies AND ARRAY[tax.uid] <@ t.taxonomies "
					+ "            ) "
					+ "            ELSE ( "
					+ "                SELECT "
					+ "                    COUNT(DISTINCT mt.module) "
					+ "                FROM "
					+ "                    module_taxonomies mt "
					+ "                    INNER JOIN taxonomy tax1 on mt.taxonomy = tax1.uid "
					+ "                    INNER JOIN taxonomy_type typ1 ON typ1.id = tax1.type "
					+ "                WHERE "
					+ "                    typ1.name = 'Program Type' "
					+ "                    and tax.uid = mt.taxonomy "
					+ "            ) "
					+ "        END "
					+ "    ) AS ref_count, "
					+ "    tax.uid AS tax_uid, "
					+ "    tax.nid AS tax_nid, "
					+ "    ( "
					+ "        SELECT "
					+ "            to_jsonb(project_ids) "
					+ "        FROM "
					+ "            ( "
					+ "                SELECT "
					+ "                    uid, "
					+ "                    nid "
					+ "                FROM "
					+ "                    project "
					+ "                WHERE "
					+ "                    uid = tax.project "
					+ "            ) project_ids "
					+ "    ) AS project_id, "
					+ "    tax.name AS tax_name, "
					+ "    tax.custom_properties AS tax_customprops, "
					+ "    typ.id AS typ_id, "
					+ "    typ.name AS typ_name, "
					+ "    cat.id AS cat_id, "
					+ "    cat.name AS cat_name "
					+ "FROM "
					+ "    taxes, "
					+ "    taxonomy tax ")
					.addArg(PgUtil.arrayFromCollection(PgType.INT, EntityId.allNids(Assert.assertNotNull(moduleCountsReferencingTaxonomies))))
					.append("INNER JOIN taxonomy_type typ ON typ.id = tax.type ")
					.append("INNER JOIN taxonomy_category cat ON cat.id = typ.category ")
					//LEFT OUTER required as we want All taxonomies, and the count of Modules, which might be 0
					.append("LEFT OUTER JOIN (module_taxonomies mod_tax INNER JOIN module ON module.uid = mod_tax.module)"
							+ " ON mod_tax.taxonomy = tax.uid AND module.project = tax.project")
					.with(filters::build)
					.append("GROUP BY tax_uid, tax_nid, project_id, tax_name, tax_customprops, typ_id, typ_name, cat_id, cat_name, taxes.arr")
					.toPageable(paging, (rs, n) -> extractTaxonomy(rs));
		}

		@Override
		public TaxonomyQueryBuilder byId(final EntityId id) {
			filters.accept(q -> q.appendId(id));
			return this;
		}

		@Override
		public TaxonomyQueryBuilder ofProject(final EntityId projectId) {
			filters.accept(q -> q.append("tax.project = ").with(ProjectPgDao.referenceUidOrNid(projectId)));
			return this;
		}

		@Override
		public TaxonomyQueryBuilder byIds(final Collection<EntityId> ids) {
			final var uids = EntityId.allUids(ids);
			if (uids.size() == ids.size()) {
				filters.accept(q -> q.append("tax.uid = any(?)", arrayFromCollection(PgType.UUID, uids)));
				return this;
			}

			final var nids = EntityId.allNids(ids);
			if (nids.size() == ids.size()) {
				filters.accept(q -> q.append("tax.nid = any(?)", arrayFromCollection(PgType.LONG, nids)));
				return this;
			}

			filters.accept(q -> q.append("(tax.uid = any(?) OR tax.nid = any(?))", arrayFromCollection(PgType.UUID, uids), arrayFromCollection(PgType.LONG, nids)));

			return this;
		}

		@Override
		public TaxonomyQueryBuilder ofModule(final EntityId moduleId) {
			joinModuleTaxonomies = true;
			filters.accept(q -> q.append("mod_tax.module = ").with(ModulePgDao.referenceUidOrNid(moduleId)));
			return this;
		}

		@Override
		public TaxonomyInquiryBuilder ofCategory(final Long id) {
			joinTaxonomyCategories = true;
			filters.accept(q -> q.append("cat.id = ?", id));
			return this;
		}

		@Override
		public TaxonomyInquiryBuilder ofType(final UUID id) {
			joinTaxonomyType = true;
			filters.accept(q -> q.append("typ.id = ?", id));
			return this;
		}

		@Override
		public TaxonomyQueryBuilder ofModules(final Collection<EntityId> ids) {
			joinModuleTaxonomies = true;

			final var uids = EntityId.allUids(ids);
			if (uids.size() == ids.size()) {
				filters.accept(q -> q.append("mod_tax.module = any(?)", arrayFromCollection(PgType.UUID, uids)));
				return this;
			}

			final var nids = EntityId.allNids(ids);
			if (nids.size() == ids.size()) {
				filters.accept(q -> q.append("mod_tax.module = any(SELECT uid FROM module where nid = any(?))",
						arrayFromCollection(PgType.LONG, nids)));
			} else {
				filters.accept(q -> q.append("(mod_tax.module = any(?) OR mod_tax.module = any(SELECT uid FROM module where nid = any(?)))",
						arrayFromCollection(PgType.UUID, uids), arrayFromCollection(PgType.LONG, nids)));
			}

			return this;
		}

		@Override
		public TaxonomyQueryBuilder withTypeName(final String typeName) {
			joinTaxonomyType = true;
			filters.accept(q -> q.append("typ.name = ?", typeName));
			return this;
		}
		
		@Override
		public TaxonomyInquiryBuilder notWithTypeName(final String typeName) {
			joinTaxonomyType = true;
			filters.accept(q -> q.append("typ.name != ?", typeName));
			return this;
		}
		
		@Override
		public TaxonomyQueryBuilder withName(final String name) {
			filters.accept(q -> q.append("tax.name = ?", name));
			return this;
		}
		
		@Override
		public TaxonomyQueryBuilder withNames(final Collection<String> name) {
			filters.accept(q -> q.append("tax.name = any(?)", arrayFromCollection(PgType.STRING, name)));
			return this;
		}

		@Override
		public TaxonomyQueryBuilder withTypeNames(final Collection<String> typeNames) {
			joinTaxonomyType = true;
			filters.accept(q -> q.append("typ.name = any(?)", arrayFromCollection(PgType.STRING, typeNames)));
			return this;
		}

		private QueryBuilder buildRefCountCte() {
			/* use LEFT JOIN if no filter for modules, module taxonomies and module source metrics was set */
			final boolean leftJoin = ! (joinModuleTaxonomies || joinModules || joinSourceMetrics);
			final boolean joinModuleTax = joinModuleTaxonomies;

			try {
				joinModuleTaxonomies = true;
				return query("WITH tax_ref_counts AS ("
								+ "SELECT count(mod_tax.module) as ref_count, array_agg(mod_tax.module) as modules, tax.uid as tax_id "
								+ "FROM taxonomy tax ")
						.with(q -> buildJoins(q, leftJoin))
						.with(filters::build)
						.append(" GROUP BY tax.uid) ");
			} finally {
				joinModuleTaxonomies = joinModuleTax;
			}
		}

		/**
		 * Internal query builder fetching all Taxonomies and related modules, creating a map using given mapper.
		 *
		 * @param <K> key of the result map
		 * @param <V> value of the result map
		 * @param mapper to map each row with
		 * @return Map<K, V>
		 */
		private <K, V> Map<K, V> buildModules(final RowToMapMapper<K, V> mapper) {
			final var query = buildRefCountCte()
								.append("SELECT "
										+ "(SELECT to_jsonb(module_ids) FROM (SELECT uid, nid FROM module WHERE uid = mod_tax.module) module_ids) AS module_id, "
										+ "(SELECT ref_count FROM tax_ref_counts WHERE tax_id = tax.uid) AS ref_count, "
										+ "tax.uid AS tax_uid, "
										+ "tax.nid AS tax_nid, "
										+ "(SELECT to_jsonb(project_ids) FROM (SELECT uid, nid FROM project WHERE uid = tax.project) project_ids) AS project_id, "
										+ "tax.name AS tax_name, "
										+ "tax.custom_properties AS tax_customprops, "
										+ "typ.id AS typ_id, "
										+ "typ.name AS typ_name, "
										+ "cat.id AS cat_id, "
										+ "cat.name AS cat_name "
										+ "FROM taxonomy tax ");

			joinModuleTaxonomies = joinTaxonomyCategories = joinTaxonomyType = true;
			return query.with(q -> buildJoins(q, false))
					.append(" WHERE tax.uid IN (SELECT tax_id FROM tax_ref_counts) AND mod_tax.module in (SELECT unnest(modules) FROM tax_ref_counts) ")
					.with(orders::build)
					.toMap(mapper);
		}
		/**
		 * Internal query builder fetching all Taxonomies and related modules, creating a map using given mapper.
		 *
		 * @param <K> key of the result map
		 * @param <V> value of the result map
		 * @param mapper to map each row with
		 * @return Map<K, V>
		 */
		private <K, V> Map<K, V> buildModulesIds(final RowToMapMapper<K, V> mapper) {
			joinModuleTaxonomies = true;
			return query("SELECT "
					+ "(SELECT to_jsonb(module_ids) FROM (SELECT uid, nid FROM module WHERE uid = mod_tax.module) module_ids) AS module_id, "
					+ "tax.uid, tax.nid FROM taxonomy tax ")
					.with(q -> buildJoins(q, false))
					.with(filters::build)
					.with(orders::build)
					.toMap(mapper);
		}

		/**
		 * Builds the Query for fetching TaxonomyPojos with Paging.
		 *
		 * @param paging to page the result by
		 * @return Paged Result
		 */
		public Paged.Builder<TaxonomyPojo> build(@Nullable final Pagination paging) {
			if (moduleCountsReferencingTaxonomies != null) {
				return buildWithModuleCountReferencingTaxonomies(paging);
			}

			final var query = buildRefCountCte()
								.append("SELECT tax.uid AS tax_uid, "
										+ "tax.nid AS tax_nid, "
										+ "(SELECT to_jsonb(project_ids) FROM (SELECT uid, nid FROM project WHERE uid = tax.project) project_ids) AS project_id, "
										+ "(SELECT ref_count FROM tax_ref_counts WHERE tax_id = tax.uid) AS ref_count, "
										+ "tax.name AS tax_name, "
										+ "tax.custom_properties AS tax_customprops, "
										+ "typ.id AS typ_id, "
										+ "typ.name AS typ_name, "
										+ "cat.id AS cat_id, "
										+ "cat.name AS cat_name "
										+ "FROM taxonomy tax ");

			joinTaxonomyCategories = joinTaxonomyType = true;
			return query.with(q -> buildJoins(q, false))
				.append(" WHERE tax.uid IN (SELECT tax_id FROM tax_ref_counts) ")
				.when(joinModuleTaxonomies, q -> q.append(" AND mod_tax.module in (SELECT unnest(modules) FROM tax_ref_counts) "))
				.with(orders::build)
				.toPageable(paging, (rs, n) -> extractTaxonomy(rs));
		}
		
		/**
		 * Builds and executes a count query defined by this builder
		 * @return the count
		 */
		public long buildCount() {
			return query("SELECT count(*) FROM taxonomy tax ")
					.with(q -> buildJoins(q, false))
					.with(filters::build)
					.first(rs -> Long.valueOf(rs.getLong(1)))
					.orElse(Long.valueOf(0l))
					.longValue();
		}

		/**
		 * Builds and executes a query for taxonomy ids defined by this builder
		 * @return EntityIds of fetched taxonomy
		 */
		public List<EntityId> buildIds() {
			return query("SELECT uid, nid FROM taxonomy tax ")
					.with(q -> buildJoins(q, false))
					.with(filters::build)
					.with(orders::build)
					.toList((rs, row) -> EntityId.of((UUID) rs.getObject(1), rs.getLong(2)));
		}
		
		/**
		 * Builds and executes a query for module ids defined by this builder.
		 * Used to find related modules that reference specific taxonomies
		 * @return EntityIds of filtered modules
		 */
		public List<EntityId> buildModuleIds() {
			joinModuleTaxonomies = true;
			return query("SELECT (SELECT to_jsonb(module_ids) FROM (SELECT uid, nid FROM module WHERE uid = mod_tax.module) module_ids) AS module_id FROM taxonomy tax ")
					.with(q -> buildJoins(q, false))
					.with(filters::build)
					.with(orders::build)
					.toList((rs, row) -> PgJSON.fromPGobject(rs.getObject("module_id"), EntityId.class));
		}

		/**
		 * Builds and executes a delete query for filter defined by this builder.
		 * @return number of deleted records
		 */
		public long buildDelete() {
			return query("DELETE FROM taxonomy WHERE uid IN ("
					+ "SELECT tax.uid FROM taxonomy tax ")
						.with(q -> buildJoins(q, false))
						.with(filters::build)
					.append(")")
					.update();
		}

		/**
		 * Builds and executes a delete query for filter defined by this builder.
		 * @return the module UUIDs of deleted records
		 */
		public List<UUID> buildDeleteLinks() {
			joinModuleTaxonomies = true;
			return query("DELETE FROM module_taxonomies "
					+ "WHERE (module, taxonomy) IN ("
					+ "SELECT mod_tax.module, mod_tax.taxonomy FROM taxonomy tax ")
						.with(q -> buildJoins(q, false))
						.with(filters::build)
					.append(") RETURNING module")
					.toList((rs, row) -> (UUID) rs.getObject("module"));
		}

		@Override
		public TaxonomyOrderBuilder sortId(final SortDirection direction) {
			orders.accept(q -> q.appendOrder("tax.nid", direction));
			return this;
		}

		@Override
		public TaxonomyOrderBuilder sortName(final SortDirection direction) {
			orders.accept(q -> q.appendOrder("tax.name", direction));
			return this;
		}
	}
	
	/**
	 * Query builder for filtering {@code TaxonomyType} entities.
	 */
	public class TaxonomyTypeQueryBuilder extends BaseTaxonomyQueryBuilder implements TaxonomyService.TaxonomyTypeInquiryBuilder {
		/**
		 * Builds and executes a Query for fetching TaxonomyTypes.
		 * @return list of fetched TaxonomyTypePojos
		 */
		public List<TaxonomyTypePojo> build() {
			return query("SELECT typ.id AS typ_id, "
						+ "(SELECT to_jsonb(project_ids) FROM (SELECT uid, nid FROM project WHERE uid = typ.project) project_ids) AS project_id, "
						+ "typ.name AS typ_name, "
						+ "cat.id AS cat_id, "
						+ "cat.name AS cat_name "
						+ "FROM taxonomy_type typ "
						+ "INNER JOIN taxonomy_category cat ON cat.id = typ.category ")
					.with(filters::build)
					.with(orders::build)
					.toList((rs, row) -> extractType(rs));
		}
		
		/**
		 * Builds and executes a delete query for filter defined by this builder.
		 * @return number of deleted records
		 */
		public long buildDelete() {
			return query("DELETE FROM taxonomy_type typ ")
					.with(filters::build)
					.update();
		}

		/**
		 * Builds and executes a Query for fetching a map of TaxonomyTypePojo and the number of references.
		 * @return type to number of referencing modules
		 */
		public Map<TaxonomyTypePojo, Long> buildCount() {
			joinModuleTaxonomies = true;
			joinTaxonomyCategories = true;
			joinTaxonomyType = true;

			return query("SELECT count(DISTINCT mod_tax.module), "
					+ "typ.id AS typ_id, "
					+ "(SELECT to_jsonb(project_ids) FROM (SELECT uid, nid FROM project WHERE uid = typ.project) project_ids) AS project_id, "
					+ "typ.name AS typ_name, "
					+ "cat.id AS cat_id, "
					+ "cat.name AS cat_name "
					+ "FROM taxonomy tax ")
					.with(q -> buildJoins(q, false))
					.with(filters::build)
					.append("GROUP BY cat.id, typ.id")
					.toMap((rs, m) -> m.put(extractType(rs), rs.getLong(1)));
		}

		@Override
		public TaxonomyTypeQueryBuilder sortId(final SortDirection direction) {
			orders.accept(q -> q.appendOrder("typ.nid", direction));
			return this;
		}

		@Override
		public TaxonomyTypeQueryBuilder sortName(final SortDirection direction) {
			orders.accept(q -> q.appendOrder("typ.name", direction));
			return this;
		}


		@Override
		public TaxonomyTypeQueryBuilder byId(final UUID id) {
			filters.accept(q -> q.append("typ.id = ?", id));
			return this;
		}

		@Override
		public TaxonomyTypeQueryBuilder ofProject(final EntityId projectId) {
			filters.accept(q -> q.append("typ.project = ").with(ProjectPgDao.referenceUidOrNid(projectId)));
			return this;
		}

		@Override
		public TaxonomyTypeQueryBuilder ofCategory(final Long id) {
			filters.accept(q -> q.append("typ.category = ?", id));
			return this;
		}

		@Override
		public TaxonomyTypeQueryBuilder withName(final String typeName) {
			filters.accept(q -> q.append("typ.name = ?", typeName));
			return this;
		}
	}

	/**
	 * Query builder for filtering {@code TaxonomyCategory} entities.
	 */
	public class TaxonomyCategoryQueryBuilder extends BaseTaxonomyQueryBuilder implements TaxonomyService.TaxonomyCategoryInquiryBuilder {
		/**
		 * Builds the Query for fetching TaxonomyCategories.
		 * @return list of fetched TaxonomyCategoryPojo
		 */
		public List<TaxonomyCategoryPojo> build() {
			return query("SELECT cat.id AS cat_id, "
					+ "(SELECT to_jsonb(project_ids) FROM (SELECT uid, nid FROM project WHERE uid = cat.project) project_ids) AS project_id, "
					+ "cat.name AS cat_name "
					+ "FROM taxonomy_category cat")
					.with(filters::build)
					.with(orders::build)
					.toList((rs, row) -> extractCategory(rs));
		}
		
		/**
		 * Builds the Query for fetching TaxonomyCategories.
		 * @return map of Category ID to Count of Referencing Modules
		 */
		public Map<Long, Long> buildCount() {
			joinModuleTaxonomies = joinTaxonomyCategories = joinTaxonomyType = true;
			return query("SELECT cat.id AS cat_id, count(DISTINCT mod_tax.module) mod_count "
					+ "FROM taxonomy_category cat "
					+ "LEFT OUTER JOIN taxonomy_type typ ON cat.id = typ.category "
					+ "LEFT OUTER JOIN taxonomy tax ON typ.id = tax.type "
					+ "LEFT OUTER JOIN module_taxonomies mod_tax ON mod_tax.taxonomy = tax.uid ")
					.with(filters::build)
					.append("GROUP BY cat.id")
					.with(orders::build)
					.toMap((rs, m) -> m.put(rs.getLong(1), rs.getLong(2)));
		}

		/**
		 * Builds and executes a delete query for filter defined by this builder.
		 * @return number of deleted records
		 */
		public long buildDelete() {
			return query("DELETE FROM taxonomy_category cat ")
					.with(filters::build)
					.update();
		}
		
		@Override
		public TaxonomyCategoryQueryBuilder byId(final Long id) {
			filters.accept(q -> q.append("cat.id = ?", id));
			return this;
		}
		
		@Override
		public TaxonomyCategoryQueryBuilder ofProject(final EntityId projectId) {
			filters.accept(q -> q.append("cat.project = ").with(ProjectPgDao.referenceUidOrNid(projectId)));
			return this;
		}

		@Override
		public TaxonomyCategoryInquiryBuilder ofProjectWithDefault(final EntityId projectId) {
			filters.accept(q -> q.append("(cat.project = ")
									.with(ProjectPgDao.referenceUidOrNid(projectId))
									.append(" OR cat.project = ")
									.with(ProjectPgDao.referenceUidOrNid(EntityId.of(0L)))
								.append(")"));
			return this;
		}

		@Override
		public TaxonomyCategoryQueryBuilder withName(final String catName) {
			filters.accept(q -> q.append("cat.name = ?", catName));
			return this;
		}
		
		@Override
		public TaxonomyCategoryQueryBuilder sortId(final SortDirection direction) {
			orders.accept(q -> q.appendOrder("cat.id", direction));
			return this;
		}
		
		@Override
		public TaxonomyCategoryQueryBuilder sortName(final SortDirection direction) {
			orders.accept(q -> q.appendOrder("cat.name", direction));
			return this;
		}
	}
	
	public class TaxonomyAggregationQueryBuilder extends AbstractAggregationQueryBuilder<TaxonomyFieldName, TaxonomyAggregationQueryBuilder> 
					implements TaxonomyAggregationInquiryBuilder<TaxonomyAggregationQueryBuilder> {

		private boolean joinModuleTaxonomies = false;
		private boolean joinModules = false;
		private boolean joinSourceMetrics = false;
		private boolean joinTaxonomyCategories = false;
		private boolean joinTaxonomyType = false;

		@Override
		protected String getFromClause() {
			return "taxonomy tax";
		}

		@Override
		protected void buildJoins(final QueryBuilder qb) {
			qb.when(joinModuleTaxonomies || joinModules || joinSourceMetrics, q -> q.append(" LEFT JOIN module_taxonomies mod_tax ON mod_tax.taxonomy = tax.uid "));
			qb.when(joinModules || joinSourceMetrics, q -> q.append("INNER JOIN module mod ON mod.uid = mod_tax.module "));
			qb.when(joinSourceMetrics, q -> q.append(" LEFT JOIN source_metrics sm ON sm.module = mod.uid "));
			qb.when(joinTaxonomyType || joinTaxonomyCategories, q -> q.append("INNER JOIN taxonomy_type typ ON typ.id = tax.type "));
			qb.when(joinTaxonomyCategories, q -> q.append("INNER JOIN taxonomy_category cat ON cat.id = typ.category "));
		}

		@Override
		protected String getFieldQueryFragment(final TaxonomyFieldName field) {
			switch (field) {
				case TYPE_NAME:
					joinTaxonomyType = true;
					break;
				case CATEGORY_NAME:
					joinTaxonomyCategories = true;
					break;
				case MODULE_ID:
					joinModuleTaxonomies = true;
					break;
				case MODULE_NAME:
				case MODULE_TECHNOLOGY:
				case MODULE_TYPE:
					joinModules = true;
					break;
				case MODULE_COMPLEXITY:
				case MODULE_LINES_OF_CODE:
				case MODULE_LINES_OF_COMMENT:
				case MODULE_LINES_OF_DEAD_CODE:
					joinSourceMetrics = true;
					break;
				default:
					/* nothing to join */
					break;
			}

			switch (field) {
				case ID:
					return "tax.nid AS " + field.name().toLowerCase();
				case NAME:
					return "tax.name AS " + field.name().toLowerCase();
				case TYPE_NAME:
					return "typ.name AS " + field.name().toLowerCase();
				case CATEGORY_NAME:
					return "cat.name AS " + field.name().toLowerCase();
				case MODULE_ID:
					return "mod_tax.module AS " + field.name().toLowerCase();
				case MODULE_NAME:
					return "mod.name AS " + field.name().toLowerCase();
				case MODULE_TECHNOLOGY:
					return "mod.technology AS " + field.name().toLowerCase();
				case MODULE_TYPE:
					return "mod.type AS " + field.name().toLowerCase();
				case MODULE_COMPLEXITY:
					return "sm.complexity_mc_cabe AS " + field.name().toLowerCase();
				case MODULE_LINES_OF_CODE:
					return "sm.code_lines AS " + field.name().toLowerCase();
				case MODULE_LINES_OF_COMMENT:
					return "sm.comment_lines AS " + field.name().toLowerCase();
				case MODULE_LINES_OF_DEAD_CODE:
					return "sm.dead_code_lines AS " + field.name().toLowerCase();
				default:
					throw new UnsupportedOperationException("The field is not supported yet: " + field.name());
			}
		}

		@Override
		public TaxonomyAggregationQueryBuilder ofProject(final String operator, final Object value) {
			if (OPERATOR_EQ.equals(operator)) {
				final EntityId project = toEntityId(value, "Project id");
				filters.accept(q -> q.append("tax.project = ").with(ProjectPgDao.referenceUidOrNid(project)));
			} else {
				throw new UnsupportedOperationException(String.format("The operator %s is not supported for PROJECT", operator));
			}
			return this;
		}

		@Override
		public TaxonomyAggregationQueryBuilder byId(final String operator, final Object value) {
			if (OPERATOR_EQ.equals(operator)) {
				final EntityId entity = toEntityId(value, "Taxonomy id");
				filters.accept(q -> q.append("tax.").appendId(entity));
			} else {
				throw new UnsupportedOperationException(String.format("The operator %s is not supported for ID", operator));
			}
			return this;
		}

		@Override
		public TaxonomyAggregationQueryBuilder withName(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("tax.name = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("NOT tax.name = ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("tax.name = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT tax.name = any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for NAME", operator));
			}
			return this;
		}

		@Override
		public TaxonomyAggregationQueryBuilder ofTypeName(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("typ.name = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("NOT typ.name = ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("typ.name = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT typ.name = any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for TYPE_NAME", operator));
			}
			joinTaxonomyType = true;
			return this;
		}

		@Override
		public TaxonomyAggregationQueryBuilder ofCategoryName(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("cat.name = ?", value));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("NOT cat.name = ?", value));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("cat.name = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT cat.name = any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for CATEGORY_NAME", operator));
			}
			joinTaxonomyCategories = true;
			return this;
		}

		@Override
		public TaxonomyAggregationQueryBuilder ofModuleId(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					final var entity1 = toEntityId(value, "Module id");
					filters.accept(q -> q.append("mod_tax.module = ", ModulePgDao.referenceUidOrNid(entity1)));
					break;
				case OPERATOR_NOT_EQ:
					final var entity2 = toEntityId(value, "Module id");
					filters.accept(q -> q.append("NOT mod_tax.module = ", ModulePgDao.referenceUidOrNid(entity2)));
					break;
				case OPERATOR_IN:
					joinModules = true;
					var values = toEntityIds(value, "Module ids");
					filters.accept(q -> q.appendIds(values, "mod.uid = ANY(?)", "mod.nid = any(?)"));
					break;
				case OPERATOR_NOT_IN:
					joinModules = true;
					values = toEntityIds(value, "Module ids");
					filters.accept(q -> q.append("NOT (")
							 .appendIds(values, "mod.uid = any(?)", "mod.nid = any(?)")
							 .append(")"));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for MODULE_ID", operator));
			}
			joinModuleTaxonomies = true;
			return this;
		}

		@Override
		public TaxonomyAggregationQueryBuilder ofModuleName(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("mod.name = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("NOT mod.name = ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("mod.name = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT mod.name = any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for MODULE_NAME", operator));
			}
			joinModules = true;
			return this;
		}

		@Override
		public TaxonomyAggregationQueryBuilder ofModuleTechnology(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("mod.technology = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("NOT mod.technology = ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("mod.technology = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT mod.technology = any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for MODULE_TECHNOLOGY", operator));
			}
			joinModules = true;
			return this;
		}

		@Override
		public TaxonomyAggregationQueryBuilder ofModuleType(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("mod.type = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("NOT mod.type = ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("mod.type = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT mod.type = any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for MODULE_TYPE", operator));
			}
			joinModules = true;
			return this;
		}

		@Override
		public TaxonomyAggregationQueryBuilder ofModuleRepresentation(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("mod.representation = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("NOT mod.representation = ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("mod.representation = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT mod.representation = any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for MODULE_REPRESENTATION", operator));
			}
			joinModules = true;
			return this;
		}

		@Override
		public TaxonomyAggregationQueryBuilder ofModuleComplexity(final String operator, final Object value) {
			final var number = toNumber(value, "MODULE_COMPLEXITY");
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("sm.complexity_mc_cabe = ?", number));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("NOT sm.complexity_mc_cabe = ?", number));
					break;
				case OPERATOR_GTE:
					filters.accept(q -> q.append("sm.complexity_mc_cabe >= ?", number));
					break;
				case OPERATOR_GT:
					filters.accept(q -> q.append("sm.complexity_mc_cabe > ?", number));
					break;
				case OPERATOR_LTE:
					filters.accept(q -> q.append("sm.complexity_mc_cabe <= ?", number));
					break;
				case OPERATOR_LT:
					filters.accept(q -> q.append("sm.complexity_mc_cabe < ?", number));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for MODULE_COMPLEXITY", operator));
			}
			joinSourceMetrics = true;
			return this;
		}

		@Override
		public TaxonomyAggregationQueryBuilder ofModuleCodeLines(final String operator, final Object value) {
			final var number = toNumber(value, "MODULE_CODE_LINES");
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("sm.code_lines = ?", number));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("NOT sm.code_lines = ?", number));
					break;
				case OPERATOR_GTE:
					filters.accept(q -> q.append("sm.code_lines >= ?", number));
					break;
				case OPERATOR_LTE:
					filters.accept(q -> q.append("sm.code_lines <= ?", number));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for MODULE_CODE_LINES", operator));
			}
			joinSourceMetrics = true;
			return this;
		}

		@Override
		public TaxonomyAggregationQueryBuilder ofModuleCommentLines(final String operator, final Object value) {
			final var number = toNumber(value, "MODULE_COMENT_LINES");
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("sm.comment_lines = ?", number));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("NOT sm.comment_lines = ?", number));
					break;
				case OPERATOR_GTE:
					filters.accept(q -> q.append("sm.comment_lines >= ?", number));
					break;
				case OPERATOR_LTE:
					filters.accept(q -> q.append("sm.comment_lines <= ?", number));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for MODULE_COMENT_LINES", operator));
			}
			joinSourceMetrics = true;
			return this;
		}

		@Override
		public TaxonomyAggregationQueryBuilder ofModuleDeadCodeLines(final String operator, final Object value) {
			final var number = toNumber(value, "MODULE_DEAD_CODE_LINES");
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("sm.dead_code_lines = ?", number));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("NOT sm.dead_code_lines = ?", number));
					break;
				case OPERATOR_GTE:
					filters.accept(q -> q.append("sm.dead_code_lines >= ?", number));
					break;
				case OPERATOR_LTE:
					filters.accept(q -> q.append("sm.dead_code_lines <= ?", number));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for MODULE_DEAD_CODE_LINES", operator));
			}
			joinSourceMetrics = true;
			return this;
		}
	}

	/**
	 * Returns the number of {@code taxonomy} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of matching {@code module} entities
	 */
	public long count(final BuildingConsumer<TaxonomyInquiryBuilder> builder) {
		return builder.prepare(new TaxonomyQueryBuilder())
				.buildCount();
	}

	/**
	 * Returns a map containing the sum of {@code source_metrics} {@code code_lines} per technology for the given {@code project}.
	 *
	 * @param project the {@code project} for which to get the count.
	 * @return map with sums
	 */
	public Map<String, Long> countSourceMetricsCodeLinesByTypeName(final EntityId project) {
		return query("SELECT type_modules.type_name, sum(sm.code_lines) FROM ( ")
				/* select distinct for taxonomy type and module uid so source_metrics of each matching module is taken only once into SUM aggregation */
				.append("SELECT distinct typ.name as type_name, mod.uid as module_uid FROM taxonomy tax ")
				.append("JOIN taxonomy_type typ ON typ.id = tax.type ")
				.append("JOIN module_taxonomies mod_tax ON mod_tax.taxonomy = tax.uid ")
				.append("JOIN module mod ON mod.uid = mod_tax.module ")
				.append("WHERE tax.project = ")
				.with(ProjectPgDao.referenceUidOrNid(project))
				.append(" AND mod.representation = 'PHYSICAL') AS type_modules ")
				.append("JOIN source_metrics sm ON sm.module = type_modules.module_uid ")
				.append("GROUP BY type_modules.type_name")
				.toMap((rs, map) -> map.put(rs.getString(1), Long.valueOf(rs.getLong(2))));
	}
	
	/**
	 * Returns all Categories that match the filter
	 *
	 * @param builder filter builder
	 * @return found categories
	 */
	public List<TaxonomyCategoryPojo> findCategories(final BuildingConsumer<TaxonomyCategoryInquiryBuilder> builder) {
		return builder.prepare(new TaxonomyCategoryQueryBuilder())
			.build();
	}
	
	/**
	 * Returns all Types that match the filter
	 *
	 * @param builder filter builder
	 * @return found types
	 */
	public List<TaxonomyTypePojo> findTypes(final BuildingConsumer<TaxonomyTypeInquiryBuilder> builder) {
		return builder.prepare(new TaxonomyTypeQueryBuilder())
				.build();
	}
	
	/**
	 * Returns all Taxonomies that match the filter, with Paging.
	 *
	 * @param paging Specification for retrieving a subset of a query result.
	 * @param builder filter builder
	 * @return found taxonomies
	 */
	public Paged<TaxonomyPojo> find(final Pagination paging, final BuildingConsumer<TaxonomyInquiryBuilder> builder) {
		return builder.prepare(new TaxonomyQueryBuilder())
				.build(paging)
				.page();
	}

	/**
	 * Returns all Taxonomies that match the filter.
	 *
	 * @param builder filter builder
	 * @return found taxonomies
	 */
	public List<TaxonomyPojo> find(final BuildingConsumer<TaxonomyInquiryBuilder> builder) {
		return builder.prepare(new TaxonomyQueryBuilder())
				.build(null)
				.all();
	}
	
	/**
	 * Returns all Taxonomies that match the filter.
	 *
	 * @param builder filter builder
	 * @return found taxonomies
	 */
	public Optional<TaxonomyPojo> findAny(final BuildingConsumer<TaxonomyInquiryBuilder> builder) {
		return builder.prepare(new TaxonomyQueryBuilder())
				.build(Pagination.FIRST).first();
	}

	/**
	 * Returns all IDs of Taxonomies that match the filter.
	 *
	 * @param builder filter builder
	 * @return found taxonomy IDs
	 */
	public List<EntityId> findIds(final BuildingConsumer<TaxonomyInquiryBuilder> builder) {
		return builder.prepare(new TaxonomyQueryBuilder())
				.buildIds();
	}

	/**
	 * Returns all IDs of Modules that reference Taxonomies that match the filter.
	 *
	 * @param builder filter builder
	 * @return found module IDs
	 */
	public List<EntityId> findTaxonomyModulesIds(final BuildingConsumer<TaxonomyInquiryBuilder> builder) {
		return builder.prepare(new TaxonomyQueryBuilder())
				.buildModuleIds();
	}

	/**
	 * Fetches all modules ids in given project and lists assigned taxonomies for each.
	 *
	 * @param builder filter builder
	 * @return mapping of moduleId to List of assigned Taxonomies
	 */
	public Map<EntityId, List<TaxonomyPojo>> findTaxonomiesPerModule(final BuildingConsumer<TaxonomyInquiryBuilder> builder) {
		return builder.prepare(new TaxonomyQueryBuilder())
				.buildModules((rs, m) -> m.computeIfAbsent(PgJSON.fromPGobject(rs.getObject(1), EntityId.class), k -> new LinkedList<>())
						.add(extractTaxonomy(rs)));
	}
	/**
	 * Fetches all modules ids in given project and lists assigned taxonomies for each.
	 *
	 * @param builder filter builder
	 * @return mapping of moduleId to List of assigned Taxonomies
	 */
	public Map<EntityId, Set<EntityId>> findTaxonomyIdPerModule(final BuildingConsumer<TaxonomyInquiryBuilder> builder) {
		return builder.prepare(new TaxonomyQueryBuilder())
				.buildModulesIds((rs, m) -> {
					final EntityId id = PgJSON.fromPGobject(rs.getObject("module_id"), EntityId.class);
					final Collection<EntityId> set = m.computeIfAbsent(id, k -> new LinkedHashSet<>());
					set.add(EntityId.of((UUID) rs.getObject(2), (Long) rs.getLong(3)));
				}
				);
	}

	/**
	 * Fetches all taxonomies and maps all assigned modules to it.
	 * 
	 * @param builder filter builder
	 * @return mapping of TaxonomyPojo to list of assigned modules
	 */
	public Map<TaxonomyPojo, Set<EntityId>> fetchTaxonomyToModules(final BuildingConsumer<TaxonomyInquiryBuilder> builder) {
		return builder.prepare(new TaxonomyQueryBuilder())
				.buildModules((rs, m) -> m.computeIfAbsent(extractTaxonomy(rs), k -> new LinkedHashSet<>())
						.add(PgJSON.fromPGobject(rs.getObject(1), EntityId.class)));
	}

	/**
	 * Returns a Map of CategoryId to number of referencing modules on given Project.
	 * @param builder filter builder
	 * @return Map of CategoryId to count of module referencing this category mapping
	 */
	public Map<Long, Long> fetchCategoryModuleCount(final BuildingConsumer<TaxonomyCategoryInquiryBuilder> builder) {
		return builder.prepare(new TaxonomyCategoryQueryBuilder())
			.buildCount();
	}
	
	/**
	 * Returns a mapping of Taxonomy Type to the count of modules assigned to it.
	 * @param builder filter builder
	 * @return mapping from Type to Count of assigned modules
	 */
	public Map<TaxonomyTypePojo, Long> fetchTypeModuleCount(final BuildingConsumer<TaxonomyTypeInquiryBuilder> builder) {
		return builder.prepare(new TaxonomyTypeQueryBuilder())
				.buildCount();
	}
	
	/**
	 * Retrieves the number of Modules assigned to each Taxonomy in a Project.
	 * @param projectId ID of the project.
	 * @param moduleIds Optional list of IDs limiting the Modules to include in the count.
	 * @return Map of Taxonomy IDs to number of assigned Modules.
	 */
	public Map<UUID, Long> getTaxonomyModuleCounts(final EntityId projectId, @Nullable final Collection<UUID> moduleIds) {
		return query("SELECT t.uid, count(mt.module) FROM taxonomy t"
				+ " LEFT JOIN (module_taxonomies mt INNER JOIN module m ON m.uid = mt.module")
			.when(moduleIds, (q, v) -> q.append(" AND m.uid = any(?)").addArg(PgType.UUID, v))
			.append(") ON mt.taxonomy = t.uid WHERE t.project = ").with(ProjectPgDao.referenceUidOrNid(projectId))
			.append(" GROUP BY t.uid")
			.toMap((rs, m) -> m.put((UUID) rs.getObject(1), rs.getLong(2)));
	}
	
	/**
	 * Returns a Table with aggregation data retrieved based on given builder.
	 * @param builder filter builder
	 * @return {@linkplain Table} with extracted data
	 */
	public Optional<Table> getAggregations(final BuildingConsumer<TaxonomyService.TaxonomyAggregationInquiryBuilder<?>> builder) {
		return builder.prepare(new TaxonomyAggregationQueryBuilder())
						.buildAggregation(this);
	}

	/**
	 * Creates a category database entry according to given Prototype Object
	 * @param proto specifying the category to create
	 * @return the ID of the created Category
	 */
	public long createCategory(final TaxonomyCategoryPojoPrototype proto) {
		return query("INSERT INTO taxonomy_category (project, name) VALUES (")
			.with(ProjectPgDao.referenceUidOrNid(proto.project.getNonNull()))
			.append(", ?) RETURNING id", proto.name.getNonNull())
			.first(rs -> rs.getLong(1)).orElseThrow().longValue();
	}

	/**
	 * Creates a Taxonomy database entry according to given Prototype Object
	 * @param proto specifying the Taxonomy to create
	 * @return the ID of the created Taxonomy
	 */
	public EntityId create(final TaxonomyPojoPrototype proto) {
		return createOrUpdate(proto, true);
	}

	/**
	 * Creates multiple Taxonomy database entry according to given Prototype Objects
	 * @param protos specifying the Taxonomies to create
	 * @return the IDs of the created Taxonomies
	 */
	public List<EntityId> create(final List<TaxonomyPojoPrototype> protos) {
		//this can't be done as a batch insert, as we need the returned nids.
		//only option to do it as an PostgresBatch Update would be
		//to safe the 'uid' into a list, and later fetch all NIDs to the created UIDs
		final List<EntityId> newTaxonomyIds = new ArrayList<>();
		for (final TaxonomyPojoPrototype proto : protos) {
			final var uid = proto.uid.orElseNonNull(UUID::randomUUID);
			final FieldBuilder fields = new FieldBuilder()
					.add(proto.project.required(true), "project", ProjectPgDao::referenceUidOrNid)
					.add("uid", "?", uid)
					.add(proto.type.required(true), "type", "?")
					.add(proto.name.required(true), "name", "?")
					.add(proto.customProperties, "custom_properties", "?", PgJSON::toPGobject);

			final QueryBuilder query = query("INSERT INTO taxonomy ");
			fields.buildUpsert(query, "uid");
			final var nid = query.append(" RETURNING nid")
					.first(rs -> Long.valueOf(rs.getLong(1)))
					.orElseThrow(() -> new PersistenceException("Failed to create taxonomy: '" + proto));

			newTaxonomyIds.add(EntityId.of(uid, nid));
		}
		return newTaxonomyIds;
	}

	/**
	 * Creates a Type database entry according to given Prototype Object
	 * @param proto specifying the Type to create
	 * @return the ID of the created Type
	 */
	public UUID createType(final TaxonomyTypePojoPrototype proto) {
		final EntityId project = proto.project.getNonNull();
		final String name = proto.name.getNonNull();
		return query("INSERT INTO taxonomy_type (id, project, category, name) VALUES (")
				.whenDefined(proto.id, (q, u) -> q.append("?", u))
				.when(! proto.id.isDefined(), q -> q.append("gen_random_uuid()"))
				.append(", ")
				.with(ProjectPgDao.referenceUidOrNid(project))
				.append(", ")
				.with(TaxonomyPgDao.referenceUidOrDefaultCategory(proto))
				.append(", ?) RETURNING id", name)
				.first(rs -> (UUID) rs.getObject(1)).orElseThrow();
	}
	
	/**
	 * Creating Taxonomies should fall back to the default taxonomy id specified in the project
	 * 
	 * @param proto to get values from
	 * @return the query builder
	 */
	static Consumer<QueryBuilder> referenceUidOrDefaultCategory(final TaxonomyTypePojoPrototype proto) {
		return q1 -> q1.whenDefined(proto.categoryId, (q, u) -> q.append("?", u))
						.when(! proto.categoryId.isDefined(), 
							q -> q.append("(SELECT default_taxonomy_category FROM project p WHERE p.uid = ")
								.with(ProjectPgDao.referenceUidOrNid(proto.project.getNonNull()))
								.append(")")
						)
;
	}

	/**
	 * Updates a Taxonomy in the DB
	 * Assumes that the referenced type exists
	 *
	 * @param proto -type specifying the fields to update in the DB
	 * @return the id of the updated entry
	 */
	public EntityId update(final TaxonomyPojoPrototype proto) {
		return createOrUpdate(proto, false);
	}

	/**
	 * Creates a new {@code Taxonomy} entity or updates an existing one.
	 *
	 * @param proto the {@link FieldBuilder} to create
	 * @param isNew {@code true} to create a new {@code module} entity. {@code false} to update an existing {@code module} entity
	 * @return the {@link EntityId} of the created {@code Taxonomy}.
	 */
	private EntityId createOrUpdate(final TaxonomyPojoPrototype proto, final boolean isNew) {
		final FieldBuilder fields = new FieldBuilder()
				.add(proto.type.exclusive(isNew), "type", "?")
				.add(proto.name.required(isNew), "name", "?")
				.add(proto.customProperties, "custom_properties", "?", PgJSON::toPGobject);

		final UUID uid;
		final QueryBuilder query;
		if (isNew) {
			uid = proto.uid.orElseNonNull(UUID::randomUUID);
			fields.add("uid", "?", uid)
				.add(proto.project.required(isNew), "project", ProjectPgDao::referenceUidOrNid);

			query = query("INSERT INTO taxonomy ");
			fields.buildUpsert(query, "uid");
		} else {
			uid = proto.uid.getNonNull();
			query = query("UPDATE taxonomy SET ");
			fields.buildUpdate(query);
			query.append(" WHERE uid = ?", uid);
		}

		final var nid = query.append(" RETURNING nid")
			.first(rs -> Long.valueOf(rs.getLong(1)))
			.orElseThrow(() -> new PersistenceException("Failed to create taxonomy: '" + proto));
		
		return EntityId.of(uid, nid);
	}

	/**
	 * Updates a TaxonomyType in the DB
	 * Assumes that the referenced category exists
	 *
	 * @param proto -type to update in the db
	 * @return the id of the updated entry
	 */
	public long updateType(final TaxonomyTypePojoPrototype proto) {
		final var fields = new FieldBuilder()
				.add(proto.categoryId, "category", "?")
				.add(proto.name, "name", "?");

		final UUID id = proto.id.get();
		return query("UPDATE taxonomy_type SET ")
				.with(fields::buildUpdate)
				.append(" WHERE id = ? ", id)
				.update();
	}
	
	public long updateCategory(final TaxonomyCategoryPojoPrototype proto) {
		final Long id = proto.id.getNonNull();
		//Ignoring Project, as it can't be updated on an existing Category
		query("UPDATE taxonomy_category SET ")
				.whenDefined(proto.name, (q, t) -> q.append("name = ? ", t))
				.append("WHERE id = ?", id)
				.update();
		return id;
	}

	/**
	 * Executes a delete query for filter defined by the builder.
	 * @param builder Query builder for filtering {@code Taxonomy} entities.
	 * @return number of deleted records
	 */
	public long delete(final BuildingConsumer<TaxonomyInquiryBuilder> builder) {
		return builder.prepare(new TaxonomyQueryBuilder())
				.buildDelete();
	}

	/**
	 * Executes a delete query for filter defined by the builder.
	 * @param builder Query builder for filtering {@code TaxonomyCategory} entities.
	 * @return number of deleted records
	 */
	public long deleteCategory(final BuildingConsumer<TaxonomyCategoryInquiryBuilder> builder) {
		return builder.prepare(new TaxonomyCategoryQueryBuilder())
				.buildDelete();
	}

	/**
	 * Executes a delete query for filter defined by the builder.
	 * @param builder Query builder for filtering {@code TaxonomyType} entities.
	 * @return number of deleted records
	 */
	public long deleteType(final BuildingConsumer<TaxonomyTypeInquiryBuilder> builder) {
		return builder.prepare(new TaxonomyTypeQueryBuilder())
				.buildDelete();
	}

	/**
	 * Deletes all entries from the module_taxonomies Table where the module is part of {@code modules} and the taxonomy is contained in {@code taxonomies}
	 *
	 * @param builder Query builder for filtering {@code Taxonomy} entities to delete.
	 * @return the amount of rows deleted
	 */
	public List<UUID> deleteModuleLinks(final BuildingConsumer<TaxonomyInquiryBuilder> builder) {
		return builder.prepare(new TaxonomyQueryBuilder())
				.buildDeleteLinks();
	}

	/**
	 * Inserts or Updates the Category defined by given prototype in the DB
	 *
	 * @param proto the category definition to update
	 * @return the ID of the Modified Category
	 */
	public long upsertCategory(final TaxonomyCategoryPojoPrototype proto) {
		if (proto.id.isDefined()) {
			return updateCategory(proto);
		}
		
		final EntityId projectId = proto.project.getNonNull();
		final String name = proto.name.getNonNull();
		
		return query("INSERT INTO taxonomy_category (project, name) VALUES (")
				.with(ProjectPgDao.referenceUidOrNid(projectId))
				.append(", ?) ", name)
				.append("ON CONFLICT (project, name) ")
				.append("DO UPDATE SET name = EXCLUDED.name ")//identical Name, update it to return an ID
				.append("RETURNING id")
				.first(rs -> rs.getLong(1))
				.orElseThrow().longValue();
	}
	
	/**
	 * Adds an entry to the module_taxonomies table from each Taxonomy in {@code taxonomies} to given {@code module}
	 *
	 * @param module to link taxonomies to
	 * @param taxonomies to link to the module
	 * @return number of updated rows
	 */
	public long createModuleLinks(final EntityId module, final Collection<EntityId> taxonomies) {

		final Object moduleUid = module.getUidOptional().orElse(null);
		final var moduleNid = module.getNidOptional().orElse(null);
		final var batchArgs = taxonomies.stream()
				 .map(t -> Stream.of(
						 //stream of module and taxonomy uids and nids, containing nulls, handled by postgres
						 moduleUid,
						 moduleNid,
						 t.getUidOptional().orElse(null),
						 t.getNidOptional().orElse(null))
				 );

	    final int[] result = query("INSERT INTO module_taxonomies (module, taxonomy) VALUES ("
					+ "(SELECT uid FROM module WHERE uid = ? OR nid = ?), "
					+ "(SELECT uid FROM taxonomy WHERE uid = ? OR nid = ?)"
				+ ") ON CONFLICT DO NOTHING")
			.updateBatch(batchArgs, 1_000);

		return IntStream.of(result).sum();
	}

	/**
	 * Adds an entry to the module_taxonomies table from each Module in {@code modules} to each Taxonomy in {@code taxonomies}
	 * Creates modules.size() * taxonomies.size() links.
	 *
	 * @param modules to each link all taxonomies to
	 * @param taxonomies to link to each module
	 * @return number of updated rows
	 */
	public long createModuleLinks(final Collection<EntityId> modules, final Collection<EntityId> taxonomies) {
		final List<Stream<Object>> batchArgs = new ArrayList<>();

		for (final EntityId module : modules) {
			for (final EntityId taxonomy : taxonomies) {
				//stream of module and taxonomy uids and nids, containing nulls that are handled by postgres
				batchArgs.add(Stream.of(
						(Object) module.getUidOptional().orElse(null),
						module.getNidOptional().orElse(null),
						taxonomy.getUidOptional().orElse(null),
						taxonomy.getNidOptional().orElse(null))
				);
			}
		}

		//Create the links if they don't exist already
		final int[] result = query("INSERT INTO module_taxonomies (module, taxonomy) VALUES ("
					+ "(SELECT uid FROM module WHERE uid = ? OR nid = ?), "
					+ "(SELECT uid FROM taxonomy WHERE uid = ? OR nid = ?)"
				+ ") ON CONFLICT (module, taxonomy) "
				+ "DO NOTHING")
			.updateBatch(batchArgs.stream(), 1_000);

		return IntStream.of(result).sum();
	}
	
	/**
	 * Adds an entry to the module_taxonomies table from each Taxonomy in {@code taxonomies} to given {@code module}
	 *
	 * @param module to link taxonomy to
	 * @param taxonomy to link to the module
	 * @return number of updated rows
	 */
	public long createModuleLink(final UUID module, final EntityId taxonomy) {
		if (taxonomy.hasUid()) {
			return query("INSERT INTO module_taxonomies (module, taxonomy) "
					+ "VALUES (?,  ?) "
					+ "ON CONFLICT (module, taxonomy) DO NOTHING")
					.addArg(module)
					.addArg(taxonomy.getUid())
					.update();
		} else {
			return query("INSERT INTO module_taxonomies (module, taxonomy) "
					+ "VALUES (?,  (SELECT nid FROM taxonomy WHERE uid = ?)) "
					+ "ON CONFLICT (module, taxonomy) DO NOTHING")
					.addArg(module)
					.addArg(taxonomy.getNid())
					.update();
		}
	}
	
	/**
	 * Extracts the taxonomy from given result set. 
	 * Also extracts the Type and Category (referenced by the Taxonomy)
	 *
	 * @param rs to extract from
	 * @return the created Pojo
	 * @throws SQLException when theres a problem with accessing the ResultSet
	 */
	private TaxonomyPojo extractTaxonomy(final ResultSet rs) throws SQLException {
		return new TaxonomyPojo(
			(UUID) rs.getObject("tax_uid"), 
			rs.getLong("tax_nid"), 
			PgJSON.fromPGobject(rs.getObject("project_id"), EntityId.class), null, null,
			extractType(rs),
			rs.getString("tax_name"), 
			rs.getLong("ref_count"), 
			new CustomPropertiesMap(PgJSON.fromPGobject(rs.getObject("tax_customprops")))
		);
	}

	/**
	 * Extracts the taxonomy type from given result set. 
	 * Also extracts the Category (referenced by the Type)
	 *
	 * @param rs to extract from
	 * @return the created Pojo
	 * @throws SQLException when theres a problem with accessing the ResultSet
	 */
	private TaxonomyTypePojo extractType(final ResultSet rs) throws SQLException {
		return new TaxonomyTypePojo(
			(UUID) rs.getObject("typ_id"), 
			PgJSON.fromPGobject(rs.getObject("project_id"), EntityId.class), null, null, /* project */
			extractCategory(rs), 
			rs.getString("typ_name")
		);
	}
	
	/**	
	 * Extracts the taxonomy category from given result set. 
	 *
	 * @param rs to extract from
	 * @return the created Pojo
	 * @throws SQLException when theres a problem with accessing the ResultSet
	 */
	private TaxonomyCategoryPojo extractCategory(final ResultSet rs) throws SQLException {
		return new TaxonomyCategoryPojo(
			rs.getLong("cat_id"), 
			PgJSON.fromPGobject(rs.getObject("project_id"), EntityId.class), null, null, /* project */
			rs.getString("cat_name")
		);
	}
}

/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.access.postgres;

import static innowake.mining.shared.access.FilterUtil.toCollection;
import static innowake.mining.shared.access.FilterUtil.toEntityId;
import static innowake.mining.shared.access.FilterUtil.toEntityIds;
import static innowake.mining.shared.access.FilterUtil.toStrings;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_EQ;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_IN;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_NOT_EQ;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_NOT_IN;

import java.sql.Timestamp;
import java.time.Instant;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import javax.persistence.EntityNotFoundException;

import org.apache.commons.lang.StringUtils;
import org.springframework.jdbc.core.JdbcTemplate;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.AnnotationService.AnnotationAggregationInquiryBuilder;
import innowake.mining.shared.access.AnnotationService.AnnotationCategoryInquiryBuilder;
import innowake.mining.shared.access.AnnotationService.AnnotationInquiryBuilder;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.access.Table;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.lang.ConditionalConsumer;
import innowake.mining.shared.model.AnnotationCategory;
import innowake.mining.shared.model.AnnotationFieldName;
import innowake.mining.shared.model.AnnotationReport;
import innowake.mining.shared.model.AnnotationReportSearchParameter;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;

/**
 * Postgres specific access methods for Annotations.
 */
public class AnnotationPgDao extends PgDao {
	
	private static final String SOURCE_COLUMN = "CASE WHEN a.source IS null THEN module_location_substr(a.location, s.content) ELSE a.source END";
	private static final String SELECT_MODULE_TAXONOMY = "a.module IN (SELECT mt.module FROM module_taxonomies mt JOIN taxonomy t ON t.uid = mt.taxonomy ";

	public AnnotationPgDao(final JdbcTemplate jdbcTemplate) {
		super(jdbcTemplate);
	}
	
	static Consumer<QueryBuilder> referenceUidOrNid(final EntityId annotationId) {
		return q -> q.appendId(annotationId, "?", "(SELECT uid FROM annotation WHERE nid = ?)");
	}

	public class AnnotationQueryBuilder implements AnnotationService.AnnotationInquiryBuilder, CustomPropertiesPgDao.CustomPropertiesInquiryBuilderStub {

		protected final OrderStreamBuilder orders = new OrderStreamBuilder();
		protected final FilterStreamBuilder filters = new FilterStreamBuilder();

		/**
		 * Adds all registered joins to given query builder
		 * @param qb to add joins
		 */
		protected void buildJoins(final QueryBuilder qb) {
			qb.append("INNER JOIN module m ON m.uid = a.module ");
			qb.append("LEFT JOIN annotation_category c ON c.id = a.category ");
			qb.append("LEFT JOIN source s ON s.id = m.source ");
		}

		protected Paged.Builder<AnnotationPojo> build(@Nullable final Pagination paging) {
			/* For a module multiple functional_blocks per annotation can exist. To avoid duplicates the clumsy mode starts when hasFnGroupName is true */
			if (paging != null) {
				orders.accept(q -> q.appendOrder("a.uid", SortDirection.ASCENDING));
			}
			return query()
					.append("SELECT a.uid, a.nid, "
					+ "(SELECT to_jsonb(project_ids) FROM (SELECT uid, nid FROM project WHERE uid = m.project) project_ids),"
					+ " a.name, a.state, a.type, "
					+ "a.category, c.name, a.created_by, "
					+ "a.updated_by, m.uid, m.nid, "
					+ "m.name, m.path, to_jsonb(a.location), m.source, " 
					+ SOURCE_COLUMN + ", a.translation, a.reasons,"
					+ " (SELECT array_agg(entry) FROM data_dictionary_annotations WHERE annotation = a.uid), "
					+ " a.custom_properties")
				.append(" FROM annotation a ")
				.with(this::buildJoins)
				.with(filters::build)
				.with(orders::build)
				.toPageable(paging, (rs, n) -> new AnnotationPojo(
					(UUID) rs.getObject(1), /* uid (UUID) */
					rs.getLong(2), /* nid (Long) */
					PgJSON.fromPGobject(rs.getObject(3), EntityId.class), null, null, /* project (EntityId) */
					rs.getString(4), /* name (String) */
					WorkingState.valueOf(rs.getString(5)), /* state (WorkingState) */
					AnnotationType.valueOf(rs.getString(6)), /* type (AnnotationType) */
					(Long) rs.getObject(7), /* categoryId [Long], can be null */
					rs.getString(8), /* categoryName [String] */
					rs.getString(9), /* createdBy (String) */
					rs.getString(10), /* updatedBy [String] */
					EntityId.of((UUID) rs.getObject(11), rs.getLong(12)), null, null, /* module (EntityId) */
					rs.getString(13), /* moduleName (String) */
					rs.getString(14), /* modulePath [String] */
					PgJSON.fromPGobject(rs.getObject(15), ModuleLocation.class), /* location (ModuleLocation) */
					(UUID) rs.getObject(16), /* source [UUID] */
					mapNullable(rs.getBytes(17), BinaryString::new), /* sourceAttachment [String] */
					rs.getString(18), /* englishTranslation [String] */
					PgUtil.<String>streamArray(rs.getArray(19)).collect(Collectors.toUnmodifiableList()), /* reasons (List<String>) */
					PgUtil.<UUID>streamArray(rs.getArray(20)).collect(Collectors.toUnmodifiableList()), /* ddEntries (List<UUID>) */
					new CustomPropertiesMap(PgJSON.fromPGobject(rs.getObject(21))) /* customProperties (CustomPropertiesMap) */
				));
		}
		
		protected QueryBuilder buildDelete() {
			return query("WITH annotations_updated AS ("
					+ "DELETE FROM annotation _delete USING annotation a ")
					.with(this::buildJoins)
					.append(" WHERE _delete.uid = a.uid")
					.with(filters::buildSubsequent)
					.append(" RETURNING a.uid, a.nid, a.module m_uid, (SELECT nid FROM module WHERE uid = a.module) m_nid")
					.append(") UPDATE module m SET modified_date = ? FROM annotations_updated a WHERE m.uid = a.m_uid"
							+ " RETURNING a.uid, a.nid, a.m_uid, a.m_nid", Timestamp.from(Instant.now()));
		}
		
		protected QueryBuilder buildCount() {
			return query("SELECT count(*) FROM annotation a ")
					.with(this::buildJoins)
					.with(filters::build);
		}
		
		@Override
		public AnnotationInquiryBuilder byId(final EntityId id) {
			filters.accept(q -> q.appendId(id, "a"));
			return this;
		}

		@Override
		public AnnotationInquiryBuilder byMinId(final EntityId id) {
			if (id.hasNid()) {
				filters.accept(q -> q.append("a.nid >= ?", id.getNid()));
			} else {
				filters.accept(q -> q.append("a.nid >= (SELECT annotation.nid FROM annotation WHERE annotation.uid = ?)", id.getUid()));
			}
			return this;
		}

		@Override
		public AnnotationInquiryBuilder byMaxId(final EntityId id) {
			if (id.hasNid()) {
				filters.accept(q -> q.append("a.nid <= ?", id.getNid()));
			} else {
				filters.accept(q -> q.append("a.nid <= (SELECT annotation.nid FROM annotation WHERE annotation.uid = ?)", id.getUid()));
			}
			return this;
		}

		@Override
		public AnnotationInquiryBuilder byIds(final Collection<EntityId> ids) {
			final var uids = EntityId.allUids(ids);
			if (uids.size() == ids.size()) {
				filters.accept(q -> q.append("a.uid = any(?)")
									 .addArg(PgType.UUID, uids));
				return this;
			}

			final var nids = EntityId.allNids(ids);
			if (nids.size() == ids.size()) {
				filters.accept(q -> q.append("a.nid = any(?)")
									 .addArg(PgType.LONG, nids));
				return this;
			}

			filters.accept(q -> q.append("(a.uid = any(?) OR a.nid = any(?))")
								 .addArg(PgType.UUID, uids)
								 .addArg(arrayFromCollection(PgType.LONG, nids)));
			return this;
		}

		@Override
		public AnnotationInquiryBuilder byNids(final Collection<Long> ids) {
			filters.accept(q -> q.append("a.nid = any(?)").addArg(PgType.LONG, ids));
			return this;
		}
		
		@Override
		public AnnotationInquiryBuilder ofProject(final EntityId project) {
			filters.accept(q -> q.append("m.project = ").with(ProjectPgDao.referenceUidOrNid(project)));
			return this;
		}
		
		@Override
		public AnnotationInquiryBuilder withName(final String name) {
			filters.accept(q -> q.append("a.name ILIKE ?", PgUtil.pgPattern(name)));
			return this;
		}

		@Override
		public AnnotationInquiryBuilder withState(final WorkingState state) {
			filters.accept(q -> q.append("a.state = ?::" + PgMiningType.WORKING_STATE, state.name()));
			return this;
		}
		
		@Override
		public AnnotationInquiryBuilder withStates(final Collection<WorkingState> states) {
			filters.accept(q -> q.append("a.state = any(?)").addArg(PgMiningType.WORKING_STATE, states));
			return this;
		}
		
		@Override
		public AnnotationInquiryBuilder withCategories(final Collection<Long> categories) {
			filters.accept(q -> q.append("a.category = any(?)").addArg(PgType.LONG, categories));
			return this;
		}
		
		@Override
		public AnnotationInquiryBuilder withModuleName(final String name, final boolean caseInsensitive) {
			if (caseInsensitive) {
				filters.accept(q -> q.append("m.name ILIKE ?", PgUtil.pgPattern(name)));
			} else {
				filters.accept(q -> q.append("m.name LIKE ?", PgUtil.pgPattern(name)));
			}
			return this;
		}
		
		@Override
		public AnnotationInquiryBuilder withLikeModuleName(final String name, final boolean caseInsensitive) {
			/* This should be used when the entered search term is not the whole pattern */
			if (caseInsensitive) {
				filters.accept(q -> q.append("m.name ILIKE ?", "%" + PgUtil.pgPattern(name) + "%"));
			} else {
				filters.accept(q -> q.append("m.name LIKE ?", "%" + PgUtil.pgPattern(name) + "%"));
			}
			return this;
		}

		@Override
		public AnnotationInquiryBuilder withFunctionalGroupName(final String name) {
			filters.accept(q -> q.append("a.nid IN ("
						+ "SELECT gf.annotation_nid FROM functional_block_generated_from gf "
						+ "JOIN functional_block fu ON gf.functional_block = fu.uid "
						+ "JOIN functional_block_children c ON c.child = fu.uid "
						+ "JOIN functional_block fg ON fg.uid = c.parent "
						+ "WHERE fg.flags->'TYPE' ?? 'FUNCTIONAL_GROUP' AND fg.name ILIKE ?"
					+ " ORDER BY gf.annotation_nid)", PgUtil.pgPattern(name)));
			return this;
		}

		@Override
		public AnnotationInquiryBuilder withFunctionalBlocks(final List<UUID> uids) {
			filters.accept(q -> q.append("a.nid IN ("
						+ "SELECT gf.annotation_nid FROM functional_block_generated_from gf WHERE functional_block = any(?)"
					+ ")").addArg(PgType.UUID, uids));
			return this;
		}

		@Override
		public AnnotationInquiryBuilder withoutFunctionalGroupAssignment() {
			filters.accept(q -> q.append("a.nid NOT IN (SELECT gf.annotation_nid FROM functional_block_generated_from gf" +
					" JOIN functional_block fu ON gf.functional_block = fu.uid" +
					" JOIN functional_block_children c ON c.child = fu.uid" +
					" JOIN functional_block fg ON fg.uid = c.parent" +
					" WHERE fg.flags->'TYPE' ?? 'FUNCTIONAL_GROUP')"));
			return this;
		}

		@Override
		public AnnotationInquiryBuilder withTaxonomyNames(final Collection<String> name) {
			filters.accept(q -> q.append(SELECT_MODULE_TAXONOMY + "WHERE t.name = any(?))").addArg(PgType.STRING, name));
			return this;
		}

		@Override
		public AnnotationInquiryBuilder withTaxonomyTypeName(final String typeName) {
			filters.accept(q -> q.append(SELECT_MODULE_TAXONOMY + "JOIN taxonomy_type tt ON tt.id = t.type "
					+ "WHERE tt.name = ?)").addArg(typeName));
			return this;
		}

		@Override
		public AnnotationInquiryBuilder notWithTaxonomyTypeName(final String typeName) {
			filters.accept(q -> q.append("a.module NOT IN (SELECT mt.module FROM module_taxonomies mt "
					+ "JOIN taxonomy t ON t.uid = mt.taxonomy "
					+ "JOIN taxonomy_type tt ON tt.id = t.type "
					+ "WHERE tt.name = ?)").addArg(typeName));
			return this;
		}

		@Override
		public AnnotationInquiryBuilder withModulePath(final String path) {
			filters.accept(q -> q.append("m.path LIKE ?", path));
			return this;
		}
		
		@Override
		public AnnotationInquiryBuilder ofModule(final EntityId module) {
			filters.accept(q -> q.appendId(module, "m"));
			return this;
		}

		@Override
		public AnnotationInquiryBuilder withModuleType(final Type type) {
			filters.accept(q -> q.append("m.type = ?", type.name()));
			return this;
		}

		@Override
		public AnnotationInquiryBuilder withModuleTypes(final Collection<Type> types) {
			filters.accept(q -> q.append("m.type = any(?)", arrayFromCollection(PgType.STRING, types.stream().map(Type::name).collect(Collectors.toList()))));
			return this;
		}

		@Override
		public AnnotationInquiryBuilder withModuleTechnology(final Technology technologyName) {
			filters.accept(q -> q.append("m.technology = ?", technologyName.name()));
			return this;
		}

		@Override
		public AnnotationInquiryBuilder withModuleTechnologies(final Collection<Technology> technologies) {
			final List<String> technologyNames = technologies.stream().map(Technology::name).collect(Collectors.toList());
			filters.accept(q -> q.append("m.technology = any(?)", arrayFromCollection(PgType.STRING, technologyNames)));
			return this;
		}
		
		@Override
		public AnnotationInquiryBuilder ofModules(final Collection<EntityId> modules) {
			final var uids = EntityId.allUids(modules);
			if (uids.size() == modules.size()) {
				filters.accept(q -> q.append("m.uid = any(?)")
									 .addArg(PgType.UUID, uids));
				return this;
			}

			final var nids = EntityId.allNids(modules);
			if (nids.size() == modules.size()) {
				filters.accept(q -> q.append("m.nid = any(?)")
									 .addArg(PgType.LONG, nids));
				return this;
			}

			filters.accept(q -> q.append("(m.uid = any(?) OR m.nid = any(?))")
								 .addArg(PgType.UUID, uids)
								 .addArg(arrayFromCollection(PgType.LONG, nids)));
			return this;
		}

		@Override
		public AnnotationInquiryBuilder ofDataDictionaryEntry(final EntityId dde) {
			filters.accept(q -> q.append("a.uid in (SELECT annotation FROM data_dictionary_annotations WHERE entry = ")
					.with(DataDictionaryPgDao.referenceUidOrNid(dde)).append(")"));
			return this;
		}
		
		@Override
		public AnnotationInquiryBuilder withTypes(final Collection<AnnotationType> types) {
			filters.accept(q -> q.append("a.type = any(?)").addArg(PgType.STRING, types));
			return this;
		}

		@Override
		public AnnotationInquiryBuilder withType(final AnnotationType type) {
			filters.accept(q -> q.append("a.type = ?", type.name()));
			return this;
		}

		@Override
		public AnnotationInquiryBuilder withCategory(@Nullable final String categoryName) {
			if (categoryName == null) {
				filters.accept(q -> q.append("c.name IS NULL"));
			} else {
				filters.accept(q -> q.append("c.name LIKE ?", categoryName));
			}
			return this;
		}

		@Override
		public AnnotationInquiryBuilder withCategoryNames(@Nullable final Collection<String> categoryNames) {
			if (categoryNames == null || categoryNames.isEmpty()) {
				filters.accept(q -> q.append("c.name IS NULL"));
			} else {
				filters.accept(q -> q.append("c.name LIKE any(?)", arrayFromCollectionNonEmpty(PgType.STRING, categoryNames)));
			}
			return this;
		}

		@Override
		public AnnotationInquiryBuilder withReason(@Nullable final String reason) {
			if (reason == null) {
				filters.accept(q -> q.append("a.reasons IS NULL"));
			} else {
				filters.accept(q -> q.append("? = any(a.reasons)", reason));
			}
			return this;
		}

		@Override
		public AnnotationInquiryBuilder withReasons(@Nullable final Collection<String> reasons) {
			if (reasons == null || reasons.isEmpty()) {
				filters.accept(q -> q.append("array_length(a.reasons, 1) = 0"));
			} else {
				final var filter = new StringBuilder();
				if (reasons.contains(null)) {
					filter.append("a.reasons IS NULL OR ");
					reasons.remove(null);
				}
				filter.append("a.reasons && ?");
				filters.accept(q -> q.append(filter.toString()).addArg(PgType.STRING, reasons));
			}
			return this;
		}

		@Override
		public AnnotationInquiryBuilder withLocation(final ModuleLocation location) {
			final Integer offset = location.getOffset() >= 0 ? location.getOffset() : null;
			final Integer length = location.getLength() >= 0 ? location.getLength() : null;
			filters.accept(q -> q.append("(a.location).offset = ? AND (a.location).length = ?", offset, length));
			return this;
		}

		@Override
		public AnnotationInquiryBuilder withLocationInRange(final ModuleLocation location) {
			final Integer startOffset = location.getOffset() > 0 ? location.getOffset() : null;
			final Integer endOffset = startOffset != null && location.getLength() > 0 ? startOffset + location.getLength() - 1 : null;
			filters.accept(q -> q.append("((a.location).offset BETWEEN ? AND ? OR ((a.location).offset + (a.location).length) BETWEEN ? AND ?)",
					startOffset, endOffset, startOffset, endOffset));
			return this;
		}

		@Override
		public AnnotationInquiryBuilder withOffsetBetween(final Integer start, final Integer end) {
			filters.accept(q -> q.append("(a.location).offset BETWEEN ? AND ?", start, end));
			return this;
		}

		@Override
		public AnnotationInquiryBuilder filterHasSource(final boolean hasSource) {
			if (hasSource) {
				filters.accept(q -> q.append("a.source IS NOT Null"));
			} else {
				filters.accept(q -> q.append("a.source IS Null"));
			}
			return this;
		}

		@Override
		public AnnotationInquiryBuilder filterSourceContains(final String content) {
			//this is quick, but case sensitive
//			filters.accept(q -> q.append("position(?::bytea in a.source::bytea) > 0", content));

			//might be slow, due to encode the whole content
			if(content.contains("*")) {
				//The asterisk (*) wildcard is replaced with the SQL wildcard (%) to facilitate filtering in queries.
				filters.accept(q -> q.append("encode(a.source, 'escape') ILIKE ?", content.replace('*', '%')));
			}
			else {
				filters.accept(q -> q.append("encode(a.source, 'escape') ILIKE ?", "%" + content + "%"));
			}
			return this;
		}

		@Override
		public AnnotationInquiryBuilder withMinOffset(final Integer offset) {
			filters.accept(q -> q.append("(a.location).offset >= ?", offset));
			return this;
		}

		@Override
		public AnnotationInquiryBuilder sortNid(final SortDirection direction) {
			orders.accept(q -> q.appendOrder("a.nid", direction));
			return this;
		}
		
		@Override
		public AnnotationInquiryBuilder sortName(final SortDirection direction) {
			orders.accept(q -> q.appendOrder("a.name", direction));
			return this;
		}

		@Override
		public AnnotationInquiryBuilder sortType(final SortDirection direction) {
			orders.accept(q -> q.appendOrder("a.type", direction));
			return this;
		}

		@Override
		public AnnotationInquiryBuilder sortState(final SortDirection direction) {
			orders.accept(q -> q.appendOrder("a.state", direction));
			return this;
		}
		
		@Override
		public AnnotationInquiryBuilder sortCategory(final SortDirection direction) {
			orders.accept(q -> q.appendOrder("a.category", direction));
			return this;
		}

		@Override
		public AnnotationInquiryBuilder sortReasons(final SortDirection direction) {
			orders.accept(q -> q.appendOrder("a.reasons", direction));
			return this;
		}

		@Override
		public void appendCustomPropertiesField(final QueryBuilder q) {
			q.append("a.custom_properties");
		}
		
		@Override
		public void acceptCustomPropetiesFilter(final ConditionalConsumer<QueryBuilder> clause) {
			filters.accept(clause);
		}
		
		@Override
		public AnnotationInquiryBuilder sortByModuleName(final SortDirection direction) {
			orders.accept(q -> q.appendOrder("m.name", direction));
			return this;
		}

		@Override
		public AnnotationInquiryBuilder sortByFunctionalGroupName(final SortDirection direction) {
			orders.accept(q -> q.appendOrder("(SELECT "
					+ "array_agg(parent_fb.name ORDER BY fbc.ordinal, parent_fb.name) " //sorting this aggregation by direction makes UX weird
					+ "FROM functional_block_generated_from gf "
					+ "LEFT JOIN functional_block child_fb ON gf.functional_block = child_fb.uid "
					+ "LEFT JOIN functional_block_children fbc ON gf.functional_block = fbc.child "
					+ "LEFT JOIN functional_block parent_fb ON fbc.parent = parent_fb.uid "
					+ "WHERE parent_fb.flags->'TYPE' ?? 'FUNCTIONAL_GROUP' "
					+ " AND gf.annotation_nid = a.nid) COLLATE CI", direction));
			return this;
		}
		@Override
		public AnnotationInquiryBuilder sortByModuleLocation(final SortDirection direction) {
			orders.accept(q -> q.appendOrder("(a.location).offset", direction));
			return this;
		}
		
		@Override
		public AnnotationInquiryBuilder sortByCustomProperties(final SortDirection direction) {
			orders.accept(q -> q.appendOrder("a.custom_properties", direction));
			return this;
		}
	}

	public class AnnotationCategoryQueryBuilder implements AnnotationService.AnnotationCategoryInquiryBuilder {
		protected final FilterStreamBuilder filters = new FilterStreamBuilder();

		protected Paged.Builder<AnnotationCategory> build(@Nullable final Pagination paging) {
			return query("SELECT id, (SELECT to_jsonb(project_ids) FROM (SELECT uid, nid FROM project WHERE uid = ac.project) project_ids) as projectIds, name, types FROM annotation_category ac ")
					.with(filters::build)
					.toPageable(paging, (rs, n) -> {
						final var c = new AnnotationCategory();
						c.setId(rs.getLong(1));
						c.setProjectId(PgJSON.fromPGobject(rs.getObject(2), EntityId.class));
						c.setName(rs.getString(3));
						c.setTypes(PgUtil.<String>streamArray(rs.getArray(4)).map(AnnotationType::valueOf).collect(Collectors.toList()));
						return c;
					});
		}

		protected Paged.Builder<Long> buildIds(@Nullable final Pagination paging) {
			return query("SELECT id FROM annotation_category ac ")
					.with(filters::build)
					.toPageable(paging, (rs, n) -> rs.getLong(1));
		}

		@Override
		public AnnotationCategoryInquiryBuilder byId(final Long id) {
			filters.accept(q -> q.append("id = ?", id));
			return this;
		}

		@Override
		public AnnotationCategoryInquiryBuilder ofProject(final EntityId project) {
			filters.accept(q -> 
				q.append("project = ").with(ProjectPgDao.referenceUidOrNid(project))
			);
			return this;
		}
		
		@Override
		public AnnotationCategoryInquiryBuilder ofProjectWithDefault(final EntityId project) {
			filters.accept(q -> 
				q.append("(project = ").with(ProjectPgDao.referenceUidOrNid(project))
					.append(" OR project = ").with(ProjectPgDao.referenceUidOrNid(EntityId.of(0L)))
					.append(")")
			);
			return this;
		}

		@Override
		public AnnotationCategoryInquiryBuilder withName(final String name) {
			filters.accept(q -> q.append("name = ?", name));
			return this;
		}

		@Override
		public AnnotationCategoryInquiryBuilder withTypes(final Collection<AnnotationType> types) {
			filters.accept(q -> q.append("types && ?").addArg(PgType.STRING, types));
			return this;
		}
	}

	public class AnnotationAggregationQueryBuilder extends AbstractAggregationQueryBuilder<AnnotationFieldName, AnnotationAggregationQueryBuilder> 
						implements AnnotationAggregationInquiryBuilder<AnnotationAggregationQueryBuilder> {

		private boolean joinModule;
		private boolean joinTaxonomies;
		private boolean joinSource;
		private boolean joinCategory;

		@Override
		protected String getFromClause() {
			return "annotation a";
		}

		@Override
		protected void buildJoins(final QueryBuilder query) {
			query.when(joinModule || joinSource,
					q -> q.append("INNER JOIN module m ON m.uid = a.module "));
			query.when(joinCategory,
					q -> q.append("LEFT JOIN annotation_category c ON c.id = a.category "));
			query.when(joinSource,
					q -> q.append("LEFT JOIN source s ON s.id = m.source "
								+ "LEFT JOIN source_info si ON s.id = si.uid"));
			query.when(joinTaxonomies,
					q -> q.append("INNER JOIN module_taxonomies mt ON mt.module = a.module "));
		}

		@Override
		protected String getFieldQueryFragment(final AnnotationFieldName field) {
			switch (field) {
				case CATEGORY:
					joinCategory = true;
					break;
				case PROJECT_ID:
				case MODULE_TECHNOLOGY:
				case MODULE_TYPE:
					joinModule = true;
					break;
				case SOURCE_ATTACHMENT:
					joinSource = true;
					break;
				default:
					/* nothing to join */
					break;
			}

			switch (field) {
				case ID:
					return "a.nid AS " + field.name().toLowerCase();
				case NAME:
					return "a.name AS " + field.name().toLowerCase();
				case PROJECT_ID:
					return "m.project AS " + field.name().toLowerCase();
				case CATEGORY:
					return "c.name AS " + field.name().toLowerCase();
				case STATE:
					return "a.state AS " + field.name().toLowerCase();
				case TYPE:
					return "a.type AS " + field.name().toLowerCase();
				case CREATED_BY_USER_ID:
					return "a.created_by AS " + field.name().toLowerCase();
				case UPDATED_BY_USER_ID:
					return "a.updated_by AS " + field.name().toLowerCase();
				case SOURCE_ATTACHMENT:
					return "utf8_of_bytes(" + SOURCE_COLUMN + ") AS " + field.name().toLowerCase();
				case MODULE_TECHNOLOGY:
					return "m.technology AS " + field.name().toLowerCase();
				case MODULE_TYPE:
					return "m.type AS " + field.name().toLowerCase();
				case METADATA:
					return "UNNEST(coalesce(a.reasons, ARRAY[null])) AS " + field.name().toLowerCase();
				default:
					throw new UnsupportedOperationException("The field is not supported yet: " + field.name());
			}
		}

		@Override
		public AnnotationAggregationQueryBuilder byId(final String operator, final Object value) {
			if (OPERATOR_EQ.equals(operator)) {
				final EntityId id = toEntityId(value, "Annotation id");
				filters.accept(q -> q.append("a.").appendId(id));
			} else {
				throw new UnsupportedOperationException(String.format("The operator %s is not supported for ID", operator));
			}
			return this;
		}

		@Override
		public AnnotationAggregationQueryBuilder withName(final String operator, final Object value) {
			if (OPERATOR_EQ.equals(operator)) {
				filters.accept(q -> q.append("a.name = ?", value.toString()));
			} else {
				throw new UnsupportedOperationException(String.format("The operator %s is not supported for NAME", operator));
			}
			return this;
		}

		@Override
		public AnnotationAggregationQueryBuilder ofProject(final String operator, final Object value) {
			joinModule = true;
			if (OPERATOR_EQ.equals(operator)) {
				final EntityId project = toEntityId(value, "Project id");
				filters.accept(q -> q.append("m.project = ").with(ProjectPgDao.referenceUidOrNid(project)));
			} else {
				throw new UnsupportedOperationException(String.format("The operator %s is not supported for PROJECT", operator));
			}
			return this;
		}

		@Override
		public AnnotationAggregationQueryBuilder withCategory(final String operator, final Object value) {
			joinCategory = true;
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("c.name = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("c.name != ?", value.toString()));
					break;
				case OPERATOR_IN:
					final Collection<?> names = toCollection(value);
					filters.accept(q -> q.append("c.name = any(?)").addArg(PgType.STRING, names));
					break;
				case OPERATOR_NOT_IN:
					final Collection<?> categories = toCollection(value);
					filters.accept(q -> q.append("NOT c.name = any(?)").addArg(PgType.STRING, categories));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for CATEGORY", operator));
			}
			return this;
		}

		@Override
		public AnnotationAggregationQueryBuilder withState(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("a.state = ?::working_state", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("a.state != ?::working_state", value));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("a.state = any(?::working_state[])").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT a.state = any(?::working_state[])").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for STATE", operator));
			}
			return this;
		}

		@Override
		public AnnotationAggregationQueryBuilder withType(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("a.type = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("a.type != ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("a.type = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT a.type = any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for TYPE", operator));
			}
			return this;
		}

		@Override
		public AnnotationAggregationQueryBuilder withCreatedByUserId(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("a.created_by = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("a.created_by != ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("a.created_by = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT a.created_by = any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for CREATED_BY_USER_ID", operator));
			}
			return this;
		}

		@Override
		public AnnotationAggregationQueryBuilder withUpdatedByUserId(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("a.updated_by = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("a.updated_by != ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("a.updated_by = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT a.updated_by = any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for UPDATED_BY_USER_ID", operator));
			}
			return this;
		}

		@Override
		public AnnotationAggregationQueryBuilder withSourceAttachment(final String operator, final Object value) {
			joinSource = true;
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("si.name = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("si.name != ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("si.name = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT si.name = any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for SOURCE_ATTACHMENT", operator));
			}
			return this;
		}

		@Override
		public AnnotationAggregationQueryBuilder withModuleTechnology(final String operator, final Object value) {
			joinModule = true;
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("m.technology = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("m.technology != ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("m.technology = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT m.technology = any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for MODULE_TECHNOLOGY", operator));
			}
			return this;
		}

		@Override
		public AnnotationAggregationQueryBuilder withModuleType(final String operator, final Object value) {
			joinModule = true;
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("m.type = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("m.type != ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("m.type = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT m.type = any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for MODULE_TYPE", operator));
			}
			return this;
		}

		@Override
		public AnnotationAggregationQueryBuilder withMetadata(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("? = any(a.reasons)", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("NOT ? = any(a.reasons)", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("a.reasons @> ?").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT a.reasons @> ?").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for METADATA", operator));
			}
			return this;
		}

		@Override
		public AnnotationAggregationQueryBuilder withTaxonomy(final String operator, final Object value) {
			joinTaxonomies = true;
			switch (operator) {
				case OPERATOR_EQ:
					var tax = toEntityId(value, "Taxonomy id");
					filters.accept(q -> q.append("mt.taxonomy = ").with(TaxonomyPgDao.referenceUidOrNid(tax)));
					break;
				case OPERATOR_NOT_EQ:
					tax = toEntityId(value, "Taxonomy id");
					filters.accept(q -> q.append("mt.taxonomy != ?").with(TaxonomyPgDao.referenceUidOrNid(tax)));
					break;
				case OPERATOR_IN:
					var taxes = toEntityIds(value, "Taxonomy ids");
					filters.accept(q -> q.appendIds(taxes, "mt.taxonomy = ANY(?)", "mt.taxonomy IN (SELECT uid FROM taxonomy WHERE nid = ANY(?))"));
					break;
				case OPERATOR_NOT_IN:
					taxes = toEntityIds(value, "Taxonomy ids");
					filters.accept(q -> q.append("NOT (")
										 .appendIds(taxes, "mt.taxonomy = ANY(?)", "mt.taxonomy IN (SELECT uid FROM taxonomy WHERE nid = ANY(?))")
										 .append(")"));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for FIELD_USAGE", operator));
			}
			return this;
		}
	}

	public List<AnnotationPojo> find(final BuildingConsumer<AnnotationInquiryBuilder> builder) {
		return builder.prepare(new AnnotationQueryBuilder()).build(null).all();
	}
	
	public long count(final BuildingConsumer<AnnotationInquiryBuilder> builder) {
		return builder.prepare(new AnnotationQueryBuilder()).buildCount()
				.first(rs -> Long.valueOf(rs.getLong(1)))
				.orElse(Long.valueOf(0l))
				.longValue();
	}
	
	public Optional<AnnotationPojo> findAny(final BuildingConsumer<AnnotationInquiryBuilder> builder) {
		return builder.prepare(new AnnotationQueryBuilder()).build(Pagination.FIRST).first();
	}
	
	public Paged<AnnotationPojo> find(final Pagination page, final BuildingConsumer<AnnotationInquiryBuilder> builder) {
		final Paged.Builder<AnnotationPojo> query = builder.prepare(new AnnotationQueryBuilder()).build(page);
		return query.page();
	}

	public Optional<Table> getAggregations(final BuildingConsumer<AnnotationAggregationInquiryBuilder<?>> builder) {
		return builder.prepare(new AnnotationAggregationQueryBuilder()).distinct().buildAggregation(this);
	}

	public EntityId put(final AnnotationPojoPrototype annotation, final boolean isNew) {
		return queryPut(annotation, isNew).append(" SELECT uid, nid FROM annotations_updated")
				.first(rs -> EntityId.of((UUID) rs.getObject(1), Long.valueOf(rs.getLong(2))))
				.orElseThrow(() -> new MiningEntityNotFoundException(AnnotationPojo.class, annotation.identityProvisional().toString()));
	}
	
	public int[][] put(final List<AnnotationPojoPrototype> annotations, final boolean isNew, final int batchSize) {
		return queryBatch(annotations, batchSize, a -> queryPut(a, isNew).append(" SELECT FROM annotations_updated"));
	}
	
	private QueryBuilder queryPut(final AnnotationPojoPrototype annotation, final boolean isNew) {
		final EntityId id;
		final QueryBuilder q = query("WITH annotations_updated AS (");
		final var fields = new FieldBuilder();
		
		if (isNew) {
			id = EntityId.of(annotation.uid.orElseNonNull(UUID::randomUUID));
			q.append("INSERT INTO annotation ");
			fields.add("uid", "?", id.getUid());
		} else {
			id = annotation.identityProvisional();
			q.append("UPDATE annotation SET ");
		}
		
		fields.add(annotation.module.required(isNew), "module", ModulePgDao::referenceUidOrNid)
			.add(annotation.location.required(isNew), "location", ModulePgDao::appendLocation)
			.add(annotation.name.required(isNew), "name", "?")
			.add(annotation.state.required(isNew), "state", "?::working_state", WorkingState::name)
			.add(annotation.type.required(isNew), "type", "?", AnnotationType::name)
			.add(annotation.categoryId, "category", "?")
			.add(annotation.createdByUserId.exclusive(isNew), "created_by", "?")
			.add(annotation.updatedByUserId, "updated_by", "?")
			.add(annotation.sourceAttachment, "source", "?", BinaryString::get)
			.add(annotation.reasons, "reasons", PgType.STRING, isNew)
			.add(annotation.englishTranslation, "translation", "?")
			.and(CustomPropertiesPgDao.addField(annotation, isNew));
		
		if (isNew) {
			fields.buildInsert(q);
		} else {
			fields.buildUpdate(q);
			q.append(" WHERE ");
			q.appendId(id);
		}
		q.append(" RETURNING uid, nid, module")
			.append("), update_modified_date AS ("
					+ "UPDATE module SET modified_date = ? "
					+ "WHERE module.uid = any (SELECT module FROM annotations_updated)"
				+ ")", Timestamp.from(Instant.now()));
		
		return q;
	}
	
	public Map<EntityId, EntityId> delete(final BuildingConsumer<AnnotationInquiryBuilder> builder) {
		return builder.prepare(new AnnotationQueryBuilder()).buildDelete()
				.toMap((rs, m) -> m.put(
						EntityId.of((UUID) rs.getObject(1), rs.getLong(2)),
						EntityId.of((UUID) rs.getObject(3), rs.getLong(4))));
	}
	
	public List<AnnotationCategory> findCategories(final BuildingConsumer<AnnotationCategoryInquiryBuilder> builder) {
		return builder.prepare(new AnnotationCategoryQueryBuilder())
				.build(null)
				.all();
	}

	public List<Long> findCategoryIds(final BuildingConsumer<AnnotationCategoryInquiryBuilder> builder) {
		return builder.prepare(new AnnotationCategoryQueryBuilder())
				.buildIds(null)
				.all();
	}
	
	public Optional<AnnotationCategory> findCategory(final BuildingConsumer<AnnotationCategoryInquiryBuilder> builder) {
		return builder.prepare(new AnnotationCategoryQueryBuilder())
				.build(null)
				.first();
	}
	
	public List<AnnotationType> findDeclaredTypes(final EntityId project) {
		return query("SELECT DISTINCT unnest(types) FROM annotation_category c INNER JOIN project p ON p.uid = c.project WHERE p.nid = 0 OR p.").appendId(project)
			.toList((rs, n) -> AnnotationType.valueOf(rs.getString(1)));
	}
	
	public long createCategory(final EntityId project, final String name, final Collection<AnnotationType> types) {
		return query("INSERT INTO annotation_category (project, name, types) VALUES (").with(ProjectPgDao.referenceUidOrNid(project))
			.append(", ? ,?) RETURNING id", name).addArg(PgType.STRING, types)
			.first(rs -> rs.getLong(1)).orElseThrow();
	}
	
	public void updateCategory(final EntityId project, final Long id, final Optional<String> name, final Optional<Collection<AnnotationType>> types) {
		final QueryBuilder q = query("UPDATE annotation_category SET ");
		final var fields = new FieldBuilder();
		name.ifPresent(s -> fields.add("name", "?", s));
		types.ifPresent(c -> fields.add("types", "?", arrayFromCollection(PgType.STRING, c)));
		if (fields.buildUpdate(q) > 0) {
			q.append(" WHERE project = ").with(ProjectPgDao.referenceUidOrNid(project))
				.append(" AND id = ?", id)
				.updateOrThrow(() -> new EntityNotFoundException("Annotation Catagory " + id + " in Project " + project));
		}
	}
	
	public void deleteCategory(final EntityId project, final Long id) {
		query("DELETE FROM annotation_category WHERE project = ").with(ProjectPgDao.referenceUidOrNid(project))
			.append(" AND id = ?", id)
			.updateOrThrow(() -> new EntityNotFoundException("Annotation Category " + id + " in Project " + project));
	}
	
	public Paged<AnnotationReport> getReport(final EntityId projectId, final AnnotationReportSearchParameter parameters, final int limit) {
		final var q = query("SELECT ");
		final var m = q.new ColumnMapper<AnnotationReport>()
			.appendColumn("a.nid", n -> (rs, o) -> o.setId(rs.getLong(n)))
			.appendColumn("a.name", n -> (rs, o) -> o.setName(rs.getString(n)))
			.appendColumn("m.nid", n -> (rs, o) -> o.setModuleId(rs.getLong(n)))
			.appendColumn("m.name", n -> (rs, o) -> o.setModuleName(rs.getString(n)))
			.appendColumn("a.type", n -> (rs, o) -> o.setAnnotationType(map(rs.getString(n), AnnotationType::valueOf)))
			.appendColumn("a.category", n -> (rs, o) -> o.setCategoryId(rs.getLong(n)))
			.appendColumn("c.name", n -> (rs, o) -> o.setCategoryName(rs.getString(n)))
			.appendColumn("a.state", n -> (rs, o) -> o.setAnnotationState(map(rs.getString(n), WorkingState::valueOf)))
			.appendColumn("utf8_of_bytes(" + SOURCE_COLUMN + ")", n -> (rs, o) -> o.setSourceCode(rs.getString(n)))
			.appendColumn("t.names", n -> (rs, o) -> o.setTaxonomy(rs.getString(n)))
			.appendColumn("a.created_by", n -> (rs, o) -> o.setCreatedByUserId(rs.getString(n)))
			//select createdBy if updatedBy is null.
			.appendColumn("COALESCE(a.updated_by, a.created_by)", n -> (rs, o) -> o.setUpdatedByUserId(rs.getString(n)));
		q.append(" FROM annotation a"
				+ " INNER JOIN module m ON m.uid = a.module"
				+ " LEFT JOIN annotation_category c ON c.id = a.category"
				+ " LEFT JOIN source s ON s.id = m.source"
				+ " LEFT JOIN (SELECT tm.module, array_agg(tt.name) names FROM module_taxonomies tm "
				+ " INNER JOIN taxonomy tt ON tt.uid = tm.taxonomy GROUP BY tm.module) t ON t.module = m.uid");
		q.append(" WHERE m.project = ").with(ProjectPgDao.referenceUidOrNid(projectId))
			.when(parameters.getDescription(), StringUtils::isNotEmpty, (qq, v) -> qq.append(" AND a.name LIKE ?", v))
			.when(parameters.getAnnotationType(), (qq, v) -> qq.append(" AND a.type = ?", v.name()))
			.when(parameters.getAnnotationCategory(), StringUtils::isNotEmpty, (qq, v) -> qq.append(" AND c.name = ?", v))
			.when(parameters.getUpdatedBy(), StringUtils::isNotEmpty, (qq, v) -> qq.append(" AND (a.updated_by = ? OR a.created_by = ?)", v, v))
			.when(parameters.getModuleName(), StringUtils::isNotEmpty, (qq, v) -> qq.append(" AND m.name LIKE ?", v))
			.when(parameters.getModuleTaxonomy(), (qq, v) -> qq.append(" AND ? = any(t.names)", v));
		return q.toPageable(new Pagination(0, limit), (rs, n) -> m.map(rs, new AnnotationReport())).page();
	}
	
}

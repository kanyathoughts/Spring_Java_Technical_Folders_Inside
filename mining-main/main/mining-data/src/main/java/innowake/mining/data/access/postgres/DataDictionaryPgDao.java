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
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_GTE;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_IN;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_IS_FALSE;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_IS_TRUE;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_LTE;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_NOT_EQ;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_NOT_IN;

import java.sql.Timestamp;
import java.time.Instant;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.DataDictionaryService.DataDictionaryAggregationInquiryBuilder;
import innowake.mining.shared.access.DataDictionaryService.DataDictionaryInquiryBuilder;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.access.Table;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.lang.ConditionalConsumer;
import innowake.mining.shared.model.DataDictionaryFieldName;
import innowake.mining.shared.model.DataDictionaryVariableScope;
import innowake.mining.shared.model.DefinedLocation;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.WorkingState;
import org.springframework.jdbc.core.JdbcTemplate;

/**
 * Postgres specific access methods for DataDictionary.
 */
public class DataDictionaryPgDao extends PgDao {

	private static final int BATCH_SIZE = 1_000;

	/**
	 * The constructor.
	 * @param jdbcTemplate the data source object
	 */
	public DataDictionaryPgDao(final JdbcTemplate jdbcTemplate) {
		super(jdbcTemplate);
	}
	
	static Consumer<QueryBuilder> referenceUidOrNid(final EntityId ddeId) {
		return q -> q.appendId(ddeId, "?", "(SELECT uid FROM data_dictionary WHERE nid = ?)");
	}
	
	public class DataDictionaryQueryBuilder extends DataDictionaryAggregationQueryBuilder implements DataDictionaryService.DataDictionaryInquiryBuilder,
			CustomPropertiesPgDao.CustomPropertiesInquiryBuilderStub {

		protected boolean joinDataDictionaryAnnotation = false;

		protected boolean joinDataFlowNode = false;

		@SuppressWarnings("unchecked")
		protected Paged.Builder<DataDictionaryPojo> build(@Nullable final Pagination paging) {
			joinModules = true;
			return query("SELECT d.uid, d.nid, m.uid, m.nid, to_jsonb(d.location), d.name, d.description, d.format, d.scopes, d.length, d.created_by, d.updated_by,"
					+ " d.pic_clause, d.defined_location, d.state, d.is_business, d.field_transformation, d.source_input, d.target_output, d.is_referenced,"
					+ " d.usage, d.is_candidate, d.field_level, d.parent_group, d.group_path, d.indentation, d.initial_value, d.translated_field,"
					+ " (SELECT array_agg(annotation) FROM data_dictionary_annotations WHERE entry = d.uid), d.custom_properties"
					+ " FROM data_dictionary d ")
				.with(this :: buildJoins)
				.with(filters :: build)
				.with(orderBys :: build)
				.toPageable(paging, (rs, n) -> new DataDictionaryPojo(
						(UUID) rs.getObject(1),												/* uid (UUID) */
						rs.getLong(2),														/* nid (Long) */
						EntityId.of((UUID) rs.getObject(3), rs.getLong(4)), null, null,		/* module (EntityId) */
						PgJSON.fromPGobject(rs.getObject(5), ModuleLocation.class),			/* location (ModuleLocation) */
						rs.getString(6),													/* name (String) */
						rs.getString(7),													/* description (String) */
						rs.getString(8),													/* format (String) */
						PgJSON.fromPGobject(rs.getObject(9)).entrySet().stream().collect(Collectors.toMap(
								e -> DataDictionaryVariableScope.valueOf(e.getKey()),
								e -> (Map<String, String>)  (e.getValue() == null ? Collections.emptyMap() : e.getValue()))),	/* scope (Map<String>) */
						(Long) rs.getObject(10),											/* length (Long) */
						rs.getString(11),													/* createdBy (String) */
						rs.getString(12),													/* updatedBy [String] */
						rs.getString(13),													/* picClause (String) */
						mapNullable(rs.getString(14), DefinedLocation :: valueOf),			/* definedLocation (String) */
						mapNullable(rs.getString(15), WorkingState :: valueOf),				/* state (WorkingState) */
						(Boolean) rs.getObject(16),											/* isBusiness (boolean) */
						rs.getString(17),													/* fieldTransformation (String) */
						rs.getString(18),													/* sourceInput (String) */
						rs.getString(19),													/* targetOutput (String) */
						(Boolean) rs.getObject(20),											/* isReferenced (boolean) */
						rs.getString(21),													/* usage (String) */
						rs.getBoolean(22),													/* isCandidate (boolean) */
						(Long) rs.getObject(23),											/* fieldLevel (Long) */
						rs.getString(24),													/* parentGroup (String) */
						rs.getString(25),													/* groupPath (String) */
						(Long) rs.getObject(26),											/* indentation (Long) */
						rs.getString(27),													/* initialValue (String) */
						rs.getString(28),													/* translatedFieldValue (String) */
						PgUtil.<UUID>streamArray(rs.getArray(29))
								.map(EntityId :: of)
								.collect(Collectors.toUnmodifiableList()),					/* annotations (List<EntityId>) */
						new CustomPropertiesMap(PgJSON.fromPGobject(rs.getObject(30)))		/* customProperties (CustomPropertiesMap) */
				));
		}
		
		protected Long buildCount() {
			return query("SELECT count(*) FROM data_dictionary d ")
				.with(this :: buildJoins)
				.with(filters :: build)
				.first(rs -> rs.getLong(1)).orElse(0L);
		}
		
		protected List<EntityId> buildModules(final boolean distinct) {
			return query(new StringBuilder("SELECT").append(distinct ? " DISTINCT " : " ").append("d.module FROM data_dictionary d ").toString())
				.with(this :: buildJoins)
				.with(filters :: build)
				.toList((rs, n) -> PgJSON.fromPGobject(rs.getObject(1), EntityId.class));
		}
		
		protected QueryBuilder buildDelete() {
			return query("WITH modules_updated AS ( ")
					.append("DELETE FROM data_dictionary WHERE uid = any(SELECT d.uid FROM data_dictionary d ")
					.with(this :: buildJoins)
					.with(filters :: build)
					.append(") RETURNING module ")
					.append(" ) UPDATE module SET modified_date = ? WHERE uid = any(SELECT module FROM modules_updated) ", Timestamp.from(Instant.now()));
		}

		protected Paged.Builder<EntityId> buildId(@Nullable final Pagination paging) {
			return query("SELECT d.uid, d.nid FROM data_dictionary d ")
					.with(this::buildJoins)
					.with(filters::build)
					.with(orderBys::build)
					.toPageable(paging, (rs, row) -> EntityId.of((UUID) rs.getObject(1), Long.valueOf(rs.getLong(2))));
		}

		protected Paged.Builder<UUID> buildUid(@Nullable final Pagination paging) {
			return query("SELECT d.uid FROM data_dictionary d ")
					.with(this::buildJoins)
					.with(filters::build)
					.with(orderBys::build)
					.toPageable(paging, (rs, row) -> (UUID) rs.getObject(1));
		}

		@Override
		protected void buildJoins(final QueryBuilder qb) {
			/* Joining Modules is always required to correctly build the Module EntityId.
			 * once we don't need the NID of the Module EntityId anymore, we can go back to d.module and switch the default of joinModules back to false. */
			joinModules = true;

			super.buildJoins(qb);

			qb.when(joinDataDictionaryAnnotation , q -> q.append("INNER JOIN data_dictionary_annotations dde_anno ON dde_anno.entry = d.uid "));
			qb.when(joinDataFlowNode, q -> q.append("INNER JOIN ast_node a ON (a.module = d.module OR a.included_module = d.module) "
					+ "AND (a.location).retraced_offset < (d.location).offset "
					+ "AND (d.location).offset < (a.location).retraced_offset + (a.location).retraced_length "));
			qb.when(joinDataFlowNode, q -> q.append("INNER JOIN data_flow_node n ON a.id = n.ast_Node "));
		}

		@Override
		public DataDictionaryQueryBuilder byId(final EntityId id) {
			filters.accept(q -> q.appendId(id, "d"));
			return this;
		}

		@Override
		public DataDictionaryInquiryBuilder byIds(final Collection<EntityId> ids) {
			final var uids = EntityId.allUids(ids);
			if (uids.size() == ids.size()) {
				filters.accept(q -> q.append("d.uid = any(?)")
						 			 .addArg(PgType.UUID, uids));
				return this;
			}

			final var nids = EntityId.allNids(ids);
			if (nids.size() == ids.size()) {
				return byNids(nids);
			}

			filters.accept(q -> q.append("d.uid = any(?) OR d.nid = any(?)")
								 .addArg(PgType.UUID, uids)
								 .addArg(PgType.LONG, nids));
			return this;
		}
		
		@Override
		public DataDictionaryQueryBuilder byNids(final Collection<Long> nids) {
			filters.accept(q -> q.append("d.nid = any(?)", arrayFromCollection(PgType.LONG, nids)));
			return this;
		}
		
		@Override
		public DataDictionaryQueryBuilder ofModule(final EntityId module) {
			filters.accept(q -> q.append("d.module = ").with(ModulePgDao.referenceUidOrNid(module)));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder ofModules(final Collection<EntityId> modules) {
			final var uids = EntityId.allUids(modules);
			if (uids.size() == modules.size()) {
				return ofModuleUuids(uids);
			}

			final var nids = EntityId.allNids(modules);
			if (nids.size() == modules.size()) {
				return ofModuleNids(nids);
			}

			filters.accept(q -> q.append("(d.module = any(?) OR d.module IN (SELECT uid FROM module WHERE nid = any(?)))")
								 .addArg(PgType.UUID, uids)
								 .addArg(PgType.LONG, nids));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder ofModuleUuids(final Collection<UUID> modules) {
			filters.accept(q -> q.append("d.module = any(?)").addArg(PgType.UUID, modules));
			return this;
		}
		
		@Override
		public DataDictionaryQueryBuilder ofModuleNids(final Collection<Long> modules) {
			filters.accept(q -> q.append("m.nid = any(?)").addArg(PgType.LONG, modules));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder ofModuleName(final String moduleName) {
			filters.accept(q -> q.append("m.name LIKE ?", moduleName));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder ofModulePath(final String path) {
			filters.accept(q -> q.append("m.path LIKE ?", path));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder withStates(final Collection<WorkingState> states) {
			filters.accept(q -> q.append("d.state = any(?)").addArg(PgMiningType.WORKING_STATE, states));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder withScopes(final Collection<String> scopes) {
			filters.accept(q -> q.append("d.scopes :: jsonb ??| array[?]").addArg(PgType.STRING, scopes));
			return this;
		}

		@Override
		public DataDictionaryService.DataDictionaryInquiryBuilder withScopeAttributes(final DataDictionaryVariableScope scope, final String attribute,
				final Collection<String> attributes) {
			filters.accept(q -> q.append("d.scopes-> ? ->'attributes'->> ? = any(?)", scope.name(), attribute).addArgs(PgType.STRING, attributes));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder withName(final String name) {
			filters.accept(q -> q.append("d.name ILIKE ?", name));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder withLocation(final ModuleLocation location, final boolean isWithin) {
			final Integer offset = location.getOffset();
			final Integer length = location.getLength();
			if (isWithin) {
				filters.accept(q -> q.append(
						"(d.location).offset >= ? AND (d.location).length <= ? AND (d.location).offset <= ?", offset, length, (offset + length)));
			} else {
				filters.accept(q -> q.append("(d.location).offset = ? AND (d.location).length = ?", offset, length));
			}
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder withMinOffset(final Integer offset) {
			filters.accept(q -> q.append("(d.location).offset >= ?", offset));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder withOffsetBetween(final Integer start, final Integer end) {
			filters.accept(q -> q.append("(d.location).offset BETWEEN ? AND ?", start, end));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder ofModuleProject(final EntityId project) {
			filters.accept(q -> q.append("m.project = ").with(ProjectPgDao.referenceUidOrNid(project)));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder withDescription(final String description) {
			filters.accept(q -> q.append("d.description ILIKE ?", description));
			return this;
		}

		@Override
		public DataDictionaryService.DataDictionaryInquiryBuilder withInitialValue(final String initialValue) {
			filters.accept(q -> q.append("d.initial_value ILIKE ?", initialValue));
			return this;
		}

		@Override
		public DataDictionaryService.DataDictionaryInquiryBuilder withParentGroup(final String parentGroup) {
			filters.accept(q -> q.append("d.parent_group ILIKE ?", parentGroup));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder ofAnnotation(final EntityId rule) {
			joinDataDictionaryAnnotation = true;
			filters.accept(q -> q.append("dde_anno.annotation = ").with(AnnotationPgDao.referenceUidOrNid(rule)));
			return this;
		}
		
		@Override
		public DataDictionaryQueryBuilder ofAnnotations(final Collection<EntityId> annotations) {
			joinDataDictionaryAnnotation = true;
			final var uids = EntityId.allUids(annotations);
			if (uids.size() == annotations.size()) {
				filters.accept(q -> q.append("dde_anno.annotation = any(?)").addArg(PgType.UUID, uids));
				return this;
			}
	
			final var nids = EntityId.allNids(annotations);
			if (nids.size() == annotations.size()) {
				filters.accept(q -> q.append("dde_anno.annotation = (SELECT uid FROM annotation WHERE nid = any(?))").addArg(PgType.LONG, nids));
				return this;
			}
	
			filters.accept(q -> q.append("(dde_anno.annotation = any(?) OR dde_anno.annotation IN (SELECT uid FROM annotation WHERE nid = any(?)))")
								 .addArg(PgType.UUID, uids)
								 .addArg(PgType.LONG, nids));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder notOfAnnotations() {
			filters.accept(q -> q.append("d.uid NOT IN (SELECT entry FROM data_dictionary_annotations WHERE entry = d.uid)"));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder withIsBusiness(final boolean isBusiness) {
			filters.accept(q -> q.append("d.is_business = ?", isBusiness));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder withDataFlowIds(final Collection<String> dataFlowIds) {
			joinDataFlowNode = true;
			filters.accept(q -> q.append("n.data_flow_id = any(?)").addArg(PgType.STRING, dataFlowIds));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder withDefinedLocations(final Collection<DefinedLocation> locations) {
			filters.accept(q -> q.append("d.defined_location = any(?)").addArg(
					PgType.STRING, locations.stream().map(DefinedLocation::name).collect(Collectors.toList())));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder withFieldTransformation(final String transformation) {
			filters.accept(q -> q.append("d.field_transformation ILIKE ?", transformation));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder withUsages(final Collection<String> usages) {
			filters.accept(q -> q.append("d.usage = any(?)").addArg(PgType.STRING, usages));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder withPicClause(final String picClause) {
			filters.accept(q -> q.append("d.pic_clause ILIKE ?", picClause));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder withIsReferenced(final boolean isReferenced) {
			filters.accept(q -> q.append("d.is_referenced = ?", isReferenced));
			return this;
		}

		@Override
		public DataDictionaryService.DataDictionaryInquiryBuilder withIsReferencedIn(final Collection<Boolean> values) {
			filters.accept(q -> q.append("d.is_referenced = any(?)").addArg(PgType.BOOLEAN, values));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder withSourceInput(final String input) {
			filters.accept(q -> q.append("d.source_input ILIKE ?", input));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder withTargetOutput(final String output) {
			filters.accept(q -> q.append("d.target_output ILIKE ?", output));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder withFormats(final Collection<String> formats) {
			filters.accept(q -> q.append("d.format = any(?)").addArg(PgType.STRING, formats));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder withGroupField(final String group) {
			filters.accept(q -> q.append("d.parent_group ILIKE ?", group));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder sortModuleName(final SortDirection direction) {
			orderBys.accept(q -> q.appendOrder("m.name", direction));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder sortNid(final SortDirection direction) {
			orderBys.accept(q -> q.appendOrder("d.nid", direction));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder sortName(final SortDirection direction) {
			orderBys.accept(q -> q.appendOrder("d.name", direction));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder sortFieldLevel(final SortDirection direction) {
			orderBys.accept(q -> q.appendOrder("d.field_level", direction));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder sortLength(final SortDirection direction) {
			orderBys.accept(q -> q.appendOrder("d.length", direction));
			return this;
		}

		@Override
		public DataDictionaryQueryBuilder sortParentGroup(final SortDirection direction) {
			orderBys.accept(q -> q.appendOrder("d.parent_group", direction));
			return this;
		}
		
		@Override
		public DataDictionaryQueryBuilder sortTranslatedFieldValue(final SortDirection direction) {
			orderBys.accept(q -> q.appendOrder("d.translated_field", direction));
			return this;
		}
		
		@Override
		public DataDictionaryQueryBuilder sortCustomProperties(final SortDirection direction) {
			orderBys.accept(q -> q.appendOrder("d.custom_properties", direction));
			return this;
		}

		@Override
		public void appendCustomPropertiesField(final QueryBuilder q) {
			q.append("d.custom_properties");
		}

		@Override
		public void acceptCustomPropetiesFilter(final ConditionalConsumer<QueryBuilder> clause) {
			filters.accept(clause);
		}
	}

	public class DataDictionaryAggregationQueryBuilder extends AbstractAggregationQueryBuilder<DataDictionaryFieldName, DataDictionaryAggregationQueryBuilder> 
						implements DataDictionaryService.DataDictionaryAggregationInquiryBuilder<DataDictionaryAggregationQueryBuilder> {
	
		protected boolean joinModules;
		protected boolean joinTaxonomies;
		protected boolean joinOtherScope;

		@Override
		protected void buildJoins(final QueryBuilder query) {
			query.when(joinModules || joinOtherScope, q -> q.append("INNER JOIN module m ON m.uid = d.module "));
			query.when(joinTaxonomies, q -> q.append("INNER JOIN module_taxonomies mt ON mt.module = d.module "));
			query.when(joinOtherScope, q -> q.append("INNER JOIN data_dictionary_other_scope oth_scope ON oth_scope.project = m.project "));
		}

		@Override
		protected String getFromClause() {
			return "data_dictionary d ";
		}

		@Override
		protected String getFieldQueryFragment(final DataDictionaryFieldName field) {
			switch (field) {
				case OTHER_SCOPE_LINK:
				case OTHER_SCOPE_SOURCE:
					joinOtherScope = true;
					break;
				case MODULE_TECHNOLOGY:
				case MODULE_TYPE:
					joinModules = true;
					break;
				default:
					/* nothing to join */
					break;
			}

			switch (field) {
				case ID:
					return "d.uid AS " + field.name().toLowerCase();
				case DATA_ELEMENT_NAME:
					return "d.name AS " + field.name().toLowerCase();
				case DESCRIPTION:
					return "d.description AS " + field.name().toLowerCase();
				case FORMAT:
					return "d.format AS " + field.name().toLowerCase();
				case LENGTH:
					return "d.length AS " + field.name().toLowerCase();
				case OTHER_SCOPE_LINK:
					return "d.scopes->>'scope' AS " + field.name().toLowerCase();
				case OTHER_SCOPE_SOURCE:
					return "d.scopes->>'source' AS " + field.name().toLowerCase();
				case CREATED_BY_USER_ID:
					return "d.created_by AS " + field.name().toLowerCase();
				case UPDATED_BY_USER_ID:
					return "d.updated_by AS " + field.name().toLowerCase();
				case MODULE_TECHNOLOGY:
					return "m.technology AS " + field.name().toLowerCase();
				case MODULE_TYPE:
					return "m.type AS " + field.name().toLowerCase();
				case SCOPE_LINK:
					return "jsonb_object_keys(d.scopes) AS " + field.name().toLowerCase();
				case SCOPE_ATTRIBUTES:
					return "d.scopes->'PARAMETER'->>'accessType' AS " + field.name().toLowerCase();
				case IS_CANDIDATE:
					return "d.is_candidate AS " + field.name().toLowerCase();
				case PIC_CLAUSE:
					return "d.pic_clause AS " + field.name().toLowerCase();
				case DEFINED_LOCATION:
					return "d.defined_location AS " + field.name().toLowerCase();
				case STATE:
					return "d.state AS " + field.name().toLowerCase();
				case IS_BUSINESS:
					return "d.is_business AS " + field.name().toLowerCase();
				case FIELD_TRANSFORMATION:
					return "d.field_transformation AS " + field.name().toLowerCase();
				case SOURCE_INPUT:
					return "d.source_input AS " + field.name().toLowerCase();
				case TARGET_OUTPUT:
					return "d.target_output AS " + field.name().toLowerCase();
				case IS_REFERENCED:
					return "d.is_referenced AS " + field.name().toLowerCase();
				case FIELD_USAGE:
					return "d.usage AS " + field.name().toLowerCase();
				case PROJECT_ID:
					/* no aggregations on project supported yet */
				default:
					throw new UnsupportedOperationException("The field is not supported yet: " + field.name());
			}
		}

		@Override
		public DataDictionaryAggregationQueryBuilder ofProject(final String operator, final Object value) {
			joinModules = true;
			if (OPERATOR_EQ.equals(operator)) {
				final EntityId project = toEntityId(value, "Project id");
				filters.accept(q -> q.append("m.project = ").with(ProjectPgDao.referenceUidOrNid(project)));
			} else {
				throw new UnsupportedOperationException(String.format("The operator %s is not supported for PROJECT", operator));
			}
			return this;
		}

		@Override
		public DataDictionaryAggregationQueryBuilder byId(final String operator, final Object value) {
			if (OPERATOR_EQ.equals(operator)) {
				final EntityId entity = toEntityId("DataDictionary id", operator);
				filters.accept(q -> q.append("d.").appendId(entity));
			} else {
				throw new UnsupportedOperationException(String.format("The operator %s is not supported for ID", operator));
			}
			return this;
		}

		@Override
		public DataDictionaryAggregationQueryBuilder withName(final String operator, final Object value) {
			if (OPERATOR_EQ.equals(operator)) {
				filters.accept(q -> q.append("d.name = ?", value.toString()));
			} else {
				throw new UnsupportedOperationException(String.format("The operator %s is not supported for DATA_ELEMENT_NAME", operator));
			}
			return this;
		}

		@Override
		public DataDictionaryAggregationQueryBuilder withDescription(final String operator, final Object value) {
			if (OPERATOR_EQ.equals(operator)) {
				filters.accept(q -> q.append("d.description ILIKE ?", value));
			} else {
				throw new UnsupportedOperationException(String.format("The operator %s is not supported for DESCRIPTION", operator));
			}
			return this;
		}

		@Override
		public DataDictionaryAggregationQueryBuilder withFormat(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("d.format = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("d.format != ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("d.format = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("d.format != any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for FORMAT", operator));
			}
			return this;
		}

		@Override
		public DataDictionaryAggregationQueryBuilder withLength(final String operator, final Object value) {
			final var length = toNumber(value, "Length");
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("d.length = ?", length));
					break;
				case OPERATOR_GTE:
					filters.accept(q -> q.append("d.length >= ?", length));
					break;
				case OPERATOR_LTE:
					filters.accept(q -> q.append("d.length <= ?", length));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for LENGTH", operator));
			}
			return this;
		}

		@Override
		public DataDictionaryAggregationQueryBuilder withOtherScope(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("d.scopes->'OTHER'->>'scope' = ?", value));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("d.scopes->'OTHER'->>'scope' != ?", value));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("d.scopes->'OTHER'->>'scope' = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("d.scopes->'OTHER'->>'scope' != any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for OTHER_SCOPE_SOURCE", operator));
			}
			return this;
		}

		@Override
		public DataDictionaryAggregationQueryBuilder withOtherScopeSource(final String operator, final Object value) {
			joinOtherScope = true;
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("d.scopes->'OTHER'->>'source' = ?", value));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("d.scopes->'OTHER'->>'source' != ?", value));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("d.scopes->'OTHER'->>'source' = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("d.scopes->'OTHER'->>'source' != any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for OTHER_SCOPE_SOURCE", operator));
			}
			return this;
		}

		@Override
		public DataDictionaryAggregationQueryBuilder withCreatedByUserId(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("d.created_by = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("d.created_by != ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("d.created_by = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("d.created_by != any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for CREATED_BY_USER_ID", operator));
			}
			return this;
		}

		@Override
		public DataDictionaryAggregationQueryBuilder withUpdatedByUserId(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("d.updated_by = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("d.updated_by != ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("d.updated_by = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("d.updated_by != any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for UPDATED_BY_USER_ID", operator));
			}
			return this;
		}

		@Override
		public DataDictionaryAggregationQueryBuilder ofModuleTechnology(final String operator, final Object value) {
			joinModules = true;
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
					filters.accept(q -> q.append("m.technology != any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for MODULE_TECHNOLOGY", operator));
			}
			return this;
		}

		@Override
		public DataDictionaryAggregationQueryBuilder ofModuleType(final String operator, final Object value) {
			joinModules = true;
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("m.type = ?", value));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("m.type != ?", value));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("m.type = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("m.type != any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for MODULE_TYPE", operator));
			}
			return this;
		}

		@Override
		public DataDictionaryAggregationQueryBuilder withScope(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("d.scopes :: jsonb ?? ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("d.scopes :: jsonb ??| ?").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("NOT d.scopes :: jsonb ??| ?").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for SCOPE", operator));
			}
			return this;
		}

		@Override
		public DataDictionaryAggregationQueryBuilder withScopeAttribute(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("d.scopes :: jsonb @> ? :: jsonb", value.toString()));
					break;
				case OPERATOR_IN:
					final var values = toStrings(value);
					filters.accept(q -> {
						for (int i = 0; i < values.size(); i++) {
							q.append("d.scopes :: jsonb @> ? :: jsonb", values.get(i));
							if (i > 0) {
								q.append(" OR ");
							}
						}
					});
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for SCOPE_ATTRIBUTE", operator));
			}
			return this;
		}

		@Override
		public DataDictionaryAggregationQueryBuilder withScopeAccessType(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("scopes->'PARAMETER'->>'accessType' LIKE ?").addArg("%" + value.toString() + "%"));
					break;
				case OPERATOR_IN:
					final var values = toStrings(value);
					final var adjustedStrings = values.stream().map(s -> "%" + s + "%").toList();
					filters.accept(q -> q.append("scopes->'PARAMETER'->>'accessType' LIKE ANY(?)").addArgs(arrayFromCollection(PgType.STRING, adjustedStrings)));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for SCOPE_ATTRIBUTE", operator));
			}
			return this;
		}

		@Override
		public DataDictionaryAggregationQueryBuilder withIsCandidate(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("d.is_candidate = ?", toBoolean(value)));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("d.is_candidate != ?", toBoolean(value)));
					break;
				case OPERATOR_IS_TRUE:
					filters.accept(q -> q.append("d.is_candidate = true"));
					break;
				case OPERATOR_IS_FALSE:
					filters.accept(q -> q.append("d.is_candidate = false"));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for IS_CANDIDATE", operator));
			}
			return this;
		}

		@Override
		public DataDictionaryAggregationQueryBuilder withPicClause(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("d.pic_clause = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("d.pic_clause != ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("d.pic_clause = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("d.pic_clause != any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for PIC_CLAUSE", operator));
			}
			return this;
		}

		@Override
		public DataDictionaryAggregationQueryBuilder withDefinedLocation(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("d.defined_location = ?", value));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("d.defined_location != ?", value));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("d.defined_location = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("d.defined_location != any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for DEFINED_LOCATION", operator));
			}
			return this;
		}

		@Override
		public DataDictionaryAggregationQueryBuilder withState(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("d.state = ?", value));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("d.state != ?", value));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("d.state = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("d.state != any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for STATE", operator));
			}
			return this;
		}

		@Override
		public DataDictionaryAggregationQueryBuilder withIsBusiness(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("d.is_business = ?", toBoolean(value)));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("d.is_business != ?", toBoolean(value)));
					break;
				case OPERATOR_IS_TRUE:
					filters.accept(q -> q.append("d.is_business = true"));
					break;
				case OPERATOR_IS_FALSE:
					filters.accept(q -> q.append("d.is_business = false"));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for IS_BUSINESS", operator));
			}
			return this;
		}

		@Override
		public DataDictionaryAggregationQueryBuilder withFieldTransformation(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("d.field_transformation ILIKE ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("NOT d.field_transformation ILIKE ?", value.toString()));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for FIELD_TRANSFORMATION", operator));
			}
			return this;
		}

		@Override
		public DataDictionaryAggregationQueryBuilder withSourceInput(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("d.source_input ILIKE ?", value));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("NOT d.source_input ILIKE ?", value));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for SOURCE_INPUT", operator));
			}
			return this;
		}

		@Override
		public DataDictionaryAggregationQueryBuilder withTargetOutput(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("d.target_output ILIKE ?", value));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("NOT d.target_output ILIKE ?", value));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for TARGET_OUTPUT", operator));
			}
			return this;
		}

		@Override
		public DataDictionaryAggregationQueryBuilder withIsReferenced(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("d.is_referenced = ?", toBoolean(value)));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("d.is_referenced != ?", toBoolean(value)));
					break;
				case OPERATOR_IS_TRUE:
					filters.accept(q -> q.append("d.is_referenced = true"));
					break;
				case OPERATOR_IS_FALSE:
					filters.accept(q -> q.append("d.is_referenced = false"));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for IS_REFERENCED", operator));
			}
			return this;
		}

		@Override
		public DataDictionaryAggregationQueryBuilder withFieldUsage(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("d.usage = ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("d.usage != ?", value.toString()));
					break;
				case OPERATOR_IN:
					var values = toStrings(value);
					filters.accept(q -> q.append("d.usage = any(?)").addArg(PgType.STRING, values));
					break;
				case OPERATOR_NOT_IN:
					values = toStrings(value);
					filters.accept(q -> q.append("d.usage != any(?)").addArg(PgType.STRING, values));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for FIELD_USAGE", operator));
			}
			return this;
		}

		@Override
		public DataDictionaryAggregationQueryBuilder withTaxonomy(final String operator, final Object value) {
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

	/**
	 * Counts all or a filtered subset of {@linkplain DataDictionaryPojo Data Dictionaries}.
	 * @param builder Builder for filter criteria and sorting options.
	 * @return count of all matching {@linkplain DataDictionaryPojo Data Dictionaries}.
	 */
	public Long count(final BuildingConsumer<DataDictionaryService.DataDictionaryInquiryBuilder> builder) {
		return builder.prepare(new DataDictionaryQueryBuilder()).buildCount();
	}

	/**
	 * Finds all or a filtered subset of Module IDs containing Data Dictionaries.
	 * @param builder Builder for filter criteria and sorting options.
	 * @param distinct {@code true} if result should be distinct
	 * @return all matching Module Entity IDS.
	 */
	public List<EntityId> findModules(final BuildingConsumer<DataDictionaryService.DataDictionaryInquiryBuilder> builder, final boolean distinct) {
		return builder.prepare(new DataDictionaryQueryBuilder()).buildModules(distinct);
	}

	/**
	 * Retrieves all or a filtered subset of {@linkplain DataDictionaryPojo Data Dictionaries}.
	 * @param builder Builder for filter criteria and sorting options.
	 * @return All matching {@linkplain DataDictionaryPojo Data Dictionaries}.
	 */
	public List<DataDictionaryPojo> find(final BuildingConsumer<DataDictionaryService.DataDictionaryInquiryBuilder> builder) {
		return builder.prepare(new DataDictionaryQueryBuilder()).build(null).all();
	}

	/**
	 * Retrieves a paged subset of optionally filtered {@linkplain DataDictionaryPojo Data Dictionaries}.
	 * @param paging Pagination specification.
	 * @param builder Builder for filter criteria and sorting options.
	 * @return Paged subset of matching {@linkplain DataDictionaryPojo Data Dictionaries}.
	 */
	public Paged<DataDictionaryPojo> find(final Pagination paging, final BuildingConsumer<DataDictionaryService.DataDictionaryInquiryBuilder> builder) {
		return builder.prepare(new DataDictionaryQueryBuilder()).build(paging).page();
	}

	/**
	 * Retrieve any {@linkplain DataDictionaryPojo Data Dictionaries} by filters.
	 * @param builder Builder for filter criteria and sorting options.
	 * @return The Project if it exists.
	 */
	public Optional<DataDictionaryPojo> findAny(final BuildingConsumer<DataDictionaryService.DataDictionaryInquiryBuilder> builder) {
		return builder.prepare(new DataDictionaryQueryBuilder()).build(Pagination.FIRST).first();
	}

	public List<EntityId> findDataDictionaryIds(final BuildingConsumer<DataDictionaryInquiryBuilder> builder) {
		return builder.prepare(new DataDictionaryQueryBuilder())
				.buildId(null)
				.all();
	}

	/**
	 * Returns aggregation values for the filters and selected aggregations in the given {@code builder}.
	 *
	 * @param builder the {@linkplain DataDictionaryAggregationInquiryBuilder} containing the aggregation operations and filter criteria
	 * @return container with the aggregation values
	 */
	public Optional<Table> getAggregations(final BuildingConsumer<DataDictionaryService.DataDictionaryAggregationInquiryBuilder<?>> builder) {
		return builder.prepare(new DataDictionaryAggregationQueryBuilder()).distinct().buildAggregation(this);
	}

	/**
	 * Deletes {@linkplain DataDictionaryPojo Data Dictionary} entries based on filters.
	 * @param builder Builder for filter criteria and sorting options.
	 * @return number of records deleted
	 */
	public int delete(final BuildingConsumer<DataDictionaryService.DataDictionaryInquiryBuilder> builder) {
		return builder.prepare(new DataDictionaryQueryBuilder()).buildDelete().update();
	}

	/**
	 * Creates a new {@linkplain DataDictionaryPojo Data Dictionary}.
	 * @param dde the {@linkplain DataDictionaryPojo Data Dictionary}.
	 * @return the created entity ID.
	 */
	public EntityId create(final DataDictionaryPojoPrototype dde) {
		return put(dde, true);
	}

	public int[][] create(final Collection<DataDictionaryPojoPrototype> ddes) {
		return queryBatch(ddes, 1_000, a -> queryPut(a, true).append(" SELECT FROM records_updated"));
	}

	/**
	 * Set the Data Dictionaries with the provided UUIDs as business variables
	 * @param uids the UUIDs
	 */
	public void markAsBusinessVariables(final Collection<UUID> uids) {
		query("WITH modules_updated AS ( ")
				.append("UPDATE data_dictionary set is_business = true where uid = any(?) RETURNING module").addArg(PgType.UUID, uids)
				.append(" ) UPDATE module SET modified_date = ? WHERE uid = any(SELECT module FROM modules_updated)", Timestamp.from(Instant.now()))
				.update();
	}

	/**
	 * Link Annotations to Data Dictionaries
	 * @param dde the Data Dictionary ID
	 * @param annotation the Annotation ID
	 */
	public void linkAnnotations(final EntityId dde, final EntityId annotation) {
		query("INSERT INTO data_dictionary_annotations(entry, annotation) VALUES (")
			.with(referenceUidOrNid(dde)).append(", ").with(AnnotationPgDao.referenceUidOrNid(annotation)).append(")")
			.update();
	}

	/**
	 * Updates a new {@linkplain DataDictionaryPojo Data Dictionary}.
	 * @param dde the {@linkplain DataDictionaryPojo Data Dictionary}.
	 * @return the updated entity ID.
	 */
	public EntityId update(final DataDictionaryPojoPrototype dde) {
		return put(dde, false);
	}

	/**
	 * This method allows you to update multiple Data Dictionaries with the same values.
	 *
	 * <p>Updates all Data Dictionaries that match with the filters in the given {@code builder}. The defined (set) fields in the given {@code values} are used
	 * for building the update query.</p>
	 *
	 * @param builder the {@linkplain DataDictionaryInquiryBuilder} containing the filter criteria
	 * @param values the {@link DataDictionaryPojoPrototype} containing the to be updated fields
	 * @return the number of updated {@code data dictionaries}, 0 if no data dictionary matched with the filer
	 */
	public int update(final BuildingConsumer<DataDictionaryInquiryBuilder> builder, final DataDictionaryPojoPrototype values) {
		final var fields = createFieldBuilder(values, false);
		final var filter = builder.prepare(new DataDictionaryQueryBuilder());

		return query("UPDATE data_dictionary d SET ")
				.with(fields::buildUpdate)
				.with(filter.filters::build)
				.update();
	}

	/**
	 * Updates existing Data Dictionaries if they are identified as Candidates
	 *
	 * @param ddes the Data Dictionaries
	 */
	public void updateExistingCandidates(final Collection<DataDictionaryPojoPrototype> ddes) {
		final var query = query("WITH modules_updated AS ( ")
				.append("UPDATE data_dictionary SET updated_by = ?, source_input = ?, target_output = ?, scopes = ? WHERE uid = ? RETURNING module")
				.append(" ) UPDATE module SET modified_date = ? WHERE uid = any(SELECT module FROM modules_updated)");
		query.updateBatch(ddes.stream().map(dde -> Stream.<Object>of(
					dde.updatedByUserId.getNonNull(),
					dde.sourceInput.get(),
					dde.targetOutput.get(),
					PgJSON.toPGobject(dde.scopes.get()),
					dde.uid.getNonNull(),
					Timestamp.from(Instant.now())
				)), BATCH_SIZE);
	}

	/**
	 * Returns all {@code data_dictionary_other_scope} entities for the given {@code project}.
	 *
	 * @param project the project to query for
	 * @return list of DataDictionaryOtherScopes
	 */
	public List<String> findDataDictionaryOtherScope(final EntityId project) {
		return query("SELECT name FROM data_dictionary_other_scope WHERE project = ")
				.with(ProjectPgDao.referenceUidOrNid(project))
				.toList((rs, n) -> rs.getString(1));
	}

	private EntityId put(final DataDictionaryPojoPrototype dde, final boolean isNew) {
		return queryPut(dde, isNew)
				.append(" SELECT uid, nid FROM records_updated;")
				.first(rs -> EntityId.of((UUID) rs.getObject(1), Long.valueOf(rs.getLong(2))))
				.orElseThrow(() -> new MiningEntityNotFoundException(DataDictionaryPojo.class, dde.toString()));
	}

	private QueryBuilder queryPut(final DataDictionaryPojoPrototype dde, final boolean isNew) {
		final EntityId id;
		final QueryBuilder q = query("WITH records_updated AS ( ");
		final var fields = createFieldBuilder(dde, isNew);

		if (isNew) {
			id = EntityId.of(dde.uid.orElseNonNull(UUID :: randomUUID));
			q.append("INSERT INTO data_dictionary ");
			fields.add("uid", "?", id.getUid());
			fields.buildInsert(q);
		} else {
			id = dde.identityProvisional();
			q.append("UPDATE data_dictionary SET ");
			fields.buildUpdate(q);
			q.append(" WHERE ");
			q.appendId(id);
		}

		return q.append(" RETURNING uid, nid, module ")
				.append("), update_modified_date AS ( ")
				.append("UPDATE module SET modified_date = ? WHERE uid = (SELECT module FROM records_updated)", Timestamp.from(Instant.now()))
				.append(")");
	}

	private FieldBuilder createFieldBuilder(final DataDictionaryPojoPrototype dde, final boolean isNew) {
		return new FieldBuilder()
			.add(dde.module.required(isNew), "module", ModulePgDao::referenceUidOrNid)
			.add(dde.location.required(isNew), "location", ModulePgDao::appendLocation)
			.add(dde.name.required(isNew), "name", "?")
			.add(dde.description.required(isNew), "description", "?")
			.add(dde.format, "format", "?")
			.add(dde.scopes, "scopes", "?", PgJSON :: toPGobject)
			.add(dde.length, "length", "?")
			.add(dde.createdByUserId.exclusive(isNew), "created_by", "?")
			.add(dde.updatedByUserId.required(! isNew), "updated_by", "?")
			.add(dde.picClause, "pic_clause", "?")
			.add(dde.definedLocation, "defined_location", "?", passNull(DefinedLocation::name))
			.add(dde.state, "state", "? :: working_state", passNull(WorkingState::name))
			.add(dde.isBusiness, "is_business", "?")
			.add(dde.fieldTransformation, "field_transformation", "?")
			.add(dde.sourceInput, "source_input", "?")
			.add(dde.targetOutput, "target_output", "?")
			.add(dde.isReferenced, "is_referenced", "?")
			.add(dde.usage, "usage", "?")
			.add(dde.isCandidate, "is_candidate", "?")
			.add(dde.fieldLevel, "field_level", "?")
			.add(dde.parentGroup, "parent_group", "?")
			.add(dde.groupPath, "group_path", "?")
			.add(dde.indentation, "indentation", "?")
			.add(dde.initialValue, "initial_value", "?")
			.add(dde.translatedFieldValue, "translated_field", "?")
			.and(CustomPropertiesPgDao.addField(dde, isNew));
	}
}

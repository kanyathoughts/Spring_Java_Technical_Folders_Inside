/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.data.access.postgres;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.FunctionalBlockService.FunctionalBlockAggregationInquiryBuilder;
import innowake.mining.shared.access.FunctionalBlockService.FunctionalBlockInquiryBuilder;
import innowake.mining.shared.access.FunctionalBlockService.FunctionalBlockLinkInquiryBuilder;
import innowake.mining.shared.access.FunctionalBlockService.FunctionalBlockOrderBuilder;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.access.Table;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFieldName;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLink;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkCondition;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockStatus;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.GeneratedFrom;
import innowake.mining.shared.entities.functionalblocks.ModulePart;
import innowake.mining.shared.entities.functionalblocks.ReachabilityDataPojo;
import innowake.mining.shared.entities.functionalblocks.ReachabilityDataPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.ResolvedModulePart;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.jdbc.core.JdbcTemplate;

import java.sql.Array;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static innowake.mining.shared.access.FilterUtil.toEntityId;
import static innowake.mining.shared.access.FilterUtil.toStrings;
import static innowake.mining.shared.access.FilterUtil.toUuid;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_EQ;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_IN;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_NOT_EQ;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.OPERATOR_NOT_IN;

/**
 * DAO for {@link FunctionalBlockPojo}.
 */
public class FunctionalBlockPgDao extends PgDao {

	/* "flag" that is set on functional blocks returned from the "childrenDeep" query - it holds the UUID of the root parent */
	public static final String ROOT_BLOCK_FLAG = "__ROOT_BLOCK";
	public static final String DEPTH_FLAG = "__DEPTH";

	private static final String FB_FIELDS = " fb.uid," + /* 1 */
			" fb.custom_properties," + /* 2 */
			" fb.project," + /* 3 */
			" fb.name," + /* 4 */
			" fb.description," + /* 5 */
			" fb.updated," + /* 6 */
			" (SELECT array_agg(parent) FROM functional_block_children WHERE child = fb.uid) parents," + /* 7 */
			" (SELECT array_agg(child ORDER BY ordinal) FROM functional_block_children WHERE parent = fb.uid) children," + /* 8 */
			" mp.module_link_hashes, mp.offsets, mp.lengths," + /* 9, 10, 11 */
			" (SELECT nid FROM project p WHERE p.uid = fb.project)," + /* 12 */
			" fb.flags"; /* 13 */

	private static final String FB_JOIN = " LEFT JOIN (SELECT mp.functional_block," +
			" array_agg(mp.module_link_hash) module_link_hashes," +
			" array_agg((mp.location).offset) offsets," +
			" array_agg((mp.location).length) lengths" +
			" FROM functional_block_module_part mp GROUP BY mp.functional_block) mp" +
			" ON mp.functional_block = fb.uid";
	private static final String BASE_REACHABILITY_DATA_SELECT_QUERY =
			" SELECT rd.functional_block, rd.upper_bound_module," +
			" (SELECT nid FROM module WHERE  uid = rd.upper_bound_module) AS upper_bound_module_nid," +
			" rd.lower_bound_module, (SELECT nid FROM module WHERE uid = rd.lower_bound_module) AS lower_bound_module_nid,";
	private static final String REACHABILITY_DATA_QUERY = "rd.access_module, "
			+ "          (SELECT nid FROM module WHERE uid = rd.access_module) AS access_module_nid, "
			+ "          rd.access_type,"
			+ "          (SELECT Array_agg(Json_build_object('uid', im.intermediate_module, 'nid',"
			+ "                         (SELECT nid"
			+ "                         FROM module"
			+ "                         WHERE uid = im.intermediate_module)))"
			+ "                  FROM   reachability_data_intermediate_modules im"
			+ "                  WHERE  rd.uid = im.reachability_data"
			+ "          GROUP BY im.reachability_data) AS intermediate_modules ";
	private static final String REACHABILITY_DATA_WITH_AGGREGATED_ACCESS_MODULES_QUERY = "Array_agg(Json_build_object('uid', rd.access_module, 'nid'," +
			" (SELECT nid FROM module WHERE  uid = rd.access_module))) AS access_module," +
			" array( select distinct unnest(rd_inner.access_type)" +
			"       FROM functional_block_reachability_data AS rd_inner" +
			"                 WHERE rd_inner.functional_block = rd.functional_block" +
			"                 AND rd.upper_bound_module = rd_inner.upper_bound_module" +
			"                 AND rd.lower_bound_module = rd_inner.lower_bound_module ) AS aggregated_access_type," +
			" (SELECT array_agg(json_build_object('uid', im.intermediate_module, 'nid', (SELECT nid FROM module WHERE  uid = im.intermediate_module)))" +
			" FROM reachability_data_intermediate_modules im WHERE  im.reachability_data = ANY(array_agg(rd.uid)) ) AS intermediate_modules ";

	private static final String FB_FROM = " FROM functional_block fb" + FB_JOIN;

	private static final String FB_RECURSIVE_FIELDS = " uid," +
			" root_block," +
			" path, " +
			" seqPath, " +
			" depth";

	private static final String SELECT_FB_UID = "SELECT fb.uid" + FB_FROM;
	
	private static final String SELECT_FB = "SELECT" + FB_FIELDS + FB_FROM;
	/* sub-query for retrieving functional blocks based on specific filter criteria */
	private static final String SELECT_FB_FOR_FILTERING = "SELECT fb.uid" + FB_FROM;

	private static final String SELECT_CHILDREN_DEEP_RECURSION =
			" WITH RECURSIVE children_deep(" + FB_RECURSIVE_FIELDS + ") AS (" +
			" SELECT c.child, c.parent, ARRAY[c.child], ARRAY[c.ordinal] as seqPath, 0" +
			" FROM functional_block_children c" +
			" WHERE c.parent = any(?)" +
			" UNION ALL" +
			" SELECT c.child, cd.root_block, cd.path || c.child, cd.seqPath || c.ordinal, cd.depth + 1" +
			" FROM functional_block_children c" +
			" JOIN children_deep cd ON c.parent = cd.uid" +
			" WHERE NOT c.child = any(cd.path) AND NOT cd.depth = ?" +
			")";

	private static final String SELECT_CHILDREN_DEEP = SELECT_CHILDREN_DEEP_RECURSION +
			" SELECT " + FB_FIELDS + ", cd.root_block, cd.depth" +
			" FROM functional_block fb" +
			" JOIN children_deep cd ON cd.uid=fb.uid" +
			FB_JOIN;
	
	private static final String SELECT_CHILDREN_IDS_DEEP = SELECT_CHILDREN_DEEP_RECURSION +
			" SELECT cd.root_block, cd.uid" +
			" FROM children_deep cd";

	/* sub-query selecting the parent for the withParent() filter option */
	private static final String SELECT_PARENT_FOR_FILTERING = "SELECT p.child FROM functional_block_children p JOIN functional_block fb ON fb.uid = p.parent" ;

	/* sub-query selecting the children for the withChild() filter option */
	private static final String SELECT_CHILDREN_FOR_FILTERING = "SELECT p.parent FROM functional_block_children p JOIN functional_block fb ON fb.uid = p.child ";

	/* sub-query selecting "peers" for the withPeer() filter option */
	private static final String SELECT_PEER_BASE =
			/* select peers - peers are blocks that reference at least one same or overlapping module part(s)
			 * and neither block is transitive parent or child of the other */
			FB_FROM +
			/* peers must reference the same module */
			" JOIN functional_block_resolved_module_part rmp ON rmp.functional_block=fb.uid" +
			" JOIN functional_block_resolved_module_part peer_rmp ON rmp.module = peer_rmp.module" +
			" JOIN functional_block peer_fb ON peer_rmp.functional_block = peer_fb.uid" +
			/* join peer block as transitive child of block - we later check that this is NULL to check that
			 * the peer block is NOT a transitive child of the block */
			" LEFT JOIN functional_block_children_deep fb_cd ON fb.uid=fb_cd.parent AND peer_fb.uid=fb_cd.child" +
			/* same, but in the other direction: join block as transitive child of peer block */
			" LEFT JOIN functional_block_children_deep peer_fb_cd ON peer_fb.uid=peer_fb_cd.parent AND fb.uid=peer_fb_cd.child";

	/* conditions for the WHERE clause to be used with SELECT_PEERS_FOR_FILTERING */
	private static final String SELECT_PEERS_CONDITION =
			/* peers must not be the same block (block is not peer of itself) */
			" AND fb.uid != peer_fb.uid" +
			/* and peer block is not transitive child of block */
			" AND fb_cd.parent IS NULL" +
			/* and block is not transitive child of peer block */
			" AND peer_fb_cd.parent IS NULL" +
			/* and the referenced module locations must be null, or they must overlap */
			" AND (" +
			"   rmp.location IS NULL OR peer_rmp.location IS NULL " +
			"   OR (rmp.location).offset <= (peer_rmp.location).offset + (peer_rmp.location).length" +
			"   AND (peer_rmp.location).offset <= (rmp.location).offset + (rmp.location).length" +
			" )";
	
	private static final String PEERS = "AND (parent.uid IN (SELECT peer_fb.uid FROM functional_block fb  "
			+ "                         LEFT JOIN (SELECT mp.functional_block, array_agg(mp.module_link_hash) module_link_hashes, array_agg((mp.location).offset) offsets, array_agg((mp.location).length) lengths  "
			+ "                                    FROM functional_block_module_part mp  "
			+ "                                    GROUP BY mp.functional_block) mp ON mp.functional_block = fb.uid  "
			+ "                         JOIN functional_block_resolved_module_part rmp ON rmp.functional_block=fb.uid  "
			+ "                         JOIN functional_block_resolved_module_part peer_rmp ON rmp.module = peer_rmp.module  "
			+ "                         JOIN functional_block peer_fb ON peer_rmp.functional_block = peer_fb.uid  "
			+ "                         LEFT JOIN functional_block_children_deep fb_cd ON fb.uid=fb_cd.parent AND peer_fb.uid=fb_cd.child  "
			+ "                         LEFT JOIN functional_block_children_deep peer_fb_cd ON peer_fb.uid=peer_fb_cd.parent AND fb.uid=peer_fb_cd.child  "
			+ "                         WHERE (fb.uid = any(?)) AND fb.uid != peer_fb.uid AND fb_cd.parent IS NULL AND peer_fb_cd.parent IS NULL  "
			+ "                               AND (rmp.location IS NULL OR peer_rmp.location IS NULL OR (rmp.location).offset <= (peer_rmp.location).offset + (peer_rmp.location).length  "
			+ "                               AND (peer_rmp.location).offset <= (rmp.location).offset + (rmp.location).length))) "
			+ "";
			
	private static final String MODULES = " AND parent.uid IN (SELECT functional_block FROM functional_block_resolved_module_part "
			+ "rmp JOIN module m ON m.uid = rmp.module WHERE m.nid = any(?)) ";
			
	private static final String TAXONOMY_FILTER = "AND parent.uid IN (SELECT functional_block FROM functional_block_resolved_module_part rmp WHERE "
			+ " rmp.module IN (SELECT module FROM module_taxonomies WHERE taxonomy IN (SELECT uid FROM taxonomy WHERE nid = any(?)))) ";
			
	private static final String SEARCH_QUERY_WITHOUT_FILTER = "SELECT DISTINCT ON (parent.uid) "
			+ "    parent.uid AS parent_uid, "
			+ "    parent.name AS parent_name, "
			+ "    child.uid AS child_uid "
			+ "FROM "
			+ "    public.functional_block AS parent "
			+ "LEFT JOIN "
			+ "    public.functional_block_children_deep "
			+ "ON "
			+ "    parent.uid = public.functional_block_children_deep.parent "
			+ "LEFT JOIN "
			+ "    public.project as project "
			+ "ON "
			+ "    project.uid = parent.project "
			+ "LEFT JOIN "
			+ "    public.functional_block AS child "
			+ "ON "
			+ "    child.uid = public.functional_block_children_deep.child "
			+ "WHERE "
			+ "    project.nid = ? AND "
			+ "    NOT EXISTS ( "
			+ "        SELECT 1 "
			+ "        FROM public.functional_block_children_deep fbc "
			+ "        WHERE fbc.child = parent.uid "
			+ "    )   "
			+ "    AND  "
			+ "(parent.flags->'" + FunctionalBlockFlag.TYPE.name() + "' ?? '" + FunctionalBlockType.FUNCTIONAL_GROUP + "'"+ ")"
			+ "    AND  "
			+ "    (parent.name ILIKE any(?) OR (child.name ILIKE any(?) "
			+ " AND child.flags->'" + FunctionalBlockFlag.TYPE.name() + "' ?? '" + FunctionalBlockType.FUNCTIONAL_GROUP + "'"+ "))";

	private static final String REFERENCED_DATA_DICTIONARY = " AND parent.uid IN (SELECT functional_block FROM functional_block_referenced_data_dictionary rdd"
			+ " WHERE rdd.data_dictionary = any(?))";
	private static final String DELETE_FROM_FUNCTIONAL_BLOCK = "DELETE FROM functional_Block ";
	/* sub-query selecting "peers" for the withPeer() filter option */
	private static final String SELECT_PEERS_FOR_FILTERING = "SELECT peer_fb.uid " + SELECT_PEER_BASE;
	private static final String SELECT_PEERS_FOR_FIND_PEERS = "SELECT fb.uid, peer_fb.uid " + SELECT_PEER_BASE;

	protected class FunctionalBlockQueryBuilder extends FunctionalBlockAggregationQueryBuilder implements FunctionalBlockService.FunctionalBlockInquiryBuilder {

		/* Can not use orderBy of FunctionalBlockAggregationQueryBuilder here. The BuildingConsumerHolder in FunctionalBlocksGraphQlController calls
		 * sort methods of FunctionalBlockQueryBuilder instead of sort methods of FunctionalBlockAggregationQueryBuilder which leads to invalid SQL */
		protected final OrderStreamBuilder order = new OrderStreamBuilder();

		protected String queryString;
		protected boolean filterByRootBlock = false;
		@Nullable
		protected List<UUID> rootUids;
		protected boolean hasMaxDepth = false;
		protected int maxDepth;
		protected boolean joinGeneratedFrom = false;
		@Nullable
		protected UUID sortChildParent;
		@Nullable
		private AnnotationType annotationType;

		protected FunctionalBlockQueryBuilder(final String queryString) {
			this.queryString = queryString;
		}

		@Override
		protected void buildJoins(final QueryBuilder query) {
			query.when(joinGeneratedFrom, q -> q.append(" LEFT JOIN functional_block_generated_from gf ON gf.functional_block = fb.uid"));
			query.when(sortChildParent != null, q ->
					q.append(" LEFT JOIN (SELECT child, ordinal FROM functional_block_children WHERE parent = ?) sc ON sc.child = fb.uid",
							sortChildParent));
			query.when(annotationType != null, q ->
			q.append(" LEFT JOIN functional_block_generated_from gf ON fb.uid = gf.functional_block "
					+ "LEFT JOIN annotation a ON gf.annotation_nid = a.nid"));
			super.buildJoins(query);
		}

		protected PgDao.QueryBuilder buildSelectFrom() {
			return query(queryString)
					.when(filterByRootBlock, q -> q.addArg(PgType.UUID, rootUids))
					.when(hasMaxDepth, q -> q.addArg(maxDepth))
					.with(this::buildJoins);
		}

		protected PgDao.QueryBuilder buildQuery() {
			return buildSelectFrom()
					.when(filterByRootBlock, q -> q.append(" WHERE fb.uid IN (SELECT uid from children_deep)")) /* workaround for slow recursive query */
					.with(filterByRootBlock ? filters::buildSubsequent : filters::build)
					.when(filterByRootBlock, q -> order.accept(q1 -> q1.appendOrder("cd.depth", SortDirection.ASCENDING)))
					.with(order::build);
		}

		protected Paged.Builder<FunctionalBlockPojo> build(@Nullable final Pagination paging) {
			if (paging != null) {
				order.accept(q -> q.appendOrder("fb.uid", SortDirection.ASCENDING));
			}
			return buildQuery().toPageable(paging, (rs, row) -> {
				Map<String, Object> flags = PgJSON.fromPGobject(rs.getObject(13)); /* flags */
				if (filterByRootBlock) {
					flags = new HashMap<>(flags);
					flags.put(ROOT_BLOCK_FLAG, rs.getObject(14)); /* root_block */
					flags.put(DEPTH_FLAG, rs.getObject(15)); /* depth */
					flags = Collections.unmodifiableMap(flags);
				}
				return new FunctionalBlockPojo(
						(UUID) rs.getObject(1), /* uid */
						new CustomPropertiesMap(PgJSON.fromPGobject(rs.getObject(2))), /* custom_properties */
						EntityId.of((UUID) rs.getObject(3), rs.getLong(12)),	/* project */
						mapModuleParts(unpivot(rs, 9, 10, 11)), /* module_parts */
						mapUUIDs(rs.getArray(7)), /* parents */
						mapUUIDs(rs.getArray(8)), /* children */
						rs.getString(4), /* name */
						rs.getString(5), /* description */
						flags,
						map(rs.getTimestamp(6), Timestamp::toInstant) /* updated */
				);
			});
		}
		
		protected List<UUID> buildUids() {
			return buildQuery().toList((rs, rowNum) -> (UUID) rs.getObject(1));
		}
		
		protected FunctionalBlockQueryBuilder ofRootBlock(final UUID rootUid) {
			filterByRootBlock = true;
			this.rootUids = Collections.singletonList(rootUid);
			return this;
		}

		protected FunctionalBlockQueryBuilder ofRootBlocks(final List<UUID> rootUids) {
			filterByRootBlock = true;
			this.rootUids = rootUids;
			return this;
		}

		protected FunctionalBlockQueryBuilder withMaxDepth(final int maxDepth) {
			hasMaxDepth = true;
			this.maxDepth = maxDepth;
			return this;
		}

		protected FunctionalBlockQueryBuilder sortByChildrenDeepOrdinal(final SortDirection direction) {
			order.accept(q -> q.appendOrder("cd.seqPath", direction));
			return this;
		}

		@Override
		public FunctionalBlockQueryBuilder byUid(final UUID uid) {
			filters.accept(q -> q.append("fb.uid = ?", uid));
			return this;
		}

		@Override
		public FunctionalBlockQueryBuilder byUids(final Collection<UUID> uids) {
			filters.accept(q -> q.append("fb.uid = any(?)").addArg(PgType.UUID, uids));
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder notByUid(final UUID uid) {
			filters.accept(q -> q.append("fb.uid != ?", uid));
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder ofProject(final EntityId projectId) {
			filters.accept(q -> q.append("fb.project = ").with(ProjectPgDao.referenceUidOrNid(projectId)));
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder withName(final String name) {
			filters.accept(q -> q.append("fb.name ILIKE ?", name));
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder withNames(final Collection<String> names) {
			filters.accept(q -> q.append("fb.name = any(?)").addArg(PgType.STRING, names));
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder withFlag(final FunctionalBlockFlag flag, final String value) {
			filters.accept(q -> q.append("fb.flags->'"+ flag.name() + "' ?? ?", value));
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder withFlag(final FunctionalBlockFlag flag, final boolean value) {
			filters.accept(q -> q.append(String.format("fb.flags @> '{\"%s\": %b}'::jsonb", flag.name(), value)));
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder notWithFlag(final FunctionalBlockFlag flag, final boolean value) {
			filters.accept(q -> q.append(String.format("NOT fb.flags @> '{\"%s\": %b}'::jsonb", flag.name(), value)));
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder withType(final FunctionalBlockType type) {
			filters.accept(q -> q.append("fb.flags->'" + FunctionalBlockFlag.TYPE.name() + "' ?? ?", type.name()));
			return this;
		}
		
		@Override
		public FunctionalBlockInquiryBuilder withAnnotationType(final AnnotationType type) {
			annotationType = type;
			filters.accept(q -> q.append("a.type = ?", type.toString()));
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder notWithType(final FunctionalBlockType type) {
			filters.accept(q -> q.append("NOT fb.flags->'" + FunctionalBlockFlag.TYPE.name() + "' ?? ?", type.name()));
			return this;
		}

		@Override
		public FunctionalBlockQueryBuilder withTypes(final Collection<FunctionalBlockType> types) {
			filters.accept(q -> q.append("fb.flags->'" + FunctionalBlockFlag.TYPE.name() + "' ?? any(?)")
					.addArg(PgType.STRING, types.stream().map(FunctionalBlockType::name).collect(Collectors.toList())));
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder withLowerBoundAccessType(final String lbAccessType) {
			filters.accept(q -> q.append("fb.flags->'" + FunctionalBlockFlag.RA_ACCESS_TYPE.name() + "' ?? ?", lbAccessType));
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder withLowerBoundAccessTypes(final Collection<String> lbAccessTypes) {
			filters.accept(q -> q.append("fb.flags->'" + FunctionalBlockFlag.RA_ACCESS_TYPE.name() + "' ?? any(?)")
					.addArg(PgType.STRING, lbAccessTypes));
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder withStatus(final FunctionalBlockStatus status) {
			filters.accept(q -> q.append("fb.flags->>'" + FunctionalBlockFlag.STATUS.name() + "' = ?", status.name()));
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder notWithStatus(final FunctionalBlockStatus status) {
			filters.accept(q -> q.append("flags IS NULL OR NOT flags ?? ? OR NOT fb.flags->>? = ?", FunctionalBlockFlag.STATUS.name(),
					FunctionalBlockFlag.STATUS.name(), status.name()));
			return this;
		}

		@Override
		public FunctionalBlockQueryBuilder withResolvedModulePart(final EntityId moduleId) {
			super.withResolvedModulePart(moduleId);
			return this;
		}
		
		@Override
		public FunctionalBlockInquiryBuilder withResolvedModuleParts(final Collection<EntityId> moduleIds) {
			final boolean allHaveUid = moduleIds.stream().allMatch(EntityId::hasUid);
			boolean allHaveNid = false;
			if ( ! allHaveUid) {
				allHaveNid = moduleIds.stream().allMatch(EntityId::hasNid);
			}
			if ( ! (allHaveUid || allHaveNid)) {
				throw new IllegalArgumentException("You can not mix UUIDs and numerical ids in the call to withResolvedModuleParts()");
			}
			if (allHaveUid) {
				filters.accept(q -> q.append("fb.uid IN (SELECT functional_block FROM functional_block_resolved_module_part rmp" +
						" WHERE rmp.module = any(?))")
						.addArg(PgType.UUID, moduleIds.stream().map(EntityId::getUid).collect(Collectors.toList())));
			} else {
				filters.accept(q -> q.append("fb.uid IN (SELECT functional_block FROM functional_block_resolved_module_part rmp" +
						" JOIN module m ON m.uid = rmp.module WHERE m.nid = any(?))")
						.addArg(PgType.LONG, moduleIds.stream().map(EntityId::getNid).collect(Collectors.toList())));
			}
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder withResolvedModulePartModuleName(final String moduleName) {
			joinModule = true;
			filters.accept(q -> q.append("mod.name = ?", moduleName));
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder withResolvedModulePartModuleNames(final Collection<String> moduleNames) {
			joinModule = true;
			filters.accept(q -> q.append("mod.name = any(?)")
								 .addArg(PgType.STRING, moduleNames));
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder withResolvedModulePartTechnology(final Technology moduleTechnology) {
			joinModule = true;
			filters.accept(q -> q.append("mod.technology = ?", moduleTechnology.name()));
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder withResolvedModulePartTechnologies(final Collection<Technology> moduleTechnologies) {
			joinModule = true;
			filters.accept(q -> q.append("mod.technology = any(?)")
								 .addArg(PgType.STRING, moduleTechnologies.stream().map(Technology::name).collect(Collectors.toList())));
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder withResolvedModulePartType(final Type moduleType) {
			joinModule = true;
			filters.accept(q -> q.append("mod.type = ?", moduleType.name()));
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder withResolvedModulePartTypes(final Collection<Type> moduleTypes) {
			joinModule = true;
			filters.accept(q -> q.append("mod.type = any(?)")
								 .addArg(PgType.STRING, moduleTypes.stream().map(Type::name).collect(Collectors.toList())));
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder withResolvedModulePartHavingTaxonomy(final EntityId taxonomyId) {
			return extractResolvedModulePartsHavingTaxonomy(Collections.singletonList(taxonomyId));
		}

		@Override
		public FunctionalBlockInquiryBuilder withResolvedModulePartHavingTaxonomies(final Collection<EntityId> taxonomyIds) {
			return extractResolvedModulePartsHavingTaxonomy(taxonomyIds);
		}

		@Override
		public FunctionalBlockQueryBuilder withResolvedModulePartAtOffset(final EntityId moduleId, final int offset) {
			moduleId.perform(
					uid -> filters.accept(q -> q.append("fb.uid IN (SELECT functional_block FROM functional_block_resolved_module_part rmp" +
							" WHERE rmp.module = ?" +
							" AND ((rmp.location) IS NULL OR (rmp.location).offset <= ? AND (rmp.location).offset + (rmp.location).length - 1 >= ?))",
							uid, offset, offset)),
					nid -> filters.accept(q -> q.append("fb.uid IN (SELECT functional_block FROM functional_block_resolved_module_part rmp" +
							" JOIN module m ON m.uid = rmp.module" +
							" WHERE m.nid = ?" +
							" AND ((rmp.location) IS NULL OR (rmp.location).offset <= ? AND (rmp.location).offset + (rmp.location).length - 1 >= ?))",
							nid, offset, offset))
			);
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder withParent(final BuildingConsumer<FunctionalBlockInquiryBuilder> parentFilter) {
			return withParent(parentFilter, false);
		}

		@Override
		public FunctionalBlockInquiryBuilder notWithParent(final BuildingConsumer<FunctionalBlockInquiryBuilder> parentFilter) {
			return withParent(parentFilter, true);
		}

		private FunctionalBlockInquiryBuilder withParent(final BuildingConsumer<FunctionalBlockInquiryBuilder> parentFilter,
				final boolean not) {

			final QueryBuilder parentQuery = parentFilter.prepare(new FunctionalBlockQueryBuilder(SELECT_PARENT_FOR_FILTERING)).buildQuery();
			filters.accept(q -> q.append("fb.uid" + (not ? " NOT" : "") + " IN (" + parentQuery + ")", parentQuery.getArgs()));
			return this;
		}

		@Override
		public FunctionalBlockQueryBuilder withChild(final BuildingConsumer<FunctionalBlockInquiryBuilder> childrenFilter) {
			final QueryBuilder childrenQuery = childrenFilter.prepare(new FunctionalBlockQueryBuilder(SELECT_CHILDREN_FOR_FILTERING)).buildQuery();
			filters.accept(q -> q.append("fb.uid" + " IN (" + childrenQuery + ")", childrenQuery.getArgs()));
			return this;
		}

		@Override
		public FunctionalBlockQueryBuilder withChildForMergedAndSingleReachabilityBlocks
				(final BuildingConsumer<FunctionalBlockInquiryBuilder> childrenFilter) {
			final QueryBuilder childrenQueryForSimpleFunctionalBLocks = childrenFilter.prepare(new FunctionalBlockQueryBuilder(
					SELECT_CHILDREN_FOR_FILTERING)).buildQuery();
			final QueryBuilder childrenQueryForMergedFunctionalBlocks = childrenFilter.prepare(new FunctionalBlockQueryBuilder(
					SELECT_CHILDREN_FOR_FILTERING + " WHERE fb.uid IN ( "+ SELECT_CHILDREN_FOR_FILTERING)).buildQuery();
			final List<Object> combinedArgs = new ArrayList<>();
			combinedArgs.addAll(Arrays.asList(childrenQueryForSimpleFunctionalBLocks.getArgs()));
			combinedArgs.addAll(Arrays.asList(childrenQueryForMergedFunctionalBlocks.getArgs()));
			filters.accept(q -> q.append("fb.uid" + " IN ((" + childrenQueryForSimpleFunctionalBLocks +
					") UNION ALL (" + childrenQueryForMergedFunctionalBlocks + ")))", combinedArgs.toArray()));
			return this;
		}

		@Override
		public FunctionalBlockQueryBuilder withPeer(final BuildingConsumer<FunctionalBlockInquiryBuilder> peerFilter) {
			super.withPeer(peerFilter);
			return this;
		}

		@Override
		public FunctionalBlockQueryBuilder notWithPeer(final BuildingConsumer<FunctionalBlockInquiryBuilder> peerFilter) {
			return withPeer(peerFilter, true);
		}

		private FunctionalBlockQueryBuilder withPeer(final BuildingConsumer<FunctionalBlockInquiryBuilder> peerFilter,
				final boolean not) {

			final FunctionalBlockQueryBuilder peerBuilder = peerFilter.prepare(new FunctionalBlockQueryBuilder(SELECT_PEERS_FOR_FILTERING));
			final QueryBuilder peerQuery = peerBuilder.buildSelectFrom();
			peerQuery.with(peerBuilder.filters::build);
			peerQuery.append(SELECT_PEERS_CONDITION);
			filters.accept(q -> q.append("fb.uid" + (not ? " NOT" : "") + " IN (" + peerQuery + ")", peerQuery.getArgs()));
			return this;
		}

		@Override
		public FunctionalBlockQueryBuilder generatedFromAnnotation(final EntityId annotationId) {
			joinGeneratedFrom = true;
			filters.accept(q -> q.append("gf.annotation_nid = ?", annotationId.getNid()));
			return this;
		}
		
		@Override
		public FunctionalBlockInquiryBuilder generatedFromModule(final String linkHash) {
			joinGeneratedFrom = true;
			filters.accept(q -> q.append("gf.module_link_hash = ?", linkHash));
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder generatedFromModules(final Collection<String> linkHashes) {
			joinGeneratedFrom = true;
			filters.accept(q -> q.append("gf.module_link_hash = any(?)").addArg(PgType.STRING, linkHashes));
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder generatedFromAnnotations(final List<EntityId> annotationIds) {
			joinGeneratedFrom = true;
			filters.accept(q -> q.append("gf.annotation_nid = any(?)")
					.addArg(PgType.LONG, annotationIds.stream().map(EntityId::getNid).collect(Collectors.toList())));
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder withContentChangedAfter(final Instant modifiedAfter) {
			joinGeneratedFrom = true;
			filters.accept(q -> q.append("gf.content_changed > ?")
					.addArg(Timestamp.from(modifiedAfter)));
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder withMissingSinceAfter(final Instant missingAfter) {
			joinGeneratedFrom = true;
			filters.accept(q -> q.append("gf.missing_since > ?").addArg(Timestamp.from(missingAfter)));
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder withDependencyChangedAfter(final Instant dependencyChangedAfter) {
			joinGeneratedFrom = true;
			filters.accept(q -> q.append("gf.dependency_changed > ?")
					.addArg(Timestamp.from(dependencyChangedAfter)));
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder withContentChangedOrDependencyChangedAfter(final Instant modifiedAfter, final Instant dependencyChangedAfter) {
			joinGeneratedFrom = true;
			filters.accept(q -> q.append(" ( gf.content_changed > ? OR gf.dependency_changed > ? ) ")
					.addArg(Timestamp.from(modifiedAfter))
					.addArg(Timestamp.from(dependencyChangedAfter)));
			return this;
		}

		private FunctionalBlockInquiryBuilder extractResolvedModulePartsHavingTaxonomy(final Collection<EntityId> taxonomyIds) {
			filters.accept(q -> {
				q.append("fb.uid IN (SELECT functional_block FROM functional_block_resolved_module_part rmp" +
						" WHERE rmp.module IN (SELECT module FROM module_taxonomies WHERE taxonomy ");

				final var uids = EntityId.allUids(taxonomyIds);
				if (uids.size() == taxonomyIds.size()) {
					q.append("= any(?)))", arrayFromCollection(PgType.UUID, uids));
				} else {
					final var nids = EntityId.allNids(taxonomyIds);
					if (nids.size() == taxonomyIds.size()) {
						q.append("IN (SELECT uid FROM taxonomy WHERE nid = any(?))))", 
								arrayFromCollection(PgType.LONG, nids));
					} else {
						q.append("= any(?) OR taxonomy IN (SELECT uid FROM taxonomy WHERE nid = any(?))))", 
								arrayFromCollection(PgType.UUID, uids), 
								arrayFromCollection(PgType.LONG, nids));
					}
				}
			});
			return this;
		}

		protected List<ModulePart> mapModuleParts(final Stream<Object[]> rawData) {
			return rawData.filter(raw -> raw[0] != null)
					.map(raw -> new ModulePart((String) raw[0], raw[1] == null ? null : new ModuleLocation((Integer) raw[1], (Integer) raw[2])))
					.collect(Collectors.toList());
		}

		protected List<UUID> mapUUIDs(@Nullable final Array array) throws SQLException {
			if (array == null) {
				return Collections.emptyList();
			}
			return Arrays.asList((UUID[]) array.getArray());
		}

		@Override
		public FunctionalBlockService.FunctionalBlockOrderBuilder sortChildOrdinal(final UUID parent) {
			sortChildParent = parent;
			order.accept(q -> q.append("sc.ordinal"));
			return this;
		}
		
		@Override
		public FunctionalBlockOrderBuilder sortName(final SortDirection direction) {
			order.accept(q -> q.appendOrder("fb.name", direction));
			return this;
		}
		
		@Override
		public FunctionalBlockOrderBuilder sortByLastModified(final SortDirection direction) {
			order.accept(q -> q.appendOrder("fb.updated", direction));
			return this;
		}

		@Override
		public FunctionalBlockOrderBuilder sortByFlagContainsValue(final FunctionalBlockFlag flag, final Object value, final SortDirection direction) {
			order.accept(q -> q.appendOrder("fb.flags->'" + flag.name() + "' ?? '" + value + "' ", direction));
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder withReferenceToDataDictionaries(final List<UUID> dataDictionaries) {
			filters.accept(q -> q.append(
					"fb.uid IN (SELECT functional_block FROM functional_block_referenced_data_dictionary rdd" + " WHERE rdd.data_dictionary = any(?))")
				.addArg(PgType.UUID, dataDictionaries.stream().map(UUID::toString).collect(Collectors.toList())));
			return this;
		}

		@Override
		public FunctionalBlockInquiryBuilder withReferenceToDataDictionaryNames(final List<String> dataDictionaryNames) {
			filters.accept(q -> q.append("fb.uid IN (SELECT functional_block FROM functional_block_referenced_data_dictionary rdd "
					+ "LEFT JOIN data_dictionary dd ON rdd.data_dictionary = dd.uid WHERE dd.name = any(?))")
					.addArg(PgType.STRING, dataDictionaryNames));
			return this;
		}
	}

	public class FunctionalBlockLinkQueryBuilder implements FunctionalBlockLinkInquiryBuilder {

		protected final FilterStreamBuilder filters = new FilterStreamBuilder();
		private boolean joinSharedResourceModule = false;

		protected List<FunctionalBlockLink> build() {
			return query("SELECT l.uid, l.child_a, l.child_b, l.condition_label, l.flags, c.uid, c.label, l.parent " +
					"FROM functional_block_link l " +
					"LEFT JOIN functional_block_link_condition c ON c.uid = l.condition ")
					.when(joinSharedResourceModule, q -> q.append("JOIN functional_block fb ON l.parent = fb.uid ")
							.append("INNER JOIN functional_block_generated_from fg ON l.flags->'RA_SHARED_RESOURCE_ID' ?? fg.functional_block::text ")
							.append("JOIN module m ON m.link_hash = fg.module_link_hash AND m.project = fb.project "))
					.with(filters::build)
					.toList((rs, row)-> {
						final UUID uuid = (UUID) rs.getObject(1);
						final UUID childA = (UUID) rs.getObject(2);
						final UUID childB = (UUID) rs.getObject(3);
						final String conditionLabel = rs.getString(4);
						final Map<String, Object> flags = PgJSON.fromPGobject(rs.getObject(5));
						final UUID conditionId = (UUID) rs.getObject(6);
						final String label = rs.getString(7);
						final UUID parent = (UUID) rs.getObject(8);
						final FunctionalBlockLinkCondition condition;
						if (conditionId != null) {
							condition = new FunctionalBlockLinkCondition(conditionId, label);
						} else {
							condition = null;
						}
						return new FunctionalBlockLink(uuid, parent, childA, childB, conditionLabel, flags, condition);
					});
		}

		protected QueryBuilder delete() {
			return query("DELETE FROM functional_block_link ").with(filters::build);
		}

		@Override
		public FunctionalBlockLinkQueryBuilder byUid(final UUID uid) {
			filters.accept(q -> q.append("uid = ?", uid));
			return this;
		}

		@Override
		public FunctionalBlockLinkQueryBuilder withFlag(final FunctionalBlockLinkFlag flag, final String value) {
			filters.accept(q -> q.append("l.flags->'"+ flag.name() + "' ?? ?", value));
			return this;
		}

		@Override
		public FunctionalBlockLinkQueryBuilder ofParent(final UUID parent) {
			filters.accept(q -> q.append("parent = ?", parent));
			return this;
		}

		@Override
		public FunctionalBlockLinkQueryBuilder withGeneratedBy(final Collection<String> generatedBy) {
			filters.accept(q -> q.append("flags->'" + FunctionalBlockFlag.GENERATED_BY.name() + "' ?? any(?)").addArg(PgType.STRING, generatedBy));
			return this;
		}

		@Override
		public FunctionalBlockLinkQueryBuilder withSharedResourceTechnologyTypes(final Collection<Pair<Technology, Type>> technologyTypes) {
			joinSharedResourceModule = true;
			filters.accept(q -> {
				q.append(" ( ");
				technologyTypes.forEach(pair -> {
					q.append(" (m.technology = ? AND m.type = ?) OR ", pair.getLeft().name(), pair.getRight().name());
				});
				q.append(" FALSE ) ");
			});
			return this;
		}

		@Override
		public FunctionalBlockLinkInquiryBuilder notWithChildAs(final Collection<UUID> childAs) {
			filters.accept(q -> q.append("NOT child_a = any(?)").addArg(PgType.UUID, childAs));
			return this;
		}

		@Override
		public FunctionalBlockLinkInquiryBuilder notWithChildBs(final Collection<UUID> childBs) {
			filters.accept(q -> q.append("NOT child_b = any(?)").addArg(PgType.UUID, childBs));
			return this;
		}
	}

	class ReachabilityDataQueryBuilder implements FunctionalBlockService.ReachabilityDataInquiryBuilder {

		protected final FilterStreamBuilder filters = new FilterStreamBuilder();
		protected final OrderStreamBuilder order = new OrderStreamBuilder();

		protected String queryString;

		protected boolean aggregateAccessModules = true;
		protected boolean joinUpperBoundModules = false;
		protected boolean joinLowerBoundModules = false;
		protected boolean joinIntermediateModule = false;

		protected ReachabilityDataQueryBuilder(final String queryString) {
			this.queryString = queryString;
		}

		protected Paged.Builder<ReachabilityDataPojo> build(@Nullable final Pagination paging) {
			return query(queryString)
					.when(aggregateAccessModules, q -> q.append(REACHABILITY_DATA_WITH_AGGREGATED_ACCESS_MODULES_QUERY))
					.when( ! aggregateAccessModules, q-> q.append(REACHABILITY_DATA_QUERY))
					.append(" FROM functional_block_reachability_data rd")
					.append(" LEFT JOIN functional_block fb ON fb.uid = rd.functional_block")
					.when(joinUpperBoundModules, q -> q.append(" LEFT JOIN module um ON um.uid = rd.upper_bound_module"))
					.when(joinLowerBoundModules, q -> q.append(" LEFT JOIN module lm ON lm.uid = rd.lower_bound_module"))
					.when(joinIntermediateModule, q -> q.append(" LEFT JOIN reachability_data_intermediate_modules im ON im.reachability_data = rd.uid "))
					.with(filters::build)
					.when(aggregateAccessModules, q -> q.append(" GROUP BY rd.upper_bound_module, rd.lower_bound_module, rd.functional_block"))
					.when(aggregateAccessModules && joinUpperBoundModules, q -> q.append(", um.name "))
					.when(aggregateAccessModules && joinLowerBoundModules, q -> q.append(", lm.name "))
					.with(order::build)
					.toPageable(paging, (rs, row) -> {
						try {
							return new ReachabilityDataPojo(
									null,
									(UUID) rs.getObject(1),
									EntityId.of(rs.getString(2), rs.getLong(3)),
									mapLowerBound(rs.getString(4), rs.getLong(5)),
									aggregateAccessModules ? mapEntityIds(rs.getArray(6))
											: Collections.singletonList(EntityId.of(rs.getString(6), rs.getLong(7))),
									aggregateAccessModules ? mapStrings(rs.getArray(7)) : mapStrings(rs.getArray(8)),
									aggregateAccessModules ? mapEntityIds(rs.getArray(8)) : mapEntityIds(rs.getArray(9))
							);
						} catch (final Exception e) {
							throw new IllegalStateException("Exception in ReachabilityData Build Method", e.getCause());
						}
					});
		}

		@Override
		public ReachabilityDataQueryBuilder ofFunctionalBlock(final UUID uid) {
			filters.accept(q -> q.append("fb.uid = ?", uid));
			return this;
		}

		@Override
		public ReachabilityDataQueryBuilder ofFunctionalBlocks(final List<UUID> uids) {
			filters.accept(q -> q.append("fb.uid = any(?)").addArg(PgType.UUID, uids));
			return this;
		}

		@Override
		public ReachabilityDataQueryBuilder aggregateAccessModulesPerLowerBound(final boolean aggregateLowerBounds) {
			this.aggregateAccessModules = aggregateLowerBounds;
			return this;
		}

		@Override
		public ReachabilityDataQueryBuilder sortByUpperBoundModuleName(final SortDirection direction) {
			joinUpperBoundModules = true;
			order.accept(q -> q.appendOrder("um.name COLLATE ci", direction));
			return this;
		}

		@Override
		public ReachabilityDataQueryBuilder sortByLowerBoundModuleName(final SortDirection direction) {
			joinLowerBoundModules = true;
			order.accept(q -> q.appendOrder("lm.name COLLATE ci", direction, true));
			return this;
		}

		@Override
		public ReachabilityDataQueryBuilder ofUpperBoundTaxonomy(final EntityId taxonomyId) {
			if (taxonomyId.hasUid()) {
				filters.accept(q -> q.append("rd.upper_bound_module IN (SELECT module FROM module_taxonomies WHERE taxonomy = ?)", taxonomyId.getUid()));
			} else {
				filters.accept(q -> q.append(
						"rd.upper_bound_module IN (SELECT module FROM module_taxonomies WHERE taxonomy IN" + "(SELECT uid FROM taxonomy WHERE nid = ?))",
						taxonomyId.getNid()));
			}
			return this;
		}

		@Override
		public ReachabilityDataQueryBuilder ofUpperBoundTaxonomies(final Collection<EntityId> taxonomyIds) {
			final boolean allHaveUid = taxonomyIds.stream().allMatch(EntityId::hasUid);
			boolean allHaveNid = false;
			if ( ! allHaveUid) {
				allHaveNid = taxonomyIds.stream().allMatch(EntityId::hasNid);
			}
			if ( ! (allHaveUid || allHaveNid)) {
				throw new IllegalArgumentException("You can not mix UUIDs and numerical ids in the call to ofUpperBoundTaxonomies()");
			}
			if (allHaveUid) {
				filters.accept(q -> q.append("rd.upper_bound_module IN (SELECT module FROM module_taxonomies WHERE taxonomy = any(?))")
						.addArg(PgType.UUID, taxonomyIds.stream().filter(EntityId::hasUid).map(EntityId::getUid).toList()));
			} else {
				filters.accept(q -> q.append("rd.upper_bound_module IN (SELECT module FROM module_taxonomies WHERE taxonomy IN" +
						"(SELECT uid FROM taxonomy WHERE nid = any(?)))")
						.addArg(PgType.LONG, taxonomyIds.stream().filter(EntityId::hasNid).map(EntityId::getNid).toList()));
			}
			return this;
		}

		@Override
		public ReachabilityDataQueryBuilder ofLowerBoundTaxonomy(final EntityId taxonomyId) {
			if (taxonomyId.hasUid()) {
				filters.accept(q -> q.append("rd.lower_bound_module IN (SELECT module FROM module_taxonomies WHERE taxonomy = ?)", taxonomyId.getUid()));
			} else {
				filters.accept(q -> q.append(
						"rd.lower_bound_module IN (SELECT module FROM module_taxonomies WHERE taxonomy IN" + "(SELECT uid FROM taxonomy WHERE nid = ?))",
						taxonomyId.getNid()));
			}
			return this;
		}

		@Override
		public ReachabilityDataQueryBuilder ofLowerBoundTaxonomies(final Collection<EntityId> taxonomyIds) {
			final boolean allHaveUid = taxonomyIds.stream().allMatch(EntityId::hasUid);
			boolean allHaveNid = false;
			if ( ! allHaveUid) {
				allHaveNid = taxonomyIds.stream().allMatch(EntityId::hasNid);
			}
			if ( ! (allHaveUid || allHaveNid)) {
				throw new IllegalArgumentException("You can not mix UUIDs and numerical ids in the call to ofLowerBoundTaxonomies()");
			}
			if (allHaveUid) {
				filters.accept(q -> q.append("rd.lower_bound_module IN (SELECT module FROM module_taxonomies WHERE taxonomy = any(?))")
						.addArg(PgType.UUID, taxonomyIds.stream().filter(EntityId::hasUid).map(EntityId::getUid).toList()));
			} else {
				filters.accept(q -> q.append("rd.lower_bound_module IN (SELECT module FROM module_taxonomies WHERE taxonomy IN" +
						"(SELECT uid FROM taxonomy WHERE nid = any(?)))")
						.addArg(PgType.LONG, taxonomyIds.stream().filter(EntityId::hasNid).map(EntityId::getNid).toList()));
			}
			return this;
		}

		@Override
		public ReachabilityDataQueryBuilder ofPathTaxonomy(final EntityId taxonomyId) {
			this.joinIntermediateModule = true;
			if (taxonomyId.hasUid()) {
				filters.accept(q -> q.append("(im.intermediate_module in (select module from module_taxonomies where taxonomy = ?) " +
						" OR rd.access_module in (select module from module_taxonomies where taxonomy = ?))", taxonomyId.getUid(), taxonomyId.getUid()));
			} else {
				filters.accept(q -> q.append("(im.intermediate_module in (select module from module_taxonomies where taxonomy in" +
						"(select uid from taxonomy where nid = ?)) " +
						" OR rd.access_module in (select module from module_taxonomies where taxonomy in" +
						"(select uid from taxonomy where nid = ?)))", taxonomyId.getNid(), taxonomyId.getNid()));
			}
			return this;
		}

		@Override
		public ReachabilityDataQueryBuilder ofPathTaxonomies(final Collection<EntityId> taxonomyIds) {
			this.joinIntermediateModule = true;
			final boolean allHaveUid = taxonomyIds.stream().allMatch(EntityId::hasUid);
			boolean allHaveNid = false;
			if ( ! allHaveUid) {
				allHaveNid = taxonomyIds.stream().allMatch(EntityId::hasNid);
			}
			if ( ! (allHaveUid || allHaveNid)) {
				throw new IllegalArgumentException("You can not mix UUIDs and numerical ids in the call to ofPathTaxonomies()");
			}
			if (allHaveUid) {
				final List<UUID> taxonomyUids = taxonomyIds.stream().filter(EntityId::hasUid).map(EntityId::getUid).toList();
				filters.accept(q -> q.append("(im.intermediate_module in (select module from module_taxonomies where taxonomy = any(?)) " +
						" OR rd.access_module in (select module from module_taxonomies where taxonomy = any(?)))")
						.addArg(PgType.UUID, taxonomyUids)
						.addArg(PgType.UUID, taxonomyUids));
			} else {
				final List<Long> taxonomyNids = taxonomyIds.stream().filter(EntityId::hasNid).map(EntityId::getNid).toList();
				filters.accept(q -> q.append("(im.intermediate_module in (select module from module_taxonomies where taxonomy in" +
						"(select uid from taxonomy where nid = any(?))) " +
						" OR rd.access_module in (select module from module_taxonomies where taxonomy in" +
						"(select uid from taxonomy where nid = any(?))))")
						.addArg(PgType.LONG, taxonomyNids)
						.addArg(PgType.LONG, taxonomyNids));
			}
			return this;
		}

		private Optional<EntityId> mapLowerBound(@Nullable final String lowerBoundModule, @Nullable final Long lowerBoundModuleNid) {
			if (lowerBoundModule == null) {
				return Optional.empty();
			}
			return Optional.of(EntityId.of(lowerBoundModule, lowerBoundModuleNid));
		}

		private List<EntityId> mapEntityIds(@Nullable final Array array) throws SQLException, JsonProcessingException {
			if (array == null) {
				return Collections.emptyList();
			}
			final ObjectMapper objectMapper = new ObjectMapper();
			final Set<EntityId> accessModules = new HashSet<>();
			for (final String jsonString : (String[]) array.getArray()) {
				final JsonNode jsonNode = objectMapper.readTree(jsonString);
				if ( ! jsonNode.get("uid").asText().equals("null")) {
					accessModules.add(EntityId.of(jsonNode.get("uid").textValue(), jsonNode.get("nid").asLong()));
				}
			}
			return new ArrayList<>(accessModules);
		}

		private List<String> mapStrings(@Nullable final Array array) throws SQLException {
			if (array == null) {
				return Collections.emptyList();
			}
			return Arrays.stream((String[]) array.getArray()).filter(Objects::nonNull).collect(Collectors.toList());
		}
	}

	public class FunctionalBlockAggregationQueryBuilder extends AbstractAggregationQueryBuilder<FunctionalBlockFieldName, FunctionalBlockAggregationQueryBuilder>
					implements FunctionalBlockAggregationInquiryBuilder<FunctionalBlockAggregationQueryBuilder> {
	
		protected boolean joinModule;

		@Override
		protected String getFromClause() {
			return "functional_block fb";
		}

		@Override
		protected void buildJoins(final QueryBuilder query) {
			query.when(joinModule, q -> q.append(" INNER JOIN functional_block_resolved_module_part rmp on rmp.functional_block = fb.uid"
											   + " INNER JOIN module mod on rmp.module = mod.uid"));
		}

		@Override
		protected String getFieldQueryFragment(final FunctionalBlockFieldName field) {
			switch (field) {
				case UID:
					return "fb.uid AS " + field.name().toLowerCase();
				case TYPE:
					return "fb.flags->'"+ FunctionalBlockFlag.TYPE.name() + "' AS " + field.name().toLowerCase();
				case REFERENCED_MODULE_TECHNOLOGY:
					joinModule = true;
					return "mod.technology AS " + field.name().toLowerCase();
				case REFERENCED_MODULE_TYPE:
					joinModule = true;
					return "mod.type AS " + field.name().toLowerCase();
				default:
					throw new UnsupportedOperationException("The field is not supported yet: " + field.name());
			}
		}

		@Override
		public FunctionalBlockAggregationQueryBuilder byUid(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("fb.uid = ?", toUuid(value, "Type")));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("fb.uid != ?", toUuid(value, "Type")));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for TYPE", operator));
			}
			return this;
		}

		@Override
		public FunctionalBlockAggregationQueryBuilder ofProject(final String operator, final Object value) {
			if (OPERATOR_EQ.equals(operator)) {
				final EntityId entity = toEntityId(value, "Project id");
				filters.accept(q -> q.append("fb.project = ").with(ProjectPgDao.referenceUidOrNid(entity)));
			} else {
				throw new UnsupportedOperationException(String.format("The operator %s is not supported for PROJECT_ID", operator));
			}
			return this;
		}

		@Override
		public FunctionalBlockAggregationQueryBuilder withType(final String operator, final Object value) {
			switch (operator) {
				case OPERATOR_EQ:
					filters.accept(q -> q.append("fb.flags->'" + FunctionalBlockFlag.TYPE.name() + "' ?? ?", value.toString()));
					break;
				case OPERATOR_NOT_EQ:
					filters.accept(q -> q.append("NOT fb.flags->'" + FunctionalBlockFlag.TYPE.name() + "' ?? ?", value.toString()));
					break;
				default:
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for TYPE", operator));
			}
			return this;
		}

		@Override
		public FunctionalBlockAggregationQueryBuilder withModuleTechnology(final String operator, final Object value) {
			joinModule = true;
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
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for REFERENCED_MODULE_TECHNOLOGY", operator));
			}
			return this;
		}

		@Override
		public FunctionalBlockAggregationQueryBuilder withPeer(final BuildingConsumer<FunctionalBlockInquiryBuilder> peerFilter) {
			final FunctionalBlockQueryBuilder peerBuilder = peerFilter.prepare(new FunctionalBlockQueryBuilder(SELECT_PEERS_FOR_FILTERING));
			final QueryBuilder peerQuery = peerBuilder.buildSelectFrom();
			peerQuery.with(peerBuilder.filters::build);
			peerQuery.append(SELECT_PEERS_CONDITION);
			filters.accept(q -> q.append("fb.uid" + " IN (" + peerQuery + ")", peerQuery.getArgs()));
			return this;
		}

		@Override
		public FunctionalBlockAggregationQueryBuilder withModuleType(final String operator, final Object value) {
			joinModule = true;
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
					throw new UnsupportedOperationException(String.format("The operator %s is not supported for REFERENCED_MODULE_TYPE", operator));
			}
			return this;
		}

		@Override
		public FunctionalBlockAggregationQueryBuilder withResolvedModulePart(final EntityId moduleId) {
			moduleId.perform(
					uid -> filters.accept(q -> q.append("fb.uid IN (SELECT functional_block FROM functional_block_resolved_module_part rmp" +
							" WHERE rmp.module = ?)", uid)),
					nid -> filters.accept(q -> q.append("fb.uid IN (SELECT functional_block FROM functional_block_resolved_module_part rmp" +
							" JOIN module m ON m.uid = rmp.module WHERE m.nid = ?)", nid))
			);
			return this;
		}
	}

	/**
	 * Create a new DAO with the given data source
	 * @param jdbcTemplate the {@link JdbcTemplate} to use
	 */
	public FunctionalBlockPgDao(final JdbcTemplate jdbcTemplate) {
		super(jdbcTemplate);
	}

	protected FunctionalBlockQueryBuilder queryFunctionalBlocks() {
		return new FunctionalBlockQueryBuilder(SELECT_FB);
	}
	
	protected FunctionalBlockQueryBuilder queryFunctionalBlocksUid() {
		return new FunctionalBlockQueryBuilder(SELECT_FB_UID);
	}

	protected FunctionalBlockQueryBuilder queryChildrenDeep(final List<UUID> rootUids, final int maxDepth) {
		return new FunctionalBlockQueryBuilder(SELECT_CHILDREN_DEEP).ofRootBlocks(rootUids).withMaxDepth(maxDepth);
	}
	
	protected ReachabilityDataQueryBuilder queryReachabilityData() {
		return new ReachabilityDataQueryBuilder(BASE_REACHABILITY_DATA_SELECT_QUERY);
	}

	/**
	 * Returns the functional block with the given uid.
	 * @param uid the id of the functional block
	 * @return the functional block, if it exists
	 */
	public Optional<FunctionalBlockPojo> find(final UUID uid) {
		return queryFunctionalBlocks().byUid(uid).build(null).first();
	}

	/**
	 * Returns the functional blocks with the given uids.
	 * @param uids the ids of the functional blocks
	 * @return the list of functional blocks
	 */
	public List<FunctionalBlockPojo> find(final Collection<UUID> uids) {
		return queryFunctionalBlocks().byUids(uids).build(null).all();
	}

	/**
	 * Returns the functional blocks matching the given inquiry builder.
	 * @param builder the inquiry builder for filtering functional blocks
	 * @return the list of matching functional blocks
	 */
	public List<FunctionalBlockPojo> find(final BuildingConsumer<FunctionalBlockInquiryBuilder> builder) {
		return builder.prepare(queryFunctionalBlocks()).build(null).all();
	}

	/**
	 * Returns a paged subset of the functional blocks matching the given inquiry builder.
	 * @param paging Pagination specification
	 * @param builder the inquiry builder for filtering functional blocks
	 * @return the list of matching functional blocks
	 */
	public Paged<FunctionalBlockPojo> find(final Pagination paging,
										   final BuildingConsumer<FunctionalBlockInquiryBuilder> builder) {
		return builder.prepare(queryFunctionalBlocks()).build(paging).page();
	}
	
	/**
	 * Returns list functional block uids matching the given inquiry builder.
	 * @param builder the inquiry builder for filtering functional blocks
	 * @return the list of matching functional blocks uid
	 */
	public List<UUID> findUids(final BuildingConsumer<FunctionalBlockInquiryBuilder> builder) {
		return builder.prepare(queryFunctionalBlocksUid())
				.buildUids();
	}

	public List<FunctionalBlockPojo> findChildrenDeep(final List<UUID> parents, final int maxDepth,
			final BuildingConsumer<FunctionalBlockInquiryBuilder> builder) {
		return builder.prepare(queryChildrenDeep(parents, maxDepth))
				.sortByChildrenDeepOrdinal(SortDirection.ASCENDING).build(null).all();
	}

	public Paged<FunctionalBlockPojo> findChildrenDeep(final UUID parent, final Pagination paging, final int maxDepth,
													   final BuildingConsumer<FunctionalBlockInquiryBuilder> builder) {
		return builder.prepare(queryChildrenDeep(List.of(parent), maxDepth)).sortByChildrenDeepOrdinal(SortDirection.ASCENDING).build(paging).page();
	}

	public Map<UUID, List<UUID>> findChildrenIdsDeep(final List<UUID> parents, final int maxDepth) {
		return query(SELECT_CHILDREN_IDS_DEEP).addArg(PgType.UUID, parents).addArg(maxDepth).toMap((rs, map) -> {
			final List<UUID> children = map.computeIfAbsent((UUID) rs.getObject(1), k -> new ArrayList<>());
			children.add((UUID) rs.getObject(2));
		});
	}

	/**
	 * Returns aggregation values for the filters and selected aggregations in the given {@code builder}.
	 *
	 * @param builder the {@linkplain FunctionalBlockAggregationInquiryBuilder} containing the aggregation operations and filter criteria
	 * @return container with the aggregation values
	 */
	public Optional<Table> getAggregations(final BuildingConsumer<FunctionalBlockAggregationInquiryBuilder<?>> builder) {
		return builder.prepare(new FunctionalBlockQueryBuilder(""))
						.buildAggregation(this);
	}
	
	/**
	 * Finds all functional blocks that matches the given Search criteria.
	 * @param pagination the pagination attributes
	 * @param projectId id of the project
	 * @param moduleIds List of id of the modules referenced by the functional blocks
	 * @param taxonomyIds List of id of the taxonomy referenced by the Modules
	 * @param peers filter for matching the peers
	 * @param searchNames filter for matching the functional blocks by names
	 * @param ddIds filter for matching the functional blocks by reference DD
	 * @return the list of matching functional blocks by name
	 */
	public Paged<FunctionalBlockPojo> findByName(final Pagination pagination, final Long projectId, final List<Long> moduleIds, final List<Long> taxonomyIds,
			final List<UUID> peers, final List<String> searchNames, final List<UUID> ddIds) {
		final List<String> replacedNames = searchNames.stream()
				.map(n -> n.replace("*", "%"))
				.collect(Collectors.toList());
				return query(SEARCH_QUERY_WITHOUT_FILTER).addArg(projectId).addArg(replacedNames.stream().toArray(String[]::new))
						.addArg(replacedNames.stream().toArray(String[]::new))
				.when( ! taxonomyIds.isEmpty(), q -> q.append(TAXONOMY_FILTER).addArg(taxonomyIds.stream().toArray(Long[]::new)))
				.when( ! moduleIds.isEmpty(), q -> q.append(MODULES).addArg(moduleIds.stream().toArray(Long[]::new)))
				.when( ! peers.isEmpty(), q -> q.append(PEERS).addArg(peers.stream().toArray(UUID[]::new)))
				.when( ! ddIds.isEmpty(), q -> q.append(REFERENCED_DATA_DICTIONARY).addArg(ddIds.stream().toArray(UUID[]::new)))
				.toPageable(pagination, (rs, row) ->
					new FunctionalBlockPojo((UUID) rs.getObject(1), /* uid */
							rs.getString(2), /* name */
							(UUID) rs.getObject(3), EntityId.of(projectId))
				).page();
	}
	
	protected List<ModulePart> mapModuleParts(final Stream<Object[]> rawData) {
		return rawData.filter(raw -> raw[0] != null)
				.map(raw -> new ModulePart((String) raw[0], raw[1] == null ? null : new ModuleLocation((Integer) raw[1], (Integer) raw[2])))
				.collect(Collectors.toList());
	}

	protected List<UUID> mapUUIDs(@Nullable final Array array) throws SQLException {
		if (array == null) {
			return Collections.emptyList();
		}
		return Arrays.asList((UUID[]) array.getArray());
	}

	public Set<String> findFunctionalBlockNamesByAnnotationId(final Long annotationId) {
		return query("SELECT fu.name FROM functional_block_generated_from gf JOIN functional_block_children c on c.child = gf.functional_block \r\n"
				+ " JOIN functional_block fu ON c.parent = fu.uid WHERE gf.annotation_nid= ?"
				+" AND fu.flags->'"+ FunctionalBlockFlag.TYPE.name() + "' ?? '" + FunctionalBlockType.FUNCTIONAL_GROUP + "'")
				.addArg(annotationId).toSet((rs, row) -> rs.getString(1));
	}
	
	public Paged<ReachabilityDataPojo> findReachabilityData(final Pagination paging,
			final BuildingConsumer<FunctionalBlockService.ReachabilityDataInquiryBuilder> builder) {
		return builder.prepare(queryReachabilityData()).build(paging).page();
	}

	public List<ReachabilityDataPojo> findReachabilityData(final BuildingConsumer<FunctionalBlockService.ReachabilityDataInquiryBuilder> builder) {
		return builder.prepare(queryReachabilityData()).build(null).all();
	}

	public void setReachabilityData(final UUID functionalBlockId, final Collection<ReachabilityDataPojoPrototype> reachabilityData) {
		deleteReachabilityDataForFunctionalBlock(functionalBlockId);
		final Collection<Stream<Object>> batchArgs = new ArrayList<>();
		final Collection<Stream<Object>> intermediateModulesBatchArgs = new ArrayList<>();
		for(final ReachabilityDataPojoPrototype rd : reachabilityData) {
			final UUID reachabilityDataUuid = rd.uid.isDefined() ? rd.uid.getNonNull() : UUID.randomUUID();
			final UUID lowerModuleId = rd.lowerBoundModuleId.isDefined() ? rd.lowerBoundModuleId.getNonNull().getUid() : null;
			final UUID accessModuleId = rd.accessModuleId.isDefined() ? rd.accessModuleId.getNonNull().getUid() : null;
			final Collection<String> accessTypes = rd.accessTypes.isDefined() ? rd.accessTypes.getNonNull() : Collections.emptyList();
			batchArgs.add(Stream.of(
					reachabilityDataUuid,
					functionalBlockId,
					rd.upperBoundModuleId.getNonNull().getUid(),
					lowerModuleId,
					accessModuleId,
					arrayFromCollection(PgType.STRING, accessTypes)
			));
			if (rd.intermediateModules.isDefined() && ! rd.intermediateModules.getNonNull().isEmpty()) {
				rd.intermediateModules.getNonNull().forEach(module -> intermediateModulesBatchArgs.add(Stream.of(reachabilityDataUuid, module.getNid())));
			}
		}
		query("INSERT INTO functional_block_reachability_data (uid, functional_block, upper_bound_module, lower_bound_module, access_module, access_type)" +
				" VALUES (?, ?, ?, ?, ?, ?)").updateBatch(batchArgs.stream(), 1_000);
		if ( ! intermediateModulesBatchArgs.isEmpty()) {
			query("INSERT INTO reachability_data_intermediate_modules (reachability_data, intermediate_module)" +
					" VALUES (?, (SELECT uid FROM module WHERE nid = ?)) ON CONFLICT DO NOTHING").updateBatch(intermediateModulesBatchArgs.stream(), 1_000);
		}
	}
	
	/**
	 * Creates functional block from given {@linkplain FunctionalBlockPojoPrototype functionalBlockPojoPrototype}
	 *
	 * @param functionalBlockPojoPrototype the POJO prototype used to create a functional block.
	 * @return FunctionalBlockPojo the Functional Block POJO
	 */
	public UUID createFunctionalBlock(final FunctionalBlockPojoPrototype functionalBlockPojoPrototype) {
		return insertOrUpdate(functionalBlockPojoPrototype, false);
	}

	/**
	 * Updates functional block from given {@linkplain FunctionalBlockPojoPrototype functionalBlockPojoPrototype}
	 *
	 * @param functionalBlockPojoPrototype the POJO prototype used to update a functional block.
	 * @return the functional block uuid.
	 */
	public UUID updateFunctionalBlock(final FunctionalBlockPojoPrototype functionalBlockPojoPrototype) {
		return insertOrUpdate(functionalBlockPojoPrototype, true);
	}

	public void updateBlocksStatus(final Collection<UUID> functionalBlockUids, final FunctionalBlockStatus status) {
		if (status == FunctionalBlockStatus.INACTIVE) {
			query("UPDATE functional_block SET flags = jsonb_set(flags, '{" + FunctionalBlockFlag.STATUS.name() + "}', to_jsonb(?)) WHERE uid = any(?)")
					.addArg(status.name())
					.addArg(PgType.UUID, functionalBlockUids)
					.update();
		} else {
			query("UPDATE functional_block SET flags = flags - '" + FunctionalBlockFlag.STATUS.name() + "' WHERE uid = any(?)")
					.addArg(PgType.UUID, functionalBlockUids).update();
		}
	}

	/**
	 * Deletes the functional block entity for the given {@code uid}
	 *
	 * @param uid the uid of functional block
	 */
	public void deleteFunctionalBlock(final UUID uid) {
		query("DELETE FROM functional_block WHERE uid = ?").addArg(uid)
				.updateOrThrow(() -> new MiningEntityNotFoundException(FunctionalBlockPojo.class, uid.toString()));
	}

	/**
	 * Deletes the functional block entity for the given {@code uids}
	 *
	 * @param uids the uids of functional block
	 */
	public void deleteFunctionalBlocks(final Collection<UUID> uids) {
		if (uids.isEmpty()) {
			/* would otherwise throw */
			return;
		}
		query("DELETE FROM functional_block WHERE uid = Any(?)").addArg(PgType.UUID, uids)
				.updateOrThrow(() -> new MiningEntityNotFoundException(FunctionalBlockPojo.class, uids.toString()));
	}

	/**
	 * Deletes all functional blocks of the given project.
	 * @param projectId the id of the project
	 */
	public void deleteAllOfProject(final EntityId projectId) {
		query("DELETE FROM functional_block WHERE project = ").with(ProjectPgDao.referenceUidOrNid(projectId)).update();
	}

	/**
	 * Deletes the child functional block entity for the given {@code uid}
	 * @param uid the id of parent functional block
	 */
	public void deleteChildFunctionalConditionsAndFunctionalAsts(final UUID uid) {
		query(DELETE_FROM_FUNCTIONAL_BLOCK
				+ "fu WHERE uid in (SELECT child FROM functional_block_children WHERE parent = ?) AND "
				+"(fu.flags->'"+ FunctionalBlockFlag.TYPE.name() + "' ?? '" + FunctionalBlockType.FUNCTIONAL_STATEMENT + "'"
				+" OR fu.flags->'"+ FunctionalBlockFlag.TYPE.name() + "' ?? '" + FunctionalBlockType.FUNCTIONAL_CONDITION + "')")
				.addArg(uid).update();
	}

	/**
	 * Deletes the functional block entity for the given annotationId
	 *
	 * @param annotationId the id of annotation
	 */
	public void deleteGeneratedFromAnnotation(final Long annotationId) {
		query(DELETE_FROM_FUNCTIONAL_BLOCK
				+ "WHERE uid = (SELECT functional_block FROM functional_block_generated_from WHERE annotation_nid = ?)").addArg(annotationId)
				.update();
	}
	
	/**
	 * Deletes the functional block entities for the given annotationIds
	 *
	 * @param annotationIds the annotation ids
	 */
	public void deleteGeneratedFromAnnotations(final Collection<EntityId> annotationIds) {
		query(DELETE_FROM_FUNCTIONAL_BLOCK
				+ "WHERE uid in (SELECT functional_block FROM functional_block_generated_from WHERE annotation_nid = any(?))")
				.addArg(PgType.LONG, annotationIds.stream().map(EntityId::getNid).collect(Collectors.toList()))
				.update();
	}
	
	/**
	 * updates the functional block name and description for the given annotationId
	 *
	 * @param annotationId the id of annotation
	 * @param updatedAnnotation The updated annotation containing the new description and name.
	 */
	public void updateFunctionBlockDescriptionAndNameOnAnnotationUpdate(final Long annotationId,
			final AnnotationPojo updatedAnnotation) {
		query("UPDATE functional_Block SET description=?,"
				+ "name=? WHERE uid =(SELECT functional_block FROM functional_block_generated_from "
				+ "WHERE annotation_nid =?)")
				.addArg(updatedAnnotation.getName()).addArg(updatedAnnotation.getName()).addArg(annotationId).update();
	}

	/**
	 * Returns a Map of Annotation ids and ids of functional blocks that were
	 * generated from the corresponding annotation.
	 * 
	 * @param annotationIds the ids of the annotations
	 * @return a map containing an entry for each annotation id and the functional
	 *         block generated from that annotation, if exists
	 */
	public Map<Long, UUID> findGeneratedFromAnnotations(final Collection<EntityId> annotationIds) {
		return query("SELECT fb.uid, gf.annotation_nid from functional_block fb" +
				" JOIN functional_block_generated_from gf ON gf.functional_block = fb.uid" +
				" WHERE gf.annotation_nid = any(?)")
				.addArg(PgType.LONG, annotationIds.stream().map(EntityId::getNid).collect(Collectors.toList()))
				.toMap((rs, map) -> map.put(rs.getLong(2), (UUID) rs.getObject(1)));
	}

	/**
	 * Returns a Map of Module link hashes and ids of functional blocks that were generated from the corresponding modules.
	 * @param moduleLinkHashes the link hashes of the modules
	 * @param project the project id
	 * @return a map containing an entry for each module link hash and the functional block generated from that module, if exists
	 */
	public Map<String, UUID> findGeneratedFromModules(final Collection<String> moduleLinkHashes, final EntityId project) {
		return query("SELECT fb.uid, gf.module_link_hash from functional_block fb" +
				" JOIN functional_block_generated_from gf ON gf.functional_block = fb.uid" +
				" WHERE gf.module_link_hash = any(?) ")
				.addArg(PgType.STRING, moduleLinkHashes)
				.when(project.hasUid(), q -> q.append(" AND fb.project = ?", project.getUid()))
				.when(project.hasNid() && ! project.hasUid(), q -> q.append(" AND fb.project = (SELECT uid from project WHERE nid = ? )", project.getNid()))
				.toMap((rs, map) -> map.put(rs.getString(2), (UUID) rs.getObject(1)));
	}

	public Map<UUID, List<ModulePart>> getModuleParts(final Collection<UUID> uids) {
		return query("SELECT mp.functional_block, mp.module_link_hash, (mp.location).offset, (mp.location).length"
				+ " FROM functional_block_module_part mp"
				+ " WHERE mp.functional_block = any(?)")
				.addArg(PgType.UUID, uids).toMap((rs, map)-> {
					final UUID functionalBlock = (UUID) rs.getObject(1);
					final String linkHash = rs.getString(2);
					final int offset = rs.getInt(3);
					final boolean hasLocation = ! rs.wasNull();
					final int length = rs.getInt(4);
					final List<ModulePart> moduleParts = map.computeIfAbsent(functionalBlock, key -> new ArrayList<>());
					moduleParts.add(new ModulePart(linkHash, hasLocation ? new ModuleLocation(offset, length) : null));
				});
	}

	/**
	 * Set Resolved Module Parts of a functional block
	 *
	 * @param uid the functional block id
	 * @param resolvedModuleParts the list of resolved module parts
	 */
	public void setResolvedModuleParts(final UUID uid, final Collection<ResolvedModulePart> resolvedModuleParts) {
		deleteResolvedModulePartsForFunctionalBlock(uid);
		query("INSERT INTO functional_block_resolved_module_part (functional_block, module, location)" +
				" VALUES (?, CASE WHEN ? THEN ? ELSE (SELECT uid FROM module WHERE nid = ?) END, jsonb_populate_record(null::module_location, ?))" +
				" ON CONFLICT DO NOTHING")
			.updateBatch(resolvedModuleParts.stream()
				.map(part -> Stream.<Object>of(uid, part.getModuleId().hasUid(),
					part.getModuleId().getUidOptional().orElse(null), part.getModuleId().getNidOptional().orElse(null),
					part.getLocation().map(PgJSON::toPGobject).orElse(null))), 1_000);
	}

	/**
	 * Returns Resolved Module Parts for a given functional block uid.
	 *
	 * @param uid the functional block uid.
	 * @return the list of ResolvedModulePart.
	 */
	public List<ResolvedModulePart> getResolvedModuleParts(final UUID uid) {
		return query("SELECT mp.module, module.nid, (mp.location).offset, (mp.location).length"
				+ " FROM functional_block_resolved_module_part mp"
				+ " LEFT JOIN module on mp.module = module.uid"
				+ " WHERE mp.functional_block = ?")
				.addArgs(uid).toList((rs, row)-> {
					final EntityId moduleId = EntityId.of((UUID) rs.getObject(1), rs.getLong(2));
					final int offset = rs.getInt(3);
					final boolean hasLocation = ! rs.wasNull();
					final int length = rs.getInt(4);
					return new ResolvedModulePart(moduleId, hasLocation ? new ModuleLocation(offset, length) : null);
				});
	}

	/**
	 * Gets the resolved module parts for a list of blocks.
	 * @param uids the ids of the functional blocks
	 * @return a map from functional block id to resolved module parts
	 */
	public Map<UUID, List<ResolvedModulePart>> getResolvedModuleParts(final Collection<UUID> uids) {
		return query("SELECT mp.module, module.nid, (mp.location).offset, (mp.location).length, mp.functional_block"
				+ " FROM functional_block_resolved_module_part mp"
				+ " LEFT JOIN module on mp.module = module.uid"
				+ " WHERE mp.functional_block = any(?)")
				.addArg(PgType.UUID, uids).toMap((rs, map)-> {
					final EntityId moduleId = EntityId.of((UUID) rs.getObject(1), rs.getLong(2));
					final int offset = rs.getInt(3);
					final boolean hasLocation = ! rs.wasNull();
					final int length = rs.getInt(4);
					final List<ResolvedModulePart> resolvedModuleParts = map.computeIfAbsent((UUID) rs.getObject(5), key -> new ArrayList<>());
					resolvedModuleParts.add(new ResolvedModulePart(moduleId, hasLocation ? new ModuleLocation(offset, length) : null));
				});
	}

	/**
	 * Attaches {@link GeneratedFrom} information to a functional block.
	 * @param uid the uid of the functional block
	 * @param generatedFrom the {@link GeneratedFrom} information to attach to the block
	 */
	public void setGeneratedFrom(final UUID uid, final GeneratedFrom generatedFrom) {
		final FieldBuilder fields = new FieldBuilder();
		fields.add("functional_block", "?", uid);
		fields.add("module_link_hash", "?", generatedFrom.getModuleLinkHash().orElse(null));
		fields.add("module_content_hash", "?", generatedFrom.getModuleContentHash().orElse(null));
		fields.add("annotation_nid", "?", generatedFrom.getAnnotationId().map(EntityId::getNid).orElse(null));
		fields.add("content_changed", "?", generatedFrom.getContentChanged().map(Timestamp::from).orElse(null));
		fields.add("missing_since", "?", generatedFrom.getMissingSince().map(Timestamp::from).orElse(null));
		fields.add("module_dependency_hash", "?", generatedFrom.getModuleDependencyHash().orElse(null));
		fields.add("dependency_changed", "?", generatedFrom.getDependencyChanged().map(Timestamp::from).orElse(null));

		final QueryBuilder q = query("INSERT INTO functional_block_generated_from ");
		fields.buildUpsert(q, "functional_block");
		q.update();
	}

	/**
	 * Retrieves the {@link GeneratedFrom} information attached to a functional block, if present.
	 * @param uid the uid of the functional block
	 * @return the {@link GeneratedFrom} information attached to the block, if present
	 */
	public Optional<GeneratedFrom> getGeneratedFrom(final UUID uid) {
		return query("SELECT module_link_hash, module_content_hash, annotation_nid,"
				+ " content_changed, missing_since, module_dependency_hash, dependency_changed FROM functional_block_generated_from WHERE functional_block = ?")
				.addArgs(uid)
				.first(rs-> {
					final String moduleLinkHash = rs.getString(1);
					final String moduleContentHash = rs.getString(2);
					final long annotationId = rs.getLong(3);
					final boolean hasAnnotationId = ! rs.wasNull();
					final Timestamp contentChanged = rs.getTimestamp(4);
					final Timestamp missingSince = rs.getTimestamp(5);
					final String moduleDependencyHash = rs.getString(6);
					final Timestamp dependencyChanged = rs.getTimestamp(7);
					return new GeneratedFrom(moduleLinkHash, moduleContentHash, hasAnnotationId ? EntityId.of(annotationId) : null,
							contentChanged == null ? null : map(contentChanged, Timestamp::toInstant),
							missingSince == null ? null : map(missingSince, Timestamp::toInstant),
							moduleDependencyHash, dependencyChanged == null ? null : map(dependencyChanged, Timestamp::toInstant));
				});
	}

	/**
	 * Gets information from which entities the blocks were generated, if applicable.
	 * @param uids the ids of the functional blocks
	 * @return a map from functional block id to the stored "generated from" data, map will contain {@code null} value if no information present for block
	 */
	public Map<UUID, GeneratedFrom> getGeneratedFrom(final Collection<UUID> uids) {
		return query("SELECT module_link_hash, module_content_hash, annotation_nid, content_changed, missing_since,"
				+ "functional_block, module_dependency_hash, dependency_changed " +
				" FROM functional_block_generated_from WHERE functional_block = any(?)")
				.addArg(PgType.UUID, uids)
				.toMap((rs, map)-> {
					final String moduleLinkHash = rs.getString(1);
					final String moduleContentHash = rs.getString(2);
					final long annotationId = rs.getLong(3);
					final boolean hasAnnotationId = ! rs.wasNull();
					final Timestamp contentChanged = rs.getTimestamp(4);
					final Timestamp missingSince = rs.getTimestamp(5);
					final UUID functionalBlockUid = (UUID) rs.getObject(6);
					final String moduleDependencyHash = rs.getString(7);
					final Timestamp dependencyChanged = rs.getTimestamp(8);
					final GeneratedFrom generatedFrom = new GeneratedFrom(moduleLinkHash, moduleContentHash, hasAnnotationId ? EntityId.of(annotationId) : null,
							contentChanged == null ? null : map(contentChanged, Timestamp::toInstant),
							missingSince == null ? null : map(missingSince, Timestamp::toInstant),
							moduleDependencyHash, dependencyChanged == null ? null : map(dependencyChanged, Timestamp::toInstant));
					map.put(functionalBlockUid, generatedFrom);
				});
	}

	/**
	 * Persists a cached list of recursive children for the block. The list should be obtained from
	 * {@link #findChildrenDeep(UUID, Pagination, int, BuildingConsumer)} with no filters.
	 *
	 * @param parent the uid of the parent block
	 * @param childrenDeep the list of recursive children
	 */
	public void setChildrenDeep(final UUID parent, final Collection<UUID> childrenDeep) {
		deleteChildrenDeepForFunctionalBlock(parent);
		query("INSERT INTO functional_block_children_deep (parent, child)" +
				" VALUES (?, ?)" +
				" ON CONFLICT DO NOTHING")
			.updateBatch(childrenDeep.stream()
					.map(child -> Stream.<Object>of(parent, child)), 1_000);
	}

	/**
	 * Returns cached list of recursive children of the given block. This returns the same as {@link #findChildrenDeep(UUID, Pagination, int, BuildingConsumer)}
	 * with no additional filtering applied, but only if the functional block computation was executed on the block and is up-to-date.
	 *
	 * @param parent the uid of the parent block
	 * @return the cached list of recursive children
	 */
	public List<UUID> getChildrenDeep(final UUID parent) {
		return query("SELECT child FROM functional_block_children_deep WHERE parent = ?")
				.addArg(parent)
				.toList((rs, row)-> (UUID) rs.getObject(1));
	}

	/**
	 * Deletes the cached list of recursive children for the given functional block.
	 * @param builder the inquiry builder for filtering functional blocks
	 */
	public List<FunctionalBlockLink> getFunctionalBlockLinks(final BuildingConsumer<FunctionalBlockLinkInquiryBuilder> builder) {
		return builder.prepare(new FunctionalBlockLinkQueryBuilder()).build();
	}

	public Map<UUID ,List<FunctionalBlockLink>> getFunctionalBlockLinkMap(final Collection<UUID> uids) {
		return query("SELECT l.uid, l.child_a, l.child_b, l.condition_label, l.flags, l.parent, c.uid, c.label " +
				"FROM functional_block_link l " +
				"LEFT JOIN functional_block_link_condition c ON c.uid = l.condition " +
				"WHERE parent = any(?)")
				.addArg(PgType.UUID, uids)
				.toMap((rs, map)-> {
					final UUID uuid = (UUID) rs.getObject(1);
					final UUID childA = (UUID) rs.getObject(2);
					final UUID childB = (UUID) rs.getObject(3);
					final String conditionLabel = rs.getString(4);
					final Map<String, Object> flags = PgJSON.fromPGobject(rs.getObject(5));
					final UUID parent = (UUID) rs.getObject(6);
					final UUID conditionId = (UUID) rs.getObject(7);
					final String label = rs.getString(8);
					final FunctionalBlockLinkCondition condition;
					if (conditionId != null) {
						condition = new FunctionalBlockLinkCondition(conditionId, label);
					} else {
						condition = null;
					}
					final List<FunctionalBlockLink> functionalBlockLinks = map.computeIfAbsent(parent, k -> new ArrayList<>());
					functionalBlockLinks.add(new FunctionalBlockLink(uuid, parent, childA, childB,
							conditionLabel, flags, condition));
				});
	}

	private void createFunctionalBlockChildren(final UUID parentUuid, final Collection<UUID> childUuids) {
		final AtomicInteger ordinalCounter = new AtomicInteger(1);
		final Stream<Stream<Object>> batchArgs = childUuids.stream()
				.map(child -> Stream.<Object>of(parentUuid, child, ordinalCounter.getAndIncrement()));
		query("INSERT INTO functional_block_children (parent, child, ordinal) VALUES (?, ?, ?)" +
				" ON CONFLICT (parent, child) DO NOTHING")
			.updateBatch(batchArgs, 1_000);
	
	}

	private void deleteFunctionalBlockChildren(final UUID parentUuid) {
		query("DELETE FROM functional_block_children WHERE parent = ?").addArg(parentUuid).update();
	}

	public void createFunctionalBlockLinks(final UUID parentUUId, final List<FunctionalBlockLink> links) {
		final Stream<Stream<Object>> batchArgs = links.stream()
				.map(link -> Stream.<Object>of(
						link.getUid() != null ? link.getUid() : UUID.randomUUID() ,
						parentUUId,
						link.getChildA(),
						link.getChildB(),
						Optional.ofNullable(link.getCondition()).map(FunctionalBlockLinkCondition::getUid).orElse(null),
						link.getConditionLabel(),
						PgJSON.toPGobject(link.getFlags())));
		query("INSERT INTO functional_block_link (uid, parent, child_a, child_b, condition, condition_label, flags) VALUES (?, ?, ?, ?, ?, ?, ?)" +
				" ON CONFLICT DO NOTHING")
			.updateBatch(batchArgs, 1_000);

	}

	public void createFunctionalBlockCondition(final FunctionalBlockLinkCondition functionalBlockLinkCondition) {
	  	query("INSERT INTO functional_block_link_condition (uid, label) VALUES (?, ?) ON CONFLICT (uid) DO NOTHING")
				.addArg(functionalBlockLinkCondition.getUid())
				.addArg(functionalBlockLinkCondition.getLabel())
				.update();
	}

	private void createModulePartsForFunctionalBlock(final UUID parentUuid, final Collection<ModulePart> moduleParts) {
		query("INSERT INTO functional_block_module_part (functional_block, module_link_hash, location)" +
				" VALUES (?, ?, jsonb_populate_record(null::module_location, ?))" +
				" ON CONFLICT DO NOTHING")
			.updateBatch(moduleParts.stream()
				.map(part -> Stream.<Object>of(parentUuid, part.getModuleLinkHash(), part.getLocation().map(PgJSON::toPGobject).orElse(null))), 1_000);
	}
	
	/**
	 * Fetches the referenced data dictionary ids for the given functional block.
	 *
	 * @param blockUid the uid of the functional block
	 * @return the list of referenced data dictionary ids for the given functional block
	 */
	public List<UUID> getReferencedDataDictionaries(final UUID blockUid) {
		return query("SELECT data_dictionary FROM functional_block_referenced_data_dictionary WHERE functional_block = ?")
			.addArg(blockUid)
			.toList((rs, row) -> (UUID) rs.getObject(1));
	}

	/**
	 * Stores the given data dictionary ids as referenced data dictionaries for the given functional block.
	 *
	 * @param blockUid the uid of the functional block
	 * @param dataDictionaryIds the list of data dictionary ids
	 */
	public void setReferencedDataDictionaries(final UUID blockUid, final Collection<UUID> dataDictionaryIds) {
		query("INSERT INTO functional_block_referenced_data_dictionary (functional_block, data_dictionary)" +
			" VALUES (?, ?)" +
			" ON CONFLICT DO NOTHING")
			.updateBatch(dataDictionaryIds.stream()
				.map(dataDictionaryId -> Stream.<Object>of(blockUid, dataDictionaryId)), 1_000);
	}

	public int deleteLinks(final BuildingConsumer<FunctionalBlockLinkInquiryBuilder> builder) {
		return builder.prepare(new FunctionalBlockLinkQueryBuilder())
				.delete()
				.update();
	}

	/**
	 * Gets the map of annotations and their associated functional units that have the provided annotation type and a single parent.
	 *
	 * @param projectId the project Id
	 * @param blockUid the uid of the root functional block
	 * @param  annotationType the type of the annotation
	 * @return the map of annotationId and associated uids of the functional blocks
	 */
	public Map<EntityId, UUID> getSingleParentFunctionalUnitsByAnnotationType(final EntityId projectId, final UUID blockUid,
			final String annotationType) {
		return query("SELECT fb.uid, a.nid "
				+ "FROM functional_block fb "
				+ "INNER JOIN functional_block_generated_from gf ON fb.uid = gf.functional_block "
				+ "INNER JOIN annotation a ON gf.annotation_nid = a.nid "
				+ "INNER JOIN functional_block_children fbc ON fbc.child = fb.uid "
				+ "WHERE fb.project = (SELECT uid FROM project where nid = ?) and a.type = ? and fbc.parent= ? "
				+ "AND NOT EXISTS ( "
				+ "  SELECT 1 "
				+ "  FROM functional_block_children fbc2 "
				+ "  INNER JOIN functional_block fb2 ON fbc2.parent = fb2.uid "
				+ "  WHERE fbc2.child = fb.uid "
				+ "  AND fbc2.parent <> ? "
				+ "  AND fb2.flags -> '" + FunctionalBlockFlag.TYPE.name() + "' ?? '" + FunctionalBlockType.FUNCTIONAL_GROUP + "'" + ")"
				)
				.addArg(projectId.getNid())
				.addArg(annotationType)
				.addArg(blockUid)
				.addArg(blockUid)
				.toMap((rs, map) -> map.put(EntityId.of(rs.getLong(2)), (UUID) rs.getObject(1)));
	}

	/**
	 * Finds all peer functional blocks that matches the given Search criteria.
	 * <p>Performance optimized version of {@link #findPeers(BuildingConsumer, BuildingConsumer)} for finding peers of a specific type.</p>
	 *
	 * @param blockFilter the filter for the block
	 * @param peerFilter the type of the peer
	 * @return the list of matching peers
	 */
	public List<Pair<UUID, UUID>> findPeers(final BuildingConsumer<FunctionalBlockInquiryBuilder> blockFilter,
			final BuildingConsumer<FunctionalBlockInquiryBuilder> peerFilter) {
		final FilterStreamBuilder filters = new FilterStreamBuilder();
		final FunctionalBlockQueryBuilder blockBuilder = blockFilter.prepare(new FunctionalBlockQueryBuilder(SELECT_FB_FOR_FILTERING));
		final QueryBuilder blockQuery = blockBuilder.buildSelectFrom();
		blockQuery.with(blockBuilder.filters::build);
		filters.accept(q -> q.append("fb.uid" + " IN (" + blockQuery + ")", blockQuery.getArgs()));
		final FunctionalBlockQueryBuilder peerBuilder = peerFilter.prepare(new FunctionalBlockQueryBuilder(SELECT_FB_FOR_FILTERING));
		final QueryBuilder peerQuery = peerBuilder.buildSelectFrom();
		peerQuery.with(peerBuilder.filters::build);
		filters.accept(q -> q.append("peer_fb.uid" + " IN (" + peerQuery + ")", peerQuery.getArgs()));
		return query(SELECT_PEERS_FOR_FIND_PEERS)
				.with(filters::build)
				.append(SELECT_PEERS_CONDITION)
				.toList((rs, row) -> Pair.of((UUID) rs.getObject(1), (UUID) rs.getObject(2)));
	}

	/**
	 * Finds all peer functional blocks that matches the given Search criteria.
	 * <p>Performance optimized version of {@link #findPeers(BuildingConsumer, BuildingConsumer)} for finding peers of a specific type.</p>
	 *
	 * @param blockFilter the filter for the block
	 * @param peerType the type of the peer
	 * @return the list of matching peers
	 */
	public List<Pair<UUID, UUID>> findPeers(final BuildingConsumer<FunctionalBlockInquiryBuilder> blockFilter,
			final FunctionalBlockType peerType) {
		final FilterStreamBuilder filters = new FilterStreamBuilder();
		final FunctionalBlockQueryBuilder blockBuilder = blockFilter.prepare(new FunctionalBlockQueryBuilder(SELECT_FB_FOR_FILTERING));
		final QueryBuilder blockQuery = blockBuilder.buildSelectFrom();
		blockQuery.with(blockBuilder.filters::build);
		filters.accept(q -> q.append("fb.uid" + " IN (" + blockQuery + ")", blockQuery.getArgs()));
		filters.accept(q -> q.append("peer_fb.flags -> 'TYPE' ?? ?", peerType.name()));
		return query(SELECT_PEERS_FOR_FIND_PEERS)
				.with(filters::build)
				.append(SELECT_PEERS_CONDITION)
				.toList((rs, row) -> Pair.of((UUID) rs.getObject(1), (UUID) rs.getObject(2)));
	}

	/**
	 * Updates the {@link FunctionalBlockFlag#COMPUTED_AT} flag for a collection of blocks.
	 * @param blockUids the UUIDs of the functional blocks to update
	 * @param computedAt the new timestamp to set for {@link FunctionalBlockFlag#COMPUTED_AT}
	 */
	public void setComputedAt(final Collection<UUID> blockUids, final Instant computedAt) {
		final Map<String, Object> flags = Map.of(FunctionalBlockFlag.COMPUTED_AT.name(), computedAt.toEpochMilli());
		query("UPDATE functional_block SET flags = jsonb_merge(flags, ?) WHERE uid = any(?)")
				.addArg(PgJSON.toPGobject(flags))
				.addArg(PgType.UUID, blockUids)
				.update();
	}

	private void deleteModulePartsForFunctionalBlock(final UUID parentUuid) {
		query("DELETE FROM functional_block_module_part WHERE functional_block = ?").addArg(parentUuid).update();
	}

	private void deleteResolvedModulePartsForFunctionalBlock(final UUID parentUuid) {
		query("DELETE FROM functional_block_resolved_module_part WHERE functional_block = ?").addArg(parentUuid).update();
	}

	private void deleteReachabilityDataForFunctionalBlock(final UUID uuid) {
		query("DELETE FROM functional_block_reachability_data WHERE functional_block = ?").addArg(uuid).update();
	}

	private void deleteChildrenDeepForFunctionalBlock(final UUID parent) {
		query("DELETE FROM functional_block_children_deep WHERE parent = ?").addArg(parent).update();
	}
	
	private UUID insertOrUpdate(final FunctionalBlockPojoPrototype functionalBlockPojoPrototype, final boolean isUpdate) {
		final UUID id = functionalBlockPojoPrototype.uid.required(isUpdate).orElseNonNull(UUID::randomUUID);

		final FieldBuilder fields = new FieldBuilder();
		if ( ! isUpdate) {
			/* uid and project can not be changed after block is created */
			fields.add("uid", "?", id);
			fields.add(functionalBlockPojoPrototype.project, "project", ProjectPgDao::referenceUidOrNid);
		}
		fields.add(functionalBlockPojoPrototype.name, "name", "?");
		fields.add(functionalBlockPojoPrototype.description, "description", "?");
		fields.add(functionalBlockPojoPrototype.flags, "flags", flags -> qb ->
				qb.append(isUpdate ? "jsonb_strip_nulls(flags || ?)" : "?", PgJSON.toPGobject(flags)));

		final QueryBuilder q;
		final int updatedFields;
		if (isUpdate) {
			q = query("UPDATE functional_block SET ");
			updatedFields = fields.buildUpdate(q);
			q.append(" WHERE uid = ?", id);
		} else {
			q = query("INSERT INTO functional_block ");
			updatedFields = fields.buildInsert(q);
		}

		if(updatedFields > 0) {
			q.updateOrThrow(() -> new MiningEntityNotFoundException(FunctionalBlockPojo.class, id.toString()));
		}

		if (functionalBlockPojoPrototype.moduleParts.isDefined()) {
			if (isUpdate) {
				deleteModulePartsForFunctionalBlock(id);
			}
			createModulePartsForFunctionalBlock(id, functionalBlockPojoPrototype.moduleParts.getNonNull());
		}

		if (functionalBlockPojoPrototype.children.isDefined()) {
			if (isUpdate) {
				deleteFunctionalBlockChildren(id);
			}
			createFunctionalBlockChildren(id, functionalBlockPojoPrototype.children.getNonNull());
		}

		return id;
	}
}

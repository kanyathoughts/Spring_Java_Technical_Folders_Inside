/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.graphql.controller;

import graphql.GraphQLContext;
import graphql.execution.DataFetcherResult;
import graphql.language.Field;
import graphql.schema.DataFetchingEnvironment;
import graphql.schema.DataFetchingFieldSelectionSet;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.datapoints.FilterObjectService;
import innowake.mining.data.datapoints.SortObjectService;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.graphql.GraphQLAggregationRequest;
import innowake.mining.server.graphql.MiningGraphQLQueries;
import innowake.mining.server.graphql.MiningQueryMapping;
import innowake.mining.shared.access.*;
import innowake.mining.shared.access.FunctionalBlockService.FunctionalBlockInquiryBuilder;
import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.annotations.MiningDataPointIgnore;
import innowake.mining.shared.datapoints.annotations.Usage;
import innowake.mining.shared.datapoints.annotations.UsageAttribute;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.SortByAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.miningui.TableAttributes;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.SqlDetailsPojo;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFieldName;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLink;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockStatus;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.GeneratedFrom;
import innowake.mining.shared.entities.functionalblocks.ModulePart;
import innowake.mining.shared.entities.functionalblocks.ReachabilityDataPojo;
import innowake.mining.shared.entities.functionalblocks.ResolvedModulePart;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;
import innowake.mining.shared.service.UserRoleService;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.dataloader.BatchLoaderEnvironment;
import org.dataloader.DataLoader;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.BatchMapping;
import org.springframework.graphql.data.method.annotation.SchemaMapping;
import org.springframework.graphql.execution.BatchLoaderRegistry;
import org.springframework.stereotype.Controller;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static innowake.mining.server.graphql.GraphQLAggregationRequest.toAggregationRequest;
import static innowake.mining.server.graphql.controller.FunctionalBlocksDataPointSource.FUNCTIONAL_BLOCK_LINK_TYPE_NAME;
import static innowake.mining.server.graphql.controller.FunctionalBlocksDataPointSource.FUNCTIONAL_BLOCK_TYPE_NAME;
import static innowake.mining.shared.entities.functionalblocks.FunctionalBlockFieldName.REFERENCED_MODULE_TECHNOLOGY;
import static innowake.mining.shared.entities.functionalblocks.FunctionalBlockFieldName.REFERENCED_MODULE_TYPE;
import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.VIEWER;

/**
 * GraphQL controller for retrieving Functional Blocks and associated data.
 */
@Controller
public class FunctionalBlocksGraphQlController {

	public static final String QUERY_NAME = "functionalBlocks";
	/* id of queried project is put into the GraphQLContext */
	public static final String PROJECT_ID_CONTEXT_KEY = "projectId";
	private static final String FB_NAME = "content_deepName";
	private static final String TAXONOMY_ID = "content_resolvedModuleParts_referencedTaxonomies_id";
	private static final String MODULE_ID = "content_resolvedModuleParts_moduleId";
	private static final String PEERS = "content_peers";

	private static final String DD_IDS = "content_referencedDataDictionaries";
	private static final String IN = "in";

	private final FunctionalBlockService functionalBlockService;
	private final ProjectService projectService;
	private final ModuleService moduleService;
	private final SqlDetailsService sqlDetailsService;
	private final TaxonomyService taxonomyService;
	private final AnnotationService annotationService;
	private final DataDictionaryService dataDictionaryService;
	private final FilterObjectService filterObjectService;
	private final SortObjectService sortObjectService;
	private final UserRoleService userRoleService;
	@Value("${dbcutter-db-postgres.enabled}")
	private boolean dbcutterDbPostgresEnabled;

	/**
	 * Base class for information that is passed to data loader when fetching data for a "parent" functional block.
	 */
	protected abstract static class BatchLoadInformation {

		final Field field;
		final Long projectId;
		final UUID parent;
		@Nullable
		final Pagination pagination;
		@Nullable
		final Map<String, Object> filterObject;

		protected BatchLoadInformation(final Field field, final Long projectId, final UUID parent, @Nullable final Pagination pagination,
				@Nullable final Map<String, Object> filterObject) {
			this.field = field;
			this.projectId = projectId;
			this.parent = parent;
			this.pagination = pagination;
			this.filterObject = filterObject;
		}

		@Override
		public boolean equals(@Nullable final Object o) {
			if (this == o) {
				return true;
			}
			if ( ! (o instanceof BatchLoadInformation)) {
				return false;
			}
			final BatchLoadInformation that = (BatchLoadInformation) o;
			return Objects.equals(field, that.field) && Objects.equals(parent, that.parent);
		}

		@Override
		public int hashCode() {
			return Objects.hash(field, parent);
		}
	}

	/**
	 * Batch loading information for "children". Class is public only because it is part of the method signature of
	 * the public controller method "children".
	 */
	public static class ChildrenBatchLoadInformation extends BatchLoadInformation {

		final List<UUID> children;

		public ChildrenBatchLoadInformation(final Field field, final Long projectId, final UUID parent, final List<UUID> children,
				@Nullable final Pagination pagination, @Nullable final Map<String, Object> filterObject) {
			super(field, projectId, parent, pagination, filterObject);
			this.children = children;
		}
	}

	/* decorator class, required for batch loader registration */
	public static class ChildrenResult {
		final Paged<FunctionalBlockPojo> result;

		public ChildrenResult(final Paged<FunctionalBlockPojo> result) {
			this.result = result;
		}
	}

	/**
	 * Batch loading information for "childrenDeep". Class is public only because it is part of the method signature of
	 * the public controller method "childrenDeep".
	 */
	public static class ChildrenDeepBatchLoadInformation extends BatchLoadInformation {

		final int maxDepth;

		public ChildrenDeepBatchLoadInformation(final Field field, final Long projectId, final UUID parent, @Nullable final Pagination pagination,
				@Nullable final Map<String, Object> filterObject, final int maxDepth) {
			super(field, projectId, parent, pagination, filterObject);
			this.maxDepth = maxDepth;
		}
	}

	/* decorator class, required for batch loader registration */
	public static class ChildrenDeepResult {
		final Paged<FunctionalBlockPojo> result;

		public ChildrenDeepResult(final Paged<FunctionalBlockPojo> result) {
			this.result = result;
		}
	}

	/**
	 * Batch loading information for "peers". Class is public only because it is part of the method signature of
	 * the public controller method "peers".
	 */
	public static class PeersBatchLoadInformation extends BatchLoadInformation {

		private final FunctionalBlockType peerType;

		public PeersBatchLoadInformation(final Field field, final Long projectId, final UUID parent,
				@Nullable final Pagination pagination, @Nullable final Map<String, Object> filterObject,
				@Nullable final FunctionalBlockType peerType) {
			super(field, projectId, parent, pagination, filterObject);
			this.peerType = peerType;
		}
	}

	/* decorator class, required for batch loader registration */
	public static class PeersResult {
		final Paged<FunctionalBlockPojo> result;
		public PeersResult(final Paged<FunctionalBlockPojo> result) {
			this.result = result;
		}
	}

	public FunctionalBlocksGraphQlController(final FunctionalBlockService functionalBlockService,
											 final ProjectService projectService,
											 final ModuleService moduleService,
											 final SqlDetailsService sqlDetailsService,
											 final TaxonomyService taxonomyService,
											 final AnnotationService annotationService,
											 final DataDictionaryService dataDictionaryService,
											 final FilterObjectService filterObjectService,
											 final SortObjectService sortObjectService,
											 final UserRoleService userRoleService,
											 final BatchLoaderRegistry registry) {
		
		this.functionalBlockService = functionalBlockService;
		this.projectService = projectService;
		this.moduleService = moduleService;
		this.sqlDetailsService = sqlDetailsService;
		this.taxonomyService = taxonomyService;
		this.annotationService = annotationService;
		this.dataDictionaryService = dataDictionaryService;
		this.filterObjectService = filterObjectService;
		this.sortObjectService = sortObjectService;
		this.userRoleService = userRoleService;

		/* register batch loaders used by "children", "childrenDeep" and "peers" */
		registry.forTypePair(ChildrenBatchLoadInformation.class, ChildrenResult.class).registerMappedBatchLoader(this::loadChildrenBatched);
		registry.forTypePair(ChildrenDeepBatchLoadInformation.class, ChildrenDeepResult.class).registerMappedBatchLoader(this::loadChildrenDeepBatched);
		registry.forTypePair(PeersBatchLoadInformation.class, PeersResult.class).registerMappedBatchLoader(this::loadPeersBatched);
	}

	@SuppressWarnings("unchecked")
	@MiningQueryMapping
	@Nature({MINING})
	@Role({VIEWER})
	public DataFetcherResult<Paged<FunctionalBlockPojo>> functionalBlocks(@Argument final Long projectId,
			@Argument @Nullable final Integer page,
			@Argument @Nullable final Integer size,
			@Argument ("sortObject") @Nullable final List<Map<String, String>> sortObject,
			@Argument("filterObject") @Nullable final Map<String, Object> filterObject,
			final DataFetchingEnvironment env,
			final GraphQLContext graphQLContext) {

		graphQLContext.put(PROJECT_ID_CONTEXT_KEY, projectId);

		final Pagination pagination = getPagination(page, size);
		final BuildingConsumer<FunctionalBlockInquiryBuilder> buildingConsumer = q -> {
			q.ofProject(EntityId.of(projectId));
			if (filterObject != null && ! filterObject.isEmpty()) {
				filterObjectService.applyFilterObject(projectId, QUERY_NAME, filterObject, q);
			}
			if (sortObject != null && ! sortObject.isEmpty()) {
				sortObjectService.applySortObject(projectId, MiningGraphQLQueries.FUNCTIONAL_BLOCKS, sortObject, q);
			}
			q.sortByFlagContainsValue(FunctionalBlockFlag.TYPE, FunctionalBlockType.MERGE_PARENT, SortDirection.DESCENDING);
		};
		final Paged<FunctionalBlockPojo> result;

		if (filterObject != null && filterObject.containsKey(FB_NAME) && ((Map<String, List<String>>)filterObject.get(FB_NAME)).get(IN) != null
				&& ! ((Map<String, List<String>>)filterObject.get(FB_NAME)).get(IN).isEmpty()) {
			/* Special handling for Search by name */
			result = getSearchResult(projectId, filterObject, pagination);

		} else if (isSelectingOnlyAggregations(env.getSelectionSet())) {
			/* if only aggregations are selected, we don't need to execute the actual query */
			result = Paged.empty();
		} else {
			result = functionalBlockService.find(pagination, buildingConsumer);
		}

		return DataFetcherResult.<Paged<FunctionalBlockPojo>>newResult()
				.data(result)
				.localContext(getLocalContext(projectId).withLocalContext(new BuildingConsumerHolder(buildingConsumer)))
				.build();
	}
	
	@SuppressWarnings({
			"unchecked", "cast"
	})
	private Paged<FunctionalBlockPojo> getSearchResult(final Long projectId, final Map<String, Object> filterObject, final Pagination pagination) {
		final List<EntityId> taxIds = filterObject.containsKey(TAXONOMY_ID)
				? ((Map<String, List<EntityId>>) filterObject.get(TAXONOMY_ID)).values().iterator().next()
				: new ArrayList<>();
		final List<Long> nids = taxIds.stream().map(EntityId::getNid).toList();
		final List<EntityId> moduleIds = filterObject.containsKey(MODULE_ID)
				? ((Map<String, List<EntityId>>) filterObject.get(MODULE_ID)).values().iterator().next()
				: new ArrayList<>();
		final List<Long> moduleNids = moduleIds.stream().map(EntityId::getNid).toList();
		final List<UUID> peers = new ArrayList<>();
		if (filterObject.containsKey(PEERS)) {
			final Map<String, Map<String, Map<String, List<UUID>>>> peersMap = (Map<String, Map<String, Map<String, List<UUID>>>>) filterObject.get(PEERS);
			final Map<String, Map<String, List<UUID>>> eqMap = (Map<String, Map<String, List<UUID>>>) peersMap.values().iterator().next();
			final Map<String, List<UUID>> contentMap = (Map<String, List<UUID>>) eqMap.values().iterator().next();
			peers.addAll(contentMap.values().iterator().next());
		}
		final List<UUID> ddIds = filterObject.containsKey(DD_IDS)
				? ((Map<UUID, List<UUID>>) filterObject.get(DD_IDS)).values().iterator().next()
				: new ArrayList<>();

		final Map<String, List<String>> nameMap = (Map<String, List<String>>) filterObject.get(FB_NAME);
		final List<String> searchNames = nameMap.get(IN);
		return functionalBlockService.findByName(pagination, projectId, moduleNids, nids, peers, searchNames, ddIds);
	}

	/**
	 * Query for a single "root" functional block. Note that the query can actually contain any number of blocks, if you query for the
	 * "parents" or "children" fields.
	 * @param projectId the id of the project
	 * @param uid the UUID of the root block to query
	 * @param graphQLContext context object, used to hold the project id
	 * @return the root functional block
	 */
	@MiningQueryMapping
	@Nature({MINING})
	@Role({VIEWER})
	@Nullable
	public DataFetcherResult<FunctionalBlockPojo> functionalBlock(@Argument final Long projectId, @Argument final UUID uid, final GraphQLContext graphQLContext) {
		final BuildingConsumer<FunctionalBlockInquiryBuilder> buildingConsumer = q -> q.byUid(uid);
		final FunctionalBlockPojo functionalBlock = functionalBlockService.find(uid).orElse(null);
		if (functionalBlock != null) {
			graphQLContext.put(PROJECT_ID_CONTEXT_KEY, functionalBlock.getProject().getNid());
		}

		return DataFetcherResult.<FunctionalBlockPojo>newResult()
				.data(functionalBlock)
				.localContext(functionalBlock == null ? null : getLocalContext(functionalBlock.getProject().getNid()).withLocalContext(new BuildingConsumerHolder(buildingConsumer)))
				.build();
	}

	/**
	 * Query for {@linkplain ReachabilityDataPojo reachabilityData} of a project.
	 *
	 * @param projectId the ID of the project that contains the reachabilityData
	 * @param functionalBlocks the reachabilityData for the functional Block
	 * @param page the page number
	 * @param size the number of elements
	 * @param sortObject sort conditions in object format
	 * @param includeIntermediateModulesPerAccessModule whether to include intermediate modules per access module
	 * @param filterObject filtering conditions in object format
	 * @param graphQLContext context object, used to hold the project id
	 * @return the Page of reachabilityData
	 */
	@MiningQueryMapping
	@Nature({MINING})
	@Role({VIEWER})
	public Paged<ReachabilityDataPojo> reachabilityData(@Argument final Long projectId,
			@Argument final List<UUID> functionalBlocks,
			@Argument @Nullable final Integer page,
			@Argument @Nullable final Integer size,
			@Argument @Nullable final Boolean includeIntermediateModulesPerAccessModule,
			@Argument @Nullable final List<Map<String, String>> sortObject,
			@Argument("filterObject") @Nullable final Map<String, Object> filterObject,
			final GraphQLContext graphQLContext) {

		graphQLContext.put(PROJECT_ID_CONTEXT_KEY, projectId);
		final Pagination pagination = getPagination(page, size);

		final Boolean includeIntermediateModulesPerAccessModuleValue = Optional.ofNullable(includeIntermediateModulesPerAccessModule).orElse(Boolean.FALSE);

		return functionalBlockService.findReachabilityData(pagination, q -> {
			q.aggregateAccessModulesPerLowerBound( ! includeIntermediateModulesPerAccessModuleValue)
					.ofFunctionalBlocks(functionalBlocks);
			if (filterObject != null && ! filterObject.isEmpty()) {
				filterObjectService.applyFilterObject(projectId, MiningGraphQLQueries.REACHABILITY_DATA, filterObject, q);
			}
			if (sortObject != null && ! sortObject.isEmpty()) {
				sortObjectService.applySortObject(projectId, MiningGraphQLQueries.REACHABILITY_DATA, sortObject, q);
			}
		});
	}

	@BatchMapping(typeName = FUNCTIONAL_BLOCK_TYPE_NAME)
	public Map<FunctionalBlockPojo, ProjectPojo> project(final List<FunctionalBlockPojo> functionalBlocks) {
		if (functionalBlocks.isEmpty()) {
			return Collections.emptyMap();
		}

		/* we assume that all functional blocks belong to the same project */
		final ProjectPojo project = projectService.get(functionalBlocks.get(0).getProject());
		return functionalBlocks.stream().collect(Collectors.toMap(Function.identity(), fb -> project));
	}

	@SchemaMapping(typeName = FUNCTIONAL_BLOCK_TYPE_NAME)
	public UUID uid(final FunctionalBlockPojo functionalBlock) {
		return functionalBlock.getUid();
	}

	@SchemaMapping(typeName = FUNCTIONAL_BLOCK_TYPE_NAME)
	public List<ModulePart> moduleParts(final FunctionalBlockPojo functionalBlock) {
		return functionalBlock.getModuleParts();
	}

	@BatchMapping(typeName = FUNCTIONAL_BLOCK_TYPE_NAME)
	public Map<FunctionalBlockPojo, List<ResolvedModulePart>> resolvedModuleParts(final List<FunctionalBlockPojo> functionalBlocks) {
		final Map<UUID, List<ResolvedModulePart>> resolvedModuleParts = functionalBlockService.getResolvedModuleParts(
				functionalBlocks.stream().map(FunctionalBlockPojo::getUid).toList());

		/* can not use Collectors.toMap() because it doesn't support null values (JDK bug) :-( */
		final Map<FunctionalBlockPojo, List<ResolvedModulePart>> ret = new HashMap<>();
		functionalBlocks.forEach(fb -> ret.put(fb, resolvedModuleParts.get(fb.getUid())));

		return ret;
	}

	@BatchMapping(typeName = FUNCTIONAL_BLOCK_TYPE_NAME)
	public Map<FunctionalBlockPojo, List<FunctionalBlockLink>> links(final List<FunctionalBlockPojo> functionalBlocks) {
		final Map<UUID, List<FunctionalBlockLink>> linkMap =
				functionalBlockService.getLinks(functionalBlocks.stream().map(FunctionalBlockPojo::getUid).toList());

		final Map<FunctionalBlockPojo, List<FunctionalBlockLink>> ret = new HashMap<>();
		for (final FunctionalBlockPojo fb : functionalBlocks) {
			ret.put(fb, linkMap.get(fb.getUid()));
		}
		return ret;
	}

	@SchemaMapping(typeName = FUNCTIONAL_BLOCK_LINK_TYPE_NAME)
	public FunctionalBlockPojo childA(final FunctionalBlockLink link) {
		return functionalBlockService.find(link.getChildA()).orElse(null);
	}

	@SchemaMapping(typeName = FUNCTIONAL_BLOCK_LINK_TYPE_NAME)
	public FunctionalBlockPojo childB(final FunctionalBlockLink link) {
		return functionalBlockService.find(link.getChildB()).orElse(null);
	}

	@BatchMapping(typeName = "ResolvedModulePart", field = "module")
	public Map<ResolvedModulePart, ModulePojo> resolvedModulePartModule(final List<ResolvedModulePart> resolvedModuleParts) {
		final List<EntityId> resolvedModuleIds = getResolvedModuleIds(resolvedModuleParts);
		final Map<Long, ModulePojo> modules = getModulePojosById(resolvedModuleIds);

		/* map each ResolvedModulePart to the correct ModulePojo */
		final Map<ResolvedModulePart, ModulePojo> ret = new HashMap<>(modules.size());
		for (final ResolvedModulePart modulePart : resolvedModuleParts) {
			ret.put(modulePart, modules.get(modulePart.getModuleId().getNid()));
		}

		return ret;
	}

	private List<EntityId> getResolvedModuleIds(final List<ResolvedModulePart> resolvedModuleParts) {
		return resolvedModuleParts.stream()
				.map(ResolvedModulePart::getModuleId)
				.toList();
	}

	private Map<Long, ModulePojo> getModulePojosById(final List<EntityId> moduleIds) {
		return moduleService.findModules(q -> q.byIds(moduleIds)
						.includeContent(true))
				.stream()
				.collect(Collectors.toMap(ModulePojo::getId, Function.identity()));
	}

	@BatchMapping(typeName = "ResolvedModulePart")
	public Map<ResolvedModulePart, List<TaxonomyPojo>> referencedTaxonomies(final List<ResolvedModulePart> resolvedModuleParts, final GraphQLContext graphQLContext) {
		/* get Taxonomies for each Module */
		final List<EntityId> resolvedModuleIds = getResolvedModuleIds(resolvedModuleParts);
		final Long projectId = graphQLContext.get(PROJECT_ID_CONTEXT_KEY);
		final Map<EntityId, List<TaxonomyPojo>> taxonomies =
				taxonomyService.findTaxonomiesPerModule(q -> q.ofProject(EntityId.of(projectId)).ofModules(resolvedModuleIds));

		/* map each ResolvedModulePart to the correct list of TaxonomyPojos */
		final Map<ResolvedModulePart, List<TaxonomyPojo>> ret = new HashMap<>(resolvedModuleParts.size());

		for (final ResolvedModulePart resolvedModulePart : resolvedModuleParts) {
			final List<TaxonomyPojo> taxonomiesForPart = taxonomies.get(resolvedModulePart.getModuleId());
			if (taxonomiesForPart != null) {
				ret.put(resolvedModulePart, taxonomiesForPart);
			}
		}

		return ret;
	}

	@SchemaMapping(typeName = FunctionalBlocksDataPointSource.FUNCTIONAL_BLOCK_TYPE_NAME)
	public DataFetcherResult<Paged<FunctionalBlockPojo>> parents(final FunctionalBlockPojo functionalBlock,
			@Argument @Nullable final Integer page,
			@Argument @Nullable final Integer size,
			/* you can use the same filters here as on the "functionalBlocks" query, hence specifying the "referenceTypeName */
			@MiningDataPoint(referenceTypeName = "FilterObject_functionalBlocks") @Argument("filterObject") @Nullable final Map<String, Object> filterObject,
			final DataFetchingEnvironment env) {

		final Pagination pagination = getPagination(page, size);
		final BuildingConsumer<FunctionalBlockInquiryBuilder> buildingConsumer = q-> {
			q.byUids(functionalBlock.getParents());
			if (filterObject != null) {
				filterObjectService.applyFilterObject(functionalBlock.getProject().getNid(), QUERY_NAME, filterObject, q);
			}
		};

		final Paged<FunctionalBlockPojo> result;
		if (isSelectingOnlyAggregations(env.getSelectionSet())) {
			/* if only aggregations are selected, we don't need to execute the actual query */
			result = Paged.empty();
		} else {
			result = functionalBlockService.find(pagination, buildingConsumer);
		}

		return DataFetcherResult.<Paged<FunctionalBlockPojo>>newResult()
				.data(result)
				.localContext(env.<ControllerLocalContext>getLocalContext().withLocalContext(new BuildingConsumerHolder(buildingConsumer)))
				.build();
	}

	@SchemaMapping(typeName = FUNCTIONAL_BLOCK_TYPE_NAME)
	public Mono<DataFetcherResult<Paged<FunctionalBlockPojo>>> children(final FunctionalBlockPojo functionalBlock,
			final DataLoader<ChildrenBatchLoadInformation, ChildrenResult> loader,
			final DataFetchingEnvironment env,
			@Argument@Nullable final Integer page,
			@Argument @Nullable final Integer size,
			/* you can use the same filters here as on the "functionalBlocks" query, hence specifying the "referenceTypeName */
			@MiningDataPoint(referenceTypeName = "FilterObject_functionalBlocks") @Argument("filterObject") @Nullable final Map<String, Object> filterObject) {

		final Pagination pagination;
		if (size == null) {
			pagination = null;
		} else {
			pagination = getPagination(page, size);
		}

		final Mono<Paged<FunctionalBlockPojo>> result;
		if (isSelectingOnlyAggregations(env.getSelectionSet())) {
			/* if only aggregations are selected, we don't need to execute the actual query */
			result = Mono.just(Paged.empty());
		} else {
			/* record ChildrenBatchLoadInformation - GraphQL collects these in a list and then calls loadChildrenBatched() */
			final ChildrenBatchLoadInformation childrenBatchLoadInformation = new ChildrenBatchLoadInformation(
					env.getField(),
					functionalBlock.getProject().getNid(),
					functionalBlock.getUid(),
					functionalBlock.getChildren(),
					pagination,
					filterObject);
			result = Mono.fromFuture(loader.load(childrenBatchLoadInformation))
					.map(r -> r.result); /* unwrap the ChildrenResult */
		}

		final BuildingConsumer<FunctionalBlockInquiryBuilder> buildingConsumer = q -> {
			q.byUids(functionalBlock.getChildren());
			q.sortChildOrdinal(functionalBlock.getUid());
			if (filterObject != null) {
				filterObjectService.applyFilterObject(functionalBlock.getProject().getNid(), QUERY_NAME, filterObject, q);
			}
		};

		return result.map(r -> DataFetcherResult.<Paged<FunctionalBlockPojo>>newResult()
				.data(r)
				.localContext(env.<ControllerLocalContext>getLocalContext().withLocalContext(new BuildingConsumerHolder(buildingConsumer)))
				.build());
	}

	@SuppressWarnings("unused")
	private Mono<Map<ChildrenBatchLoadInformation, ChildrenResult>> loadChildrenBatched(final Set<ChildrenBatchLoadInformation> batchInfo,
			final BatchLoaderEnvironment env) {

		/* if there are multiple "children" fields in the GraphQL request at the same level, each may have different parameters passed to it,
		 * but GraphQL execution will still batch all of them together.
		 * We manually group by field here and then do separate batch loads for each field. */
		final var grouped = batchInfo.stream().collect(Collectors.groupingBy(info -> info.field));

		Mono<Map<ChildrenBatchLoadInformation, ChildrenResult>> ret = Mono.just(new HashMap<>());
		for (final List<ChildrenBatchLoadInformation> batchInfoGroup : grouped.values()) {
			/* the Pagination and FilterObject parameters will be the same for each field, so we can sample one batchInfo to get these parameters */
			final ChildrenBatchLoadInformation sample = batchInfoGroup.get(0);

			final Long projectId = sample.projectId;
			final Map<String, Object> filterObject = sample.filterObject;
			final Pagination pagination = sample.pagination;

			/* when pagination is used, we must currently use separate requests for each parent. This is inefficient, but otherwise we can not limit
			 * the number of children per parent. */
			ret = ret.map(map -> {
				if (pagination == null) {
					map.putAll(fetchChildrenUnpaged(projectId, batchInfoGroup, filterObject));
				} else {
					map.putAll(fetchChildrenPaged(projectId, batchInfoGroup, filterObject, pagination));
				}
				return map;
			});
		}
		return ret;
	}

	private Map<ChildrenBatchLoadInformation, ChildrenResult> fetchChildrenUnpaged(final Long projectId,
			final List<ChildrenBatchLoadInformation> batchInfo, @Nullable final Map<String, Object> filterObject) {

		final Map<ChildrenBatchLoadInformation, ChildrenResult> ret = new HashMap<>();
		final Map<UUID, ChildrenBatchLoadInformation> parentMap = new HashMap<>();
		for (final ChildrenBatchLoadInformation info :  batchInfo) {
			parentMap.put(info.parent, info);
		}

		/* make one "big" query fetching all children from all parents */
		final Set<UUID> childUids = batchInfo.stream().flatMap(i -> i.children.stream()).collect(Collectors.toSet());
		final Map<UUID, FunctionalBlockPojo> children = functionalBlockService.find(q -> {
			q.byUids(new ArrayList<>(childUids));
			if (filterObject != null) {
				filterObjectService.applyFilterObject(projectId, QUERY_NAME, filterObject, q);
			}
		}).stream().collect(Collectors.toMap(FunctionalBlockPojo::getUid, Function.identity()));

		/* now assign the children back to the correct parent(s) */
		for (final ChildrenBatchLoadInformation info : batchInfo) {
			ret.put(info, new ChildrenResult(Paged.ofContent(info.children.stream().map(children::get).filter(Objects::nonNull).toList())));
		}

		return ret;
	}

	private Map<ChildrenBatchLoadInformation, ChildrenResult> fetchChildrenPaged(final Long projectId,
			final List<ChildrenBatchLoadInformation> batchInfo, @Nullable final Map<String, Object> filterObject, final Pagination pagination) {

		final Map<ChildrenBatchLoadInformation, ChildrenResult> ret = new HashMap<>();

		/* make a separate request for each "ChildrenBatchLoadInformation" (i.e. for each parent) - this effectively disables batch loading,
		 * but it is currently the only way to apply the pagination attributes correctly, which must be done for each parent */
		for (final ChildrenBatchLoadInformation info : batchInfo) {
			ret.put(info, new ChildrenResult(functionalBlockService.find(pagination, q -> {
				q.byUids(info.children);
				q.sortChildOrdinal(info.parent);
				if (filterObject != null) {
					filterObjectService.applyFilterObject(projectId, QUERY_NAME, filterObject, q);
				}
			})));
		}
		return ret;
	}

	@SchemaMapping(typeName = FUNCTIONAL_BLOCK_TYPE_NAME)
	public Mono<DataFetcherResult<Paged<FunctionalBlockPojo>>> childrenDeep(final FunctionalBlockPojo functionalBlock,
			final DataLoader<ChildrenDeepBatchLoadInformation, ChildrenDeepResult> loader,
			final DataFetchingEnvironment env,
			@Argument @Nullable final Integer page,
			@Argument @Nullable final Integer size,
			/* you can use the same filters here as on the "functionalBlocks" query, hence specifying the "referenceTypeName */
			@MiningDataPoint(referenceTypeName = "FilterObject_functionalBlocks") @Argument("filterObject") @Nullable final Map<String, Object> filterObject,
			@Argument @Nullable final Integer maxDepth) {

		final Pagination pagination;
		if (size == null) {
			pagination = null;
		} else {
			pagination = getPagination(page, size);
		}

		/* record ChildrenDeepBatchLoadInformation - GraphQL collects these in a list and then calls loadChildrenDeepBatched() */
		final ChildrenDeepBatchLoadInformation batchLoadInformation = new ChildrenDeepBatchLoadInformation(
				env.getField(),
				functionalBlock.getProject().getNid(),
				functionalBlock.getUid(),
				pagination,
				filterObject,
				maxDepth == null ? -1 : maxDepth);

		final Mono<Paged<FunctionalBlockPojo>> result = Mono.fromFuture(loader.load(batchLoadInformation))
				.map(r -> r.result); /* unwrap the ChildrenDeepResult */

		return result.map(r -> {
			final BuildingConsumer<FunctionalBlockInquiryBuilder> buildingConsumer = q -> {
				final List<FunctionalBlockPojo> childrenDeep;
				if (pagination == null) {
					/* not using pagination: result contains full list of UUIDs for aggregation */
					childrenDeep = r.getContent();
				} else {
					/* using pagination: we must query childrenDeep again */
					childrenDeep = functionalBlockService.findChildrenDeep(functionalBlock.getUid(), maxDepth == null ? -1 : maxDepth, childrenFilter -> {
						if (filterObject != null) {
							filterObjectService.applyFilterObject(functionalBlock.getProject().getNid(), QUERY_NAME, filterObject, childrenFilter);
						}
					});
				}
				/* perform aggregation using list of UUIDs */
				q.byUids(childrenDeep.stream().map(FunctionalBlockPojo::getUid).toList());
			};

			return DataFetcherResult.<Paged<FunctionalBlockPojo>>newResult()
					.data(r)
					.localContext(env.<ControllerLocalContext>getLocalContext().withLocalContext(new BuildingConsumerHolder(buildingConsumer)))
					.build();
		});
	}

	@SuppressWarnings("unused")
	private Mono<Map<ChildrenDeepBatchLoadInformation, ChildrenDeepResult>> loadChildrenDeepBatched(final Set<ChildrenDeepBatchLoadInformation> batchInfo,
			final BatchLoaderEnvironment env) {

		/* if there are multiple "childrenDeep" fields in the GraphQL request at the same level, each may have different parameters passed to it,
		 * but GraphQL execution will still batch all of them together.
		 * We manually group by field here and then do separate batch loads for each field. */
		final var grouped = batchInfo.stream().collect(Collectors.groupingBy(info -> info.field));

		Mono<Map<ChildrenDeepBatchLoadInformation, ChildrenDeepResult>> ret = Mono.just(new HashMap<>());
		for (final List<ChildrenDeepBatchLoadInformation> batchInfoGroup : grouped.values()) {
			/* the Pagination and FilterObject parameters will be the same for each field, so we can sample one batchInfo to get these parameters */
			final ChildrenDeepBatchLoadInformation sample = batchInfoGroup.get(0);

			final Long projectId = sample.projectId;
			final Map<String, Object> filterObject = sample.filterObject;
			final Pagination pagination = sample.pagination;
			final int maxDepth = sample.maxDepth;

			/* when pagination is used, we must currently use separate requests for each parent. This is inefficient, but otherwise we can not limit
			 * the number of children per parent. */
			ret = ret.map(map -> {
				if (pagination == null) {
					map.putAll(fetchChildrenDeepUnpaged(projectId, batchInfoGroup, filterObject, maxDepth));
				} else {
					map.putAll(fetchChildrenDeepPaged(projectId, batchInfoGroup, filterObject, pagination, maxDepth));
				}
				return map;
			});

		}
		return ret;
	}

	private Map<ChildrenDeepBatchLoadInformation, ChildrenDeepResult> fetchChildrenDeepUnpaged(final Long projectId,
			final List<ChildrenDeepBatchLoadInformation> batchInfo, @Nullable final Map<String, Object> filterObject, final int maxDepth) {

		final Map<ChildrenDeepBatchLoadInformation, ChildrenDeepResult> ret = new HashMap<>();
		final Map<UUID, ChildrenDeepBatchLoadInformation> parentMap = new HashMap<>();
		for (final ChildrenDeepBatchLoadInformation info :  batchInfo) {
			parentMap.put(info.parent, info);
		}

		/* make one "big" query fetching all children from all parents */
		functionalBlockService.findChildrenDeep(new ArrayList<>(parentMap.keySet()), maxDepth, q -> {
			if (filterObject != null) {
				filterObjectService.applyFilterObject(projectId, QUERY_NAME, filterObject, q);
			}
		}).forEach((k, v) -> ret.put(parentMap.get(k), new ChildrenDeepResult(Paged.ofContent(v))));

		return ret;
	}

	private Map<ChildrenDeepBatchLoadInformation, ChildrenDeepResult> fetchChildrenDeepPaged(final Long projectId,
			final List<ChildrenDeepBatchLoadInformation> batchInfo, @Nullable final Map<String, Object> filterObject, final Pagination pagination, final int maxDepth) {

		final Map<ChildrenDeepBatchLoadInformation, ChildrenDeepResult> ret = new HashMap<>();

		/* make a separate request for each "ChildrenDeepBatchLoadInformation" (i.e. for each parent) - this effectively disables batch loading,
		 * but it is currently the only way to apply the pagination attributes correctly, which must be done for each parent */
		for (final ChildrenDeepBatchLoadInformation info : batchInfo) {
			ret.put(info, new ChildrenDeepResult(functionalBlockService.findChildrenDeep(info.parent, pagination, maxDepth, q -> {
				if (filterObject != null) {
					filterObjectService.applyFilterObject(projectId, QUERY_NAME, filterObject, q);
				}
			})));
		}
		return ret;
	}

	@SchemaMapping(typeName = FUNCTIONAL_BLOCK_TYPE_NAME)
	public Mono<Paged<FunctionalBlockPojo>> peers(final FunctionalBlockPojo functionalBlock,
			final DataLoader<PeersBatchLoadInformation, PeersResult> loader,
			final DataFetchingEnvironment env,
			@Argument @Nullable final Integer page,
			@Argument @Nullable final Integer size,
			/* you can use the same filters here as on the "functionalBlocks" query, hence specifying the "referenceTypeName */
			@MiningDataPoint(referenceTypeName = "FilterObject_functionalBlocks") @Argument("filterObject") @Nullable final Map<String, Object> filterObject,
			@Argument("peerType") @Nullable final FunctionalBlockType peerType) {

		final Pagination pagination;
		if (size == null) {
			pagination = null;
		} else {
			pagination = getPagination(page, size);
		}
		return Mono.fromFuture(loader.load(new PeersBatchLoadInformation(
						env.getField(),
						functionalBlock.getProject().getNid(),
						functionalBlock.getUid(),
						pagination,
						filterObject,
						peerType)))
				.map(r -> r.result);
	}

	@SchemaMapping(typeName = FunctionalBlocksDataPointSource.FUNCTIONAL_BLOCK_TYPE_NAME)
	public String name(final FunctionalBlockPojo functionalBlock) {
		return functionalBlock.getName();
	}

	@SchemaMapping(typeName = FUNCTIONAL_BLOCK_TYPE_NAME)
	public String description(final FunctionalBlockPojo functionalBlock) {
		return functionalBlock.getDescription();
	}

	@SchemaMapping(typeName = FUNCTIONAL_BLOCK_TYPE_NAME)
	@MiningDataPoint(scalarType = MiningDataPointDefinition.ScalarType.JSON)
	public Map<String, Object> flags(final FunctionalBlockPojo functionalBlock) {
		return functionalBlock.getFlags();
	}

	@SchemaMapping(typeName = FUNCTIONAL_BLOCK_TYPE_NAME)
	public Set<FunctionalBlockType> type(final FunctionalBlockPojo functionalBlock) {
		final Object types = functionalBlock.getFlags().get(FunctionalBlockFlag.TYPE.name());
		if (types == null) {
			return Collections.emptySet();
		}

		final Collection<?> typeCollection;
		if (types instanceof Collection) {
			typeCollection = (Collection<?>) types;
		} else {
			typeCollection = Collections.singleton(types);
		}

		return typeCollection.stream().map(type -> FunctionalBlockType.valueOf(type.toString())).collect(Collectors.toSet());
	}

	@BatchMapping(typeName = FUNCTIONAL_BLOCK_TYPE_NAME)
	public Map<FunctionalBlockPojo, GeneratedFrom> generatedFrom(final List<FunctionalBlockPojo> functionalBlocks) {
		final Map<UUID, GeneratedFrom> generatedFrom = functionalBlockService.getGeneratedFrom(
				functionalBlocks.stream().map(FunctionalBlockPojo::getUid).toList());

		/* can not use Collectors.toMap() because it doesn't support null values (JDK bug) :-( */
		final Map<FunctionalBlockPojo, GeneratedFrom> ret = new HashMap<>();
		functionalBlocks.forEach(fb -> ret.put(fb, generatedFrom.get(fb.getUid())));

		return ret;
	}

	@SchemaMapping(typeName = FUNCTIONAL_BLOCK_TYPE_NAME)
	@Nullable
	public FunctionalBlockStatus status(final FunctionalBlockPojo functionalBlock) {
		final Map<String, Object> flags = functionalBlock.getFlags();
		final Object status = flags.get(FunctionalBlockFlag.STATUS.name());
		return status == null ? null : FunctionalBlockStatus.valueOf(status.toString());
	}

	@SchemaMapping(typeName = FUNCTIONAL_BLOCK_TYPE_NAME)
	@Nullable
	public Boolean outdatedBlock(final FunctionalBlockPojo functionalBlock) {
		final Object value = functionalBlock.getFlags().get(FunctionalBlockFlag.OUTDATED.name());
		return value == null ? null : Boolean.parseBoolean(value.toString());
	}

	@SchemaMapping(typeName = FUNCTIONAL_BLOCK_TYPE_NAME)
	@Nullable
	public Boolean blocksWithDeletedUB(final FunctionalBlockPojo functionalBlock) {
		final Object value = functionalBlock.getFlags().get(FunctionalBlockFlag.DELETED.name());
		return value == null ? null : Boolean.parseBoolean(value.toString());
	}

	@BatchMapping(typeName = FUNCTIONAL_BLOCK_TYPE_NAME)
	public Map<FunctionalBlockPojo, Set<String>> lowerBoundAccessTypes(final Collection<FunctionalBlockPojo> functionalBlock) {
		final Map<UUID, FunctionalBlockPojo> functionalBlockMap =
				functionalBlock.stream().collect(Collectors.toMap(FunctionalBlockPojo::getUid, Function.identity()));
		final Map<UUID, List<FunctionalBlockPojo>> children = functionalBlockService.findChildrenDeep(functionalBlock.stream()
						.map(FunctionalBlockPojo::getUid).toList(), 1,
				q -> q.withType(FunctionalBlockType.RA_LOWER_BOUND));
		return children.entrySet().stream().collect(Collectors.toMap(e ->
				functionalBlockMap.get(e.getKey()), e -> e.getValue().stream().map(child -> child.getFlags()
						.get(FunctionalBlockFlag.RA_ACCESS_TYPE.name())).filter(Objects::nonNull)
						.flatMap(accessTypes -> accessTypes instanceof Collection
						? ((Collection<?>) accessTypes).stream().map(Object::toString) : Stream.of(accessTypes.toString()))
				.collect(Collectors.toSet())));
	}

	@BatchMapping(typeName = "GeneratedFrom")
	@Nullable
	public Map<GeneratedFrom, ModulePojo> module(final List<GeneratedFrom> generatedFrom, final GraphQLContext graphQLContext) {
		final EntityId projectId = EntityId.of((Long) graphQLContext.get(PROJECT_ID_CONTEXT_KEY));
		final List<String> linkHashes = generatedFrom.stream()
				.map(GeneratedFrom::getModuleLinkHash)
				.filter(Optional::isPresent)
				.map(Optional::get)
				.toList();
		final Map<String, ModulePojo> modules = moduleService.findModules(q -> q.ofProject(projectId)
						.includeContent(true)
						.withLinkHashes(linkHashes))
				.stream()
				.collect(Collectors.toMap(ModulePojo :: getLinkHash, Function.identity()));

		final HashMap<GeneratedFrom, ModulePojo> ret = new HashMap<>(generatedFrom.size());
		for (final GeneratedFrom g : generatedFrom) {
			final Optional<String> moduleLinkHash = g.getModuleLinkHash();
			moduleLinkHash.ifPresent(s -> ret.put(g, modules.get(s)));
		}
		return ret;
	}


	@SchemaMapping(typeName = "GeneratedFrom", field = "annotation")
	@Nullable
	public AnnotationPojo generatedFromAnnotation(final GeneratedFrom generatedFrom) {
		return generatedFrom.getAnnotationId()
				.flatMap(annotationId -> annotationService.findAny(q -> q.byId(annotationId)))
				.orElse(null);
	}

	@SchemaMapping(typeName = FunctionalBlocksDataPointSource.FUNCTIONAL_BLOCK_TYPE_NAME, field = "reachabilityData")
	public Paged<ReachabilityDataPojo> reachabilityDataForFunctionalBlock(final FunctionalBlockPojo functionalBlock,
			@Argument @Nullable final Integer page,
			@Argument @Nullable final Integer size,
			final GraphQLContext graphQLContext) {
		return reachabilityData(functionalBlock.getProject().getNid(), Collections.singletonList(functionalBlock.getUid()), page, size, null,
				null, null, graphQLContext);
	}

	@MiningDataPointIgnore /* defined by FunctionalBlocksDataPointSource */
	@SchemaMapping(typeName = "PAGED_FunctionalBlock")
	public List<AggregationResult<String>> aggregations(final DataFetchingEnvironment env) {
		final Long projectId = env.getGraphQlContext().get(PROJECT_ID_CONTEXT_KEY);
		final AggregationRequest<FunctionalBlockFieldName> aggregationRequest = toAggregationRequest(env.getSelectionSet().getFields(), FunctionalBlockFieldName.class);
		aggregationRequest.setFilterObject(Map.of(FunctionalBlockFieldName.PROJECT_ID, Map.of("eq", projectId)));
		final ControllerLocalContext localContext = env.getLocalContext();
		final BuildingConsumerHolder buildingConsumer = localContext.getLocalContext(BuildingConsumerHolder.class);

		final List<AggregationResult<FunctionalBlockFieldName>> aggregationResults;
		if (buildingConsumer == null) {
			aggregationResults = functionalBlockService.getAggregations(aggregationRequest);
		} else {
			aggregationResults = functionalBlockService.getAggregations(buildingConsumer.getBuildingConsumer()::accept, aggregationRequest);
		}

		/* we need to convert AggregationResult<FunctionalBlockFieldName> to AggregationResult<String>, or else GraphQL is unable to retrieve the fields */
		return aggregationResults.stream()
				.filter(agg ->
						/* WMIN-12238: fix to filter all results whih have no technology and no type */
						! (aggregationRequest.getGroupBy().contains(REFERENCED_MODULE_TECHNOLOGY) && agg.getGroup().get(REFERENCED_MODULE_TECHNOLOGY) == null ||
								aggregationRequest.getGroupBy().contains(REFERENCED_MODULE_TYPE) && agg.getGroup().get(REFERENCED_MODULE_TYPE) == null)
				)
				.map(GraphQLAggregationRequest::toAggregationResultString).toList();
	}

	/**
	 * It Collect the upperBoundModule of ReachabilityBlock.
	 * @param reachabilityData the List of {@link ReachabilityDataPojo}
	 * @return upperBoundModule of ReachabilityBlock.
	 */
	@BatchMapping(typeName = "ReachabilityData")
	@Usage(value = Usages.GRAPHQL_QUERY_PREFIX + "reachabilityData", attributes = {
			/* This is required, otherwise the sort Object will not be added as an argument to the schema */
			@UsageAttribute(key = SortByAttributes.SQL_FRAGMENT_ORDER_BY, value = "")
	})
	public Map<ReachabilityDataPojo, ModulePojo> upperBoundModules(final List<ReachabilityDataPojo> reachabilityData) {
		final List<EntityId> moduleIds = reachabilityData.stream()
				.map(ReachabilityDataPojo::getUpperBoundModuleId)
				.filter(EntityId::hasUid)
				.toList();
		final Map<Long, ModulePojo> modulesById = moduleService.findModules(q -> q.byIds(moduleIds)
						.includeContent(true)).stream()
				.collect(Collectors.toMap(ModulePojo::getId, Function.identity()));

		return reachabilityData.stream().filter(rb -> rb.getUpperBoundModuleId().hasUid())
				.collect(Collectors.toMap(Function.identity(), rd -> modulesById.get(rd.getUpperBoundModuleId().getNid())));
	}

	/**
	 * It Collects the List of Taxonomies of ReachabilityBlock.
	 * @param reachabilityData the List of {@link ReachabilityDataPojo}
	 * @param moduleType the type of module
	 * @param graphQLContext the {@link GraphQLContext}
	 * @return List of Taxonomies of ReachabilityBlock.
	 */
	@SchemaMapping(typeName = "ReachabilityData")
	public List<String> moduleTaxonomies(final ReachabilityDataPojo reachabilityData, @Argument final String moduleType, final GraphQLContext graphQLContext) {
		final List<EntityId> moduleIds = new ArrayList<>();
		if (moduleType.equalsIgnoreCase("UpperBound")) {
			moduleIds.add(reachabilityData.getUpperBoundModuleId());
		} else if (moduleType.equalsIgnoreCase("LowerBound")) {
			reachabilityData.getLowerBoundModuleId().ifPresent(moduleIds::add);
		} else {
			moduleIds.addAll(reachabilityData.getIntermediateModules());
			moduleIds.addAll(reachabilityData.getAccessModuleIds());
		}
		final EntityId projectId = EntityId.of((Long) graphQLContext.get(PROJECT_ID_CONTEXT_KEY));
		return taxonomyService.find(q -> q.ofProject(projectId).ofModules(moduleIds)).stream()
				.map(t -> StringUtils.trimToEmpty(t.getType().getName()) + ": " + StringUtils.trimToEmpty(t.getName())).distinct()
				.toList();
	}

	/**
	 * It Collect the LowerBoundModule of ReachabilityBlock.
	 * @param reachabilityData the List of {@link ReachabilityDataPojo}
	 * @return LowerBoundModule of ReachabilityBlock.
	 */
	@BatchMapping(typeName = "ReachabilityData")
	@Usage(value = Usages.GRAPHQL_QUERY_PREFIX + "reachabilityData", attributes = {
			/* This is required, otherwise the sort Object will not be added as an argument to the schema */
			@UsageAttribute(key = SortByAttributes.SQL_FRAGMENT_ORDER_BY, value = "")
	})
	public Map<ReachabilityDataPojo, ModulePojo> lowerBoundModules(final List<ReachabilityDataPojo> reachabilityData) {
		final List<EntityId> moduleIds = reachabilityData.stream()
				.map(ReachabilityDataPojo::getLowerBoundModuleId)
				.filter(Optional::isPresent)
				.map(Optional::get)
				.toList();

		final Map<Long, ModulePojo> modulesById = moduleService.findModules(q -> q.byIds(moduleIds)
						.includeContent(true)).stream()
				.collect(Collectors.toMap(ModulePojo::getId, Function.identity()));

		return reachabilityData.stream().filter(rd -> rd.getLowerBoundModuleId().isPresent())
				.collect(Collectors.toMap(Function.identity(), rd -> modulesById.get(rd.getLowerBoundModuleId().get().getNid())));
	}

	/**
	 * It Collects the List of SQL details of the accessing module corresponding to the LowerBound SQL Module.
	 * @param reachabilityData the {@link ReachabilityDataPojo}
	 * @param graphQLContext the {@link GraphQLContext}
	 * @return List of SQL details of the accessing module corresponding to the LowerBound SQL Module.
	 */
	@MiningDataPointIgnore /* defined by FunctionalBlocksDataPointSource */
	@SchemaMapping(typeName = "ReachabilityData")
	public List<SqlDetailsPojo> sqlDetails(final ReachabilityDataPojo reachabilityData, final GraphQLContext graphQLContext) {
		if ( ! dbcutterDbPostgresEnabled) {
			return Collections.emptyList();
		}
		final Optional<EntityId> lowerBoundModuleId = reachabilityData.getLowerBoundModuleId();
		if (lowerBoundModuleId.isPresent()) {
			final ModuleLightweightPojo lowerBoundModule = moduleService.findAnyModuleLightweight(q -> q.byId(lowerBoundModuleId.get()))
					.orElseThrow(() -> new IllegalStateException("Lower bound module not found"));
			if (lowerBoundModule.getTechnology() == Technology.SQL) {
				final List<String> linkHashes = moduleService.findModules(q -> q.byIds(reachabilityData.getAccessModuleIds()))
						.stream()
						.map(ModulePojo::getLinkHash)
						.toList();
				final EntityId projectId = EntityId.of((Long) graphQLContext.get(PROJECT_ID_CONTEXT_KEY));

				return sqlDetailsService.findSqlDetails(x -> x.ofProject(projectId.getNid().intValue())
						.withModuleLinkHashes(linkHashes)
						.withTableName(lowerBoundModule.getName()));
			}
		}
		return Collections.emptyList();
	}
	
	/**
	 * It Collects the List of AccessModule of ReachabilityBlock.
	 * @param reachabilityData the List of {@link ReachabilityDataPojo}
	 * @return List of AccessModule of ReachabilityBlock.
	 */
	@BatchMapping(typeName = "ReachabilityData")
	public Map<ReachabilityDataPojo, List<ModulePojo>> accessModules(final List<ReachabilityDataPojo> reachabilityData) {
		final List<EntityId> moduleIds = reachabilityData.stream()
				.map(ReachabilityDataPojo::getAccessModuleIds)
				.flatMap(List::stream)
				.toList();

		final Map<Long, ModulePojo> modulesById = moduleService.findModules(q -> q.byIds(moduleIds)
						.includeContent(true)).stream()
				.collect(Collectors.toMap(ModulePojo::getId, Function.identity()));

		return reachabilityData.stream()
				.collect(Collectors.toMap(Function.identity(), rd -> rd.getAccessModuleIds().stream().map(id -> modulesById.get(id.getNid()))
						.toList()));
	}

	/**
	 * Collects the List of Data Access Type of ReachabilityBlock.
	 * @param reachabilityData the List of {@link ReachabilityDataPojo}
	 * @return List of Data Access Type of ReachabilityBlock.
	 */
	@SchemaMapping(typeName = "ReachabilityData")
	@MiningDataPoint(displayName = "Data Access", description = "Access types by which the upper bound accesses the lower bound")
	@Usage(value = Usages.MINING_UI_REACHABILITY_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "6")
	})
	public List<String> dataAccessType(final ReachabilityDataPojo reachabilityData) {
		return reachabilityData.getAccessTypes();
	}

	/**
	 * Collects the List of IntermediateModule of ReachabilityBlock.
	 * @param reachabilityData the List of {@link ReachabilityDataPojo}
	 * @return List of IntermediateModule of ReachabilityBlock.
	 */
	@BatchMapping(typeName = "ReachabilityData")
	public Map<ReachabilityDataPojo, List<ModulePojo>> intermediateModulesData(final List<ReachabilityDataPojo> reachabilityData) {
		final List<EntityId> moduleIds = reachabilityData.stream()
				.map(ReachabilityDataPojo::getIntermediateModules)
				.flatMap(List::stream)
				.toList();
		final Map<Long, ModulePojo> modulesById = moduleService.findModules(q -> q.byIds(moduleIds)).stream()
				.collect(Collectors.toMap(ModulePojo::getId, Function.identity()));

		return reachabilityData.stream()
				.collect(Collectors.toMap(Function.identity(), rd -> rd.getIntermediateModules().stream().map(id -> modulesById.get(id.getNid()))
						.toList()));
	}
	
	@SchemaMapping(typeName = FUNCTIONAL_BLOCK_TYPE_NAME)
	public String deepName(final FunctionalBlockPojo functionalBlock) {
		return functionalBlock.getName();
	}
	@SchemaMapping(typeName = FunctionalBlocksDataPointSource.FUNCTIONAL_BLOCK_TYPE_NAME)
	@MiningDataPoint(displayName = "Referenced Data Dictionaries", description = "Data Dictionaries referenced by the Functional Block")
	public List<UUID> referencedDataDictionaries(final FunctionalBlockPojo functionalBlock) {
		return functionalBlockService.getReferencedDataDictionaries(functionalBlock.getUid());
	}

	@SchemaMapping(typeName = FunctionalBlocksDataPointSource.FUNCTIONAL_BLOCK_TYPE_NAME)
	@MiningDataPoint(displayName = "Referenced Data Dictionaries Names", description = "Data Dictionaries Names referenced by the Functional Block.")
	public List<String> referencedDataDictionaryNames(final FunctionalBlockPojo functionalBlock) {
		final List<EntityId> ddeUids = functionalBlockService.getReferencedDataDictionaries(functionalBlock.getUid()).
				stream().map(EntityId::of).toList();
		return dataDictionaryService.find(q -> q.byIds(ddeUids)).stream().map(DataDictionaryPojo ::getName).toList();
	}

	private Mono<Map<PeersBatchLoadInformation, PeersResult>> loadPeersBatched(final Set<PeersBatchLoadInformation> batchInfo,
			final BatchLoaderEnvironment env) {
		final Map<UUID, List<FunctionalBlockPojo>> peersMap = new HashMap<>();
		final var grouped = batchInfo.stream().collect(Collectors.groupingBy(info -> info.field));
		for (final Map.Entry<Field, List<PeersBatchLoadInformation>> group : grouped.entrySet()) {
			final List<UUID> functionalBlockIds = group.getValue().stream().map(g -> g.parent).toList();
			final Optional<Long> projectId = group.getValue().stream().map(g -> g.projectId).findAny();
			final Optional<Map<String, Object>> filterObject = group.getValue().stream().map(g -> g.filterObject)
					.filter(Objects::nonNull)
					.findAny();

			final var peerType = group.getValue().stream().map(g -> g.peerType)
					.filter(Objects::nonNull)
					.findAny();
			final List<Pair<UUID, UUID>> peers;
			if (peerType.isPresent()) {
				peers = functionalBlockService.findPeers(q -> q.byUids(functionalBlockIds), peerType.get());
			} else {
				peers = functionalBlockService.findPeers(q -> q.byUids(functionalBlockIds), p -> {
					if (filterObject.isPresent() && projectId.isPresent()) {
						filterObjectService.applyFilterObject(projectId.get(), QUERY_NAME, filterObject.get(), p);
					}
				});
			}
			final Map<UUID, FunctionalBlockPojo> blockMap = functionalBlockService.get(peers.stream().map(Pair::getRight).toList()).stream()
					.collect(Collectors.toMap(FunctionalBlockPojo::getUid, Function.identity()));
			for (final Pair<UUID, UUID> peer : peers) {
				peersMap.computeIfAbsent(peer.getKey(), k -> new ArrayList<>()).add(blockMap.get(peer.getValue()));
			}
		}
		final Map<PeersBatchLoadInformation, PeersResult> result = new HashMap<>();
		for (final PeersBatchLoadInformation info : batchInfo) {
			final List<FunctionalBlockPojo> content = peersMap.get(info.parent);
			if (content != null) {
				result.put(info, new PeersResult(Paged.ofContent(content)));
			}
		}
		return Mono.just(result);
	}

	private Pagination getPagination(@Nullable final Integer page, @Nullable final Integer size) {
		if (size != null && size > 0) {
			return Pagination.at(page == null ? 0 : page, size);
		} else {
			return Pagination.at(0,0);
		}
	}

	private ControllerLocalContext getLocalContext(final Long projectId) {
		return new ControllerLocalContext(projectId, userRoleService.getProjectIds(), userRoleService.getClientAdminIds(), userRoleService.isAdmin());
	}

	private boolean isSelectingOnlyAggregations(final DataFetchingFieldSelectionSet selectionSet) {
		/* checks if only the "aggregations" field (and sub-fields) is selected */
		return selectionSet.getFields().stream().allMatch(field -> field.getQualifiedName().startsWith("aggregations"));
	}
}

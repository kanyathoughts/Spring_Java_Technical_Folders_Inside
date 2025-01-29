package innowake.mining.opensearch.index;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.opensearch.controller.model.aggregations.AggregationRequestString;
import innowake.mining.opensearch.controller.model.aggregations.AggregationResultString;
import innowake.mining.opensearch.graphql.DataPointQueryResult;
import innowake.mining.opensearch.graphql.DataPointSelection;
import innowake.mining.opensearch.index.OpenSearchApiService.BulkIndexRequestBuilder;
import innowake.mining.opensearch.index.model.AggregationIndexResult;
import innowake.mining.opensearch.index.model.DataPointDefinition;
import innowake.mining.opensearch.index.model.IndexDefinition;
import innowake.mining.opensearch.index.model.IndexStatus;
import innowake.mining.opensearch.index.model.MatchedSearchHit;
import innowake.mining.opensearch.util.FutureUtil;
import innowake.mining.opensearch.util.IndexNameUtil;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinitionWithPath;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.SearchFilterAttributes;
import innowake.mining.shared.model.aggregations.AggregationOperator;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.opensearch.client.json.JsonData;
import org.opensearch.client.opensearch.OpenSearchClient;
import org.opensearch.client.opensearch._types.OpenSearchException;
import org.opensearch.client.opensearch._types.aggregations.Aggregate;
import org.opensearch.client.opensearch._types.aggregations.Aggregation;
import org.opensearch.client.opensearch._types.aggregations.AggregationBuilders;
import org.opensearch.client.opensearch._types.aggregations.CompositeAggregation;
import org.opensearch.client.opensearch._types.aggregations.CompositeAggregationSource;
import org.opensearch.client.opensearch._types.aggregations.CompositeBucket;
import org.opensearch.client.opensearch._types.aggregations.SumAggregation;
import org.opensearch.client.opensearch._types.aggregations.TermsAggregation;
import org.opensearch.client.opensearch._types.aggregations.ValueCountAggregation;
import org.opensearch.client.opensearch._types.query_dsl.MultiMatchQuery;
import org.opensearch.client.opensearch._types.query_dsl.Query;
import org.opensearch.client.opensearch.core.SearchRequest;
import org.opensearch.client.opensearch.core.SearchResponse;
import org.opensearch.client.opensearch.core.search.Hit;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClientResponseException;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import javax.annotation.PostConstruct;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.stream.Collectors;

/**
 * Service responsible for managing and building indexes.
 */
@Service
public class IndexService {

	private static final int PAGE_SIZE = 1000;
	
	private static final String moduleIndex = "moduletable";
	private static final String annotationIndex = "annotationtable";

	private final Map<String, IndexDefinition> indexDefinitions;
	private final Map<String, IndexStatus> indexStatusMap = new HashMap<>();
	private final MiningApiService miningApiService;
	private final OpenSearchApiService openSearchApiService;
	@Autowired
	private OpenSearchClient openSearchClient;

	private final ExecutorService executor;
	
    private static final Logger LOG = LoggerFactory.getLogger(MiningApiService.class);

	public IndexService(final Map<String, IndexDefinition> indexDefinitions, final MiningApiService miningApiService,
			final OpenSearchApiService openSearchApiService, final int indexerThreads) {
		this.indexDefinitions = indexDefinitions;
		this.miningApiService = miningApiService;
		this.openSearchApiService = openSearchApiService;
		this.executor = Executors.newFixedThreadPool(indexerThreads);
	}

	@PostConstruct
	public void loadIndexStatus() {
		openSearchApiService.listIndexes()
		.filter(indexStatus -> indexDefinitions.keySet().stream().anyMatch(indexDefName -> indexStatus.getIndex().startsWith(indexDefName)))
		.collect(Collectors.toList())
		.block()
		.forEach(openSearchIndexStatus -> {
			final Pair<String, Long> parsedName = IndexNameUtil.parseIndexName(openSearchIndexStatus.getIndex());
			final String indexName = parsedName.getLeft();
			final Long projectId = parsedName.getRight();
			final IndexDefinition definition = indexDefinitions.get(indexName);
			//TODO: must wait until mining is available
			final List<MiningDataPointDefinitionWithPath> dataPointDefinitions = getDataPointDefinitions(definition, projectId);

			final IndexStatus indexStatus = new IndexStatus(indexName, projectId, definition, dataPointDefinitions);
			indexStatus.setIndexProgress(1.0);
			indexStatus.setStatus(IndexStatus.Status.READY);
			indexStatusMap.put(openSearchIndexStatus.getIndex(), indexStatus);
		});
	}

	public Map<String, IndexStatus> getIndexStatus() {
		return Collections.unmodifiableMap(indexStatusMap);
	}

	public void rescanIndex(final String indexName, final Long projectId, @Nullable final Long docId) {
		final IndexDefinition definition = indexDefinitions.get(indexName);
		if (definition == null) {
			throw new IllegalArgumentException("no definition for index '" + indexName + "'." +
					" Add the definition in the 'mining-opensearch.indexes' config property.");
		}
		final String combinedName = IndexNameUtil.buildIndexName(indexName, projectId);

		final List<MiningDataPointDefinitionWithPath> dataPoints;
		if (! StringUtils.isEmpty(definition.getUsage())) {
			dataPoints = getDataPointDefinitions(definition, projectId);
			if (docId == null) {
				if ( ! indexStatusMap.isEmpty() && indexStatusMap.containsKey(combinedName)) {
					if (indexStatusMap.get(combinedName).getStatus().equals(IndexStatus.Status.READY)) {
						deleteIndex(indexName, projectId).block();
					}
				}
				try {
					createMapping(combinedName, dataPoints);
				} catch (final WebClientResponseException e) {
					throw new IllegalStateException("Index rebuild failed", e);
				}
			}
		} else {
			dataPoints = definition.getDataPoints().stream().map(dpDef -> new MiningDataPointDefinitionWithPath(
					new MiningDataPointDefinition(getDataPointBaseName(dpDef.getPath()),
							"dontCare",
							MiningDataPointDefinition.ScalarType.STRING,
							false,
							false), dpDef.getPath()))
					.collect(Collectors.toList());
			createFixedMapping(combinedName, definition.getDataPoints());
		}


		final IndexStatus indexStatus = new IndexStatus(indexName, projectId, definition, dataPoints);
		indexStatus.setIndexProgress(0.0);
		indexStatus.setStatus(IndexStatus.Status.INDEXING);
		indexStatusMap.put(combinedName, indexStatus);

		dataPoints.add(getIdDataPoint(definition));
		try {
			loadPagedData(combinedName, definition, projectId, docId, dataPoints);
		} catch (final ExecutionException e) {
			throw new IllegalStateException("index rebuild failed", e);
		} catch (final InterruptedException e) {
			Thread.currentThread().interrupt();
			throw new IllegalStateException("index rebuild failed", e);
		}
	}
	
	private String getDataPointBaseName(final String path) {
		final String[] split = path.split("\\.");
		if (split.length == 0) {
			return "";
		}
		return split[split.length - 1];
	}

	private void createFixedMapping(final String indexName,final List<DataPointDefinition> dpDefs) {
		final Map<String, Map<String, Object>> propertyMappings = new HashMap<>();
		for (final DataPointDefinition dpDef : dpDefs) {
			final String key = dpDef.getPath().replace(".", "_");
			final String type = dpDef.getType();
			propertyMappings.put(key, Map.of("type", type));
		}
		openSearchApiService.createMappings(indexName, propertyMappings);
	}

	private List<MiningDataPointDefinitionWithPath> getDataPointDefinitions(final IndexDefinition definition, final Long projectId) {
		return miningApiService.getDataPointsForQueryWithUsage(definition.getRootType(), definition.getUsage(), projectId)
				.stream()
				.filter(dp -> isDataPointFilterableOrSortable(dp, definition.getUsage()))
				.collect(Collectors.toList());
	}

	private boolean isDataPointFilterableOrSortable(final MiningDataPointDefinition dp, final String usage) {
		final Map<String, String> generalFilterAttributes = dp.getUsageAttributes().get(Usages.SEARCH_FILTER);
		final Map<String, String> usageSpecificAttributes = dp.getUsageAttributes().get(usage);

		/* checking for SEARCH_FILTER usage below should be enough, but just to be safe, we're checking for "filterMode" usage attribute as well
		*
		* for some data points the "filterMode" attribute is defined "globally" on the "general.searchFilter" usage,
		* whereas for others, it is defined on the table-specific usage, e.g. "miningUi.modulesTable" */
		if (generalFilterAttributes != null && generalFilterAttributes.containsKey(SearchFilterAttributes.FILTER_MODE)) {
			return true;
		}
		if (usageSpecificAttributes != null && usageSpecificAttributes.containsKey(SearchFilterAttributes.FILTER_MODE)) {
			return true;
		}

		if (dp.getUsages().contains(Usages.SEARCH_FILTER) || dp.getUsages().contains(Usages.SORT_BY)) {
			return true;
		}

		return false;
	}

	private void createMapping(final String indexName, final List<MiningDataPointDefinitionWithPath> dataPoints) {
		final Map<String, Map<String, Object>> propertyMappings = new HashMap<>();
		for (final MiningDataPointDefinitionWithPath dp : dataPoints) {
			final String key = dp.getPath().replace(".", "_");
			final String type = mapType(dp);
			if (type.equals("text")) {
				propertyMappings.put(key, Map.of("type", type, "fielddata", true));
			} else if (type.equals("keyword")) {
				propertyMappings.put(key, Map.of("type", type, "normalizer", "toLowercase"));
			} else {
				propertyMappings.put(key, Map.of("type", type));
			}
		}
		openSearchApiService.createMappings(indexName, propertyMappings);
	}

	/**
	 * Map data point {@link MiningDataPointDefinition.ScalarType} to type names used by OpenSearch.
	 *
	 * @return type name for OpenSearch
	 */
	private String mapType(final MiningDataPointDefinition dp) {
		final MiningDataPointDefinition.ScalarType scalarType = dp.getScalarType();
		if (scalarType == null) {
			//TODO: for now, assume it is an enum
			return "keyword";
		}
		switch (scalarType) {
			case BOOLEAN:
				return "boolean";
			case INT:
				return "integer";
			case LONG:
				return "long";
			case JSON:
				return "object";
			case DATETIME:
			case TIMESTAMP:
				return "date";
			case FLOAT:
				return "float";
			case STRING:
			default:
				//TODO: for now, using "keyword" on everything instead of full-text index
				return "keyword";
		}
	}

	private MiningDataPointDefinitionWithPath getIdDataPoint(final IndexDefinition definition) {
		final String[] path = definition.getId().split("\\.");
		return new MiningDataPointDefinitionWithPath(new MiningDataPointDefinition(path[path.length - 1], "dontCare",
				MiningDataPointDefinition.ScalarType.LONG, false, false),
				definition.getId());
	}

	private void loadPagedData(final String indexName, final IndexDefinition indexDefinition, final Long projectId, @Nullable final Long docId,
			final List<MiningDataPointDefinitionWithPath> dataPoints) throws ExecutionException, InterruptedException {
		
		final DataPointQueryResult firstPage = miningApiService.queryDataPoints(indexDefinition, projectId, docId, dataPoints, 0, PAGE_SIZE);
		
		final IndexStatus indexStatus = indexStatusMap.get(indexName);

		if (firstPage.getTotalPages() == 0) {
			LOG.warn("No data found for indexing using OpenSearch for index " + indexName);
			indexStatus.setIndexProgress(1.0);
			indexStatus.setStatus(IndexStatus.Status.READY);
			return;
		}

		final List<Future<?>> futures = new ArrayList<>(firstPage.getTotalPages());
		futures.add(executor.submit(() -> loadDataIntoOpenSearch(indexName, indexDefinition, docId, firstPage, dataPoints)));

		for (int i = 1; i < firstPage.getTotalPages(); i++) {
			final int index = i;
			futures.add(executor.submit(() -> {
				System.out.println("*** Loading Page " + index);
				final DataPointQueryResult page = miningApiService.queryDataPoints(indexDefinition, projectId, docId, dataPoints, index, PAGE_SIZE);
				System.out.println("*** Finished loading Page " + index);
				System.out.println("*** Loading Page into OpenSearch " + index);
				loadDataIntoOpenSearch(indexName, indexDefinition, docId, page, dataPoints);
				System.out.println("*** Finished loading Page into OpenSearch " + index);
				indexStatus.setIndexProgress(index / (double) firstPage.getTotalPages());
			}));
		}

		FutureUtil.awaitAll(futures);
		indexStatus.setIndexProgress(1.0);
		indexStatus.setStatus(IndexStatus.Status.READY);
	}
	
	private void loadDataIntoOpenSearch(final String indexName, final IndexDefinition indexDefinition, @Nullable final Long docId, 
			final DataPointQueryResult data, final List<MiningDataPointDefinitionWithPath> dataPoints) {

		final Map<String, DataPointSelection> selections = new HashMap<>(dataPoints.size());
		for (final MiningDataPointDefinitionWithPath dp : dataPoints) {
			selections.put(dp.getPath().replace(".", "_"), data.getDataPoint(dp.getPath()));
		}

		final BulkIndexRequestBuilder bulkRequest = openSearchApiService.bulkIndexRequest(indexName, docId);
		for (int i = 0; i < data.getSize(); i++) {
			final Object id = data.getDataPoint(indexDefinition.getId()).getValue(i);
			final HashMap<String, Object> dataForOpenSearch = new HashMap<>(selections.size());
			for (final Map.Entry<String, DataPointSelection> selection : selections.entrySet()) {
				final Object value = selection.getValue().getValue(i);
				if (value != null) {
					dataForOpenSearch.put(selection.getKey(), value);
				}
			}
			bulkRequest.addData(id, dataForOpenSearch);
		}
		bulkRequest.sendBulkRequest();
	}

	public Mono<Void> deleteIndex(final String indexName, final Long projectId) {
		final String combinedName = IndexNameUtil.buildIndexName(indexName, projectId);
		indexStatusMap.remove(combinedName);
		return openSearchApiService.deleteIndex(combinedName);
	}

	@SuppressWarnings("unchecked")
	public Mono<Page<String>> queryIndex(final String indexName, final Long projectId,
			final int page,
			final int size,
			@Nullable final Map<String, Object> filterObject,
			@Nullable final List<Map<String, String>> sortObject) {

		final Map<String, Object> openSearchRequest = new HashMap<>();

		/* calculate pagination attributes */
		int realSize;
		int from;
		if (size > 0) {
			realSize = size;
			from = page * size;
		} else {
			realSize = PAGE_SIZE;
			from = 0;
		}
		openSearchRequest.put("size", realSize);
		openSearchRequest.put("from", from);

		/* do not include the full document in the result - we need the ids only */
		openSearchRequest.put("_source", false);
		/* we need the total number of results to compute pagination */
		openSearchRequest.put("track_total_hits", true);

		if (filterObject != null && ! filterObject.isEmpty()) {
			openSearchRequest.put("query", Map.of("bool", convertFilterObjectToOpenSearchQuery(indexName, projectId, filterObject)));
		}

		if (sortObject != null && ! sortObject.isEmpty()) {
			openSearchRequest.put("sort", sortObject.stream()
					.flatMap(so -> so.entrySet().stream())
					.map(entry -> Map.of(entry.getKey(), Map.of("order", entry.getValue().toLowerCase())))
					.collect(Collectors.toList()));
		}

		return openSearchApiService.queryIndex(IndexNameUtil.buildIndexName(indexName, projectId), openSearchRequest).map(response -> {
			final Map<String, Object> hits = (Map<String, Object>) response.get("hits");
			final Map<String, Object> total = (Map<String, Object>) hits.get("total");
			final Long totalValue = Long.valueOf(total.get("value").toString());
			final List<Map<String, Object>> contents = (List<Map<String, Object>>) hits.get("hits");
			final List<String> contentIds = contents.stream().map(doc -> doc.get("_id").toString()).collect(Collectors.toList());
			return new PageImpl<>(contentIds, PageRequest.of(from / realSize, realSize), totalValue);
		});
	}

	@SuppressWarnings("unchecked")
	private Map<String, Object> convertFilterObjectToOpenSearchQuery(final String indexName, final Long projectId, final Map<String, Object> filterObject) {
		final Map<String, List<Object>> properties = new HashMap<>();
		final Map<String, List<Object>> notProperties = new HashMap<>();
		final Map<String, Pair<Long, Long>> rangeProperties = new HashMap<>();

		traverseFilterObject(filterObject, properties, notProperties, rangeProperties, false);

		final Map<String, Object> boolQuery = new HashMap<>();
		if ( ! properties.isEmpty()) {
			boolQuery.put("must", translatePropertiesToQuery(indexName, projectId, properties));
		}
		if ( ! rangeProperties.isEmpty()) {
			final List<Object> must = (List<Object>) boolQuery.computeIfAbsent("must", key -> new ArrayList<>());
			final Map<String, Object> rangeQuery = new HashMap<>();
			rangeProperties.forEach((prop, range) -> {
				final Map<String, Object> rangeOperators = new HashMap<>();
				if (range.getLeft() != null) {
					rangeOperators.put("lte", range.getLeft());
				}
				if (range.getRight() != null) {
					rangeOperators.put("gte", range.getRight());
				}
				rangeQuery.put(prop, rangeOperators);
			});
			must.add(Map.of("range", rangeQuery));
		}
		if ( !notProperties.isEmpty()) {
			boolQuery.put("must_not", translatePropertiesToQuery(indexName, projectId, notProperties));
		}

		return boolQuery;
	}

	@SuppressWarnings("unchecked")
	private void traverseFilterObject(final Map<String, Object> filterObject, final Map<String, List<Object>> properties,
			final Map<String, List<Object>> notProperties, final Map<String, Pair<Long, Long>> rangeProperties,
			final boolean insideNot) {
		for (final Map.Entry<String, Object> entry : filterObject.entrySet()) {
			if (entry.getKey().equals("_and") || entry.getKey().equals("_or")) {
				//TODO: treating _and the same as _or, lol
				final List<Map<String, Object>> filterObjects = (List<Map<String, Object>>) entry.getValue();
				filterObjects.forEach(fo -> traverseFilterObject(fo, properties, notProperties, rangeProperties, insideNot));
			} else if (entry.getKey().equals("_not")) {
				if (insideNot) {
					throw new IllegalArgumentException("parsing _not nested inside another _not is currently not supported");
				}
				traverseFilterObject((Map<String, Object>) entry.getValue(), properties, notProperties, rangeProperties, true);
			} else {
				final Map<String, Object> operatorMap = (Map<String, Object>) entry.getValue();
				for (final Map.Entry<String, Object> operator : operatorMap.entrySet()) {
					if (operator.getKey().equals("eq") || operator.getKey().equals("is")) {
						if (insideNot) {
							notProperties.computeIfAbsent(entry.getKey(), key -> new ArrayList<>()).add(operator.getValue());
						} else {
							properties.computeIfAbsent(entry.getKey(), key -> new ArrayList<>()).add(operator.getValue());
						}
					} else if (operator.getKey().equals("in")) {
						if (insideNot) {
							notProperties.computeIfAbsent(entry.getKey(), key -> new ArrayList<>()).addAll((List<Object>) operator.getValue());
						} else {
							properties.computeIfAbsent(entry.getKey(), key -> new ArrayList<>()).addAll((List<Object>) operator.getValue());
						}
					} else if (operator.getKey().equals("gte")) {
						if (insideNot) {
							throw new IllegalArgumentException("parsing ranges inside _not is currently not supported");
						}
						rangeProperties.put(entry.getKey(), mergeRangePair(
								Optional.ofNullable(rangeProperties.get(entry.getKey())).orElse(Pair.of(null, null)),
								Pair.of(null, ((Number) operator.getValue()).longValue())));
					} else if (operator.getKey().equals("lte")) {
						if (insideNot) {
							throw new IllegalArgumentException("parsing ranges inside _not is currently not supported");
						}
						rangeProperties.put(entry.getKey(), mergeRangePair(
								Optional.ofNullable(rangeProperties.get(entry.getKey())).orElse(Pair.of(null, null)),
								Pair.of(((Number) operator.getValue()).longValue(), null)));
					} else {
						throw new IllegalArgumentException("operator " + operator.getKey() + " is currently not supported");
					}
				}
			}
		}
	}

	private Pair<Long, Long> mergeRangePair(final Pair<Long, Long> existing, final Pair<Long, Long> next) {
		final Long newLower;
		if (existing.getLeft() == null) {
			newLower = next.getLeft();
		} else if (next.getLeft() == null) {
			newLower = existing.getLeft();
		} else {
			newLower = Math.min(existing.getLeft(), next.getLeft());
		}

		final Long newUpper;
		if (existing.getLeft() == null) {
			newUpper = next.getRight();
		} else if (next.getLeft() == null) {
			newUpper = existing.getRight();
		} else {
			newUpper = Math.max(existing.getRight(), next.getRight());
		}

		return Pair.of(newLower, newUpper);
	}

	private List<Object> translatePropertiesToQuery(final String indexName, final Long projectId, final Map<String, List<Object>> properties) {
		final List<Object> queries = new ArrayList<>();
		final IndexStatus indexStatus = indexStatusMap.get(IndexNameUtil.buildIndexName(indexName, projectId));	
		if (indexStatus == null) {
			throw new IllegalStateException("No index found for indexName = " + indexName + " and projectId = " + projectId);
		}
		
		final List<MiningDataPointDefinitionWithPath> dataPoints = indexStatus.getDataPoints();
		final String queryName = indexDefinitions.get(indexName).getQueryName();
		for (final Map.Entry<String, List<Object>> property : properties.entrySet()) {
			final MiningDataPointDefinitionWithPath dataPoint = dataPoints.stream()
					.filter(dp -> dp.getPath().equals(property.getKey().replace("_", ".")))
					.findFirst()
					.orElseThrow(() -> new IllegalArgumentException("No data point definition for property " + property.getKey()));

			//TODO: this should not be flags - it should be its own operator
			final boolean isBeginsWith = hasFlag(dataPoint, queryName, "beginsWith");
			final boolean isEndsWith = hasFlag(dataPoint, queryName, "endsWith");

			if (isBeginsWith || isEndsWith) {
				if (property.getValue().size() > 1) {
					throw new IllegalArgumentException("can not handle more than 1 value for property with wildcard matching");
				}
				String value = property.getValue().get(0).toString();
				if (isBeginsWith) {
					value = value + "*";
				}
				if (isEndsWith) {
					value = "*" + value;
				}
				queries.add(Map.of("wildcard", Map.of(property.getKey(), value)));
			} else if (property.getValue().size() == 1) {
				queries.add(Map.of("term", Map.of(property.getKey(), property.getValue().get(0))));
			} else {
				queries.add(Map.of("terms", Map.of(property.getKey(), property.getValue())));
			}
		}
		return queries;
	}

	private boolean hasFlag(final MiningDataPointDefinitionWithPath dp, final String queryName, final String flag) {
		final Map<String, String> attributeMap = dp.getUsageAttributes().get("graphql.query." + queryName);
		if (attributeMap == null) {
			return false;
		}
		final String sqlFragmentEqFlags = attributeMap.get("sqlFragmentEqFlags");
		if (sqlFragmentEqFlags == null) {
			return false;
		}
		return sqlFragmentEqFlags.contains(flag);
	}

	/**
	 * Returns the correct AggregationResult
	 *
	 * @param indexName The name of the index
	 * @param projectId The id of the project
	 * @param aggregationRequest The aggregation request
	 * @return The correct AggregationResult
	 */
	public Flux<AggregationResultString> queryIndex(final String indexName, final Long projectId, final AggregationRequestString aggregationRequest) {
		if ( ! aggregationRequest.getFilter().isEmpty()) {
			throw new IllegalStateException("Filter not yet supported with opensearch");
		}

		final SearchRequest srq = buildSearchRequest(indexName, aggregationRequest, projectId);

		try {
			final SearchResponse<AggregationResultString> srp = openSearchClient.search(srq, AggregationResultString.class);
			return parseToAggregationsResult(aggregationRequest, srp);
		} catch (final OpenSearchException | IOException e) {
			throw new IllegalStateException(e);
		}
	}

	/**
	 * Parses the results of the aggregation to a AggregationResult
	 * 
	 * @param aggregationRequest The original request
	 * @param srp The results returned from the server
	 * @return The parsed AggregationResult
	 */
	private Flux<AggregationResultString> parseToAggregationsResult(final AggregationRequestString aggregationRequest, final SearchResponse<AggregationResultString> srp) {
		final List<CompositeBucket> bucketList = srp.aggregations().get("agg").composite().buckets().array();
		final List<AggregationIndexResult> aggregationIndexResults = new ArrayList<>();
		final List<AggregationResultString> aggregatedResults = new ArrayList<>();

		for(final CompositeBucket bucket : bucketList) {
			final Map<String, Object> keyMap = new HashMap<>();
			final Map<String, Object> operatorMap = new HashMap<>();
			for(final Entry<String, JsonData> entry : bucket.key().entrySet()) {
				keyMap.put(entry.getKey(), entry.getValue().toString().replace("\"", "").toUpperCase());
			}

			for(final Entry<String, Aggregate> entry : bucket.aggregations().entrySet()) {
				operatorMap.put(aggregationRequest.getFields().entrySet().iterator().next().getKey(), castToCorrectAggregate(entry.getValue()));
			}

			aggregationIndexResults.add(new AggregationIndexResult(keyMap, operatorMap));
		}

		sortAccordingToRequest(aggregationIndexResults);

		for(final AggregationIndexResult aggregationIndexResult : aggregationIndexResults) {
			aggregatedResults.add(new AggregationResultString(aggregationIndexResult.getKeyMap(), aggregationIndexResult.getAggregationMap()));
		}

		return Flux.fromIterable(aggregatedResults);
	}

	/**
	 * Sorts the values of the result
	 *
	 * @param aggregationIndexResults The result
	 * @return The ordered list of results
	 */
	private void sortAccordingToRequest(final List<AggregationIndexResult> aggregationIndexResults) {
		aggregationIndexResults.sort((o1, o2) -> {
			for (final Entry<String, Object> entry : o1.getAggregationMap().entrySet()) {
				if (o2.getAggregationMap().containsKey(entry.getKey())) {
					final Object value1 = entry.getValue();
					final Object value2 = o2.getAggregationMap().get(entry.getKey());

					if (value1 instanceof Number && value2 instanceof Number) {
						@SuppressWarnings("boxing")
						final
						double doubleValue1 = (Double) value1;
						@SuppressWarnings("boxing")
						final
						double doubleValue2 = (Double) value2;

						if (doubleValue1 > doubleValue2) {
							return 1;
						} else if (doubleValue1 < doubleValue2) {
							return -1;
						}
					}
				}
			}
			return 0;
		});
	}

	/**
	 * Constructs the aggregation for the given aggregationrequest
	 *
	 * @param indexName The name of the index
	 * @param aggregationRequest The aggregation request
	 * @param projectId The project id
	 * @return Returns a submittable SearchRequest
	 */
	private SearchRequest buildSearchRequest(final String indexName, final AggregationRequestString aggregationRequest, final Long projectId) {
		final SearchRequest.Builder srqb = new SearchRequest.Builder();
		srqb.index(indexName + "_project_" + projectId);
		srqb.size(Integer.valueOf(0));

		final Aggregation aggregation = new Aggregation.Builder()
				.composite(getCompositeAggregation(indexName, aggregationRequest))
				.aggregations("operations", getOperationAggregation(indexName, aggregationRequest).get(0))
				.build();

		srqb.aggregations("agg", aggregation);

		return srqb.build();
	}

	/**
	 * The composite aggregation is used to aggregate all the given fields in the aggregationrequest
	 *
	 * @param aggregationRequest The aggregationrequest to parse
	 * @return The composite aggregation
	 */
	private CompositeAggregation getCompositeAggregation(final String indexName, final AggregationRequestString aggregationRequest) {
		final CompositeAggregation.Builder compositeAggBuilder = AggregationBuilders.composite();

		compositeAggBuilder.size(Integer.valueOf(10000));

		final List<Map<String, CompositeAggregationSource>> casList = new ArrayList<>();

		for (final String s : aggregationRequest.getGroupBy()) {
			casList.add(Map.of(s, new CompositeAggregationSource.Builder()
					.terms(new TermsAggregation.Builder().field(mapField(indexName, s)).build()).build()));
		}

		compositeAggBuilder.sources(casList);

		return compositeAggBuilder.build();
	}

	/**
	 * Returns all operator aggregations
	 *
	 * @param aggregationRequest The requested aggregation to translate
	 * @return Returns all operator aggregations
	 */
	private List<Aggregation> getOperationAggregation(final String indexName, final AggregationRequestString aggregationRequest) {
		final List<Aggregation> allOperators = new ArrayList<>();
		for (final Map.Entry<String, AggregationOperator> entry: aggregationRequest.getFields().entrySet()) {
			allOperators.add(setCorrectAggBuilder(indexName, entry));
		}
		return allOperators;
	}

	/**
	 * Sets the correct aggregator depending on the operation specified in entry
	 * 
	 * @param entry The entry to map from
	 * @return The correct aggregator
	 */
	private Aggregation setCorrectAggBuilder(final String indexName, final Entry<String, AggregationOperator> entry) {
		switch (entry.getValue()) {
			case SUM:
				final SumAggregation.Builder sab = new SumAggregation.Builder();
				sab.field(mapField(indexName, entry.getKey()));
				return new Aggregation.Builder().sum(sab.build()).build();
			case COUNT:
				final ValueCountAggregation.Builder cab = new ValueCountAggregation.Builder();
				cab.field(entry.getKey());
				return new Aggregation.Builder().valueCount(cab.build()).build();
			case AVG:
				break;
			case COUNT_DISTINCT:
				break;
			case LIST:
				break;
			case MAX:
				break;
			case MEDIAN:
				break;
			case MIN:
				break;
			case MODE:
				break;
			case PERCENTILE_70:
				break;
			case PERCENTILE_80:
				break;
			case PERCENTILE_90:
				break;
			case PERCENTILE_95:
				break;
			case PERCENTILE_99:
				break;
			case STDDEV:
				break;
			case VARIANCE:
				break;
			default:
				break;

		}
		return null;
	}

	/**
	 * Translates between the openSearch mapping and the mining internal mapping
	 *
	 * @param s The field
	 * @return The corresponding openSearch mapping
	 */
	private String mapField(final String indexName, final String s) {
	
		if (moduleIndex.equals(indexName)) {
			switch(s) {
				case "PHYSICAL_LINES_OF_CODE":
					return "content_metrics_codeLines";

				case "TECHNOLOGY":
					return "content_objectTypeLink_technologyLink";

				case "TYPE":
					return "content_objectTypeLink_typeLink";

				case "LINES_OF_COMMENT":
					return "content_metrics_commentLines";

				case "LINES_OF_DEAD_CODE":
					return "content_metrics_deadCodeLines";

				case "ERRORS":
					return "content_errorCount";
				default:
			}
		} else if (annotationIndex.equals(indexName)) {
			switch(s) {
				case "CATEGORY":
					return "content_categoryLink_name";

				case "STATE":
					return "content_stateLink";

				case "TYPE":
					return "content_typeLink";
					
				case "MODULE_TECHNOLOGY":
					return "content_inHasAnnotation_out_objectTypeLink_technologyLink";

				case "MODULE_TYPE":
					return "content_inHasAnnotation_out_objectTypeLink_typeLink";
				default:
			}
		}
		
		return "field";
	}

	/**
	 * Helper method to access the numerical value of an aggregate
	 *
	 * @param resultAgg The result aggregate
	 * @return The numerical value
	 */
	private Object castToCorrectAggregate(final Aggregate resultAgg) {
		switch (resultAgg._kind().jsonValue()) {
			case "sum":
				return Double.valueOf(resultAgg.sum().value());
			case "avg":
				return Double.valueOf(resultAgg.avg().value());
			case "min":
				return Double.valueOf(resultAgg.min().value());
			case "max":
				return Double.valueOf(resultAgg.max().value());
			case "value_count":
				return Double.valueOf(resultAgg.valueCount().value());
			default:
				return null;
		}
	}

	public Flux<MatchedSearchHit> textSearch(final String indexName, final Long projectId, final String searchPhrase, final boolean caseSensitive) {
		final String combinedName = IndexNameUtil.buildIndexName(indexName, projectId);
		final IndexDefinition indexDefinition = indexDefinitions.get(indexName);
		final List<String> textDataPoints = indexDefinition.getDataPoints()
				.stream()
				.filter(dataPoint -> "text".equals(dataPoint.getType()))
				.map(dataPoint -> dataPoint.getPath().replace(".", "_"))
				.collect(Collectors.toList());
		
		final MultiMatchQuery multiMatchQuery = new MultiMatchQuery.Builder().fields(textDataPoints).query(searchPhrase).build();
		
		final SearchRequest searchRequest = new SearchRequest.Builder().index(List.of(combinedName))
				.query(new Query.Builder().multiMatch(multiMatchQuery).build())
				.size(Integer.valueOf(10000))
				.build();

		try {
			return handleResult(openSearchClient.search(searchRequest, Map.class), textDataPoints, searchPhrase, caseSensitive);
		} catch (final OpenSearchException | IOException e) {
			throw new IllegalStateException(e);
		}
	}

	@SuppressWarnings("unchecked")
	private Flux<MatchedSearchHit> handleResult(final SearchResponse<Map> searchResponse, final List<String> textDataPoints, final String searchPhrase,
			final boolean caseSensitive) {
		final ArrayList<MatchedSearchHit> matchedSearchHits = new ArrayList<>();
		for (final Hit h : searchResponse.hits().hits()) {
			final Map<String, String> hitSourceMap = (Map<String, String>) h.source();
			final List<Integer> occurencesList = findAllMatches(hitSourceMap, textDataPoints, searchPhrase, caseSensitive);
			/* Since openSearch does not support a caseSensitive-search we need to manually filter out modules depending on {caseSensitive} */
			if ( ! occurencesList.isEmpty()) {				
				matchedSearchHits.add(new MatchedSearchHit(occurencesList, String.valueOf(hitSourceMap.getOrDefault("id", h.id())), searchPhrase));
			}
		}
		return Flux.fromIterable(matchedSearchHits);
		
	}

	public List<Integer> findAllMatches(final Map<String, String> map, final List<String> textDataPoints, String searchString, final boolean caseSensitive) {
		final List<Integer> occurrences = new ArrayList<>();

		for(final String dataPoint : textDataPoints) {
			String sourceString = map.get(dataPoint);
			if ( ! caseSensitive) {
				sourceString = sourceString.toLowerCase();
				searchString = searchString.toLowerCase();
			}
			int index = sourceString.indexOf(searchString);

			while (index != -1) {
				occurrences.add(Integer.valueOf(index));
				index = sourceString.indexOf(searchString, index + 1);
			}
		}
		
		return occurrences;
	}
}

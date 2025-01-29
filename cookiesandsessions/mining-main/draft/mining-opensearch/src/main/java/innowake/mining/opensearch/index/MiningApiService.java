package innowake.mining.opensearch.index;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.opensearch.config.MiningApiConfiguration;
import innowake.mining.opensearch.graphql.DataPointQueryResult;
import innowake.mining.opensearch.graphql.GraphQlUtil;
import innowake.mining.opensearch.index.model.IndexDefinition;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinitionWithPath;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.graphql.client.ClientGraphQlResponse;
import org.springframework.graphql.client.GraphQlClient;
import org.springframework.graphql.client.HttpGraphQlClient;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Service providing access to the REST and GraphQL APIs of mining.
 */
@Service
public class MiningApiService {

    private static final Logger LOG = LoggerFactory.getLogger(MiningApiService.class);

    private static final String DATAPOINTS_FOR_TYPE_ENDPOINT = "v2/projects/%d/datapoints/for-type/%s?usages=%s";
    private static final String GRAPHQL_ENDPOINT = "v2/graphql";

    private static final MiningDataPointDefinitionWithPath SIZE_DATAPOINT = new MiningDataPointDefinitionWithPath(
            new MiningDataPointDefinition("numberOfElements", "dontCare", MiningDataPointDefinition.ScalarType.INT, false, false),
            "numberOfElements"
    );
    private static final MiningDataPointDefinitionWithPath TOTAL_PAGES_DATAPOINT = new MiningDataPointDefinitionWithPath(
            new MiningDataPointDefinition("totalPages", "dontCare", MiningDataPointDefinition.ScalarType.INT, false, false),
            "totalPages"
    );

    @Value("${mining-opensearch.mining.url:}")
    private String configuredBaseUrl;

    private String discoveredBaseUrl = "";
    
    @Autowired(required = false)
    @Qualifier("mining")
    @Nullable
    private WebClient miningWebClient;

    public String getBaseUrl() {
        if ( ! StringUtils.isEmpty(configuredBaseUrl)) {
            return configuredBaseUrl;
        } 
        throw new IllegalStateException("mining service not available");
    }

    public WebClient getWebClient() {
        if (miningWebClient != null) {
            return miningWebClient;
        } 
        throw new IllegalStateException("mining service not available");
    }

    public GraphQlClient getGraphQlClient() {
        return HttpGraphQlClient.builder(getWebClient())
                .url(getBaseUrl() + GRAPHQL_ENDPOINT)
                .build();
    }

    private GraphQlClient getGraphQlClient(final Long projectId) {
        return HttpGraphQlClient.builder(getWebClient())
                .url(getBaseUrl() + GRAPHQL_ENDPOINT + "?projectId=" + projectId)
                .build();
    }

    public List<MiningDataPointDefinitionWithPath> getDataPointsForQueryWithUsage(final String rootType, final String usage, final Long projectId) {
    	return getWebClient()
    			.get()
    			.uri(String.format(DATAPOINTS_FOR_TYPE_ENDPOINT, projectId, rootType, usage))
    			.exchangeToMono(response ->
    				response.toEntityList(MiningDataPointDefinitionWithPath.class))
    			.block()
    			.getBody();
    }

	public DataPointQueryResult queryDataPoints(final IndexDefinition indexDefinition, final Long projectId, @Nullable final Long docId, 
			final List<MiningDataPointDefinitionWithPath> dataPoints, final int pageNumber, final int pageSize) {

		final String queryName = indexDefinition.getQueryName();
        final GraphQlUtil.QueryParameters queryParameters;
        
        if (docId == null) {
        	queryParameters = new GraphQlUtil.QueryParameters(
                    Map.of("projectId", projectId,
                           "page", pageNumber,
                           "size", pageSize,
                            /* Important: prevent mining from using the OpenSearch index that we are currently trying to rebuild */
                           "useOpenSearch", false),
                    Map.of("projectId", "Long",
                           "page", "Int",
                           "size", "Int",
                            "useOpenSearch", "Boolean")
            );
        } else {
        	final Map<String, Object> filterObject = new HashMap<>();
    		final Map<String, Object> filterOperator = new HashMap<>();
    		
    		filterOperator.put("eq", docId);
    		filterObject.put(indexDefinition.getId().replace(".", "_"), filterOperator);
    		
        	queryParameters = new GraphQlUtil.QueryParameters(
                    Map.of("projectId", projectId,
                           "page", pageNumber,
                           "size", pageSize,
                           "filterObject", filterObject,
                            /* Important: prevent mining from using the OpenSearch index that we are currently trying to rebuild */
                            "useOpenSearch", false),
                    Map.of("projectId", "Long",
                           "page", "Int",
                           "size", "Int",
                           "filterObject", "FilterObject_" + queryName,
                            "useOpenSearch", "Boolean")
            );
        }

        final List<MiningDataPointDefinitionWithPath> dataPointsWithPage = new ArrayList<>(dataPoints);
        dataPointsWithPage.add(SIZE_DATAPOINT);
        dataPointsWithPage.add(TOTAL_PAGES_DATAPOINT);

        final String graphQlQuery = GraphQlUtil.getGraphQlQuery(indexDefinition.getQueryName(), queryParameters, dataPointsWithPage);

        try {
        	final ClientGraphQlResponse response = getGraphQlClient(projectId)
                    .document(graphQlQuery)
                    .variables(queryParameters.toParameterMap())
                    .execute()
                    .block();
            return new DataPointQueryResult(Collections.emptyList(), response.field(queryName).toEntity(Map.class));
        } catch (final Exception e) {
        	throw new IllegalStateException("Error retrieving the datapoints from the mining-api-server. Error was: " + e);
        }
    }
    
	public DataPointQueryResult queryspecificDataPoints(final String queryName, final Long projectId, final Long docId,
			final List<MiningDataPointDefinitionWithPath> dataPoints, final int pageNumber, final int pageSize) {
		
		final Map<String, Object> filterObject = new HashMap<>();
		final Map<String, Object> filterOperator = new HashMap<>();
		
		filterOperator.put("eq", docId);
		filterObject.put("content_id", filterOperator);
		
    	final GraphQlUtil.QueryParameters queryParameters = new GraphQlUtil.QueryParameters(
    			Map.of("projectId", projectId,
    					"page", pageNumber,
    					"size", pageSize,
    					"filterObject", filterObject,
    					/* Important: prevent mining from using the OpenSearch index that we are currently trying to rebuild */
    					"useOpenSearch", false),
    			Map.of("projectId", "Long",
    					"page", "Int",
    					"size", "Int",
    					"filterObject", "FilterObject_annotations",
    					"useOpenSearch", "Boolean")
    			);

    	final List<MiningDataPointDefinitionWithPath> dataPointsWithPage = new ArrayList<>(dataPoints);
    	dataPointsWithPage.add(SIZE_DATAPOINT);
    	dataPointsWithPage.add(TOTAL_PAGES_DATAPOINT);

    	final String graphQlQuery = GraphQlUtil.getGraphQlQuery(queryName, queryParameters, dataPointsWithPage);

    	try {
    		final ClientGraphQlResponse response = getGraphQlClient(projectId)
        			.document(graphQlQuery)
        			.variables(queryParameters.toParameterMap())
        			.execute()
        			.block();
        	return new DataPointQueryResult(Collections.emptyList(), response.field(queryName).toEntity(Map.class));
    	} catch (final Exception e) {
        	throw new IllegalStateException("Error retrieving the datapoints from the mining-api-server. Error was: " + e);
        }
    }
}

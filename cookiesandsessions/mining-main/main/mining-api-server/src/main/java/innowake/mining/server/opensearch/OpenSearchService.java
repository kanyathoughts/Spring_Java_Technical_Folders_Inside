/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.opensearch;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.apache.tinkerpop.shaded.minlog.Log;
import org.springframework.boot.web.client.RootUriTemplateHandler;
import org.springframework.context.event.EventListener;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.cache.invalidation.AnnotationsChangeHandler;
import innowake.mining.server.event.AnnotationEvent;
import innowake.mining.server.event.CustomPropertiesModifiedEvent;
import innowake.mining.server.event.ModulesModifiedEvent;
import innowake.mining.server.event.ProjectDeletedEvent;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.model.AnnotationFieldName;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;

/**
 * Service for performing searching and sorting via mining-opensearch.
 */
@Service
@Component
public class OpenSearchService {

    private final ObjectMapper objectMapper;

    @Nullable
    private RestTemplate restTemplate;
    
    private static final Logger LOG = LoggerFactory.getLogger(AnnotationsChangeHandler.class);

	private static final String annotationIndex = "annotationtable";
	private static final String moduleIndex = "moduletable";

    public OpenSearchService(final ObjectMapper objectMapper, @Nullable final String baseUrl) {
        this.objectMapper = objectMapper;
        initRestTemplate(baseUrl);
    }

    @EventListener
    public void onOpenSearchAvailability(final OpenSearchAvailabilityEvent event) {
        initRestTemplate(event.getBaseUrl());
    }

    private void initRestTemplate(final String baseUrl) {
        if (baseUrl == null) {
            restTemplate = null;
        } else {
            restTemplate = new RestTemplate();
            restTemplate.setUriTemplateHandler(new RootUriTemplateHandler(baseUrl));
        }
    }

    /**
     * Returns whether (to the best of our knowledge) the mining-opensearch service is currently available and can be contacted. Requests may still fail,
     * so callers should still be prepared to catch and handle exceptions when calling {@link #queryIndex(String, Long, int, int, Map, List)}.
     * @return {@code true} if the service is available and {@link #queryIndex(String, Long, int, int, Map, List)} can be called.
     */
    public boolean isAvailable() {
    	if (restTemplate == null) {
             return false;
         }
         final UriComponentsBuilder builder = UriComponentsBuilder.fromPath(String.format("/api/healthcheck"));
         try {
             return Optional.ofNullable(assertNotNull(restTemplate).exchange(builder.toUriString(), HttpMethod.GET, null,
                     new ParameterizedTypeReference<Boolean>() {})
                 .getBody()).orElse(Boolean.FALSE);
         } catch (final Exception e) {
        	 return false;
         }
    }
    
    /**
     * Query an index in mining-opensearch. This method will return a page of ids for whatever entity the index is for (e.g. page of module ids).
     * <p>
     * As mining-opensearch is an external system which may not always be available, callers should be prepared to handle exceptions thrown from this method
     * and if possible fall back to a different filter or sort method.
     *
     * @param indexName name of the index that shall be queried
     * @param projectId id of the project for which the index shall be queried
     * @param page page number of paged data that shall be retrieved (first page is 0)
     * @param size number of items to return per page
     * @param filterObject filter object to use for filtering the results, or {@code null} to apply no filtering
     * @param sortObject list of sort objects that determine the ordering of results, or {@code null} to apply no ordering
     * @return a page of entity ids that match the filter and sort criteria
     */
    public Paged<String> queryIndex(final String indexName, final Long projectId,
                                   final int page,
                                   final int size,
                                   @Nullable final Map<String, Object> filterObject,
                                   @Nullable final List<Map<String, String>> sortObject) {
        if (restTemplate == null) {
            throw new IllegalStateException("The mining-opensearch service is not available. It means that no fixed URL to the service" +
                    " was configured via the mining-opensearch.url configuration parameter");
        }
        final UriComponentsBuilder builder = UriComponentsBuilder.fromPath(String.format("/api/indexes/%s/project/%d/query", indexName, projectId));
        try {
            builder.queryParam("page", page);
            builder.queryParam("size", size);
            if (filterObject != null) {
                builder.queryParam("filterObject", objectMapper.writeValueAsString(filterObject));
            }
            if (sortObject != null) {
                builder.queryParam("sortObject", objectMapper.writeValueAsString(sortObject));
            }
        } catch (final JsonProcessingException e) {
            throw new IllegalStateException(e);
        }

        return assertNotNull(restTemplate).exchange(builder.toUriString(), HttpMethod.GET, null, new ParameterizedTypeReference<JsonPageImpl<String>>() {})
                .getBody();
    }

    /**
     * Query an aggregation in mining-opensearch. This method will return an aggregation result for whatever entity the index is for (e.g. page of module ids).
     * <p>
     * As mining-opensearch is an external system which may not always be available, callers should be prepared to handle exceptions thrown from this method
     * and if possible fall back to a different filter or sort method.
     *
     * @param indexName name of the index that shall be queried
     * @param projectId id of the project for which the index shall be queried
     * @param aggregationRequest aggregation object to use for aggregating the results
     * @return aggregation response
     */
    public List<AggregationResult<String>> queryAggregations(final String indexName, final Long projectId, final AggregationRequest<?> aggregationRequest) {
        if (restTemplate == null) {
            throw new IllegalStateException("The mining-opensearch service is not available. It means that no fixed URL to the service" +
            		" was configured via the mining-opensearch.url configuration parameter");
        }
        final UriComponentsBuilder builder = UriComponentsBuilder.fromPath(String.format("/api/indexes/%s/project/%d/queryAggregations", indexName, projectId));
        try {
			builder.queryParam("aggregationRequest", objectMapper.writeValueAsString(aggregationRequest));
		} catch (final JsonProcessingException e) {
			throw new IllegalStateException(e);
		}

        return assertNotNull(restTemplate).exchange(builder.toUriString(), HttpMethod.GET, null,
                    new ParameterizedTypeReference<List<AggregationResult<String>>>() {})
                .getBody();
    }

    /**
     * Building an index in open search. This method will build an index in open search using the datapoints and 
     * then load the data into the open search fetched using the graphql queries. 
     * <p>
     * As mining-opensearch is an external system which may not always be available, callers should be prepared to handle exceptions thrown from this method
     * and if possible fall back to a different filter or sort method.
     * 
     * @param indexName name of the index that shall be rescanned
     * @param projectId id of the project for which the index shall be rescanned
     * @param entityId id of the specific entity for which the index shall be rescanned
     */
    public void rescanIndex(final String indexName, final Long projectId, @Nullable final Long entityId) {
        if (restTemplate == null) {
            throw new IllegalStateException("The mining-opensearch service is not available. It means that no fixed URL to the service" +
            		" was configured via the mining-opensearch.url configuration parameter");
        }
        
        if (entityId == null) {
        	Log.info("Rescanning the whole " + indexName + "-openSearchIndex since multiple entities changed");
        } else {
        	Log.debug("Entity " + entityId + " changed, updating the current OpenSearch index");
        }
        
        final HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
		final UriComponentsBuilder builder = UriComponentsBuilder
				.fromPath(String.format("/api/indexes/%s/project/%d/rescan", indexName, projectId));
		
        builder.queryParam("docId", entityId);
        
        final ParameterizedTypeReference<Map<String, String>> response = new ParameterizedTypeReference<>() {};
		final ResponseEntity<Map<String, String>> exchange = 
				assertNotNull(restTemplate).exchange(builder.toUriString(), HttpMethod.POST, new HttpEntity<>(null, headers), response);

		if ( ! exchange.getStatusCode().equals(HttpStatus.OK)) {
			throw new IllegalStateException(
					String.format("Open search index rescan failed with status code %s and reason %s", exchange.getStatusCode(), exchange.getBody()));
		}
    }
 
    /**
     * Deleting an index in open search. This method will delete an index in open search using the datapoints along with the loaded data in open search.
     * <p>
     * As mining-opensearch is an external system which may not always be available, callers should be prepared to handle exceptions thrown from this method
     * and if possible fall back to a different filter or sort method.
     * 
     * @param indexName name of the index that shall be rescanned
     * @param projectId id of the project for which the index shall be rescanned
     */
    public void deleteIndex(final String indexName, final Long projectId) {
        if (restTemplate == null) {
            throw new IllegalStateException("The mining-opensearch service is not available. It means that no fixed URL to the service" +
            		" was configured via the mining-opensearch.url configuration parameter");
        }
        
        Log.info("Deleting the " + indexName + "-openSearchIndex on project " + projectId);
        
        final HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
		final UriComponentsBuilder builder = UriComponentsBuilder
				.fromPath(String.format("/api/indexes/%s/project/%d", indexName, projectId));
        
		final ParameterizedTypeReference<Map<String, String>> response = new ParameterizedTypeReference<>() {};
		final ResponseEntity<Map<String, String>> exchange = 
				assertNotNull(restTemplate).exchange(builder.toUriString(), HttpMethod.DELETE, new HttpEntity<>(null, headers), response);
        
		if ( ! exchange.getStatusCode().equals(HttpStatus.OK)) {
			throw new IllegalStateException(
					String.format("Open search index delete failed with status code %s and reason %s", exchange.getStatusCode(), exchange.getBody()));
		}
    }
    
    /* EVENT LISTENERS */
    /**
	 * Event listener that is notified when annotations are modified. If this 
	 * event is triggered and OpenSearch service is available, 
	 * it leads to rescanning of all/specific document in open search </p>
	 *
	 * @param event the {@code AnnotationsModifiedEvent}
	 */
	@Async
	@EventListener(condition="@openSearchService.isAvailable()")
	public void onAnnotationsModified(final AnnotationEvent event) {
		LOG.debug("Recalculating the \"annotationtable\" OpenSearch index because one or more Annotation(s) are modified");
		final EntityId projectId = event.getProjectId().orElseThrow(() -> new IllegalStateException("OpenSearch got notified of a annotation change,"
				+ " but no projectId was sent along with it, so index to update is unknown"));
		final Optional<EntityId> annotationId = event.getAnnotationId();
		rescanIndex(annotationIndex, projectId.getNid(), annotationId.map(a -> a.getNid()).orElse(null));
	}

	/**
	 * Event listener that is notified when modules are modified. If this 
	 * event is triggered and OpenSearch service is available, 
	 * it leads to rescanning of all/specific document in open search </p>
	 *
	 * @param event the {@code ModulesModifiedEvent}
	 */
	@Async
	@EventListener(condition="@openSearchService.isAvailable()")
	public void onModulesModified(final ModulesModifiedEvent event) {
		LOG.debug("Recalculating the \"moduletable\" OpenSearch index because one or more Module(s) are modified");
		final EntityId projectId = event.getProjectId().orElseThrow(() -> new IllegalStateException("OpenSearch got notified of a module change,"
				+ " but no projectId was sent along with it, so index to update is unknown"));
		final Optional<EntityId> moduleId = event.getModuleId();
		rescanIndex(moduleIndex, projectId.getNid(), moduleId.map(a -> a.getNid()).orElse(null));
	}

	/**
	 * Event listener that is notified when custom properties are modified.
	 * 
	 * @param event the {@code CustomPropertiesModifiedEvent}
	 */
	@Async
	@EventListener(condition="@openSearchService.isAvailable()")
	public void onCustomPropertiesModified(final CustomPropertiesModifiedEvent event) {
		final EntityId projectId = event.getProjectId().orElseThrow(() -> new IllegalStateException("OpenSearch got notified of a custom property change,"
				+ " but no projectId was sent along with it, so index to update is unknown"));
		rescanIndex(annotationIndex, projectId.getNid(), null);
		rescanIndex(moduleIndex, projectId.getNid(), null);
	}
	
	/**
	 * Event listener that is notified when a project is deleted.
	 * 
	 * @param event the {@link ProjectDeletedEvent}
	 */
	@Async
	@EventListener(condition="@openSearchService.isAvailable()")
	public void onProjectDeletion(final ProjectDeletedEvent event) {
		final EntityId projectId = event.getProjectId().orElseThrow(() -> new IllegalStateException("OpenSearch got notified of a project deletion,"
				+ " but no projectId was sent along with it, so index to update is unknown"));
		deleteIndex(annotationIndex, projectId.getNid());
		deleteIndex(moduleIndex, projectId.getNid());
	}

	/**
	 * Returns the parsed AggregationResult
	 *
	 * @param indexName The name of the index
	 * @param projectId The id of the project
	 * @param aggregationRequest The AggregationRequest to process
	 * @return Returns the AggregationResult
	 * @throws IllegalStateException thrown if an error happened during the parsing
	 */
	public List<AggregationResult<AnnotationFieldName>> getAggregatedValues(final String indexName, final Long projectId,
			final AggregationRequest<AnnotationFieldName> aggregationRequest) throws IllegalStateException {
		try {
			final List<AggregationResult<String>> aggregationResults = queryAggregations(indexName, projectId,
					aggregationRequest);
			final List<AggregationResult<AnnotationFieldName>> parsedAggregationResults = new ArrayList<>();
			for (final AggregationResult<String> ar : aggregationResults) {
				final AggregationResult<AnnotationFieldName> par = new AggregationResult<AnnotationFieldName>();
				ar.getFields().forEach((k, v) -> {
					par.getFields().put(AnnotationFieldName.valueOf(k), v);
				});

				ar.getGroup().forEach((k,v) -> {
					par.getGroup().put(AnnotationFieldName.valueOf(k), v);
				});
				parsedAggregationResults.add(par);
			}

			return parsedAggregationResults;
			
		} catch (final Exception e) {
			throw new IllegalStateException("Unable to parsed aggregated values in OpenSearch. Error: " + e);
		}
	}
}

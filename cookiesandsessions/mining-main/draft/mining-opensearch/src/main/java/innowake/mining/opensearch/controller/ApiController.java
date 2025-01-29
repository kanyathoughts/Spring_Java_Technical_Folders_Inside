package innowake.mining.opensearch.controller;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.opensearch.controller.model.aggregations.AggregationRequestString;
import innowake.mining.opensearch.controller.model.aggregations.AggregationResultString;
import innowake.mining.opensearch.index.IndexService;
import innowake.mining.opensearch.index.OpenSearchApiService;
import innowake.mining.opensearch.index.model.IndexStatus;
import innowake.mining.opensearch.index.model.MatchedSearchHit;

import org.springframework.data.domain.Page;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/api")
public class ApiController {

	private final IndexService indexService;
	private final ObjectMapper objectMapper;

	public ApiController(final IndexService indexService, final ObjectMapper objectMapper) {
		this.indexService = indexService;
		this.objectMapper = objectMapper;
	}
	
	@GetMapping("/healthcheck")
	public Mono<Boolean> getHealthCheck() {
		return Mono.just(Boolean.TRUE);
	}

	@GetMapping("/indexes")
	public Flux<IndexStatus> getAllIndexStatuses() {
		return Flux.fromIterable(indexService.getIndexStatus().values());
	}

	@GetMapping("/indexes/{indexName}")
	public Flux<IndexStatus> getIndexStatusesForIndex(@PathVariable final String indexName) {
		return Flux.fromIterable(indexService.getIndexStatus().values()).filter(indexStatus -> indexStatus.getIndexName().equals(indexName));
	}

	@GetMapping("/indexes/{indexName}/project/{projectId}")
	public Mono<IndexStatus> getIndexStatusForProject(@PathVariable final String indexName, @PathVariable final Long projectId) {
		return Mono.justOrEmpty(indexService.getIndexStatus().values().stream()
				.filter(indexStatus -> indexStatus.getIndexName().equals(indexName) && indexStatus.getProjectId().equals(projectId))
				.findAny());
	}

    @PostMapping("/indexes/{indexName}/project/{projectId}/rescan")
	public Mono<IndexStatus> rescanIndex(@PathVariable final String indexName, @PathVariable final Long projectId,
			@RequestParam(required = false) final Long docId) {
        return Mono.defer(() -> {
            indexService.rescanIndex(indexName, projectId, docId);
            return Mono.<IndexStatus>empty();
        }).subscribeOn(Schedulers.boundedElastic());
    }

	@DeleteMapping("/indexes/{indexName}/project/{projectId}")
	public Mono<Void> deleteIndex(@PathVariable final String indexName, @PathVariable final Long projectId) {
		return indexService.deleteIndex(indexName, projectId);
	}

	@SuppressWarnings("unchecked")
	@GetMapping(value = "/indexes/{indexName}/project/{projectId}/query")
	public Mono<Page<String>> queryIndex(@PathVariable final String indexName, @PathVariable final Long projectId,
										 @RequestParam(defaultValue = "0") final int page,
										 @RequestParam(defaultValue = "0") final int size,
										 @RequestParam(required = false) @Nullable final String filterObject,
										 @RequestParam(required = false) @Nullable final String sortObject) throws JsonProcessingException {

		final Map<String, Object> filterObjectParsed = filterObject == null
				? null
				: objectMapper.readValue(URLDecoder.decode(filterObject, StandardCharsets.UTF_8), Map.class);
		final List<Map<String, String>> sortObjectParsed = sortObject == null
				? null
				: objectMapper.readValue(URLDecoder.decode(sortObject, StandardCharsets.UTF_8), List.class);
		return indexService.queryIndex(indexName, projectId, page, size, filterObjectParsed, sortObjectParsed)
				.doOnError(err -> OpenSearchApiService.logRequestError("OpenSearch search request failed: ", err));
	}

	@GetMapping(value = "/indexes/{indexName}/project/{projectId}/queryAggregations")
	public Flux<AggregationResultString> queryAggregations(@PathVariable final String indexName, @PathVariable final Long projectId,
														   @RequestParam final String aggregationRequest) {

		try {
			final AggregationRequestString agr = objectMapper.readValue(URLDecoder.decode(aggregationRequest, StandardCharsets.UTF_8),
					AggregationRequestString.class);
			return indexService.queryIndex(indexName, projectId, agr);
		} catch (final JsonProcessingException e) {
			throw new IllegalStateException(e);
		}
	}

	@GetMapping(value = "/indexes/{indexName}/project/{projectId}/search")
	public Flux<MatchedSearchHit> textSearch(@PathVariable final String indexName, @PathVariable final Long projectId,
											 @RequestParam final String searchWord, @RequestParam(defaultValue = "false") final Boolean isCaseSensitive) {

		return indexService.textSearch(indexName, projectId, searchWord, isCaseSensitive);

	}
}

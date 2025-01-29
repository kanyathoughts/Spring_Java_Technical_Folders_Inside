package innowake.mining.opensearch.index;

import com.fasterxml.jackson.databind.ObjectMapper;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.opensearch.index.model.OpenSearchIndexStatus;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.http.MediaType;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.reactive.function.client.WebClientResponseException;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.io.IOException;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

public class OpenSearchApiService {

    private static final Log LOG = LogFactory.getLog(OpenSearchApiService.class);

    private final WebClient openSearchWebClient;
    private final ObjectMapper objectMapper;

    public OpenSearchApiService(final WebClient openSearchWebClient, final ObjectMapper objectMapper) {
        this.openSearchWebClient = openSearchWebClient;
        this.objectMapper = objectMapper;
    }

    public Mono<Void> deleteIndex(final String indexName) {
        return openSearchWebClient.delete().uri(indexName)
                .retrieve()
                .bodyToMono(Void.class);
    }

    public void createMappings(final String indexName, final Map<String, Map<String, Object>> propertyMappings) {
        final Map<String, Object> mappings = Map.of(
                "mappings", Map.of(
                        "properties", propertyMappings),
                /* define a "toLowercase" normalizer than will be applied to a */
                "settings", Map.of(
                        "analysis", Map.of(
                            "normalizer", Map.of(
                                    "toLowercase", Map.of(
                                        "type", "custom",
                                        "filter", Arrays.asList("lowercase")))),
                            "index", Map.of(
                                "max_result_window",1073741823
                        )));
        openSearchWebClient.put().uri(indexName)
                .contentType(MediaType.APPLICATION_JSON)
                .bodyValue(mappings)
                .retrieve()
                .toBodilessEntity()
                .block();
    }

    public BulkIndexRequestBuilder bulkIndexRequest(final String indexName, @Nullable final Long docId) {
        return new BulkIndexRequestBuilder(indexName, docId);
    }

    public Flux<OpenSearchIndexStatus> listIndexes() {
        return openSearchWebClient.get().uri("_cat/indices?format=json")
                .retrieve()
                .bodyToFlux(OpenSearchIndexStatus.class);
    }

    @SuppressWarnings("unchecked")
    public Mono<Map<String, Object>> queryIndex(final String indexName, final Map<String, Object> query) {
        return (Mono<Map<String, Object>>) (Mono<?>) openSearchWebClient.post().uri(indexName + "/_search")
                .bodyValue(query)
                .retrieve()
                .bodyToMono(Map.class);
    }

    public class BulkIndexRequestBuilder {

        private final String indexName;
        @Nullable
        private final Long docId; 

        private final List<Pair<Object, Map<String, Object>>> bulkData = new ArrayList<>();

        public BulkIndexRequestBuilder(final String indexName, @Nullable final Long docId) {
            this.indexName = indexName;
            this.docId = docId;
        }

        public void addData(final Object id, final Map<String, Object> data) {
            bulkData.add(Pair.of(id, data));
        }

        public void sendBulkRequest() {
            final StringWriter writer = new StringWriter();
            
            if ( ! bulkData.isEmpty()) {
                for (final Pair<Object, Map<String, Object>> data: bulkData) {
                    final Map<String, Map<String, Object>> bulkParameters = Map.of("index", Map.of("_index", indexName, "_id", data.getKey()));
                    try {
                        objectMapper.writeValue(writer, bulkParameters);
                        writer.write("\n");
                        objectMapper.writeValue(writer, data.getValue());
                        writer.write("\n");
                    } catch (final IOException e) {
                        throw new IllegalStateException("Unable to write bulk request", e);
                    }
                }
            } else if (docId != null) {
            	final Map<String, Map<String, Object>> bulkParameters = Map.of("delete", Map.of("_index", indexName, "_id", docId));
            	try {
                    objectMapper.writeValue(writer, bulkParameters);
                    writer.write("\n");            		
            	} catch (final IOException e) {
                    throw new IllegalStateException("Unable to write bulk request", e);
                }
            }

            final byte[] payload = writer.toString().getBytes(StandardCharsets.UTF_8);
            openSearchWebClient.post().uri("_bulk")
                    .contentType(MediaType.valueOf("application/x-ndjson"))
                    .contentLength(payload.length)
                    .body(BodyInserters.fromResource(new ByteArrayResource(payload)))
                    .retrieve()
                    .toBodilessEntity()
                    .doOnError(err -> logRequestError("OpenSearch bulk index request failed: ", err))
                    .block();
        }
    }

    public static void logRequestError(final String logMessage, final Throwable err) {
        final String message;
        if (err instanceof WebClientResponseException) {
            message = ((WebClientResponseException) err).getResponseBodyAsString();
        } else {
            message = "";
        }
        LOG.error(logMessage + message, err);
    }
}

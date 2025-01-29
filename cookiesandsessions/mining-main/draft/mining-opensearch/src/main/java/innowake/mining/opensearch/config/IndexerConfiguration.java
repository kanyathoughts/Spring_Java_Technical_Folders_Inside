package innowake.mining.opensearch.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import innowake.mining.opensearch.index.IndexService;
import innowake.mining.opensearch.index.MiningApiService;
import innowake.mining.opensearch.index.OpenSearchApiService;
import innowake.mining.opensearch.index.model.IndexDefinition;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.reactive.function.client.WebClient;

import java.util.HashMap;
import java.util.Map;

/**
 * Configuration providing the {@link IndexService} for building indexes
 * and the {@link MiningApiService} for talking to mining's REST and GraphQL APIs.
 */
@Configuration
@EnableConfigurationProperties
public class IndexerConfiguration {

    @Value("${mining-opensearch.performance.index-threads}")
    private int indexThreads;

    @Bean
    @ConfigurationProperties(prefix = "mining-opensearch.indexes")
    public Map<String, IndexDefinition> indexDefinitions() {
        return new HashMap<>();
    }

    @Bean
    public MiningApiService miningApiService() {
        return new MiningApiService();
    }

    @Bean
    public OpenSearchApiService openSearchApiService(@Qualifier("opensearch") final WebClient openSearchWebClient, final ObjectMapper objectMapper) {
        return new OpenSearchApiService(openSearchWebClient, objectMapper);
    }

    @Bean
    public IndexService indexerService(@Qualifier("opensearch") final WebClient openSearchWebClient, final ObjectMapper objectMapper) {
        return new IndexService(indexDefinitions(), miningApiService(), openSearchApiService(openSearchWebClient, objectMapper), indexThreads);
    }
}

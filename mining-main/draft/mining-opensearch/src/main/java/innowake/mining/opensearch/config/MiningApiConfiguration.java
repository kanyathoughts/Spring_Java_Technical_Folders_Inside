package innowake.mining.opensearch.config;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.client.reactive.ReactorClientHttpConnector;
import org.springframework.web.reactive.function.client.ExchangeStrategies;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.netty.http.client.HttpClient;

/**
 * Configuration providing a {@link WebClient} with a fixed, pre-configured URL for talking to mining.
 */
@Configuration
@ConditionalOnProperty("mining-opensearch.mining.url")
public class MiningApiConfiguration {

    @Value("${mining-opensearch.mining.url}")
    private String url;
    
    @Value("${mining-opensearch.mining.access-token}")
    private String accessToken;
    
    @Value("#{new Boolean('${mining-opensearch.mining.is-offline-token:true}')}")
    private Boolean isOfflineToken;
    
    @Bean
    @Qualifier("mining")
    public WebClient miningWebClient() {
    	final String tokenHeader;
    	if (isOfflineToken) {
    		tokenHeader = "Bearer offline." + accessToken;
    	} else {
    		tokenHeader = "Bearer " + accessToken;
    	}
    	
        return WebClient.builder()
                .clientConnector(new ReactorClientHttpConnector(
                        HttpClient.create()//.wiretap(true)
                ))
                .exchangeStrategies(ExchangeStrategies.builder()
                        .codecs(configurer -> configurer
                                .defaultCodecs()
                                .maxInMemorySize(1073741824))
                        .build())
                .baseUrl(url)
                .defaultHeader("Authorization", tokenHeader)
                .build();
    }
}

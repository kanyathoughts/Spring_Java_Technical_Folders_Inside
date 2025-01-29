/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.opensearch;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import javax.annotation.PostConstruct;

/**
 * Configuration for establishing connectivity to the mining-opensearch service via HTTP.
 * <p>
 * The mining openSearch URL can be specified via the {@code mining-opensearch.url} configuration parameter (e.g. via the command-line).
 * <p>
 */
@Configuration
public class MiningOpenSearchConfiguration {

    @Value("${mining-opensearch.url:}")
    private String openSearchUrl;

    @Autowired
    private ApplicationEventPublisher eventPublisher;

    @PostConstruct
    public void init() {
        if ( ! StringUtils.isEmpty(openSearchUrl)) {
            eventPublisher.publishEvent(new OpenSearchAvailabilityEvent(openSearchUrl));
        }
    }

    @Bean
    public OpenSearchService openSearchService(final ObjectMapper objectMapper) {
        return new OpenSearchService(objectMapper, StringUtils.trimToNull(openSearchUrl));
    }
}

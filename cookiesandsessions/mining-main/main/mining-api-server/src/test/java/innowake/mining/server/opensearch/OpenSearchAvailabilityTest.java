/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.opensearch;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.jackson.JacksonAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;

import static org.junit.jupiter.api.Assertions.assertFalse;

/**
 * Tests that OpenSearch is "not available" by default and therefore has no impact unless configured.
 */
@SpringBootTest(classes = { MiningOpenSearchConfiguration.class, JacksonAutoConfiguration.class })
class OpenSearchAvailabilityTest {

    @Autowired
    private OpenSearchService openSearchService;

    /**
     * {@link OpenSearchService#isAvailable()} should return {@code false} in default configuration,
     * because the configuration property {@code mining-opensearch.url} is empty.
     */
    @Test
    void testOpenSearchUnavailable() {
        assertFalse(openSearchService.isAvailable(), "OpenSearch should be unavailable when not configured");
    }
}

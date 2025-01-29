/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.opensearch;

import innowake.lib.core.api.lang.Nullable;

/**
 * Application event for signalling availability of the mining-opensearch service.
 */
public class OpenSearchAvailabilityEvent {

    @Nullable
    private final String baseUrl;

    public OpenSearchAvailabilityEvent(@Nullable final String baseUrl) {
        this.baseUrl = baseUrl;
    }

    /**
     * Returns the base url for the mining-opensearch REST API, or {@code null} when mining-opensearch is not available.
     * @return the base URL or {@code null}
     */
    @Nullable
    public String getBaseUrl() {
        return baseUrl;
    }
}

/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.genai.knowledgeservice;

import innowake.lib.core.api.lang.Nullable;

/**
 * Application event for signalling availability of the GenAI knowledge service.
 */
public class KnowledgeServiceAvailabilityEvent {

    @Nullable
    private final String baseUrl;

    public KnowledgeServiceAvailabilityEvent(@Nullable final String baseUrl) {
        this.baseUrl = baseUrl;
    }

    /**
     * Returns the base url for the GenAI knowledge service's REST API, or {@code null} when the GenAI knowledge service is not available.
     * @return the base URL or {@code null}
     */
    @Nullable
    public String getBaseUrl() {
        return baseUrl;
    }
}

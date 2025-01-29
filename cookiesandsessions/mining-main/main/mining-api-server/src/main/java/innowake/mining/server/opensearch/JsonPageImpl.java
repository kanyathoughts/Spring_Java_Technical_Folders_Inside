/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.opensearch;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;

import innowake.mining.shared.access.Paged;

/**
 * Implementation of {@link Paged} that can be deserialized from JSON.
 * Used to retrieve paged data from mining-opensearch's REST-API.
 * @param <T> type of the page contents
 */
public class JsonPageImpl<T> extends Paged<T> {

    /* Note: the additional, unused parameters must be specified here, as otherwise Jackson attempts to apply some sort of default deserialization
     * to them, which fails because e.g. org.springframework.data.domain.Pageable can not be deserialzied. The constructor parameters explicitly map these
     * properties to JsonNode and then ignores them  */
    @JsonCreator
    public JsonPageImpl(@JsonProperty("content") final List<T> content,
                        @JsonProperty("number") final int number,
                        @JsonProperty("size") final int size,
                        @JsonProperty("totalElements") final long totalElements,
                        @JsonProperty("pageable") final JsonNode pageable,
                        @JsonProperty("last") final boolean last,
                        @JsonProperty("totalPages") final int totalPages,
                        @JsonProperty("sort") final JsonNode sort,
                        @JsonProperty("first") final boolean first,
                        @JsonProperty("numberOfElements") final int numberOfElements) {
        super(content, size, (long) number, totalElements, null, null);
    }
}

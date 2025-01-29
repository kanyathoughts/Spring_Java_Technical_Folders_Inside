/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test;

import java.util.ArrayList;
import java.util.List;

import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;

/**
 * A response template class for Paged responses to test using Rest Template.
 * @param <T> Type of data in paged response.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class RestResponsePage<T> extends PageImpl<T> {
    @SuppressWarnings("unused")
	@JsonCreator(mode = JsonCreator.Mode.PROPERTIES)
    public RestResponsePage(@JsonProperty("content") final  List<T> content,
                            @JsonProperty("number") final int number,
                            @JsonProperty("size") final int size,
                            @JsonProperty("totalElements") final Long totalElements,
                            @JsonProperty("pageable") final JsonNode pageable,
                            @JsonProperty("last") final boolean last,
                            @JsonProperty("totalPages") final int totalPages,
                            @JsonProperty("sort") final JsonNode sort,
                            @JsonProperty("first") final boolean first,
                            @JsonProperty("numberOfElements") final int numberOfElements) {
        super(content, PageRequest.of(number, size), totalElements.longValue());
    }

    /**
     * Constructor.
     * @param content of the page
     * @param pageable pageable object with pagination details
     * @param total total count
     */
    public RestResponsePage(final List<T> content, final Pageable pageable, final long total) {
        super(content, pageable, total);
    }

    /**
     * Constructor.
     * @param content of the page
     */
    public RestResponsePage(final List<T> content) {
        super(content);
    }
    
    /**
     * Constructor.
     */
    public RestResponsePage() {
        super(new ArrayList<>());
    }
}

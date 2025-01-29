/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.codeviewer;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Collections;
import java.util.List;

/**
 * Model containing the assembled source code of a module as well as information about the location of each assembled part
 * and its origin (i.e. which module the included source code is coming from).
 */
public class AssembledContent {
    private final boolean available;
    private final String content;
    private final List<Inclusion> inclusions;

    @JsonCreator
    public AssembledContent(@JsonProperty("available") final boolean available,
                            @JsonProperty("content") final String content,
                            @JsonProperty("inclusions") final List<Inclusion> inclusions) {
        this.available = available;
        this.content = content;
        this.inclusions = inclusions;
    }

    /**
     * Returns a {@link AssembledContent} indiciating that assembled content is unavailable.
     * This is the appropriate thing to return for modules from a language that does not use assembling.
     *
     * @return an {@link AssembledContent} where {@link #isAvailable()} will return {@code false}
     */
    public static AssembledContent unavailable() {
        return new AssembledContent(false, "", Collections.emptyList());
    }

    /**
     * Return a {@link AssembledContent} with the given assembled source code string and list of inclusions.
     * This is the appropriate factory method to use when the assembled content was computed successfully.
     *
     * @param content the assembled source code of the module
     * @param inclusions the list of inclusions (i.e. assembled parts)
     * @return an {@link AssembledContent} where {@link #isAvailable()} will return {@code true}
     */
    public static AssembledContent of(final String content, final List<Inclusion> inclusions) {
        return new AssembledContent(true, content, inclusions);
    }

    /**
     * Returns whether assembled content is available for this module. Will return false for modules from languages
     * that do not use assembling.
     * @return whether assembled content is available
     */
    public boolean isAvailable() {
        return available;
    }

    /**
     * Returns the assembled source code of the module
     * @return the assembled source code
     */
    public String getContent() {
        return content;
    }

    /**
     * Returns the list of inclusions (i.e. information about the assembled parts).
     * @return the list of inclusions
     */
    public List<Inclusion> getInclusions() {
        return inclusions;
    }
}

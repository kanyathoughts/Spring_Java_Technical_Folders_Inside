/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.shared.discovery;

/**
 *  This exception is used, whenever an error happens during the reference resolution process.
 */
public class ReferenceResolutionException extends Exception {
    public enum Cause {
        NO_REFERENCE_FOUND("No reference found"),
        MULTIPLE_REFERENCES_FOUND("Multiple references found");
        final String message;
        Cause(final String message) {
            this.message = message;
        }
    }
    public ReferenceResolutionException(final Cause cause) {
        super(cause.message);
    }
}

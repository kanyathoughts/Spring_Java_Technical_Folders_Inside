/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.codeviewer;

/**
 * Enum describing the type of the target of a link. The link is either {@link #LOCAL}, meaning the link points to a location in the same file
 * or {@link #EXTERNAL}, meaning it points to a location in a different file.
 */
public enum LinkTargetType {
    /** the link target is within the same file */
    LOCAL,
    /** the link target is in a different file */
    EXTERNAL
}

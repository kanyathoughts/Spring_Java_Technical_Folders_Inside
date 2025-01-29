/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.codeviewer;

/**
 * Enum describing the nature of a link, for giving the links a distinct visual appearance.
 */
public enum LinkType {
    /**
     * The link is made from AST binding information. Typical example for this is the link from a variable reference to the location of its definition.
     */
    AST_BINDING,
    /**
     * The link is made from control flow information. This allows to jump from branch statements to the location of the target branch,
     * e.g. jumping to the label referenced by a Cobol PERFORM statement.
     */
    CONTROL_FLOW,
    /**
     * The link is made from dependency information, and typically will point to a different module. Typical example are CALL and INCLUDE (COPY) statements,
     * allowing to jump to the included or called module.
     */
    DEPENDENCY,
    /**
     * The link is made from data flow (data lineage) information and represents a read access to the field.
     */
    DATA_FLOW_READ_ACCESS,
    /**
     * The link is made from data flow (data lineage) information and represents a write access to the field.
     */
    DATA_FLOW_WRITE_ACCESS
}

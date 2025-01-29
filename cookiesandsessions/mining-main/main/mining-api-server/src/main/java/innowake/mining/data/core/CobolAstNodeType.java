/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core;

/**
 * Cobol AstNode types
 */
public enum CobolAstNodeType {
	
	/* When adding a new type, also add it to:
	 * innowake.mining.data.plugin.core.ValidationTest.validateCobolTypeNames() 
	 * otherwise the test will fail */
	
	PROCEDURE_DIVISION("ProcedureDivision"), 
	DISPLAY("CobolDisplayStmt"),
	REFERENCE_EXPRESSION("CobolReferenceExpression"),
	CONSTANT_REFERENCE("CobolConstantReference"),
	
	IF("CobolIfStmt"),
	THEN("CobolThenBlock"),
	ELSE("CobolElseBlock"),
	EVALUATE("CobolEvaluateStmt"),
	WHEN("CobolWhenStmt"),
	LABEL("CobolLabelStmt"),
	GO_BACK("CobolGoBackStmt"),
	PERFORM("CobolPerformStmt"),
	GO_TO("CobolGoToStmt"),
	EXIT("CobolExitStmt"),
	STOP("CobolStopStmt"),
	CALL("CobolCallStmt"),
	SECTION("CobolSection"),
	SIZE_GUARDED("CobolSizeGuardedStmt"),
	RETURN("CobolReturnStmt"),
	READ("CobolReadStmt"),
	WRITE("CobolWriteStmt"),
	REWRITE("CobolRewriteStmt"),
	START("CobolStartStmt"),
	SEARCH("CobolSearchStmt"),
	AT_END("CobolAtEndBlock"),
	INVALID_KEY("CobolInvalidKeyBlock"),
	ON_SIZE_ERROR("CobolSizeErrorStatement"),
	COPY("CobolCopyStmt"),
	CICS_XCTL("ExecCicsXctl"),
	CICS_RETURN("ExecCicsReturn"),
	CICS_LINK("ExecCicsLink"),
	
	DEFAULT_STATEMENT("defaultStatement");
	
    private String type;

	CobolAstNodeType(String type) {
        this.type = type;
    }
	
	public String getType() {
		return type;
	}
	
	/**
	 * Returns the enum member that represents the given type.
	 * If no special type for that String exists {@link CobolAstNodeType#DEFAULT_STATEMENT} is returned
	 *
	 * @param type the type
	 * @return the enum member that represents the given type
	 */
    public static CobolAstNodeType fromString(final String type) {
        for (final CobolAstNodeType nodeType : CobolAstNodeType.values()) {
            if (nodeType.type.equals(type)) {
                return nodeType;
            }
        }
        return DEFAULT_STATEMENT;
    }
}

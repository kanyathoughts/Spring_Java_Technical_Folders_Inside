/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core;

/**
 * Names of the classes from the C AST that are used as super types
 */
public class CAstNodeType {

	private CAstNodeType() {
		/* not supposed to be instantiated */
	}

	public static final String FUNCTION_DEFINITION = "CFunctionDefinition";
	public static final String EXPRESSION_STATEMENT = "CExpressionStatement";
	public static final String FOR_STATEMENT = "CForStatement";
	public static final String IFELSE_STATEMENT = "CIfElseStatement";
	public static final String JUMP_STATEMENT = "CGoToStatement";
	public static final String IDENTIFIER_LABELED_STATEMENT = "CIdentifierLabeledStatement";
	public static final String SWITCH_STATEMENT = "CSwitchStatement";
	public static final String BREAK_STATEMENT = "CBreakStatement";
	public static final String THEN_BRANCH = "CThenBranch";
	public static final String ELSE_BRANCH = "CElseBranch";
	public static final String CASE_BRANCH = "CCaseBranch";
	public static final String DEFAULT_BRANCH = "CDefaultBranch";

}

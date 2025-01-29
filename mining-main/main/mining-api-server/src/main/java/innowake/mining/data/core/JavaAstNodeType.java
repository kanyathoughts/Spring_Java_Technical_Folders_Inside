/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core;

/**
 * Names of the classes from the Java AST that are used as super types
 */
public class JavaAstNodeType {
	
	private JavaAstNodeType() {
		/* not supposed to be instantiated */
	}

	public static final String FUNCTION_DEFINITION = "JavaMethodDeclaration";
	public static final String BLOCK = "JavaBlock";
	public static final String EXPRESSION_STATEMENT = "JavaExpressionStatement";
	public static final String FOR_STATEMENT = "JavaForStatement";
	public static final String IFELSE_STATEMENT = "JavaIfStatement";
	public static final String IDENTIFIER_LABELED_STATEMENT = "JavaLabeledStatement";
	public static final String SWITCH_STATEMENT = "JavaSwitchStatement";
	public static final String BREAK_STATEMENT = "JavaBreakStatement";
	public static final String THEN_BRANCH = "JavaThenBranch";
	public static final String ELSE_BRANCH = "JavaElseBranch";
	public static final String CASE_BRANCH = "JavaSwitchCase";
}

/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core;
/**
 * Names of the classes from the JCL AST that are used as super types
 */
public class JclAstNodeType {

	private JclAstNodeType() {
		/* not supposed to be instantiated */
	}

	public static final String STEP_IF = "JclStepIf";
	public static final String IF_BRANCH = "JclIfStep";
	public static final String ELSE_BRANCH = "JclElseStep";
	public static final String JOB = "JclJobNode";
	public static final String STEP_EXEC = "JclStepExec";
	public static final String CONDITION = "JclCondition";
}

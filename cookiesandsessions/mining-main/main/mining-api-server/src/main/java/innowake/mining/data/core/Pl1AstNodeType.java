/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core;

/**
 * Names of the classes from the Pl1 AST that are used as super types
 */
public class Pl1AstNodeType {
	
	private Pl1AstNodeType() {
		/* not supposed to be instantiated */
	}
	
	public static final String IF_STMT = "IfStatement";
	public static final String OTHERWISE_STMT = "OtherwiseStatement";
	public static final String WHEN_STMT = "WhenStatement";
	public static final String SELECT_STMT = "SelectStatement";
	public static final String THEN_BRANCH = "ThenUnit";
	public static final String ELSE_BRANCH = "ElseUnit";
	public static final String PROCEDURE_BLOCK = "ProcedureBlock";
	public static final String SELECT_GROUP = "SelectGroup";
	public static final String DO_STMT = "DoStatement";
	public static final String BEGIN_BLOCK = "BeginBlock";

}

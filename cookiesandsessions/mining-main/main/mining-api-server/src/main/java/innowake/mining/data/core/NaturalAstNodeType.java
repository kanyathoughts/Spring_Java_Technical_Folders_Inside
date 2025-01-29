/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core;

import innowake.ndt.naturalparser.model.ArtificialNaturalAstNode;

/**
 * Names of the classes from the Natural AST that are used as super types
 */
public class NaturalAstNodeType {
	
	private NaturalAstNodeType() {
		/* not supposed to be instantiated */
	}
	
	public static final String IF_STMT = "IfStmt";
	public static final String IF_SELECTION_STMT = "IfSelectionStmt";
	public static final String DECIDE_FOR_STMT = "DecideForStmt";
	public static final String DECIDE_ON_STMT = "DecideOnStmt";
	public static final String DECIDE_FOR_CLAUSE = "ImpDecideForClauseImpl";
	public static final String DECIDE_ON_CLAUSE = "ImpDecideOnClauseImpl";
	public static final String EVERY = "Every";
	public static final String FORMAT = "FormatStmt";
	public static final String DEFINE_DATA = "DefineDataStmt";
	public static final String LABELED = "Labeled";
	public static final String SELECT = "SelectStmt";
	public static final String END_ALL = "EndAllMarkStmt";
	public static final String PERFORM = "PerformStmt";
	public static final String DEFINE_SUBROUTINE = "DefineSubroutineStmt";
	public static final String INPUT = "InputStmt";
	public static final String INPUT_USING_MAP = "InputUsingMapStmt";
	public static final String REINPUT = "ReinputStmt";
	public static final String ESCAPE = "EscapeStmt";
	public static final String BREAK = "BreakStmt";
	public static final String BEFORE_BREAK = "BeforeBreakStmt";
	public static final String START_OF_DATA = "StartOfDataStmt";
	public static final String END_OF_DATA = "EndOfDataStmt";
	public static final String WHEN_ANY = "WhenAny";
	public static final String WHEN_ALL = "WhenAll";
	public static final String REPEAT = "RepeatStmt";
	public static final String INCLUDE_STMT = "IncludeStmt";
	public static final String THEN_BRANCH = ArtificialNaturalAstNode.THEN_BRANCH;
	public static final String ELSE_BRANCH = ArtificialNaturalAstNode.ELSE_BRANCH;
	public static final String IF_NO_RECORDS = ArtificialNaturalAstNode.IF_NO_RECORDS;
	
	/**
	 * This is an artificial node that wraps the statements of a READ WORK FILE
	 * */
	public static final String EVERY_RECORD = "EveryRecord";
}

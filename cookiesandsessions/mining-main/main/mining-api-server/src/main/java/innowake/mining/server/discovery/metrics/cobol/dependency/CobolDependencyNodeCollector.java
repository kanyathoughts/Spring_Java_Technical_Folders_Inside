/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.cobol.dependency;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang.StringUtils;

import innowake.lib.core.lang.NonNull;
import innowake.lib.core.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.model.discovery.ModelStatement;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.metrics.cobol.CobolStatementUtility;
import innowake.mining.server.discovery.metrics.exec.ExecStatementUtility;
import innowake.mining.shared.model.StatementType;
import innowake.ndt.cobol.parser.ast.CobolModelTraverser;
import innowake.ndt.cobol.parser.ast.model.CobolConstantReference;
import innowake.ndt.cobol.parser.ast.model.CobolDataField;
import innowake.ndt.cobol.parser.ast.model.CobolFieldReference;
import innowake.ndt.cobol.parser.ast.model.CobolFileDefinition;
import innowake.ndt.cobol.parser.ast.model.CobolNode;
import innowake.ndt.cobol.parser.ast.model.CobolParserError;
import innowake.ndt.cobol.parser.ast.model.CobolReference;
import innowake.ndt.cobol.parser.ast.model.CobolReferenceExpression;
import innowake.ndt.cobol.parser.ast.model.CobolUnknownToken;
import innowake.ndt.cobol.parser.ast.statement.CobolCallStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolCallStmt.CobolUsing;
import innowake.ndt.cobol.parser.ast.statement.CobolEnterStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolEntryStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolEvaluateStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolIfStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolMoveStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolOpenStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolSearchStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolSelectStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolSetStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolWhenStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaAbortStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaAddStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaBTStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaCallStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaCloseStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaDclRecStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaDeclareStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaDeleteStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaEndTransactionStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaFileStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaFindStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaHoldStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaLangStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaModeStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaNameStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaOptionsStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaRUserDataStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaReadIsnStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaReadLogicalStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaReadNextStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaReadSequenceStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaReadValueStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaReleaseIsnStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaReleaseStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaTeleStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaUnknownStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaUnknownToken;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaUpdateStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaUserStmt;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaXrefStmt;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.AbstractExecAdabasQueryNode;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.ExecAdabasClose;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.ExecAdabasCommitWork;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.ExecAdabasDbClose;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.ExecAdabasDbIdOption;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.ExecAdabasDelete;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.ExecAdabasFetch;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.ExecAdabasFind;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.ExecAdabasHistogram;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.ExecAdabasHold;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.ExecAdabasHoldOption;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.ExecAdabasInsert;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.ExecAdabasIsnOption;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.ExecAdabasOpen;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.ExecAdabasPrefixOption;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.ExecAdabasReadIsn;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.ExecAdabasReadLogical;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.ExecAdabasReadPhysical;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.ExecAdabasReadUserData;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.ExecAdabasRelease;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.ExecAdabasReleaseIsn;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.ExecAdabasRollbackWork;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.ExecAdabasSaveOption;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.ExecAdabasSequenceOption;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.ExecAdabasSuffixOption;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.ExecAdabasUpdate;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.ExecAdabasWhereCurrentOf;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.ExecAdabasWhereIsn;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.ExecAdabasXDbIdOption;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsAbend;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsAddress;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsAddressSet;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsAllocate;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsAssign;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsBifDeedit;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsBrowse;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsCancel;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsChangePassword;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsConnectProcess;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsContainer;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsCreateTransaction;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsDelay;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsDeleteQTd;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsDeleteQTs;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsDeq;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsDumpTransaction;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsEnableProgram;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsEnq;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsEnterTracenum;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsExtractExit;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsFile;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsFree;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsFreemain;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsGetmain;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsHandleAbend;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsHandleAid;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsHandleCondition;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsIgnoreCondition;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsInquireConnection;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsInquireNetname;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsInquireTaskList;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsInquireTerminal;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsInquireTransaction;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsInquireTsqname;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsInvokeWebservice;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsIssueDisconnect;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsJournal;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsLink;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsLoad;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsPopHandle;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsPost;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsPurgeMessage;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsPushHandle;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsQuerySecurity;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsReadQTd;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsReadQTs;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsReceive;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsRelease;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsRetrieve;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsReturn;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsRoute;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsSend;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsSendControl;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsSendPage;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsSendReceive;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsSendText;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsSetConnection;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsSetFile;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsSetStatistics;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsSetTdQueue;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsSetTerminal;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsSignoff;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsSignon;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsSoapfaultCreate;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsStart;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsStatement;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsSyncpoint;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsSyncpointRollback;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsTime;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsVerifyPassword;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsWaitEvent;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsWriteJournalName;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsWriteOperator;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsWriteQTd;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsWriteQTs;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsXctl;
import innowake.ndt.core.parsing.ast.model.exec.ExecNode;
import innowake.ndt.core.parsing.ast.model.exec.ExecSqlNode;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlAllocateCursor;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlAlterSession;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlAssociateLocators;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlCall;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlClose;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlCommit;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlCommonTableExpression;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlConnect;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlCreateIndex;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDeclareAlias;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDeclareCursor;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDeclareSchema;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDeclareTable;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDeclareTempTable;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDeclareTransaction;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDelete;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlExecuteImmediate;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlExecutePlSql;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlExecutePrepared;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlFetch;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlFor;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlGetDiagnostics;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlGetError;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlInsert;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlLockTable;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlOpen;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlPrepare;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlRollback;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlSelect;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlSet;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlUpdate;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlValues;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlVar;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlWhenever;
import innowake.ndt.parsing.parser.exec.ExecUnknownToken;

@SuppressWarnings("rawtypes") 
public class CobolDependencyNodeCollector extends CobolModelTraverser {
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.METRICS_COLLECTOR);
	
	/** Moves are used to resolve variables in dependency statements */
	final List<CobolMoveStmt> moves = new ArrayList<>();
	final List<CobolDataField> data = new ArrayList<>();
	final List<CobolCallStmt> calls = new ArrayList<>();
	final List<CobolSelectStmt> selects = new ArrayList<>();
	final List<CobolFileDefinition> fileDefinitions = new ArrayList<>();
	final List<ExecCicsLink> links = new ArrayList<>();
	final List<ExecCicsXctl> xctls = new ArrayList<>();
	final List<ExecCicsSendReceive> maps = new ArrayList<>();
	final List<ExecSqlSelect> sqlSelects = new ArrayList<>();
	final List<ExecSqlDeclareCursor> sqlCursors = new ArrayList<>();
	final List<ExecSqlUpdate> sqlUpdates = new ArrayList<>();
	final List<ExecSqlDeclareTempTable> sqlDeclareTempTables = new ArrayList<>();
	final List<ExecSqlDeclareTable> sqlDeclareTables = new ArrayList<>();
	final List<ExecSqlDelete> sqlDeletes = new ArrayList<>();
	final List<ExecSqlInsert> sqlInserts = new ArrayList<>();
	final List<ExecSqlLockTable> sqlLockTables = new ArrayList<>();
	final List<ExecCicsFile> execCicsFiles = new ArrayList<>();
	private final List<ExecCicsReturn> execCicsReturn = new ArrayList<>();
	final List<ExecCicsCreateTransaction> execCicsTransaction = new ArrayList<>();
	final List<ExecCicsStatement> execCicsQts = new ArrayList<>();
	final List<ExecCicsStatement> execCicsQtd = new ArrayList<>();
	final List<ExecSqlCall> execSqlCalls = new ArrayList<>();
	final List<ExecSqlDeclareSchema> sqlDeclareSchema = new ArrayList<>();
	final List<CobolSetStmt> sets = new ArrayList<>();
	final List<CobolEnterStmt> enters = new ArrayList<>();
	final List<String> commonTableExpressions = new ArrayList<>();
	final List<AbstractExecAdabasQueryNode> execAdabasQueryNodes = new ArrayList<>();
	final List<AdaDeclareStmt> adaDeclareStmts = new ArrayList<>();
	final List<CobolUnknownToken> cobolUnknownTokens = new ArrayList<>();
	final List<AdaUnknownStmt> adaUnknownStmts = new ArrayList<>();
	final List<AdaUnknownToken> adaUnknownTokens = new ArrayList<>();
	final List<ExecUnknownToken<?>> execUnknownTokens = new ArrayList<>();
	final List<CobolParserError> cobolParserErrors = new ArrayList<>();
	final List<ModelStatement> statements = new ArrayList<>();
	final List<CobolOpenStmt> openStatements = new ArrayList<>();
	boolean isDebugging;

	public void setDebugging(final boolean debugging) {
		isDebugging = debugging;
	}

	public boolean isDebugging() {
		return isDebugging;
	}

	/**
	 * @return the list of collected CALL statements
	 */
	public List<CobolCallStmt> getCallStatements() {
		return Collections.unmodifiableList(calls);
	}
	
	/**
	 * @return the list of collected move statements
	 */
	public List<CobolOpenStmt> getOpenStatements() {
		return Collections.unmodifiableList(openStatements);
	}
	
	/**
	 * @return the list of collected open statements
	 */
	public List<CobolMoveStmt> getMoveStatements() {
		return Collections.unmodifiableList(moves);
	}
	
	/**
	 * @return the list of collected data fields
	 */
	public List<CobolDataField> getDataFields() {
		return Collections.unmodifiableList(data);
	}
	
	/**
	 * @return the list of collected set statements
	 */
	public List<CobolSetStmt> getSetStmts() { 
		return Collections.unmodifiableList(sets); 
	}
	
	/**
	 * @return a list of select statements.
	 */
	public List<CobolSelectStmt> getCobolSelectStatements() {
		return selects;
	}
	
	/**
	 * @return a list of select statements.
	 */
	public List<CobolFileDefinition> getCobolFileDefinitions() {
		return fileDefinitions;
	}
	
	/**
	 * @return the list of collected EXEC SQL SELECT statements
	 */
	public List<ExecSqlSelect> getSqlSelects() {
		return sqlSelects;
	}

	/**
	 * @return the list of collected EXEC SQL DECLARE CURSOR statements
	 */
	public List<ExecSqlDeclareCursor> getSqlCursors() {
		return sqlCursors;
	}

	/**
	 * @return the list of collected EXEC SQL UPDATE statements
	 */
	public List<ExecSqlUpdate> getSqlUpdates() {
		return sqlUpdates;
	}

	/**
	 * @return the list of collected EXEC SQL DECLARE TEMPORARY TABLE statements
	 */
	public List<ExecSqlDeclareTempTable> getSqlDeclareTempTables() {
		return sqlDeclareTempTables;
	}

	/**
	 * @return the list of collected EXEC SQL DECLARE TABLE statements
	 */
	public List<ExecSqlDeclareTable> getSqlDeclareTables() {
		return sqlDeclareTables;
	}

	/**
	 * @return the list of collected EXEC SQL DELETE statements
	 */
	public List<ExecSqlDelete> getSqlDeletes() {
		return sqlDeletes;
	}

	/**
	 * @return the list of collected EXEC SQL INSERT statements
	 */
	public List<ExecSqlInsert> getSqlInserts() {
		return sqlInserts;
	}

	/**
	 * @return the list of collected EXEC SQL LOCK TABLE statements
	 */
	public List<ExecSqlLockTable> getSqlLockTables() {
		return sqlLockTables;
	}

	/**
	 * @return the list of collected EXEC SQL CALL statements
	 */
	public List<ExecSqlCall> getExecSqlCalls() {
		return execSqlCalls;
	}

	/**
	 * @return the list of collected EXEC SQL DECLARE SCHEMA statements
	 */
	public List<ExecSqlDeclareSchema> getSqlDeclareSchema() {
		return sqlDeclareSchema;
	}
	
	/**
	 * @return the list of collected EXEC CICS CREATE TRANSACTION statements
	 */
	public List<ExecCicsCreateTransaction> getExecCicsTransaction() {
		return execCicsTransaction;
	}
	
	/**
	 * @return the list of collected EXEC CICS DELETEQ TD, EXEC CICS READQ TD, EXEC CICS WRITEQ TD statements
	 */
	public List<ExecCicsStatement> getExecCicsQTds() {
		return execCicsQtd;
	}
	

	/**
	 * @return the list of collected EXEC CICS DELETEQ TS, EXEC CICS READQ TS, EXEC CICS WRITEQ TS statements
	 */
	public List<ExecCicsStatement> getExecCicsQTs() {
		return execCicsQts;
	}
	
	/**
	 * @return the list of collected EXEC ADABAS FIND, EXEC ADABAS HISTOGRAM, EXEC ADABAS READ ISN, EXEC ADABAS READ LOGICAL, EXEC ADABAS READ PHYSICAL
	 *  statements
	 */
	public List<AbstractExecAdabasQueryNode> getExecAdabasQueryNodes() {
		return execAdabasQueryNodes;
	}
	
	/**
	 * @return the list of collected {@link AdaDeclareStmt}s
	 */
	public List<AdaDeclareStmt> getAdaDeclareStmts() {
		return adaDeclareStmts;
	}
	
	/**
	 * @return the list of collected {@link CobolUnknownToken}s
	 */
	public List<CobolUnknownToken> getCobolUnknownTokens() {
		return cobolUnknownTokens;
	}
	
	/**
	 * @return the list of collected {@link AdaUnknownStmt}s
	 */
	public List<AdaUnknownStmt> getAdaUnknownStmts() {
		return adaUnknownStmts;
	}
	
	/**
	 * @return the list of collected {@link AdaUnknownToken}s
	 */
	public List<AdaUnknownToken> getAdaUnknownTokens() {
		return adaUnknownTokens;
	}
	
	/**
	 * @return the list of collected {@link ExecUnknownToken}s
	 */
	public List<ExecUnknownToken<?>> getExecUnknownTokens() {
		return execUnknownTokens;
	}
	
	/**
	 * @return the list of collected {@link CobolParserError}s
	 */
	public List<CobolParserError> getCobolParserErrors() {
		return cobolParserErrors;
	}
	
	/**
	 * @return the list of collected common tables expressions.
	 */
	public List<String> getCommonTableExpressions() {
		return commonTableExpressions;
	}
	
	/**
	 * @return the list of collected {@link CobolEnterStmt}s.
	 */
	public List<CobolEnterStmt> getEnters() {
		return enters;
	}

	/**
	 * @return the list of collected {@link ExecCicsLink}s.
	 */
	public List<ExecCicsLink> getExecCicsLinks() {
		return links;
	}
	
	/**
	 * @return the list of collected {@link ExecCicsXctl}s.
	 */
	public List<ExecCicsXctl> getExecCicsXctls() {
		return xctls;
	}
	
	/**
	 * @return the list of collected {@link ExecCicsSendReceive}s.
	 */
	public List<ExecCicsSendReceive> getExecCicsSendReceives() {
		return maps;
	}
	
	/**
	 * @return the list of collected {@link ExecCicsFile}s.
	 */
	public List<ExecCicsFile> getExecCicsFiles() {
		return execCicsFiles;
	}
	
	/**
	 * @return the list of collected {@link ExecCicsReturn}s.
	 */
	public List<ExecCicsReturn> getExecCicsReturn() {
		return execCicsReturn;
	}
	
	/**
	 * @return the list of collected {@link ExecCicsFile}s.
	 */
	public List<ModelStatement> getStatements() {
		return statements;
	}
	
	@Override
	protected void handleExecCicsDeleteQTd(@Nullable final ExecCicsDeleteQTd deleteQtd) {
		super.handleExecCicsDeleteQTd(deleteQtd);
		execCicsQtd.add(deleteQtd);
		cics(deleteQtd);
	}
	
	@Override
	protected void handleExecCicsDeleteQTs(@Nullable final ExecCicsDeleteQTs deleteQts) {
		super.handleExecCicsDeleteQTs(deleteQts);
		execCicsQts.add(deleteQts);
		cics(deleteQts);
	}
	
	@Override
	protected void handleExecCicsReadQTd(@Nullable final ExecCicsReadQTd readQtd) {
		super.handleExecCicsReadQTd(readQtd);
		execCicsQtd.add(readQtd);
		cics(readQtd);
	}
	
	@Override
	protected void handleExecCicsReadQTs(@Nullable final ExecCicsReadQTs readQts) {
		super.handleExecCicsReadQTs(readQts);
		execCicsQts.add(readQts);
		cics(readQts);
	}
	
	@Override
	protected void handleExecCicsWriteQTd(@Nullable final ExecCicsWriteQTd writeQtd) {
		super.handleExecCicsWriteQTd(writeQtd);
		execCicsQtd.add(writeQtd);
		cics(writeQtd);
	}
	
	@Override
	protected void handleExecCicsWriteQTs(@Nullable final ExecCicsWriteQTs writeQts) {
		super.handleExecCicsWriteQTs(writeQts);
		execCicsQts.add(writeQts);
		cics(writeQts);
	}
	
	@Override
	protected void handleExecCicsWriteJournalName(@Nullable final ExecCicsWriteJournalName node) {
		super.handleExecCicsWriteJournalName(node);
		cics(node);
	}

	
	@Override
	protected void handleCobolSelectStmt(@Nullable final CobolSelectStmt selectStmt) {
		super.handleCobolSelectStmt(selectStmt);
		selects.add(selectStmt);
	}
	
	@Override
	protected void handleCobolFileDefinition(@Nullable final CobolFileDefinition fileDefinition) {
		super.handleCobolFileDefinition(fileDefinition);
		fileDefinitions.add(fileDefinition);
	}
	
	@Override
	protected void handleCobolDataField(@Nullable final CobolDataField data) {
		super.handleCobolDataField(data);
		this.data.add(data);
	}
	
	@Override
	protected void handleCobolMoveStmt(@NonNull final CobolMoveStmt move) {
		super.handleCobolMoveStmt(move);
		moves.add(move);
	}
	
	@Override 
	protected void handleCobolSetStmt(@Nullable final CobolSetStmt set) {
		super.handleCobolSetStmt(set);
		sets.add(set);
	}
	
	@Override
	public void handleExecSqlSelect(final ExecSqlSelect<?> sqlSelect) {
		super.handleExecSqlSelect(sqlSelect);
		/* collect all exec sql select statements we find */
		sqlSelects.add(sqlSelect);
		sql(sqlSelect);
	}
	
	@Override
	public void handleExecSqlCommonTableExpression(final ExecSqlCommonTableExpression<?> commonTableExpression) {
		super.handleExecSqlCommonTableExpression(commonTableExpression);
		commonTableExpressions.add(commonTableExpression.getTableName());
	}
	
	@Override
	public void handleExecSqlDeclareCursor(final ExecSqlDeclareCursor<?> sqlCursor) {
		super.handleExecSqlDeclareCursor(sqlCursor);
		
		/* collect all the cursors we find for later evaulation */
		sqlCursors.add(sqlCursor);
		sql(sqlCursor);
	}
	
	@Override
	public void handleExecSqlDeclareTable(final ExecSqlDeclareTable declare) {
		super.handleExecSqlDeclareTable(declare);
		sqlDeclareTables.add(declare);
		sql(declare);
	}
	
	@Override
	public void handleExecSqlDeclareTempTable(final ExecSqlDeclareTempTable declare) {
		super.handleExecSqlDeclareTempTable(declare);
		sqlDeclareTempTables.add(declare);
		sql(declare);
	}
	
	@Override
	public void handleExecSqlDelete(final ExecSqlDelete delete) {
		super.handleExecSqlDelete(delete);
		sqlDeletes.add(delete);
		sql(delete);
	}	
	
	@Override
	public void handleExecSqlInsert(final ExecSqlInsert<?> insert) {
		super.handleExecSqlInsert(insert);
		sqlInserts.add(insert);
		sql(insert);
	}
	
	@Override
	public void handleExecSqlLockTable(final ExecSqlLockTable<?> lock) {
		super.handleExecSqlLockTable(lock);
		sqlLockTables.add(lock);
		sql(lock);
	}
	
	@Override
	public void handleExecSqlUpdate(final ExecSqlUpdate<?> update) {
		super.handleExecSqlUpdate(update);
		sqlUpdates.add(update);
		sql(update);
	}
	
	@Override
	public void handleExecSqlCall(final ExecSqlCall<?> call) {
		super.handleExecSqlCall(call);
		execSqlCalls.add(call);
		sql(call);
	}
	
	@Override
	protected void handleExecCicsLink(@Nullable final ExecCicsLink link) {
		super.handleExecCicsLink(link);
		links.add(link);
		cics(link);
	}
	
	@Override
	protected void handleExecCicsXctl(@Nullable final ExecCicsXctl xctl) {
		super.handleExecCicsXctl(xctl);
		this.xctls.add(xctl);
		cics(xctl);
	}
	
	@Override
	protected void handleExecCicsSendReceive(@Nullable final ExecCicsSendReceive send) {
		super.handleExecCicsSendReceive(send);
		maps.add(send);
		cics(send);
	}
	
	@Override
	protected void handleExecCicsFile(@Nullable final ExecCicsFile file) {
		super.handleExecCicsFile(file);
		execCicsFiles.add(file);
		cics(file);
	}
	
	@Override
	protected void handleExecCicsReturn(@Nullable final ExecCicsReturn node) {
		super.handleExecCicsReturn(node);
		execCicsReturn.add(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsCreateTransaction(final ExecCicsCreateTransaction transaction) {
		super.handleExecCicsCreateTransaction(transaction);
		execCicsTransaction.add(transaction);
	}
	
	@Override
	public void handleExecSqlDeclareSchema(final ExecSqlDeclareSchema declare) {
		super.handleExecSqlDeclareSchema(declare);
		sqlDeclareSchema.add(declare);
		sqlSchema(declare);
	}
	
	@Override
	protected void handleCobolEnterStmt(@Nullable final CobolEnterStmt enter) {
		super.handleCobolEnterStmt(enter);
		enters.add(enter);
	}
	
	@Override
	protected void handleExecAdabasFind(@Nullable final ExecAdabasFind find) {
		super.handleExecAdabasFind(find);
		execAdabasQueryNodes.add(find);
		adabas(find);
	}

	@Override
	protected void handleExecAdabasHistogram(@Nullable final ExecAdabasHistogram histogram) {
		super.handleExecAdabasHistogram(histogram);
		execAdabasQueryNodes.add(histogram);
		adabas(histogram);
	}

	@Override
	protected void handleExecAdabasReadIsn(@Nullable final ExecAdabasReadIsn readIsn) {
		super.handleExecAdabasReadIsn(readIsn);
		execAdabasQueryNodes.add(readIsn);
		adabas(readIsn);
	}

	@Override
	protected void handleExecAdabasReadLogical(@Nullable final ExecAdabasReadLogical readLogical) {
		super.handleExecAdabasReadLogical(readLogical);
		execAdabasQueryNodes.add(readLogical);
		adabas(readLogical);
	}

	@Override
	protected void handleExecAdabasReadPhysical(@Nullable final ExecAdabasReadPhysical readPhysical) {
		super.handleExecAdabasReadPhysical(readPhysical);
		execAdabasQueryNodes.add(readPhysical);
		adabas(readPhysical);
	}

	@Override
	protected void handleAdaDeclareStmt(@Nullable final AdaDeclareStmt declare) {
		super.handleAdaDeclareStmt(declare);
		adaDeclareStmts.add(declare);
		adaprep(declare);
	}
	
	@Override
	protected void handleCobolUnknownToken(final @Nullable CobolUnknownToken unknownToken) {
		super.handleCobolUnknownToken(unknownToken);
		if (unknownToken != null) {
			cobolUnknownTokens.add(unknownToken);
		}
	}

	@Override
	protected void handleAdaUnknownStmt(final @Nullable AdaUnknownStmt unknownStatement) {
		super.handleAdaUnknownStmt(unknownStatement);
		if (unknownStatement != null) {
			adaUnknownStmts.add(unknownStatement);
			adaprep(unknownStatement);
		}
	}

	@Override
	protected void handleAdaUnknownToken(final @Nullable AdaUnknownToken unknownToken) {
		super.handleAdaUnknownToken(unknownToken);
		if (unknownToken != null) {
			adaUnknownTokens.add(unknownToken);
			adaprep(unknownToken);
		}
	}

	@Override
	protected void handleExecUnknownToken(final @Nullable ExecUnknownToken<?> unknownToken) {
		super.handleExecUnknownToken(unknownToken);
		if (unknownToken != null) {
			execUnknownTokens.add(unknownToken);
		}
	}

	@Override
	protected void handleCobolError(final CobolParserError parserError) {
		super.handleCobolError(parserError);
		cobolParserErrors.add(parserError);
	}
	
	@Override
	protected void handleExecAdabasClose(@Nullable final ExecAdabasClose node) {
		super.handleExecAdabasClose(node);
		adabas(node);
	}

	@Override
	protected void handleExecAdabasCommitWork(@Nullable final ExecAdabasCommitWork node) {
		super.handleExecAdabasCommitWork(node);
		adabas(node);
	}

	@Override
	protected void handleExecAdabasDbClose(@Nullable final ExecAdabasDbClose node) {
		super.handleExecAdabasDbClose(node);
		adabas(node);
	}

	@Override
	protected void handleExecAdabasDbIdOption(@Nullable final ExecAdabasDbIdOption node) {
		super.handleExecAdabasDbIdOption(node);
		adabas(node);
	}

	@Override
	protected void handleExecAdabasDelete(@Nullable final ExecAdabasDelete node) {
		super.handleExecAdabasDelete(node);
		adabas(node);
	}

	@Override
	protected void handleExecAdabasFetch(@Nullable final ExecAdabasFetch node) {
		super.handleExecAdabasFetch(node);
		adabas(node);
	}

	@Override
	protected void handleExecAdabasHold(@Nullable final ExecAdabasHold node) {
		super.handleExecAdabasHold(node);
		adabas(node);
	}

	@Override
	protected void handleExecAdabasHoldOption(@Nullable final ExecAdabasHoldOption node) {
		super.handleExecAdabasHoldOption(node);
		adabas(node);
	}

	@Override
	protected void handleExecAdabasInsert(@Nullable final ExecAdabasInsert node) {
		super.handleExecAdabasInsert(node);
		adabas(node);
	}

	@Override
	protected void handleExecAdabasIsnOption(@Nullable final ExecAdabasIsnOption node) {
		super.handleExecAdabasIsnOption(node);
		adabas(node);
	}

	@Override
	protected void handleExecAdabasOpen(@Nullable final ExecAdabasOpen node) {
		super.handleExecAdabasOpen(node);
		adabas(node);
	}

	@Override
	protected void handleExecAdabasPrefixOption(@Nullable final ExecAdabasPrefixOption node) {
		super.handleExecAdabasPrefixOption(node);
		adabas(node);
	}

	@Override
	protected void handleExecAdabasReadUserData(@Nullable final ExecAdabasReadUserData node) {
		super.handleExecAdabasReadUserData(node);
		adabas(node);
	}

	@Override
	protected void handleExecAdabasRelease(@Nullable final ExecAdabasRelease node) {
		super.handleExecAdabasRelease(node);
		adabas(node);
	}

	@Override
	protected void handleExecAdabasReleaseIsn(@Nullable final ExecAdabasReleaseIsn node) {
		super.handleExecAdabasReleaseIsn(node);
		adabas(node);
	}

	@Override
	protected void handleExecAdabasRollbackWork(@Nullable final ExecAdabasRollbackWork node) {
		super.handleExecAdabasRollbackWork(node);
		adabas(node);
	}

	@Override
	protected void handleExecAdabasSaveOption(@Nullable final ExecAdabasSaveOption node) {
		super.handleExecAdabasSaveOption(node);
		adabas(node);
	}

	@Override
	protected void handleExecAdabasSequenceOption(@Nullable final ExecAdabasSequenceOption node) {
		super.handleExecAdabasSequenceOption(node);
		adabas(node);
	}

	@Override
	protected void handleExecAdabasSuffixOption(@Nullable final ExecAdabasSuffixOption node) {
		super.handleExecAdabasSuffixOption(node);
		adabas(node);
	}

	@Override
	protected void handleExecAdabasUpdate(@Nullable final ExecAdabasUpdate node) {
		super.handleExecAdabasUpdate(node);
		adabas(node);
	}

	@Override
	protected void handleExecAdabasWhereCurrentOf(@Nullable final ExecAdabasWhereCurrentOf node) {
		super.handleExecAdabasWhereCurrentOf(node);
		adabas(node);
	}

	@Override
	protected void handleExecAdabasWhereIsn(@Nullable final ExecAdabasWhereIsn node) {
		super.handleExecAdabasWhereIsn(node);
		adabas(node);
	}

	@Override
	protected void handleExecAdabasXDbIdOption(@Nullable final ExecAdabasXDbIdOption node) {
		super.handleExecAdabasXDbIdOption(node);
		adabas(node);
	}

	/* Adaprep */

	@Override
	protected void handleAdaAbortStmt(@Nullable final AdaAbortStmt node) {
		super.handleAdaAbortStmt(node);
		adaprep(node);
	}

	@Override
	protected void handleAdaAddStmt(@Nullable final AdaAddStmt node) {
		super.handleAdaAddStmt(node);
		adaprep(node);
	}

	@Override
	protected void handleAdaBTStmt(@Nullable final AdaBTStmt node) {
		super.handleAdaBTStmt(node);
		adaprep(node);
	}

	@Override
	protected void handleAdaCallStmt(@Nullable final AdaCallStmt node) {
		super.handleAdaCallStmt(node);
		adaprep(node);
	}

	@Override
	protected void handleAdaCloseStmt(@Nullable final AdaCloseStmt node) {
		super.handleAdaCloseStmt(node);
		adaprep(node);
	}

	@Override
	protected void handleAdaDclRecStmt(@Nullable final AdaDclRecStmt node) {
		super.handleAdaDclRecStmt(node);
		adaprep(node);
	}

	@Override
	protected void handleAdaDeleteStmt(@Nullable final AdaDeleteStmt node) {
		super.handleAdaDeleteStmt(node);
		adaprep(node);
	}

	@Override
	protected void handleAdaEndTransactionStmt(@Nullable final AdaEndTransactionStmt node) {
		super.handleAdaEndTransactionStmt(node);
		adaprep(node);
	}

	@Override
	protected void handleAdaFileStmt(@Nullable final AdaFileStmt node) {
		super.handleAdaFileStmt(node);
		adaprep(node);
	}

	@Override
	protected void handleAdaFindStmt(@Nullable final AdaFindStmt node) {
		super.handleAdaFindStmt(node);
		adaprep(node);
	}

	@Override
	protected void handleAdaHoldStmt(@Nullable final AdaHoldStmt node) {
		super.handleAdaHoldStmt(node);
		adaprep(node);
	}

	@Override
	protected void handleAdaLangStmt(@Nullable final AdaLangStmt node) {
		super.handleAdaLangStmt(node);
		adaprep(node);
	}

	@Override
	protected void handleAdaModeStmt(@Nullable final AdaModeStmt node) {
		super.handleAdaModeStmt(node);
		adaprep(node);
	}

	@Override
	protected void handleAdaNameStmt(@Nullable final AdaNameStmt node) {
		super.handleAdaNameStmt(node);
		adaprep(node);
	}

	@Override
	protected void handleAdaOptionsStmt(@Nullable final AdaOptionsStmt node) {
		super.handleAdaOptionsStmt(node);
		adaprep(node);
	}

	@Override
	protected void handleAdaReadIsnStmt(@Nullable final AdaReadIsnStmt node) {
		super.handleAdaReadIsnStmt(node);
		adaprep(node);
	}

	@Override
	protected void handleAdaReadLogicalStmt(@Nullable final AdaReadLogicalStmt node) {
		super.handleAdaReadLogicalStmt(node);
		adaprep(node);
	}

	@Override
	protected void handleAdaReadNextStmt(@Nullable final AdaReadNextStmt node) {
		super.handleAdaReadNextStmt(node);
		adaprep(node);
	}

	@Override
	protected void handleAdaReadSequenceStmt(@Nullable final AdaReadSequenceStmt node) {
		super.handleAdaReadSequenceStmt(node);
		adaprep(node);
	}

	@Override
	protected void handleAdaReadValueStmt(@Nullable final AdaReadValueStmt node) {
		super.handleAdaReadValueStmt(node);
		adaprep(node);
	}

	@Override
	protected void handleAdaReleaseIsnStmt(@Nullable final AdaReleaseIsnStmt node) {
		super.handleAdaReleaseIsnStmt(node);
		adaprep(node);
	}

	@Override
	protected void handleAdaReleaseStmt(@Nullable final AdaReleaseStmt node) {
		super.handleAdaReleaseStmt(node);
		adaprep(node);
	}

	@Override
	protected void handleAdaRUserDataStmt(@Nullable final AdaRUserDataStmt node) {
		super.handleAdaRUserDataStmt(node);
		adaprep(node);
	}

	@Override
	protected void handleAdaTeleStmt(@Nullable final AdaTeleStmt node) {
		super.handleAdaTeleStmt(node);
		adaprep(node);
	}

	@Override
	protected void handleAdaUpdateStmt(@Nullable final AdaUpdateStmt node) {
		super.handleAdaUpdateStmt(node);
		adaprep(node);
	}

	@Override
	protected void handleAdaUserStmt(@Nullable final AdaUserStmt node) {
		super.handleAdaUserStmt(node);
		adaprep(node);
	}

	@Override
	protected void handleAdaXrefStmt(@Nullable final AdaXrefStmt node) {
		super.handleAdaXrefStmt(node);
		adaprep(node);
	}

	/* Exec Cics */

	@Override
	protected void handleExecCicsAbend(@Nullable final ExecCicsAbend node) {
		super.handleExecCicsAbend(node);
		cics(node);
	}
	
	
	@Override
	public void handleExecSqlValues(final ExecSqlValues<?> node) {
		super.handleExecSqlValues(node);
		sql(node);
	}
	
	@Override
	public void handleExecSqlDeclareAlias(final ExecSqlDeclareAlias<?> node) {
		super.handleExecSqlDeclareAlias(node);
		sql(node);
	}
	
	@Override
	public void handleExecSqlDeclareTransaction(final ExecSqlDeclareTransaction<?> node) {
		super.handleExecSqlDeclareTransaction(node);
		sql(node);
	}
	
	@Override
	public void handleExecSqlExecuteImmediate(final ExecSqlExecuteImmediate<?> node) {
		super.handleExecSqlExecuteImmediate(node);
		sql(node);
	}
	
	@Override
	public void handleExecSqlGetError(final ExecSqlGetError<?> node) {
		super.handleExecSqlGetError(node);
		sql(node);
	}

	@Override
	protected void handleExecCicsAddress(@Nullable final ExecCicsAddress node) {
		super.handleExecCicsAddress(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsAddressSet(@Nullable final ExecCicsAddressSet node) {
		super.handleExecCicsAddressSet(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsAllocate(@Nullable final ExecCicsAllocate node) {
		super.handleExecCicsAllocate(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsAssign(@Nullable final ExecCicsAssign node) {
		super.handleExecCicsAssign(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsBifDeedit(@Nullable final ExecCicsBifDeedit node) {
		super.handleExecCicsBifDeedit(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsBrowse(@Nullable final ExecCicsBrowse node) {
		super.handleExecCicsBrowse(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsCancel(@Nullable final ExecCicsCancel node) {
		super.handleExecCicsCancel(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsChangePassword(@Nullable final ExecCicsChangePassword node) {
		super.handleExecCicsChangePassword(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsConnectProcess(@Nullable final ExecCicsConnectProcess node) {
		super.handleExecCicsConnectProcess(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsContainer(@Nullable final ExecCicsContainer node) {
		super.handleExecCicsContainer(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsDelay(@Nullable final ExecCicsDelay node) {
		super.handleExecCicsDelay(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsDeq(@Nullable final ExecCicsDeq node) {
		super.handleExecCicsDeq(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsDumpTransaction(@Nullable final ExecCicsDumpTransaction node) {
		super.handleExecCicsDumpTransaction(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsEnableProgram(@Nullable final ExecCicsEnableProgram node) {
		super.handleExecCicsEnableProgram(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsEnq(@Nullable final ExecCicsEnq node) {
		super.handleExecCicsEnq(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsEnterTracenum(@Nullable final ExecCicsEnterTracenum node) {
		super.handleExecCicsEnterTracenum(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsExtractExit(@Nullable final ExecCicsExtractExit node) {
		super.handleExecCicsExtractExit(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsFree(@Nullable final ExecCicsFree node) {
		super.handleExecCicsFree(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsFreemain(@Nullable final ExecCicsFreemain node) {
		super.handleExecCicsFreemain(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsGetmain(@Nullable final ExecCicsGetmain node) {
		super.handleExecCicsGetmain(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsHandleAbend(@Nullable final ExecCicsHandleAbend node) {
		super.handleExecCicsHandleAbend(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsHandleAid(@Nullable final ExecCicsHandleAid node) {
		super.handleExecCicsHandleAid(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsHandleCondition(@Nullable final ExecCicsHandleCondition node) {
		super.handleExecCicsHandleCondition(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsIgnoreCondition(@Nullable final ExecCicsIgnoreCondition node) {
		super.handleExecCicsIgnoreCondition(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsInquireConnection(@Nullable final ExecCicsInquireConnection node) {
		super.handleExecCicsInquireConnection(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsInquireNetname(@Nullable final ExecCicsInquireNetname node) {
		super.handleExecCicsInquireNetname(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsInquireTaskList(@Nullable final ExecCicsInquireTaskList node) {
		super.handleExecCicsInquireTaskList(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsInquireTerminal(@Nullable final ExecCicsInquireTerminal node) {
		super.handleExecCicsInquireTerminal(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsInquireTransaction(@Nullable final ExecCicsInquireTransaction node) {
		super.handleExecCicsInquireTransaction(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsInquireTsqname(@Nullable final ExecCicsInquireTsqname node) {
		super.handleExecCicsInquireTsqname(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsInvokeWebservice(@Nullable final ExecCicsInvokeWebservice node) {
		super.handleExecCicsInvokeWebservice(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsIssueDisconnect(@Nullable final ExecCicsIssueDisconnect node) {
		super.handleExecCicsIssueDisconnect(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsJournal(@Nullable final ExecCicsJournal node) {
		super.handleExecCicsJournal(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsLoad(@Nullable final ExecCicsLoad node) {
		super.handleExecCicsLoad(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsPopHandle(@Nullable final ExecCicsPopHandle node) {
		super.handleExecCicsPopHandle(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsPost(@Nullable final ExecCicsPost node) {
		super.handleExecCicsPost(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsPurgeMessage(@Nullable final ExecCicsPurgeMessage node) {
		super.handleExecCicsPurgeMessage(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsPushHandle(@Nullable final ExecCicsPushHandle node) {
		super.handleExecCicsPushHandle(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsQuerySecurity(@Nullable final ExecCicsQuerySecurity node) {
		super.handleExecCicsQuerySecurity(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsReceive(@Nullable final ExecCicsReceive node) {
		super.handleExecCicsReceive(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsRelease(@Nullable final ExecCicsRelease node) {
		super.handleExecCicsRelease(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsRetrieve(@Nullable final ExecCicsRetrieve node) {
		super.handleExecCicsRetrieve(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsRoute(@Nullable final ExecCicsRoute node) {
		super.handleExecCicsRoute(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsSend(@Nullable final ExecCicsSend node) {
		super.handleExecCicsSend(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsSendControl(@Nullable final ExecCicsSendControl node) {
		super.handleExecCicsSendControl(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsSendPage(@Nullable final ExecCicsSendPage node) {
		super.handleExecCicsSendPage(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsSendText(@Nullable final ExecCicsSendText node) {
		super.handleExecCicsSendText(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsSetConnection(@Nullable final ExecCicsSetConnection node) {
		super.handleExecCicsSetConnection(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsSetFile(@Nullable final ExecCicsSetFile node) {
		super.handleExecCicsSetFile(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsSetStatistics(@Nullable final ExecCicsSetStatistics node) {
		super.handleExecCicsSetStatistics(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsSetTdQueue(@Nullable final ExecCicsSetTdQueue node) {
		super.handleExecCicsSetTdQueue(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsSetTerminal(@Nullable final ExecCicsSetTerminal node) {
		super.handleExecCicsSetTerminal(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsSignoff(@Nullable final ExecCicsSignoff node) {
		super.handleExecCicsSignoff(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsSignon(@Nullable final ExecCicsSignon node) {
		super.handleExecCicsSignon(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsSoapfaultCreate(@Nullable final ExecCicsSoapfaultCreate node) {
		super.handleExecCicsSoapfaultCreate(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsStart(@Nullable final ExecCicsStart node) {
		super.handleExecCicsStart(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsSyncpoint(@Nullable final ExecCicsSyncpoint node) {
		super.handleExecCicsSyncpoint(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsSyncpointRollback(@Nullable final ExecCicsSyncpointRollback node) {
		super.handleExecCicsSyncpointRollback(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsTime(@Nullable final ExecCicsTime node) {
		super.handleExecCicsTime(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsVerifyPassword(@Nullable final ExecCicsVerifyPassword node) {
		super.handleExecCicsVerifyPassword(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsWaitEvent(@Nullable final ExecCicsWaitEvent node) {
		super.handleExecCicsWaitEvent(node);
		cics(node);
	}

	@Override
	protected void handleExecCicsWriteOperator(@Nullable final ExecCicsWriteOperator node) {
		super.handleExecCicsWriteOperator(node);
		cics(node);
	}

	/* Exec SQL */

	@Override
	public void handleExecSqlAllocateCursor(final ExecSqlAllocateCursor node) {
		super.handleExecSqlAllocateCursor(node);
		sql(node);
	}

	@Override
	public void handleExecSqlAlterSession(final ExecSqlAlterSession node) {
		super.handleExecSqlAlterSession(node);
		sql(node);
	}

	@Override
	public void handleExecSqlAssociateLocators(final ExecSqlAssociateLocators node) {
		super.handleExecSqlAssociateLocators(node);
		sql(node);
	}

	@Override
	public void handleExecSqlClose(final ExecSqlClose node) {
		super.handleExecSqlClose(node);
		sql(node);
	}

	@Override
	public void handleExecSqlCommit(final ExecSqlCommit node) {
		super.handleExecSqlCommit(node);
		sql(node);
	}

	@Override
	public void handleExecSqlConnect(final ExecSqlConnect node) {
		super.handleExecSqlConnect(node);
		sql(node);
	}

	@Override
	public void handleExecSqlCreateIndex(final ExecSqlCreateIndex node) {
		super.handleExecSqlCreateIndex(node);
		sql(node);
	}

	@Override
	public void handleExecSqlExecutePlSql(final ExecSqlExecutePlSql node) {
		super.handleExecSqlExecutePlSql(node);
		sql(node);
	}

	@Override
	public void handleExecSqlExecutePrepared(final ExecSqlExecutePrepared node) {
		super.handleExecSqlExecutePrepared(node);
		sql(node);
	}

	@Override
	public void handleExecSqlFetch(final ExecSqlFetch node) {
		super.handleExecSqlFetch(node);
		sql(node);
	}

	@Override
	public void handleExecSqlFor(final ExecSqlFor node) {
		super.handleExecSqlFor(node);
		sql(node);
	}

	@Override
	public void handleExecSqlGetDiagnostics(final ExecSqlGetDiagnostics node) {
		super.handleExecSqlGetDiagnostics(node);
		sql(node);
	}

	@Override
	public void handleExecSqlOpen(final ExecSqlOpen node) {
		super.handleExecSqlOpen(node);
		sql(node);
	}

	@Override
	public void handleExecSqlPrepare(final ExecSqlPrepare node) {
		super.handleExecSqlPrepare(node);
		sql(node);
	}

	@Override
	public void handleExecSqlRollback(final ExecSqlRollback node) {
		super.handleExecSqlRollback(node);
		sql(node);
	}

	@Override
	public void handleExecSqlSet(final ExecSqlSet node) {
		super.handleExecSqlSet(node);
		sql(node);
	}

	@Override
	public void handleExecSqlVar(final ExecSqlVar node) {
		super.handleExecSqlVar(node);
		sql(node);
	}

	@Override
	public void handleExecSqlWhenever(final ExecSqlWhenever node) {
		super.handleExecSqlWhenever(node);
		sql(node);
	}

	@Override
	protected void handleCobolIfStmt(@Nullable final CobolIfStmt ifStmt) {
		super.handleCobolIfStmt(ifStmt);

		if (ifStmt != null) {
			conditional("IF " + ifStmt.getCondition());
		}
	}

	@Override
	protected void handleCobolEvaluateStmt(@Nullable final CobolEvaluateStmt evaluateStmt) {
		super.handleCobolEvaluateStmt(evaluateStmt);
		if (evaluateStmt == null) {
			return;
		}

		final String evaluate = evaluateStmt.getEvaluations().stream()
				.map(eval -> eval.isTrueEvaluation() ? "TRUE" : CobolStatementUtility.getCobolStatement(assertNotNull(eval.getEvaluation())))
				.collect(Collectors.joining(" "));

		final Stream<?> stream = evaluateStmt.getChildren().stream();
		final String whens = stream.filter(CobolWhenStmt.class::isInstance)
				.map(CobolWhenStmt.class::cast)
				.map(when -> {
					if (when.isOther()) {
						return "OTHER";
					} else {
						return CobolStatementUtility.flattenConditions(when.getConditions());
					}
				})
				.collect(Collectors.joining(", ", "[", "]"));
		/* Consider not adding EVALUATE keyword to expressions without actual EVALUATE keyword in the code. */
		if (! evaluate.startsWith("EVALUATE ")) {
			conditional("EVALUATE " + evaluate + " " + whens);
		} else {
		conditional(evaluate + " " + whens);
		}
	}

	@Override
	protected void handleCobolSearchStmt(@Nullable final CobolSearchStmt searchStmt) {
		super.handleCobolSearchStmt(searchStmt);
		if (searchStmt == null) {
			return;
		}

		final Stream<?> stream = searchStmt.getWhenBlocks().stream();
		final String whens = stream.filter(CobolWhenStmt.class::isInstance)
				.map(CobolWhenStmt.class::cast)
				.map(CobolWhenStmt::getConditions)
				.map(CobolStatementUtility::flattenConditions)
				.collect(Collectors.joining(", ", "[", "]"));

		conditional(CobolStatementUtility.getCobolStatement(searchStmt.getTableName()) + " " + whens);
	}
	
	@Override
	protected void handleCobolOpenStmt(final @Nullable CobolOpenStmt open) {
		openStatements.add(open);
	}

	@Override
	protected void handleCobolCallStmt(@Nullable final CobolCallStmt call) {
		super.handleCobolCallStmt(call);
		calls.add(call);

		if (call == null) {
			throw (new IllegalStateException("The CobolCallStmt is null."));
		}
					
		final var usingString = call.getUsings().stream()
				.map(CobolUsing::getExpression)
				.filter(Objects::nonNull)
				.map(CobolReferenceExpression.class::cast)
				.map(CobolReferenceExpression::getOp1)
				.filter(op -> isReferenceSupportedElseLog(call, op))
				.map(reference -> (reference  instanceof CobolFieldReference) ? ((CobolFieldReference) reference).getField() : ((CobolConstantReference) reference).getValue())
				.map(this::getValuesFromField)
				.collect(Collectors.joining(", ", "[", "]"));
		
		if (call.getTarget().toString().startsWith("EZA")){
			call(String.format("CALL %s USING: %s {calltype : TCP/IP-SOCKET-PGM}", call.getTarget(), usingString));				
		} else {
			call(String.format("CALL %s USING: %s%s", call.getTarget(), usingString, imsCommandType(usingString)));
		}
	}

	protected String imsCommandType(final String usingString) {
		for(final String s: Arrays.asList("AUTH", "CHNG", "CMD", "GCMD", "GN", "GU", "ICAL", "ISRT", "PURG", "SETO")) {
			if (usingString.contains("['" + s + "'"))
				return (" {calltype : IMS-TM-MSG}");
		}
		for(final String s: Arrays.asList("APSB", "CHKP", "DPSB", "GMSG", "GSCD", "ICMD", "INIT", "INQY", "LOG",
														"RCMD", "ROLB", "ROLL", "ROLS", "SETS", "SETU", "SYNC", "XRST")){
			if (usingString.contains("['" + s + "'"))
				return (" {calltype : IMS-SYS-SERVICE}");
		}
		for(final String s:  Arrays.asList("CIMS", "CLSE", "DEQ", "DLET", "FLD", "GH", "GN", "GU", "ISRT", "OPEN", "POS", "REPL", "RLSE")){
			if (usingString.contains("['" + s))
				return ("{calltype : IMS-DB-MNGMNT}");
		}
		return "";
	}


	@Override
	protected void handleCobolEntryStmt(@Nullable final CobolEntryStmt entry) {
		super.handleCobolEntryStmt(entry);
		if (entry == null) {
			throw (new IllegalStateException("The CobolEntryStmt is null."));
		}
		
		final var usingString = 
				entry.getUsings().stream()
				.map(CobolFieldReference.class::cast)
				.map(CobolFieldReference::getField)
				.map(this::getValuesFromField)
				.collect(Collectors.joining(", ", "[", "]"));
				
		entry(String.format("ENTRY %s USING: %s", entry.getName(), usingString));
	}
	
	private String getValuesFromField(@Nullable final Object field) {
		if (field == null) return "";
		if (field instanceof CobolDataField) {
			final CobolDataField cobolField = (CobolDataField) field;
			final List<Object> values = cobolField.getValues();
			if ( ! values.isEmpty()) {
				return values.get(0).toString();
			} else {
				return cobolField.getName() != null ? cobolField.getName() : StringUtils.EMPTY;
			}
		} else {
			return field.toString();
		}
	}
	
	private boolean isReferenceSupportedElseLog(final CobolCallStmt call, final CobolReference reference) {
		if (reference instanceof CobolFieldReference || reference instanceof CobolConstantReference) {
			return true;
		}
		LOG.error(() -> "Cobol Reference type " + reference.getClass().getName() 
				     + " is not supported yet for call '" + call.toString() 
				     + "'. Will skip this reference type.");
		return false;
	}
	
	private void cics(@Nullable final ExecCicsStatement node) {
		if (node != null) {
			statements.add(new ModelStatement()
										.setStatementType(StatementType.EXEC_CICS)
										.setString(CobolStatementUtility.getCobolStatement(node))
										.validate());
		}
	}
	
	private void adabas(@Nullable final CobolNode node) {
		if (node != null) {
			statements.add(new ModelStatement()
									.setStatementType(StatementType.EXEC_ADABAS)
									.setString(CobolStatementUtility.getCobolStatement(node))
									.validate());
		}
	}

	private void adaprep(@Nullable final CobolNode node) {
		if (node != null) {
			statements.add(new ModelStatement()
										.setStatementType(StatementType.ADAPREP)
										.setString(CobolStatementUtility.getCobolStatement(node))
										.validate());
		}
	}

	private void sql(final ExecSqlNode<?> node) {
		if ((node.getParent() instanceof ExecSqlNode)) {
			return;
		}
		statements.add(new ModelStatement()
								.setStatementType(StatementType.EXEC_SQL)
								.setString(ExecStatementUtility.getExecStatement(node, isDebugging))
								.validate());
	}
	
	private void sqlSchema(final ExecNode<?> node) {
		statements.add(new ModelStatement()
								.setStatementType(StatementType.RDB_DATABASE)
								.setString(ExecStatementUtility.getExecStatement(node))
								.validate());
	}
	
	private void conditional(final String statement) {
		statements.add(new ModelStatement()
								.setStatementType(StatementType.CONDITIONAL)
								.setString(statement)
								.validate());
	}

	private void call(final String statement) {
		statements.add(new ModelStatement()
								.setStatementType(StatementType.CALL)
								.setString(statement)
								.validate());
	}

	private void entry(final String statement) {
		statements.add(new ModelStatement()
								.setStatementType(StatementType.ENTRY)
								.setString(statement)
								.validate());
	}
}

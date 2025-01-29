/* Copyright (c) 2018 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.metrics.exec;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;

import innowake.lib.calcite.sql.parser.SqlParseException;
import innowake.lib.core.api.lang.NonNull;
import innowake.lib.core.lang.Assert;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.model.discovery.attribute.ModelAttributeValue.SqlStatementType;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.parser.sql.SimpleSqlCollector;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlAllocateCursor;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlAlterSession;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlAssociateLocators;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlCall;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlClose;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlComment;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlCommit;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlCommonTableExpression;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlConnect;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlCreateIndex;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDeclareCursor;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDeclareSchema;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDeclareTable;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDeclareTempTable;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDelete;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDescribe;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDescribeProcedure;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDrop;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlFetch;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlFor;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlIndicator;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlInsert;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlLockTable;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlOpen;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlPrepare;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlRollback;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlSavepoint;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlSelect;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlSet;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlTruncate;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlUpdate;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlWhenever;
import innowake.ndt.core.parsing.exec.ExecSqlVisitor;

/**
 * Use in conjunction with ndt cobol parser objects to get data from EXEC SQL
 * statements that is useful for discovery
 */
@SuppressWarnings("rawtypes") 
public class ExecSqlCollector implements ExecSqlVisitor {

	private static final Logger SQL_LOG = LoggerFactory.getLogger(Logging.SQL_PARSER);

	private final Map<String, List<SqlStatementType>> referencedTables = new HashMap<>();

	public Map<String, List<SqlStatementType>> getReferencedTables() {
		return referencedTables;
	}

	boolean isDebugging;

	public void setDebugging(final boolean debugging) {
		isDebugging = debugging;
	}

	public boolean isDebugging() {
		return isDebugging;
	}

	/**
	 * Retrieve data like tables from EXEC SQL SELECT statement.
	 *
	 * @param execSqlSelect {@link ExecSqlSelect}.
	 */
	@Override
	public void handleExecSqlSelect(final ExecSqlSelect<?> execSqlSelect) {
		final String execStatement = ExecStatementUtility.getExecStatement(execSqlSelect, isDebugging);
		getSqlTables(execStatement, SqlStatementType.SELECT);
	}

	/**
	 *
	 * Retrieve data (like tables) from EXEC SQL CURSOR statement.
	 */
	@Override
	public void handleExecSqlDeclareCursor(final ExecSqlDeclareCursor<?> arg0) {
	}

	/**
	 *
	 * Retrieve data (e.g. table) from SQL DECLARE TABLE statement
	 */
	@Override
	public void handleExecSqlDeclareTable(final ExecSqlDeclareTable declareTable) {
		/* Documentation is somewhat contradictory on location of tokens, but it seems
		 it should be DECLARE /<table_name/> TABLE (though it is sometimes transposed
		 in examples */
		final String tableName = declareTable.getTableName();
		if (StringUtils.isNotBlank(tableName)) {
			addReference(tableName, SqlStatementType.DECLARE_TABLE);
		}
	}

	@Override
	public void handleExecSqlDeclareTempTable(final ExecSqlDeclareTempTable tempTable) {
		final String tableName = tempTable.getTableName();
		if (StringUtils.isNotBlank(tableName)) {
			addReference(tableName, SqlStatementType.DECLARE_TEMP_TABLE);
		}
	}

	/**
	 *
	 * Retrieve data (e.g. table) from SQL DELETE statement
	 */
	@Override
	public void handleExecSqlDelete(final ExecSqlDelete<?> arg0) {
		final String execStatement = ExecStatementUtility.getExecStatement(arg0, isDebugging);
		getSqlTables(execStatement, SqlStatementType.DELETE);
	}

	/**
	 *
	 * Retrieve data (e.g. table) from SQL INSERT statement
	 */
	@Override
	public void handleExecSqlInsert(final ExecSqlInsert<?> arg0) {
		final String execStatement = ExecStatementUtility.getExecStatement(arg0, isDebugging);
		getSqlTables(execStatement, SqlStatementType.INSERT);
	}

	/**
	 *
	 * Retrieve data (e.g. table) from SQL LOCK statement
	 */
	@Override
	public void handleExecSqlLockTable(final ExecSqlLockTable<?> arg0) {
		final String tableName = arg0.getTable();
		if (StringUtils.isNotBlank(tableName)) {
			addReference(Assert.assertNotNull(tableName), SqlStatementType.LOCK_TABLE);
		}
	}

	/**
	 *
	 * Retrieve data (e.g. table) from SQL UPDATE statement
	 *
	 */
	@Override
	public void handleExecSqlUpdate(final ExecSqlUpdate<?> arg0) {
		final String execStatement = ExecStatementUtility.getExecStatement(arg0, isDebugging);
		getSqlTables(execStatement, SqlStatementType.UPDATE);
	}

	@Override
	public void handleExecSqlDeclareSchema(final ExecSqlDeclareSchema arg0) {
		final String schemaFileName = arg0.getSchemaFileName();
		if (StringUtils.isNotBlank(schemaFileName)) {
			addReference(schemaFileName, SqlStatementType.CALL);
		}
	}
	
	@Override
	public void handleExecSqlCall(final ExecSqlCall<?> o) {
	}
	
	@Override
	public void handleExecSqlCommonTableExpression(final ExecSqlCommonTableExpression<?> o) {
	}
	
	@Override
	public void handleExecSqlAssociateLocators(final ExecSqlAssociateLocators<?> o) {
	}
	
	@Override
	public void handleExecSqlIndictor(final ExecSqlIndicator<?> o) {
	}
	
	@Override
	public void handleExecSqlComment(@NonNull final ExecSqlComment<?> o) {
	}
	
	@Override
	public void handleExecSqlDescribe(final ExecSqlDescribe<?> o) {
	}
	
	@Override
	public void handleExecSqlDescribeProcedure(final ExecSqlDescribeProcedure<?> o) {
	}
	
	@Override
	public void handleExecSqlDrop(final ExecSqlDrop<?> o) {
	}
	
	@Override
	public void handleExecSqlSavepoint(final ExecSqlSavepoint<?> o) {
	}
	
	@Override
	public void handleExecSqlTruncate(final ExecSqlTruncate<?> o) {
	}
	
	@Override
	public void handleExecSqlIndicator(final ExecSqlIndicator<?> o) {
	}

	@Override
	public void handleExecSqlCreateIndex(ExecSqlCreateIndex<?> o) {
	}

	@Override
	public void handleExecSqlAllocateCursor(ExecSqlAllocateCursor<?> o) {
	}

	@Override
	public void handleExecSqlFor(ExecSqlFor<?> o) {
	}
	

	@Override
	public void handleExecSqlPrepare(final ExecSqlPrepare<?> o) {
	}
	
	@Override
	public void handleExecSqlAlterSession(final ExecSqlAlterSession<?> o) {
	}

	@Override
	public void handleExecSqlConnect(final ExecSqlConnect<?> o) {
	}

	@Override
	public void handleExecSqlClose(ExecSqlClose<?> arg0) {
	}

	@Override
	public void handleExecSqlCommit(final ExecSqlCommit<?> arg0) {
	}

	@Override
	public void handleExecSqlFetch(final ExecSqlFetch<?> arg0) {
	}

	@Override
	public void handleExecSqlOpen(final ExecSqlOpen<?> arg0) {
	}

	@Override
	public void handleExecSqlRollback(final ExecSqlRollback<?> arg0) {
	}

	@Override
	public void handleExecSqlSet(final ExecSqlSet<?> arg0) {
	}

	@Override
	public void handleExecSqlWhenever(final ExecSqlWhenever<?> arg0) {
	}
	
	private void addReference(final String tableName, final SqlStatementType type) {
		final List<SqlStatementType> references = referencedTables.computeIfAbsent(tableName, k -> new ArrayList<>());
		if ( ! references.contains(type)) {
			references.add(type);
		}
	}

	/**
	 * Collects all the tables with access type from an SQL query string mapped to
	 * Discovery TableAccess.
	 *
	 * @param processedSql   Processed Cobol.
	 * @param sqlStatementType {@link SqlStatementType}.
	 */
	private void getSqlTables(final String processedSql, final SqlStatementType sqlStatementType) {
		try {
			final List<String> tables = new ArrayList<>(SimpleSqlCollector.getSqlTables(processedSql).keySet());
			tables.forEach(tableName -> addReference(tableName, sqlStatementType));
		} catch (final SqlParseException e) {
			if (SQL_LOG.isWarnEnabled()) {
				   SQL_LOG.warn(String.format("Could not successfully parse sql statement %s of statement type %s",
						processedSql, sqlStatementType));
			}
		}
	}
}

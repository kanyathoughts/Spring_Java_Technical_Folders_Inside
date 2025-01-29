/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.exec;

import java.util.Collections;
import java.util.List;

import org.apache.commons.collections4.set.ListOrderedSet;
import org.apache.commons.lang3.StringUtils;

import innowake.lib.calcite.sql.parser.SqlParseException;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.data.model.discovery.ModelSqlStatement;
import innowake.mining.data.model.discovery.attribute.ModelAttributeValue.SqlStatementType;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.dawn.metrics.contributors.DawnExecStatementUtility;
import innowake.mining.server.discovery.parser.sql.SimpleSqlCollector;
import innowake.mining.server.discovery.parser.sql.SqlMetrics;
import innowake.mining.server.discovery.parser.sql.SqlValidator;
import innowake.mining.shared.model.DatabaseAccessType;
import innowake.mining.shared.model.StatementType;
import innowake.ndt.antlr.c.CToken;
import innowake.ndt.antlr.easytrieve.EasytrieveToken;
import innowake.ndt.cobol.parser.CobolToken;
import innowake.ndt.core.cobol.CobolArea;
import innowake.ndt.core.parsing.ast.model.exec.ExecNode;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDeclareCursor;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlSelect;
import innowake.ndt.core.parsing.exec.Token;
import innowake.ndt.parsing.parser.sql.preprocess.db2.Db2CobolPreprocessor;
import innowake.ndt.pl1parser.parser.exec.Pl1Token;

/**
 * Utility class for handling exec statements.
 */
public class ExecStatementUtility {

	private static final Logger SQL_LOG = LoggerFactory.getLogger(Logging.SQL_PARSER);
	private static final Logger METRICS_LOG = LoggerFactory.getLogger(Logging.METRICS_COLLECTOR);

	private ExecStatementUtility() {
		throw new IllegalStateException();
	}

	/**
	 * Extracts and return the Sql string after processing the non code.
	 *
	 * @param node instance of ExecNode
	 * @return Sql string
	 */
	public static String getExecStatement(final ExecNode<?> node) {
		return getExecStatement(node, false);
	}

	/**
	 * Extracts and return the Sql string after processing the non code.
	 *
	 * @param node instance of ExecNode
	 * @param isDebugging flag to enable debugging mode
	 * @return Sql string
	 */
	@SuppressWarnings("unchecked")
	public static String getExecStatement(final ExecNode<?> node, final boolean isDebugging) {
		String contents = "";
		final var cobolPreprocessor = new Db2CobolPreprocessor(false);
		cobolPreprocessor.setDebuggingMode(isDebugging);
		if (node.getStartToken() instanceof Pl1Token) {
			final String content = ((Pl1Token) node.getStartToken()).getContent();
			contents = content.substring(0, content.length() - 1);
		} else if (node.getStartToken() instanceof CobolToken) {
			final ExecNode<CobolToken> cobolNode = (ExecNode<CobolToken>) node;
			final int start = cobolNode.getStartToken().getOffset() ;
			final int length = getLength(cobolNode);
			contents = cobolNode.getStartToken().getContent().subSequence(start, start + length).toString();
			/* Get the original statement */
			contents = cobolPreprocessor.getSqlString(CobolArea.A.indent(contents));
		} else if (node.getStartToken() instanceof CToken) {
			final ExecNode<CToken> cNode = (ExecNode<CToken>) node;
			/* removes the leading EXEC SQL token and trailing ; token */
			contents = DawnExecStatementUtility.preprocessCSqlStatement(cNode.getStartToken().getContent());
		} else if (node.getStartToken() instanceof EasytrieveToken) {
			final EasytrieveToken startToken = ((ExecNode<EasytrieveToken>) node).getStartToken();
			if (startToken.getContent().trim().startsWith("SQL")) {
				contents = DawnExecStatementUtility.preprocessEasytrieveSqlStatement(startToken.getContent());
			} else {
				contents = StringUtils.stripEnd(startToken.getContent().trim(), ";");
			}
           /* Special case for handling EASYTRIEVE file SQL statements. */
			if (node instanceof ExecSqlSelect<?> && DawnExecStatementUtility.isFileStatementOrSelectStatementContext(node.getParent().toString())) {
				final int intoIndex = StringUtils.indexOf(contents, "INTO");
				if (intoIndex >= -1) {
					contents = StringUtils.substring(contents, 0, intoIndex);
				}
			}

			if (node.getParent() instanceof ExecSqlDeclareCursor<?>) {
				final int index = contents.indexOf(startToken.getText());
				if (index != -1) {
					contents = contents.substring(index).trim();
				}
			}
		}
		return contents;
	}

	/**
	 * Calculates the length of a given node.
	 *
	 * @param node the given node
	 * @return the length of the given node
	 */
	@SuppressWarnings("null")
	private static int getLength(final ExecNode<?> node) {
		final Token startToken = node.getStartToken();
		final Token endToken = node.getEndToken();

		return endToken != null 
				? endToken.getOffset()
				+ endToken.getLength()
				- startToken.getOffset()
				: startToken.getLength();
	}

	/**
	 * Extracts the SQL string from the given {@code sqlNode}, calculates the SQL metrics and adds a new {@link ModelSqlStatement} including the metrics to the
	 * given {@code entry}.
	 *
	 * @param sqlNode the node to extract the SQL string from
	 * @param entry the {@link ModelArtifact}
	 */
	public static void addSqlStatementForExecSql(final ExecNode<?> sqlNode, final ModelArtifact entry) {
		addSqlStatementForExecSql(getExecStatement(sqlNode), entry);
	}
	
	/**
	 * Calculates the SQL metrics for the given {@code sqlString} and adds a new {@link ModelSqlStatement} including the metrics to the given {@code entry}.
	 *
	 * @param sqlString the SQL command
	 * @param entry the {@link ModelArtifact}
	 */
	public static void addSqlStatementForExecSql(final String sqlString, final ModelArtifact entry) {
		try {
			final SqlMetrics sqlMetrics = SimpleSqlCollector.getSqlMetrics(sqlString);
			if (sqlMetrics.getSqlStatementType() == SqlStatementType.UNKNOWN) {
				addUnknownSql(entry, sqlString);
			} else {
				entry.addSqlStatement(new ModelSqlStatement()
						.setStatementType(StatementType.fromName(sqlMetrics.getSqlStatementType().name()))
						.setString(sqlString)
						.setLength(sqlMetrics.getLength())
						.setNumberOfTables(sqlMetrics.getNumberOfTables())
						.setNumberOfDistinctTables(sqlMetrics.getNumberOfDistinctTables())
						.setCustomComplexity(sqlMetrics.getCustomComplexity())
						.setHalsteadComplexity(sqlMetrics.getHalsteadComplexity())
						.setHalsteadDifficulty(sqlMetrics.getHalsteadDifficulty())
						.validate());
			}
		} catch (final SqlParseException e) {
			SQL_LOG.warn("SqlParseException while collecting SQL Metrics : ", e);
			addUnknownSql(entry, sqlString);
		} catch (final Exception e) {
			METRICS_LOG.error("Error while collecting SQL Metrics : ", e);
			addUnknownSql(entry, sqlString);
		}
	}

	private static void addUnknownSql(final ModelArtifact entry, final String sqlString) {
		entry.addSqlStatement(new ModelSqlStatement()
				.setStatementType(StatementType.fromName(SqlValidator.getSqlType(sqlString).name()))
				.setString(sqlString)
				.setLength(sqlString.length())
				.setNumberOfTables(0)
				.setNumberOfDistinctTables(0)
				.setCustomComplexity(0)
				.setHalsteadComplexity(0)
				.setHalsteadDifficulty(0)
				.validate());
	}

	/**
	 * Adds a new {@link ModelSqlStatement} for the given {@link ExecNode} to the given {@code entry}.
	 *
	 * @param node the {@link ExecNode} for which 
	 * @param statementType the {@link StatementType}
	 * @param entry the {@link ModelArtifact}
	 */
	public static void addSqlStatementForNonExecSql(final ExecNode<?> node, final StatementType statementType, final ModelArtifact entry) {
		addSqlStatementForNonExecSql(getExecStatement(node), statementType, entry);
	}

	/**
	 * Adds a new {@link ModelSqlStatement} for the given {@code sqlString} to the given {@code entry}.
	 *
	 * @param sqlString the SQL command
	 * @param statementType the {@link StatementType}
	 * @param entry the {@link ModelArtifact}
	 */
	public static void addSqlStatementForNonExecSql(final String sqlString, final StatementType statementType, final ModelArtifact entry) {
		entry.addSqlStatement(new ModelSqlStatement()
				.setString(sqlString)
				.setLength(sqlString.length())
				.setStatementType(statementType)
				.validate());
	}

	/**
	 * Identifies the db access types in the given {@code dbAccesses} list based on the SQL statement types and returns an unmodifiable list of them with
	 * unique {@link DatabaseAccessType} elements.
	 * 
	 * @param dbAccesses the access types from which the target type is mapped to 
	 * @return the resultant access types list
	 */
	public static List<DatabaseAccessType> getDbAccess(@Nullable final List<?> dbAccesses) {
		if (dbAccesses == null || dbAccesses.isEmpty()) {
			return Collections.emptyList();
		}

		final ListOrderedSet<DatabaseAccessType> accessTypes = new ListOrderedSet<>();
		for (final Object access : dbAccesses) {
			DatabaseAccessType accessType = null;
			if (access instanceof DatabaseAccessType) {
				accessType = (DatabaseAccessType) access;
			} else if (access instanceof SqlStatementType) {
				switch ((SqlStatementType) access) {
					case SELECT:
						accessType = DatabaseAccessType.READ;
						break;
					case INSERT:
						accessType = DatabaseAccessType.STORE;
						break;
					case UPDATE:
						accessType = DatabaseAccessType.UPDATE;
						break;
					case DELETE:
						accessType = DatabaseAccessType.DELETE;
						break;
					case DECLARE_TABLE:
					case LOCK_TABLE:
					default:
						accessType = DatabaseAccessType.OTHER;
						break;
				}
			}
			if (accessType != null && ! accessTypes.contains(accessType)) {
				accessTypes.add(accessType);
			}
		}

		return accessTypes.asList();
	}
}

/* Copyright (c) 2021 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.contributors;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.apache.commons.collections4.set.ListOrderedSet;
import org.apache.commons.lang3.StringUtils;

import innowake.lib.calcite.sql.parser.SqlParseException;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.StatementBuilder;
import innowake.mining.data.model.discovery.ModelSqlStatement;
import innowake.mining.data.model.discovery.attribute.ModelAttributeValue.SqlStatementType;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.parser.sql.SimpleSqlCollector;
import innowake.mining.server.discovery.parser.sql.SqlMetrics;
import innowake.mining.server.discovery.parser.sql.SqlValidator;
import innowake.mining.shared.entities.StatementPojo;
import innowake.mining.shared.model.DatabaseAccessType;
import innowake.mining.shared.model.StatementType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
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
 * Utility class for handling EXEC statements for Dawn.
 */
public class DawnExecStatementUtility {

	private static final Logger SQL_LOG = LoggerFactory.getLogger(Logging.SQL_PARSER);
	private static final Logger METRICS_LOG = LoggerFactory.getLogger(Logging.METRICS_COLLECTOR);
	private static final Pattern SQL_START = Pattern.compile("\\bSQL\\s+");
	private static final Pattern EXEC_SQL_START = Pattern.compile("\\bEXEC\\s+\\bSQL\\s+");
	private static final Pattern SQL_END = Pattern.compile(";");
	private static final String EMPTY = "";
	private static final String FILE_STATEMENT_CONTEXT = "FileStatementContext";
	private static final String SELECT_STATEMENT_CONTEXT = "SelectStatementContext";

	private static final Db2CobolPreprocessor COBOL_PREPROCESSOR = new Db2CobolPreprocessor();

	private DawnExecStatementUtility() {
		/*  Private constructor to hide implicit one of the Util class. */
	}

	/**
	 * Extracts and return the SQL string after processing the non code.
	 *
	 * @param node instance of ExecNode
	 * @return SQL string
	 */
	@SuppressWarnings("unchecked")
	public static String getExecStatement(final ExecNode<?> node) {
		String contents = "";
		if (node.getStartToken() instanceof Pl1Token) {
			final String content = ((Pl1Token) node.getStartToken()).getContent();
			contents = content.substring(0, content.length() - 1);
		} else if (node.getStartToken() instanceof CobolToken) {
			final ExecNode<CobolToken> cobolNode = (ExecNode<CobolToken>) node;
			final int start = cobolNode.getStartToken().getOffset() ;
			final int length = getLength(cobolNode);
			contents = cobolNode.getStartToken().getContent().subSequence(start, start + length).toString();
			/* Get the original statement */
			contents = COBOL_PREPROCESSOR.getSqlString(CobolArea.A.indent(contents));
		} else if (node.getStartToken() instanceof CToken) {
			final ExecNode<CToken> cNode = (ExecNode<CToken>) node;
			/* removes the leading EXEC SQL token and trailing ; token */
			contents = preprocessCSqlStatement(cNode.getStartToken().getContent());
		} else if (node.getStartToken() instanceof EasytrieveToken) {
			final EasytrieveToken startToken = ((ExecNode<EasytrieveToken>) node).getStartToken();
			if (startToken.getContent().trim().startsWith("SQL")) {
				contents = preprocessEasytrieveSqlStatement(startToken.getContent());
			} else {
				contents = StringUtils.stripEnd(startToken.getContent().trim(), ";");
			}
           /* Special case for handling EASYTRIEVE file SQL statements. */
			if (node instanceof ExecSqlSelect<?> && isFileStatementOrSelectStatementContext(node.getParent().toString())) {
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
	 * To verify whether the ExecNode parent is file statement or select statement.
	 *
	 * @param parentContext the ExecNode parent
	 * @return true if statement is fileStatementContext or SelectStatementContext
	 */
	public static boolean isFileStatementOrSelectStatementContext(final String parentContext) {
		return FILE_STATEMENT_CONTEXT.equals(parentContext) || SELECT_STATEMENT_CONTEXT.equals(parentContext);
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
	 * Extract plain SQL from the given SQL string removing the C specific constructs.
	 * 
	 * @param sql the sql to preprocess
	 * @return the preprocessed sql
	 */
	public static String preprocessCSqlStatement(final String sql) {
		return SQL_END.matcher(EXEC_SQL_START.matcher(sql).replaceAll(EMPTY)).replaceAll(EMPTY);
	}
	
	/**
	 * Extract plain SQL from the given SQL string removing the Easytrieve specific constructs.
	 * 
	 * @param sql the sql to preprocess
	 * @return the preprocessed sql
	 */
	public static String preprocessEasytrieveSqlStatement(final String sql) {
		return SQL_END.matcher(SQL_START.matcher(sql).replaceAll(EMPTY)).replaceAll(EMPTY);
	}

	/**
	 * Extracts the SQL string from the given {@code sqlNode}, calculates the SQL metrics and declares a new {@link StatementBuilder} of SQL type
	 * to the given {@link ModuleBuilder}.
	 * 
	 * @param sqlNode the node to extract the SQL string from
	 * @param builder {@link ModuleBuilder} to which the statements need to be added.
	 */
	public static void addSqlStatementForExecSql(final ExecNode<?> sqlNode, final ModuleBuilder builder) {
		addSqlStatementForExecSql(getExecStatement(sqlNode), builder, true);
	}
	
	/**
	 * Calculates the SQL metrics for the given {@code sqlString} and calculates the SQL metrics and declares a new {@link StatementBuilder}
	 *  of SQL type to the given {@link ModuleBuilder}.
	 * 
	 * @param sqlString the SQL command
	 * @param builder the {@link ModuleBuilder}
	 * @param addError {@code true} to add an error to {@code builder} in case of an error
	 */
	public static void addSqlStatementForExecSql(final String sqlString, final ModuleBuilder builder, final boolean addError) {
		try {
			final SqlMetrics sqlMetrics = SimpleSqlCollector.getSqlMetrics(sqlString);
			final SqlStatementType sqlStatementType = sqlMetrics.getSqlStatementType();
			if (sqlStatementType == SqlStatementType.UNKNOWN) {
				addUnknownSql(builder, sqlString);
			} else {
				addSqlStatement(builder, StatementType.fromName(sqlStatementType.toString()), sqlString, sqlMetrics.getLength(), sqlMetrics.getNumberOfTables(),
						sqlMetrics.getNumberOfDistinctTables(), sqlMetrics.getCustomComplexity(), (float) sqlMetrics.getHalsteadComplexity(), 
						(float) sqlMetrics.getHalsteadDifficulty());
			}
		} catch (final SqlParseException e) {
			SQL_LOG.warn("SqlParseException while collecting SQL Metrics for SQL query : {} - {} ", sqlString, e);
			if (addError) {
				builder.addError(Severity.WARNING, ErrorKey.PARSE_ERROR, "SqlParseException while collecting SQL Metrics for SQL: " + sqlString);
			}
			addUnknownSql(builder, sqlString);
		} catch (final Exception e) {
			METRICS_LOG.error("Error while collecting SQL Metrics for SQL query : {} - {} ", sqlString, e);
			if (addError) {
				builder.addError(Severity.WARNING, ErrorKey.METRICS_CALCULATION_ERROR, "Error while collecting SQL Metrics for SQL: " + sqlString);
			}
			addUnknownSql(builder, sqlString);
		}
	}

	public static void addUnknownSql(final ModuleBuilder builder, final String sqlString) {
		addSqlStatement(builder, StatementType.fromName(SqlValidator.getSqlType(sqlString).name()), sqlString, sqlString.length(), 0, 0, 0, 0f, 0f);
	}

	private static void addSqlStatement(final ModuleBuilder builder, final StatementType statementType, final String sqlString, final int sqlLength,
			final int numberOfTables, final int numberOfDistinctTables, final int customComplexity, final float halsteadComplexity, final float halsteadDifficulty) {
		final Map<String, Object> sqlData = new HashMap<>();
		sqlData.put(StatementPojo.PROPERTY_KEY_SQL_LENGTH, sqlLength);
		sqlData.put(StatementPojo.PROPERTY_KEY_TABLES, numberOfTables);
		sqlData.put(StatementPojo.PROPERTY_KEY_DISTINCT_TABLES, numberOfDistinctTables);
		sqlData.put(StatementPojo.PROPERTY_KEY_CUSTOM_COMPLEXITY, customComplexity);
		sqlData.put(StatementPojo.PROPERTY_KEY_HALSTEAD_COMPLEXITY, halsteadComplexity);
		sqlData.put(StatementPojo.PROPERTY_KEY_HALSTEAD_DIFFICULTY, halsteadDifficulty);

		builder.declareStatement(statementType)
					.setText(sqlString)
					.setProperties(sqlData)
					.setTechnology(Technology.SQL);
	}

	/**
	 * Declares a new {@link StatementBuilder} of SQL type to the given
	 * {@link ModuleBuilder}
	 *
	 * @param node the {@link ExecNode} for which 
	 * @param statementType the {@link StatementType}
	 * @param builder the {@link DiscoveryBuilderFromSource}
	 */
	public static void addSqlStatementForNonExecSql(final ExecNode<?> node, final StatementType statementType, final ModuleBuilder builder) {
		addSqlStatementForNonExecSql(getExecStatement(node), statementType, builder);
	}

	/**
	 * Adds a new {@link ModelSqlStatement} for the given {@code sqlString} to the given {@code entry}.
	 *
	 * @param sqlString the SQL command
	 * @param statementType the {@link StatementType}
	 * @param builder the {@link ModuleBuilder}
	 */
	public static void addSqlStatementForNonExecSql(final String sqlString, final StatementType statementType, final ModuleBuilder builder) {
		addSqlStatement(builder, statementType, sqlString, 0, 0, 0, 0, 0f, 0f);
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
			} else if (access instanceof StatementType) {
				switch ((StatementType) access) {
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

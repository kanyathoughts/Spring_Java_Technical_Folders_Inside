package innowake.mining.server.discovery.parser.sql;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import innowake.lib.calcite.sql.SqlNode;
import innowake.lib.calcite.sql.parser.SqlParseException;
import innowake.mining.data.model.discovery.attribute.ModelAttributeValue.SqlStatementType;
import innowake.mining.server.discovery.metrics.cobol.CobolStatementUtility;
import innowake.mining.shared.model.DatabaseAccessType;
import innowake.ndt.parsing.parser.sql.api.SqlParsedTable;
import innowake.ndt.parsing.parser.sql.api.SqlParsedTable.AccessType;
import innowake.ndt.parsing.parser.sql.db2.parse.Db2SqlParser;
import innowake.ndt.parsing.parser.sql.metrics.api.SqlComplexity;
import innowake.ndt.parsing.parser.sql.metrics.visitor.CustomSqlComplexityVisitor;
import innowake.ndt.parsing.parser.sql.visitor.db2.Db2SqlStatementTypeVisitor;
import innowake.ndt.parsing.parser.sql.visitor.db2.Db2SqlTableVisitor;

/**
 * Simple class to handle SQL specifics.
 */
public class SimpleSqlCollector {
	private static final String NON_VALID_SQL = "INVALID SQL: \"%s\", %s";
	private static final String NEW_LINE = "\n";
	private static final Db2SqlTableVisitor SQL_VISITOR = new Db2SqlTableVisitor();
	private static final Db2SqlStatementTypeVisitor SQL_TYPE_VISITOR = new Db2SqlStatementTypeVisitor();
	
	/**
	 *  Pattern for 'LIMIT TO N ROW(s)'.
	 */
	private static final Pattern ORACLE_RDB_LIMIT_STMT = Pattern.compile("(LIMIT\\s+TO\\s+\\d+ ROW(S?))");

	/**
	 * Pattern for 'READ ONLY TABLE'.
	 */
	private static final Pattern ORACLE_RDB_READ_ONLY_TABLE = Pattern.compile("READ\\s+ONLY\\s+TABLE");
	
	/**
	 * Pattern to search for two or more brackets after 'FROM'.
	 */
	private static final Pattern TWO_OR_MORE_BRACKET_AFTER_FROM = Pattern.compile("(?<=FROM\\s)\\s*?\\(\\s*?\\(");
	
	/**
	 * Pattern to search for 'JOIN' keyword.
	 */
	private static final Pattern JOIN = Pattern.compile("(?i)\\bJOIN\\b");
	
	/**
	 * Pattern to search for Brackets.
	 */
	private static final Pattern ALL_BRACKETS = Pattern.compile("[()]");
	
	/**
	 * Pattern to search for More than two spaces.
	 */
	private static final Pattern MORE_THAN_ONE_SPACE = Pattern.compile("[ ]{2,}");
	
	private SimpleSqlCollector() {}

	/**
	 * Returns the error message for the given {@code exception} and {@code sql}.
	 *
	 * @param exception the {@link SqlParseException}
	 * @param sql the SQL statement for which the {@code exception} was thrown
	 * @return trimmed the error message
	 */
	public static String getErrorMessage(final SqlParseException exception, final String sql) {
		if (exception.getMessage() != null) {
			final int index = exception.getMessage().indexOf(NEW_LINE);
			/* Trim tailing CRs to avoid test failures on Jenkins. It's also good practice to not have a '\r' at a message end */
			return String.format(NON_VALID_SQL, sql, index > -1 ? exception.getMessage().substring(0, index) : exception.getMessage()).trim();
		} else {
			return String.format("INVALID SQL: \"%s\"", sql);
		}
	}
	
	/**
	 * Collect individual SQL queries from one SQL string.
	 * 
	 * @param sql SQL string.
	 * @return list List of individual SQL queries.
	 */
	public static List<String> getSingleSqlStatements(final String sql) {
		final String preprocessedSQL = preprocessSQL(sql);
		return CobolStatementUtility.getSingleBatchSqlStatementList(preprocessedSQL);
	}

	/**
	 * Collect all the tables with access type from an SQL query string.
	 * 
	 * @param sql SQL query string.
	 * @return list List of parsed tables from the query.
	 * @throws SqlParseException lib-calcite throws an error in case SQL query cannot be parsed.
	 */
	public static List<SqlParsedTable> getParsedSqlTables(final String sql) throws SqlParseException {
		final String processedSQL = preprocessSQL(sql);
		return SQL_VISITOR.visitAllTables(Db2SqlParser.parsePlainSql(processedSQL));
	}

	/**
	 * Collect all the tables with access type from an SQL query string mapped to Discovery TableAccess.
	 * 
	 * @param sqlIn SQL query string.
	 * @return map Map of parsed tables with access types.
	 * @throws SqlParseException lib-calcite throws an error in case SQL query cannot be parsed.
	 */
	public static Map<String, List<DatabaseAccessType>> getSqlTables(final String sqlIn) throws SqlParseException {
		final String preprocessedSQL = preprocessSQL(sqlIn);
		final Map<String, List<DatabaseAccessType>> sqlTableWithAccessList = new HashMap<>();
		/* We remove the 'LIMIT TO 1 ROW' with a regex since calcite is not supporting this oracle rbd syntax. */
		for (final SqlParsedTable tbl : getParsedSqlTables(preprocessedSQL)) {
			final DatabaseAccessType accessType = mapTableAccess(tbl.getAccessType());
			final List<DatabaseAccessType> accessTypes = sqlTableWithAccessList.computeIfAbsent(tbl.toString(), k -> new ArrayList<>());
			if ( ! accessTypes.contains(accessType)) {
				accessTypes.add(accessType);
			}
		}
		return sqlTableWithAccessList;
	}

	/**
	 * Collect table names from from an SQL query string.
	 * @param sql SQL query string.
	 * @return list List of table names.
	 * @throws SqlParseException lib-calcite throws an error in case SQL query cannot be parsed.
	 */
	public static List<String> getTableResources(final String sql) throws SqlParseException {
		final String preprocessedSQL = preprocessSQL(sql);
		return getParsedSqlTables(preprocessSQL(preprocessedSQL)).stream().map(SqlParsedTable::toString).distinct().collect(Collectors.toList());
	}
	
	/**
	 * Return SQL metrics information for a given SQL string.
	 *
	 * @param sqlString is an SQL string to parse and get SQL metrics information from
	 * @return SqlMetrics contain metrics information of the SQL such as: the string length, the number of tables accessed and complexity rating based on used
	 * SQL constructs such as UNION, JOIN, CURSOR, selection list or order by list etc.
	 * @throws SqlParseException is thrown in case we cannot parse the SQL string
	 */
	public static SqlMetrics getSqlMetrics(final String sqlString) throws SqlParseException {
		String modifiedSql = sqlString;
		final Matcher matchJOIN = JOIN.matcher(sqlString.toUpperCase());
		if (matchJOIN.find()) {
			modifiedSql = preprocessSQLwithJOIN(sqlString);
		}
		final SqlNode sqlNode = Db2SqlParser.parsePlainSql(modifiedSql);
		final SqlComplexity sqlComplexity = new CustomSqlComplexityVisitor().getSqlComplexityScore(sqlNode);
		return new SqlMetrics(SqlStatementType.valueOf(SQL_TYPE_VISITOR.getStatementType(sqlNode).name()), modifiedSql, sqlComplexity);
	}

	private static DatabaseAccessType mapTableAccess(final AccessType type) {
		DatabaseAccessType tableAccess;

 		switch (type) {
 			case SELECT:
				tableAccess = DatabaseAccessType.READ;
 				break;
 			case INSERT:
				tableAccess = DatabaseAccessType.STORE;
 				break;
 			case UPDATE:
				tableAccess = DatabaseAccessType.UPDATE;
 				break;
 			case DELETE:
				tableAccess = DatabaseAccessType.DELETE;
 				break;
 			default:
				tableAccess = DatabaseAccessType.OTHER;
 				break;
 		}
		return tableAccess;
	}

	/**
	 * Preprocess sql before passing it to calcite.
	 * Currently we only remove the "LIMIT TO x ROW(s)" clause and 'READ ONLY TABLE',
	 * which is "Oracle RDB for OpenVMS"-specific and can't be handled by calcite.
	 * We are also removing all brackets from SQL string if there is two or more bracket
	 * after 'FROM', along with JOIN in inner query, as it can't be handled by Calcite,
	 * and since calcite is not performing any execution, 
	 * it is just parsing the queries to perform discovery of tables, procedures etc.
	 * 
	 * @param sql the sql to preprocess
	 * @return the processed SQL string
	 */
	private static String preprocessSQL(final String sql) {
		final String upperCaseSQL = sql.toUpperCase();
		/* Manipulate the original sql here. */
		Matcher matcher = ORACLE_RDB_LIMIT_STMT.matcher(upperCaseSQL);
		String resultingSQL = matcher.replaceAll("");
		matcher = ORACLE_RDB_READ_ONLY_TABLE.matcher(resultingSQL);
		resultingSQL = matcher.replaceAll("");
		matcher = JOIN.matcher(resultingSQL);
		if (matcher.find()) {
			return preprocessSQLwithJOIN(resultingSQL);
		}
		/* Return the modified content. */
		return resultingSQL;
	}

	/**
	 * 
	 * Method to manipulate SQL string to remove all brackets from the SQL query,
	 * whenever there is two or more brackets after 'FROM'.
	 *
	 * @param sql String to be modified
	 * @return finalString after removing all brackets
	 */
	private static String preprocessSQLwithJOIN(final String sql) {
		String finalSql = sql;
		final Matcher matcherFROM = TWO_OR_MORE_BRACKET_AFTER_FROM.matcher(finalSql);
		if (matcherFROM.find()) {
			Matcher matcher = ALL_BRACKETS.matcher(sql);
			finalSql = matcher.replaceAll("");
			matcher = MORE_THAN_ONE_SPACE.matcher(finalSql);
			finalSql = matcher.replaceAll(" ").trim();
		}
		return finalSql;
	}

}

/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.parser.sql;

import com.google.common.collect.Streams;
import groovy.lang.Tuple2;
import innowake.lib.calcite.sql.parser.SqlParseException;

import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.shared.model.DatabaseAccessType;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.core.parsing.MarginPreprocessor;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Utility class to extract tables and statements from any kind of text content.
 */
public class SqlTableExtractor {

	private static final String CREATE = "CREATE";
	private static final String ALTER = "ALTER";
	private static final String DROP = "DROP";
	private static final String DECLARE = "DECLARE";
	private static final String INSERT = "INSERT";
	private static final String WITH = "WITH";
	private static final String SELECT = "SELECT";
	private static final String UPDATE = "UPDATE";
	private static final String DELETE = "DELETE";
	private static final Pattern SELECT_PATTERN = Pattern.compile("(?i)\\bSELECT\\b.*?\\bFROM\\b\\s+(\\w+)", Pattern.DOTALL);
	private static final Pattern INSERT_PATTERN = Pattern.compile("(?i)\\bINSERT\\b\\s*\\bINTO\\b\\s*(\\w+)", Pattern.DOTALL);
	private static final Pattern UPDATE_PATTERN = Pattern.compile("(?i)\\bUPDATE\\b\\s*(\\w+)", Pattern.DOTALL);
	private static final Pattern DELETE_PATTERN = Pattern.compile("(?i)\\bDELETE\\b\\s*\\bFROM\\b\\s*(\\w+)", Pattern.DOTALL);
	private static final Pattern CREATE_PATTERN = Pattern.compile("(?i)\\bCREATE\\b\\s*\\bTABLE\\b\\s*(\\w+)", Pattern.DOTALL);
	private static final Pattern JOIN_PATTERN = Pattern.compile("(?i)\\bJOIN\\b\\s+(\\w+)", Pattern.DOTALL);
	private static final MarginPreprocessor MARGIN_PREPROCESSOR = new MarginPreprocessor(0,72, -1);
	public static final Pattern COMMENT_PATTERN = Pattern.compile("(?m)^(\\*|--).*");

	private SqlTableExtractor() {
		/* private constructor */
	}

	/**
	 * Extracts the tables and the statements from the given content.
	 * @param moduleBuilder the module builder
	 * @param content the content to extract the tables and statements from
	 * @return a tuple containing the tables and the statements
	 */
	public static Tuple2<Map<String, List<DatabaseAccessType>>, Set<String>> extract(final DiscoveryBuilder.ModuleBuilder moduleBuilder,
			final String content) {
		final var sqlString = getSqlContent(content);
		/* Empty result is returned if the SQL is not valid */
		if ( ! SqlValidator.validate(sqlString)) {
			return new Tuple2<>(Collections.emptyMap(), Collections.emptySet());
		}
		final var statements = SimpleSqlCollector.getSingleSqlStatements(sqlString);
		final var processedStatements = new HashSet<String>();
		final Map<String, List<DatabaseAccessType>> tables = new HashMap<>();
		statements.forEach(statement -> {
			if ( ! SqlValidator.validate(statement)) {
				return;
			}
			final Map<String, List<DatabaseAccessType>> tablesFromStatement;
			try {
				tablesFromStatement = SimpleSqlCollector.getSqlTables(statement);
				/* This is to prevent adding an invalid statement to the statement list */
				processedStatements.add(statement);

				tablesFromStatement.forEach((key, value) ->
						tables.merge(key, value, ListUtils::sum)
				);
			} catch (final SqlParseException e) {
				/* If the statement is not valid, we try to extract the tables from the statement using regex */
				 final var foundTables = addTablesFromRegex(tables, statement);
				final String errorMessage = SimpleSqlCollector.getErrorMessage(e, statement);
				 final var message = String.format("INVALID SQL: '%s', the dependency table(s) %s  from the " +
								 "statement may not be accurate. %s ", statement, foundTables, errorMessage);
				 moduleBuilder.addError(Severity.WARNING, ErrorKey.PARSE_ERROR, message);
			}
		});
		return new Tuple2<>(tables, processedStatements);
	}

	private static List<String> addTablesFromRegex(final Map<String, List<DatabaseAccessType>> tables, final String sqlString) {

		final var selectTables = getTableNames(SELECT_PATTERN, sqlString);
		final var insertTables = getTableNames(INSERT_PATTERN, sqlString);
		final var updateTables = getTableNames(UPDATE_PATTERN,sqlString);
		final var deleteTables = getTableNames(DELETE_PATTERN, sqlString);
		final var createTables = getTableNames(CREATE_PATTERN, sqlString);
		final var joinTables = getTableNames(JOIN_PATTERN, sqlString);
		/* Add the tables to the result */
		selectTables.forEach(table -> addDatabaseAccessType(tables, table, DatabaseAccessType.READ));
		insertTables.forEach(table -> addDatabaseAccessType(tables, table, DatabaseAccessType.STORE));
		updateTables.forEach(table -> addDatabaseAccessType(tables, table, DatabaseAccessType.UPDATE));
		deleteTables.forEach(table -> addDatabaseAccessType(tables, table, DatabaseAccessType.DELETE));
		createTables.forEach(table -> addDatabaseAccessType(tables, table, DatabaseAccessType.OTHER));
		joinTables.forEach(table -> addDatabaseAccessType(tables, table, DatabaseAccessType.READ));

		return Streams.concat(selectTables.stream(), insertTables.stream(), updateTables.stream(), deleteTables.stream(), createTables.stream())
				.distinct()
				.collect(Collectors.toList());
	}

	private static void addDatabaseAccessType(final Map<String, List<DatabaseAccessType>> tables, final String table, final DatabaseAccessType accessType) {
		final var databaseAccessTypes = tables.getOrDefault(table, new ArrayList<>());
		if ( ! databaseAccessTypes.contains(accessType)) {
			databaseAccessTypes.add(accessType);
		}
		tables.put(table, databaseAccessTypes);
	}

	private static List<String> getTableNames(final Pattern pattern, final String sqlString) {
		final var matcher = pattern.matcher(sqlString);
		final List<String> tables = new ArrayList<>();
		while (matcher.find()) {
			tables.add(matcher.group(1));
		}
		return tables;
	}

	private static String getSqlContent(final String content) {
		/* preprocess the content to remove any trailing numbers */
		final var normalizedContent = MARGIN_PREPROCESSOR.preprocess(content);
		/* Remove the comments */
		final var matcher =  COMMENT_PATTERN.matcher(normalizedContent);
		final var contentWithoutComments = matcher.find() ? matcher.replaceAll("") : normalizedContent;

		/* Try to extract the SQL content from the given content */
		final int[] indexes = {
				StringUtils.indexOf(contentWithoutComments, SELECT),
				StringUtils.indexOf(contentWithoutComments, DELETE),
				StringUtils.indexOf(contentWithoutComments, INSERT),
				StringUtils.indexOf(contentWithoutComments, UPDATE),
				StringUtils.indexOf(contentWithoutComments, CREATE),
				StringUtils.indexOf(contentWithoutComments, ALTER),
				StringUtils.indexOf(contentWithoutComments, DROP),
				StringUtils.indexOf(contentWithoutComments, DECLARE),
				StringUtils.indexOf(contentWithoutComments, WITH),

		};
		final int startIndex = Arrays.stream(indexes).sorted().filter(index -> index != -1).findFirst().orElse(-1);
		return startIndex != -1 ? StringUtils.substring(contentWithoutComments, startIndex, contentWithoutComments.length()) : contentWithoutComments;
	}
}

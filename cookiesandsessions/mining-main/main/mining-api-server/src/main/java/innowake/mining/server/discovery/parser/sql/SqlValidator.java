/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.sql;

import java.util.regex.Pattern;

import com.google.common.base.Enums;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.model.discovery.attribute.ModelAttributeValue.SqlStatementType;

/**
 * Simple and quick validator for SQL.
 */
public class SqlValidator {
	private static final String TRANSACTION = "TRANSACTION";
	private static final String ALIAS = "ALIAS";
	private static final String IMMEDIATE = "IMMEDIATE";
	private static final String GRANT = "GRANT";
	private static final String CREATE = "CREATE";
	private static final String ALTER = "ALTER";
	private static final String FUNCTION = "FUNCTION";
	private static final String PROCEDURE = "PROCEDURE";
	private static final String OR  = "OR";
	private static final String INDEX = "INDEX";
	private static final String TABLE = "TABLE";
	private static final String DROP = "DROP";
	private static final String DECLARE = "DECLARE";
	private static final String INSERT = "INSERT";
	private static final String WITH = "WITH";
	private static final String INTO = "INTO";
	private static final String SELECT = "SELECT";
	private static final String FROM = "FROM";
	private static final String UPDATE = "UPDATE";
	private static final String MERGE = "MERGE";
	private static final String SET = "SET";
	private static final String DELETE = "DELETE";
	private static final String LOCK = "LOCK";
	private static final String VIEW = "VIEW";
	private static final String CONSTRAINT = "CONSTRAINT";
	private static final String CHECK = "CHECK";
	private static final String PRIMARY = "PRIMARY";
	private static final String FOREIGN = "FOREIGN";
	private static final String KEY = "KEY";
	private static final String PREPARE = "PREPARE";
	private static final String CALL = "CALL";
	private static final String COMMIT = "COMMIT";
	private static final String ROLLBACK = "ROLLBACK";
	private static final String FETCH = "FETCH";
	private static final String OPEN = "OPEN";
	private static final String CLOSE = "CLOSE";
	private static final String ALLOCATE = "ALLOCATE";
	private static final String CURSOR = "CURSOR";
	private static final String ASSOCIATE = "ASSOCIATE";
	private static final String LOCATOR = "LOCATOR";
	private static final String WHENEVER = "WHENEVER";
	private static final String EXECUTE = "EXECUTE";
	private static final String VALUES = "VALUES";
	private static final String GETERROR = "GETERROR";
	private static final String SYNONYM = "SYNONYM";
	private static final String REGEX_NEWLINE = "\\r\\n|\\r|\\n";
	private static final String REGEX_SPACES = "\\s{2,}";
	private static final String SPACE = " ";
	private static final String COBOLVAR = ":";
	private static final String COBOLVAR_FIX = " :";
	private static final Pattern NEWLINE_PATTERN = Pattern.compile(REGEX_NEWLINE);
	private static final Pattern COBOLVAR_PATTERN = Pattern.compile(COBOLVAR);
	private static final Pattern SPACES_PATTERN = Pattern.compile(REGEX_SPACES);
	
	private SqlValidator() {}
	
	/**
	 * Check if SQL string is a valid SQL statement and Calcite Sql Parser can parse it. 
	 * @param content is a SQL string
	 * @return true if we recognize the statement is a valid SQL statement and Calcite Sql Parser can parse it
	 */
	public static boolean validate(final String content) {
		final SqlStatementType type = getSqlType(content);
		switch (type) {
			case SELECT:
			case UPDATE:
			case INSERT:
			case DELETE:
			case MERGE:
				return true;
			default:
				return false;
		}
	}
	
	/**
	 * Try to recognize the type of SQL.
	 *
	 * @param content is SQL string
	 * @return return SqlStatementType
	 */
	public static SqlStatementType getSqlType(final String content) {
		String processedStatement = NEWLINE_PATTERN.matcher(content).replaceAll(SPACE);
		processedStatement = COBOLVAR_PATTERN.matcher(processedStatement).replaceAll(COBOLVAR_FIX);
		processedStatement = SPACES_PATTERN.matcher(processedStatement).replaceAll(SPACE);
		processedStatement = processedStatement.toUpperCase();

		final String[] segments = processedStatement.split(SPACE);
		int i = 0;
		while (i < segments.length) {
			final String segment = segments[i++];
			@Nullable
			final String next = i < segments.length ? segments[i] : null;
			switch (segment) {
				case CREATE:
				case ALTER:
				case DROP:
					if (next == null) {
						return SqlStatementType.UNKNOWN;
					}
					switch(next) {
						case TABLE:
						case VIEW:
						case INDEX:
						case CONSTRAINT:
						case FUNCTION:
						case PROCEDURE:
						case SYNONYM:
							return Enums.getIfPresent(SqlStatementType.class, segment + "_" + next).or(SqlStatementType.UNKNOWN);
						case PRIMARY:
						case FOREIGN:
							return Enums.getIfPresent(SqlStatementType.class, segment + "_" + next + "_" + KEY).or(SqlStatementType.UNKNOWN);
						case CHECK:
							return Enums.getIfPresent(SqlStatementType.class, segment + "_" + next + "_" + CONSTRAINT).or(SqlStatementType.UNKNOWN);
						case OR:
							while (i < segments.length) {
								final String nextSegment = segments[i++];
								if (PROCEDURE.equals(nextSegment)) {
									return SqlStatementType.CREATE_PROCEDURE;
								}
							}
							break;
						default:
							break;
					}
					break;
				case GRANT:
					return SqlStatementType.GRANT;
				case DECLARE: 
					while (i < segments.length) {
						final String nextSegment = segments[i++];
						if (TABLE.equals(nextSegment)) {
							return SqlStatementType.DECLARE_TABLE;
						}
						if (CURSOR.equals(nextSegment)) {
							return SqlStatementType.SELECT;
						}
					}
					if (ALIAS.equals(next)) {
						return SqlStatementType.DECLARE_ALIAS;
					}
					if (TRANSACTION.equals(next)) {
						return SqlStatementType.DECLARE_TRANSACTION;
					}
					break;
				case INSERT:
					if (INTO.equals(next)) {
						return SqlStatementType.INSERT;
					}
					break;
				case WITH:
					while (i < segments.length) {
						if (SELECT.equals(segments[i++])) {
							return SqlStatementType.SELECT;
						}
					}
					break;
				case SELECT:
					while (i < segments.length) {
						if (FROM.equals(segments[i++])) {
							return SqlStatementType.SELECT;
						}
					}
					break;
				case UPDATE:
					while (i < segments.length) {
						if (SET.equals(segments[i++])) {
							return SqlStatementType.UPDATE;
						}
					}
					break;
				case MERGE:
					while (i < segments.length) {
						if (INTO.equals(segments[i++])) {
							return SqlStatementType.MERGE;
						}
					}
					break;
				case DELETE:
					/* SQL statements not followed by FROM won't be recognized in order to avoid other non-SQL related statements */
					if (FROM.equals(next)) { 
						return SqlStatementType.DELETE;
					}
					break;
				case LOCK:
					if (TABLE.equals(next)) {
						return SqlStatementType.LOCK_TABLE;
					}
					break;
				case EXECUTE:
					if (IMMEDIATE.equals(next)) {
						return SqlStatementType.EXECUTE_IMMEDIATE;
					} else {
						return SqlStatementType.EXECUTE;
					}
				case CALL:
				case PREPARE:
				case COMMIT:
				case ROLLBACK:
				case OPEN:
				case CLOSE:
				case SET:
					return Enums.getIfPresent(SqlStatementType.class, segment).or(SqlStatementType.UNKNOWN);
				case FETCH:
					while (i < segments.length) {
						if (INTO.equals(segments[i++]))
							return SqlStatementType.FETCH;
					}
					break;
				case ALLOCATE:
					while (i < segments.length) {
						if (CURSOR.equals(segments[i++]))
							return SqlStatementType.ALLOCATE_CURSOR;
					}
					break;
				case ASSOCIATE:
					while (i < segments.length) {
						if (LOCATOR.equals(segments[i++]))
							return SqlStatementType.ASSOCIATE_LOCATOR;
					}
					break;
				case WHENEVER:
					return SqlStatementType.WHENEVER;
				case VALUES:
					return SqlStatementType.VALUES;	
				case GETERROR:
					return SqlStatementType.GETERROR;	
				default:
					break;
			}
		}

		return SqlStatementType.UNKNOWN;
	}

}

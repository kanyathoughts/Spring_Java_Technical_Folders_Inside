/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.sql;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.discovery.Logging;

/**
 * Helper for simple SQL parsing.
 */
public class SqlUtil {

	/* Regular expression to get the name of a table or stored procedure that might be qualified by a schema name: <SCHEMA><.><NAME>
	 * Valid characters for the schema name: 'A'-'Z', 'a'-'z', '0'-'9', '#', '@', '$', '_', '!', ' ', '(', ')', '{', '}', '-', '.', and '^'
	 * Valid characters for the name (regular identifiers): 'A'-'Z', 'a'-'z', '0'-'9', '#', '@', '$', '_' */
	private static final String ENTITY_NAME_SCHEMA_REGEX = "((\\w|#|@|\\$|!|\\s|\\(|\\)|\\{|\\}|-|\\.|\\^)+\\.)?((\\w|#|@|\\$)+)+";

	/* The group in the matcher that contains the name of the stored procedure or table when the NAME_SCHEMA_REGEX is used */
	private static final int ENTITY_NAME_MATCH_GROUP = 3;

	/* Pattern to get the name of an entity like stored procedure or table. Matches strings like "(<SCHEMA><.>)?<NAME><(>".
	 * The name is in group 3 -> use NAME_MATCH_GROUP */
	private static final Pattern ENTITY_NAME_SCHEMA_PATTERN = Pattern.compile(String.format("%s$", ENTITY_NAME_SCHEMA_REGEX), Pattern.DOTALL);

	/* Pattern to get the name of the stored procedure out of a SQL call statement. Matches strings like "CALL (<SCHEMA><.>)?<SPNAME><(>"
	 * The name is in group 3 -> use NAME_MATCH_GROUP */
	private static final Pattern SP_CALL_PATTERN = Pattern.compile(String.format("^CALL\\s+%s\\s*\\(.*", ENTITY_NAME_SCHEMA_REGEX), Pattern.DOTALL);
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.SQL_PARSER);

	private SqlUtil() {
		throw new IllegalStateException("no instances allowed");
	}

	/**
	 * Returns name of the stored procedure or table removing schema
	 *
	 * @param name complete stored procedure or table with schema
	 * @return stored procedure or table name
	 */
	public static String getEntityName(final String name) {
		final String processedName = name.replace("\"", "");
		final Matcher matcher = ENTITY_NAME_SCHEMA_PATTERN.matcher(processedName);
		if (matcher.find()) {
			return matcher.group(ENTITY_NAME_MATCH_GROUP);
		} else {
			LOG.debug("Schema name is not present for: " + processedName);
		}
		return processedName;
	}

	/**
	 * Returns the name of the stored procedure from the given SQL {@code call} statement or {@code null} if the name could not be found.
	 *
	 * @param call The SQL call statement; not {@code null}
	 * @return the stored procedure name; can be {@code null}
	 */
	@Nullable
	public static String getStoredProcedureNameFromCall(final String call) {
		final Matcher matcher = SP_CALL_PATTERN.matcher(call);
		if (matcher.find()) {
			return matcher.group(ENTITY_NAME_MATCH_GROUP);
		}

		return null;
	}
}

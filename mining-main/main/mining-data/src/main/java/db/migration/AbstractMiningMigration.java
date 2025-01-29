/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package db.migration;

import java.util.Arrays;

import org.apache.commons.lang3.time.StopWatch;
import org.flywaydb.core.api.migration.BaseJavaMigration;
import org.flywaydb.core.api.migration.Context;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.ResultSetExtractor;
import org.springframework.jdbc.datasource.SingleConnectionDataSource;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.Logging;

/**
 * Abstract base class for mining java migrations. Offers convenience methods for batch wise inserts and updates as well as for the creation and deletion of
 * Orient functions.
 * <b>Note: </b>Be careful if you change an existing method. We must keep backward compatibility for all schema versions!
 */
abstract class AbstractMiningMigration extends BaseJavaMigration {

	static final Logger LOG = LoggerFactory.getLogger(Logging.MIGRATION);

	/**
	 * Performs the actual migration.
	 *
	 * @param jdbcTemplate The {@link JdbcTemplate}
	 */
	abstract void migrate(final JdbcTemplate jdbcTemplate);

	@Override
	public void migrate(@Nullable final Context context) throws Exception {
		migrate(new JdbcTemplate(new SingleConnectionDataSource(Assert.assertNotNull(context).getConnection(), true)));
	}

	/**
	 * Executes the given update {@code query} batch wise in chunks of {@code batchSize}. The execution time is logged.
	 *
	 * @param jdbcTemplate The {@link JdbcTemplate}
	 * @param query The query to execute
	 */
	void updateBatchwise(final JdbcTemplate jdbcTemplate, final String query, final int batchSize) {
		var count = 0;
		var totalCount = 0;

		final String batchQuery = query + " LIMIT " + batchSize;
		final var stopWatch = createStopWatch();
		do {
			count = jdbcTemplate.update(batchQuery);
			totalCount += count;
			LOG.info("Executed " + query + " and updated " + totalCount + " number of records");
		} while (count == batchSize);

		stopWatch.stop();
		final long cnt = totalCount;
		LOG.info(() -> String.format("Query took %s (H:mm:ss.SSS) to update %d records. %s", stopWatch.toString(), Long.valueOf(cnt), query));
	}

	/**
	 * Executes the given insert {@code query} batch wise in chunks of {@code batchSize}. The execution time is logged.
	 * <p>Call this method when your query fetches records from one class and inserts them into another one ({@code INSERT INTO SELECT}).</p>
	 * This method calls {@link String#format(String, Object...)} to set the {@code SKIP} and {@code LIMIT} options into your {@code query}.
	 * Therefore you have to put a string placeholder ({@code %s}) at the end of the inner {@code SELECT}. Example:
	 * <pre>
	 * insertBatchwise(jdbcTemplate, "INSERT INTO ClassB (SELECT propX, propY FROM ClassA %s)", 100_000);
	 * 
	 * ==>
	 * 
	 * INSERT INTO ClassB (SELECT propX, propY FROM ClassA LIMIT 100000 SKIP 0);
	 * INSERT INTO ClassB (SELECT propX, propY FROM ClassA LIMIT 100000 SKIP 100000);
	 * INSERT INTO ClassB (SELECT propX, propY FROM ClassA LIMIT 100000 SKIP 200000);
	 * ...
	 * 
	 * -----------------------------------------------------------------------------
	 * 
	 * insertBatchwise(jdbcTemplate, "INSERT INTO ClassB SELECT propX, 'y' as propY FROM ClassA %s", 100_000);
	 * 
	 * ==>
	 * 
	 * INSERT INTO ClassB SELECT propX, 'y' as propY FROM ClassA %s LIMIT 100000 SKIP 0);
	 * INSERT INTO ClassB SELECT propX, 'y' as propY FROM ClassA %s LIMIT 100000 SKIP 100000);
	 * INSERT INTO ClassB SELECT propX, 'y' as propY FROM ClassA %s LIMIT 100000 SKIP 200000);
	 * </pre>
	 *
	 * @param jdbcTemplate The {@link JdbcTemplate}
	 * @param query The query to execute
	 */
	void insertBatchwise(final JdbcTemplate jdbcTemplate, final String query, final int batchSize) {
		var count = 0;
		var skipCount = 0;
		var totalCount = 0;

		final var stopWatch = createStopWatch();
		do {
			count = jdbcTemplate.update(String.format(query, " LIMIT " + batchSize + " SKIP " + skipCount));
			skipCount += batchSize;
			totalCount += count;
		} while (count == batchSize);

		stopWatch.stop();
		final long cnt = totalCount;
		LOG.info(() -> String.format("Query took %s (H:mm:ss.SSS) to update %d records. %s", stopWatch.toString(), Long.valueOf(cnt), query));
	}

	/**
	 * Convenience method that returns a new, already started {@link StopWatch}.
	 *
	 * @return started {@link StopWatch}
	 */
	StopWatch createStopWatch() {
		final var stopWatch = new StopWatch();
		stopWatch.start();
		return stopWatch;
	}

	/**
	 * Creates a new OrientDB SQL function with the given {@code functionName} and {@code functionCode}.
	 * <p>If a function with the given {@code functionName} already exists then it is deleted first.</p>
	 * <p><b>Note:</b> The function name should be unique, e.g. by suffixing it with the migration version to avoid that an already existing function is 
	 * deleted. The function should be deleted when it is not required anymore:</p> 
	 * <pre>
	 * try {
	 * 	createSqlOrientFunction(jdbcTemplate, "myFunctionV175", functionCode);
	 * 	final ResultSetExtractor<Integer> rsExtractor = createFunctionCountResultSetExtractor(functionName);
	 * 	jdbcTemplate.query("SELECT " + "myFunctionV175()", rsExtractor);
	 * } finally {
	 * 	deleteFunction("myFunctionV175");
	 * }
	 * </pre>
	 * 
	 * <p><b>myFunctionV175()</b> example function code</p>
	 * <pre>
	 * let rs = SELECT expand(moduleLink) FROM (SELECT DISTINCT moduleLink FROM ExcelSheetErrors);
	 * let i = 0;
	 * FOREACH ($record IN $rs) {
	 *   UPDATE $record.@rid SET errorMarkerLinks=(SELECT FROM ExcelSheetErrors WHERE moduleLink=$record.@rid);
	 *   let $i = $i + 1;
	 * }
	 * return $i;
	 * </pre>
	 *
	 * @param jdbcTemplate The {@link JdbcTemplate}
	 * @param functionName The name of the function
	 * @param functionCode The code of the function
	 * @see #deleteOrientFunction(JdbcTemplate, String)
	 * @see #createJsFunctionCountResultSetExtractor(String)
	 */
	void createSqlOrientFunction(final JdbcTemplate jdbcTemplate, final String functionName, final String functionCode) {
		/* To allow re-runs we delete the function if exists. There is no IF NOT EXISTS option for CREATE FUNCTION */
		deleteOrientFunction(jdbcTemplate, functionName);

		/* Haven't found out how to create sql functions with parameters. It never worked for me in OrientDB studio */
		execute(jdbcTemplate, "CREATE FUNCTION " + functionName + " \"" + functionCode + "\" LANGUAGE sql");
	}

	/**
	 * Creates a new OrientDB JavaScript function with the given {@code functionName} and {@code functionCode}.
	 * <p>If a function with the given {@code functionName} already exists then it is deleted first.</p>
	 * <p><b>Note:</b> The function name should be unique, e.g. by suffixing it with the migration version to avoid that an already existing function is 
	 * deleted. The function should be deleted when it is not required anymore:</p> 
	 * <pre>
	 * try {
	 * 	createJsOrientFunction(jdbcTemplate, "myFunctionV175", functionCode, "project");
	 * 	final ResultSetExtractor<Integer> rsExtractor = createFunctionCountResultSetExtractor(functionName);
	 * 	jdbcTemplate.query("SELECT " + "myFunctionV175(" + projectRid + ")", rsExtractor);
	 * } finally {
	 * 	deleteFunction("myFunctionV175");
	 * }
	 * </pre>
	 * 
	 * <p><b>myFunctionV175(project)</b> example function code</p>
	 * <pre>
	 * var db = orient.getDatabase();
	 * var rs = db.query('SELECT expand(moduleLink) FROM (SELECT DISTINCT moduleLink FROM ExcelSheetErrors where projectLink=?)', project);
	 * for (var i = 0; i < rs.length; i++) {
	 *   db.command('UPDATE ? SET errorMarkerLinks=(SELECT FROM ExcelSheetErrors WHERE moduleLink=?)', rs[i], rs[i]);
	 * }
	 * return rs.length;
	 * </pre>
	 *
	 * @param jdbcTemplate The {@link JdbcTemplate}
	 * @param functionName The name of the function
	 * @param functionCode The code of the function
	 * @param parameters The names of the function parameters
	 * @see #deleteOrientFunction(JdbcTemplate, String)
	 * @see #createJsFunctionCountResultSetExtractor(String)
	 */
	void createJsOrientFunction(final JdbcTemplate jdbcTemplate, final String functionName, final String functionCode, final String... parameters) {
		/* To allow re-runs we delete the function if exists. There is no IF NOT EXISTS option for CREATE FUNCTION */
		deleteOrientFunction(jdbcTemplate, functionName);

		final StringBuilder strb = new StringBuilder(22 + functionName.length() + functionCode.length() + parameters.length * 8)
			.append("CREATE FUNCTION ")
			.append(functionName)
			.append(" \"")
			.append(functionCode)
			.append("\"");

		if (parameters.length != 0) {
			strb.append(" PARAMETERS")
				.append(Arrays.toString(parameters));
		}

		execute(jdbcTemplate, strb.append(" LANGUAGE javascript").toString());
	}

	/**
	 * Deletes the OrientDB function with the given {@code functionName} if it exists. If the function doesn't exists then nothing is done.
	 *
	 * @param jdbcTemplate The {@link JdbcTemplate}
	 * @param functionName The name of the function
	 */
	void deleteOrientFunction(final JdbcTemplate jdbcTemplate, final String functionName) {
		execute(jdbcTemplate, "DELETE FROM OFunction WHERE name = '" + functionName + "'");
	}

	/**
	 * Creates a new {@link ResultSetExtractor} for the Orient JavaScript with name {@code functionName}, which returns a number that is
	 * returned by the called function. The {@link ResultSetExtractor} is stateless and can be reused, e.g. in a loop of function calls.
	 *
	 * @param functionName The name of the function that gets called
	 * @return new instance of {@link ResultSetExtractor}
	 */
	ResultSetExtractor<Integer> createJsFunctionCountResultSetExtractor(final String functionName) {
		return rs -> {
			final String columnName;
			/* when functions are called with parameters then the column name must contain the parameter values too.
			 * updateModuleErrorMarkerLinksJsV_1_2_175 -> updateModuleErrorMarkerLinksJsV_1_2_175(#107:0) */
			if (rs.getMetaData().getColumnCount() == 1 && rs.getMetaData().getColumnName(1).startsWith(functionName)) {
				columnName = rs.getMetaData().getColumnName(1);
			} else if (functionName.charAt(functionName.length() - 1) != ')') {
				/* add brackets, if missing */
				columnName = functionName + "()";
			} else {
				columnName = functionName;
			}

			return Integer.valueOf(rs.getInt(columnName));
		};
	}

	/**
	 * Executes the given {@code query}. The execution time is logged.
	 *
	 * @param jdbcTemplate The {@link JdbcTemplate}
	 * @param query The query to execute
	 */
	void execute(final JdbcTemplate jdbcTemplate, final String query) {
		final var stopWatch = createStopWatch();
		jdbcTemplate.execute(query);
		stopWatch.stop();
		LOG.info(() -> String.format("Query took %s (H:mm:ss.SSS) to execute: %s", stopWatch.toString(), query));
	}
}

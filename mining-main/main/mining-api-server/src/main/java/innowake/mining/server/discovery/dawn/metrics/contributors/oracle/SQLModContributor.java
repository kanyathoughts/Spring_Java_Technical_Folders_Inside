/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.oracle;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.springframework.stereotype.Component;

import innowake.lib.calcite.sql.parser.SqlParseException;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.contributors.DawnMetricsUtility;
import innowake.mining.server.discovery.parser.sql.SimpleSqlCollector;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;

/**
 * Contributor for SQLMod files.
 */
@Component
public class SQLModContributor implements DiscoveryContributorFromSource {

	private static final Logger LOG = LoggerFactory.getLogger(SQLModContributor.class);
	
	private static final String COMMENT = "--";
	private static final Pattern LINE_DELIMITER = Pattern.compile("\n");
	private static final Pattern SPACE_PTR = Pattern.compile("\\s+");
	private static final Pattern SQL_PATTERN = Pattern.compile("^(SELECT|INSERT|UPDATE|DELETE)", Pattern.CASE_INSENSITIVE);
	private static final Pattern DECLARE_PROC_PATTERN = Pattern.compile("^\\s*(DECLARE|PROCEDURE)\\s*\\b", Pattern.CASE_INSENSITIVE);
	
	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return sourceObject.getTechnology() == Technology.ORACLE && sourceObject.getType() == Type.SQLMOD;
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		final ModuleBuilder rootModule = builder.declareRootModule(sourceObject.getName(), ModuleType.SQLMOD);
		try {
			DawnMetricsUtility.collectLinesOfCode(sourceObject, rootModule, COMMENT);
			collectModules(builder, rootModule, sourceObject);
		} catch (final Exception e) {
			LOG.error("Exception occured while parsing" + sourceObject.getPath(), e);
			rootModule.addError(Severity.ERROR, ErrorKey.MODULE_ABORT, e.getMessage());
		} catch (final Throwable e) {
			LOG.error("Unxpected error occured while parsing" + sourceObject.getPath(), e);
			rootModule.addError(Severity.ERROR, ErrorKey.MODULE_ABORT, e.getMessage());
		}
	}
	
	private void collectModules(final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule, final SourcePojo sourceObject) {
		/* Remove comments and empty lines. */
		final List<String> preprocessedContent = preprocessContent(LINE_DELIMITER.split(sourceObject.getContent().toString()));

		/* SQL statements are found in declare blocks. */
		final Map<String, String> relevantDeclareStatements = getRelevantDeclareStatements(preprocessedContent);

		/* Procedures reference the declare blocks. */
		final Map<String, Integer> relevantProcedures = getRelevantProcedures(preprocessedContent);

		/* Find SQL statements in procedures */
		final Map<String, String> sqlInProcedures = getSQLFromProcedures(preprocessedContent, relevantProcedures);

		final Map<String, String> procedureSQLMapping = linkProcWithSQL(preprocessedContent, relevantDeclareStatements, relevantProcedures);

		/* Merge maps. */
		procedureSQLMapping.putAll(sqlInProcedures);

		collectSqlModProcModules(builder, rootModule, sourceObject, relevantProcedures, procedureSQLMapping);
	}

	private void collectSqlModProcModules(final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule, final SourcePojo sourceObject,
			final Map<String, Integer> relevantProcedures, final Map<String, String> procedureSQLMapping) {
		/* Create virtual procedure modules. */
		relevantProcedures.forEach((procName, lineNumber) -> {
			final ModuleBuilder sqlProcModModule = builder.declareExternalModule(procName, ModuleType.SQLMOD_PROC);
			rootModule.declareDependency(RelationshipType.REFERENCES, sqlProcModModule).setBinding(Binding.EARLY);

			final String sql = procedureSQLMapping.get(procName);
			/* Not every SQL procedure must declares that contain sql code.*/
			if (sql == null || sql.isEmpty()) {
				return;
			}

			collectSqlTableModules(rootModule, sourceObject, sqlProcModModule, sql);
		});
	}

	private void collectSqlTableModules(final ModuleBuilder rootModule, final SourcePojo sourceObject, final ModuleBuilder sqlProcModModule,
			final String sql) {
		try {
			SimpleSqlCollector.getSqlTables(sql).forEach((tableName, dbAccessTypes) -> {
				final var moduleFilter = new ModuleFilter().setNames(tableName).setTypes(ModuleType.SQL_VIEW, ModuleType.SQL_TABLE);
				sqlProcModModule.declareDependency(RelationshipType.ACCESSES, moduleFilter)
				.addAttribute(ModelAttributeKey.DB_ACCESS_TYPE, dbAccessTypes)
				.setBinding(Binding.EARLY)
				.createIfMissing(tableName, ModuleType.SQL_TABLE);
			});
		} catch (final SqlParseException e) {
			final var errorMessage = String.format("Could not successfully parse sql statement for artifact %s SQL is: %s", sourceObject.getName(), sql);
			rootModule.addError(Severity.WARNING, ErrorKey.PARSE_ERROR, errorMessage);
			if (LOG.isWarnEnabled()) {
				LOG.warn(errorMessage);
			}
		}
	}

	/**
	 * Remove comments and beginning of document so that it can easily be split up.
	 * 
	 * @param splitContent the content to preprocess
	 * @return the processed content
	 */
	private List<String> preprocessContent(final String[] splitContent) {
		return Arrays.stream(splitContent)
				.filter(line -> ! (line.startsWith(COMMENT) || line.isEmpty()))
				.collect(Collectors.toList());
	}
	
	private Map<String, String> getRelevantDeclareStatements(final List<String> splitContent) {
		final Map<String, String> declareNameWithSQLStatement = new HashMap<>();
		for (int i = 0; i < splitContent.size(); i++) {
			final String line = splitContent.get(i);
			if (line.contains("DECLARE") && ! line.contains("DECLARE ALIAS")) {
				final String cursorName = SPACE_PTR.split(line)[1];
				final int startTokenLine = findStartTokenForSQL(splitContent, i + 1);
				final int endTokenLine = findNextDeclOrProcBlock(splitContent, i + 1);
				if (endTokenLine <= startTokenLine) {
					if (LOG.isDebugEnabled()) {
						LOG.debug("NO SQL in CURSOR " + cursorName + " found.");
					}
					break;
				}
				if (startTokenLine != -1 && endTokenLine != -1) {
					/* Remove ';' since the parser does not like this char. */
					declareNameWithSQLStatement.put(cursorName, joinSQLStatement(splitContent, startTokenLine, endTokenLine).replace(";", ""));
				}
			}
		}
		return declareNameWithSQLStatement;
	}
	
	private Map<String, Integer> getRelevantProcedures(final List<String> splitContent) {
		final Map<String, Integer> procNameLineMapping = new HashMap<>();
		for (int i = 0; i < splitContent.size(); i++) {
			final String line = splitContent.get(i);
			if (line.trim().startsWith("PROCEDURE")) {
				final String cursorName = SPACE_PTR.split(line)[1].replace("\r", "");
				procNameLineMapping.put(cursorName, Integer.valueOf(i));
			}
		}	
		return procNameLineMapping;
	}
	
	private int findStartTokenForSQL(final List<String> processedContent, final int start) {
		for (int i = start; i < processedContent.size(); i++) {
			final String currentLine = processedContent.get(i).toUpperCase().trim();
			final var matcher = SQL_PATTERN.matcher(currentLine);
			if (matcher.find()) {
				return i;
			}
		}
		return -1;
	}
	
	/**
	 * Finds the next DECLARE or PROCEDURE statement
	 * 
	 * @param processedContent 
	 * @param start the start line of the code block
	 * @return the start line of the next DECLARE or PROCEDURE statement
	 */
	private int findNextDeclOrProcBlock(final List<String> processedContent, final int start) {
		for (int i = start; i < processedContent.size(); i++) {
			final String currentLine = processedContent.get(i).toUpperCase();
			final var matcher = DECLARE_PROC_PATTERN.matcher(currentLine);
			if (matcher.find()) {
				return i;
			}
		}
		return processedContent.size();
	}
	
	private String joinSQLStatement(final List<String> splitContent, final int startIndex, final int endIndex) {
		return String.join("\n", splitContent.subList(startIndex, endIndex));
	}
	
	/**
	 * Fetch the SQL statements that are associated with in the procedures.
	 *
	 * @param splitContent the preprocessed content
	 * @param relevantProcedures the procedures in source file
	 * @return a map with "procedureName -> sql content"
	 */
	private Map<String, String> getSQLFromProcedures(final List<String> splitContent, final Map<String, Integer> relevantProcedures) {
		final Map<String, String > procNameToSQLCode = new HashMap<>();
		relevantProcedures.forEach((procName, lineNumber) -> {
			final int startTokenLine = findStartTokenForSQL(splitContent, lineNumber.intValue() + 1);
			final int endTokenLine = findNextDeclOrProcBlock(splitContent, lineNumber.intValue() + 1);
			if (endTokenLine <= startTokenLine) {
				/* No sql was found before next DECLARE or PROCEDURE. */
				return;
			}
			if (startTokenLine != -1 && endTokenLine != -1) {
				/* Remove ';' since the parser does not like this char. */
				procNameToSQLCode.put(procName, joinSQLStatement(splitContent, startTokenLine, endTokenLine).replace(";", ""));
			}
		});
		return procNameToSQLCode;
	}
	
	/**
	 * A procedure is linked to a sql statement via a DECLARE block.
	 *
	 * @param processedContent the content containing no new lines and no blank lines
	 * @param relevantDeclareStatements a map containing the declare blocks and sql-statement as value
	 * @param relevantProcedures procedure names mapped to the line number in this processedContent
	 * @return a map containing "procedureName -> sqlstatement".
	 */
	private Map<String, String> linkProcWithSQL(final List<String> processedContent, final Map<String, String> relevantDeclareStatements,
			final Map<String, Integer> relevantProcedures) {
		final Map<String, String> procSqlMapping = new HashMap<>();
		relevantProcedures.forEach((procName, lineNumber) -> 
		relevantDeclareStatements.forEach((declareName, sql) -> {
			final int endTokenLine = findNextDeclOrProcBlock(processedContent, lineNumber.intValue() + 1);
			for (int i = lineNumber.intValue(); i < endTokenLine; i++) {
				final var currentLine = processedContent.get(i);
				if (currentLine.contains(declareName)) {
					procSqlMapping.put(procName, sql);
					break;
				}
			}
		}));
		return procSqlMapping;
	}
}

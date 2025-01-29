/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.plsql;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;

import innowake.mining.server.discovery.dawn.metrics.contributors.AstNodeLocationProvider;
import innowake.mining.server.discovery.dawn.metrics.contributors.DawnMetricsUtility;
import innowake.mining.shared.model.AstNodeLocation;
import innowake.ndt.core.parsing.spi.Document;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import innowake.lib.calcite.sql.parser.SqlParseException;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerCancellationException;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.contributors.DawnExecStatementUtility;
import innowake.mining.server.discovery.dawn.metrics.contributors.GenericMetricsUtil;
import innowake.mining.server.discovery.dawn.metrics.contributors.InputProvider;
import innowake.mining.server.discovery.metrics.generic.GenericMetricsContributor;
import innowake.mining.server.discovery.metrics.generic.MetricException;
import innowake.mining.server.discovery.metrics.generic.MetricFactory;
import innowake.mining.server.discovery.metrics.generic.MetricType;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.server.discovery.parser.sql.SimpleSqlCollector;
import innowake.mining.server.discovery.parser.sql.SqlUtil;
import innowake.mining.server.discovery.parser.sqllightweight.SqlLightWeightParseResultProvider;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.DatabaseAccessType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.StatementType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.ast.visitor.TopDownVisitor;
import innowake.ndt.parsing.parser.sql.lightweight.ast.CreateProcedureStatement;
import innowake.ndt.parsing.parser.sql.lightweight.ast.CreateProcedureStatement.Language;
import innowake.ndt.parsing.parser.sql.lightweight.ast.CreateSynonymOrAliasStatement;
import innowake.ndt.parsing.parser.sql.lightweight.ast.CreateTableStatement;
import innowake.ndt.parsing.parser.sql.lightweight.ast.CreateTriggerStatement;
import innowake.ndt.parsing.parser.sql.lightweight.ast.CreateViewStatement;
import innowake.ndt.parsing.parser.sql.lightweight.ast.CteStatement;
import innowake.ndt.parsing.parser.sql.lightweight.ast.DeleteStatement;
import innowake.ndt.parsing.parser.sql.lightweight.ast.InsertStatement;
import innowake.ndt.parsing.parser.sql.lightweight.ast.SelectStatement;
import innowake.ndt.parsing.parser.sql.lightweight.ast.SqlBegin;
import innowake.ndt.parsing.parser.sql.lightweight.ast.SqlLightweightNode;
import innowake.ndt.parsing.parser.sql.lightweight.ast.UpdateStatement;
import innowake.ndt.parsing.scanner.plsql.PlSqlLexerFactory;

/**
 * Contributor for PL/SQL script files.
 */
@Component
public class PlSqlMetricsContributor implements DiscoveryContributorFromSource {

	private static final String LINE_SEPARATOR = "\n";
	private static final Logger LOG = LoggerFactory.getLogger(PlSqlMetricsContributor.class);
	private static final Pattern LINE_DELIMITER_PATTERN = Pattern.compile("\\R");
	private static final Pattern SPACE_PATTERN = Pattern.compile("\\s+");
	private static final Pattern DIGIT_PATTERN = Pattern.compile("\\d+");
	private static final Set<String> KEYWORDS = Set.of("BY", "LIMIT", "SETS", "=", "<", ">", "!", ",", "!=", "<=", ">=", "ELSE");

	@Autowired
	private ParserProviderService parserProvider;

	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return sourceObject.getTechnology() == Technology.SQL && sourceObject.getType() == Type.SCRIPT;
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		final SqlLightWeightParseResultProvider parseResultProvider = parserProvider.createSqlLightWeightParser(context);
		final GenericMetricsContributor metricsContributor = new GenericMetricsContributor(new InputProvider(sourceObject, PlSqlLexerFactory.get()));
		metricsContributor.enable(MetricFactory.get(MetricType.LOC_LOC));

		final ModuleBuilder rootModule = builder.declareRootModule(sourceObject.getName(), ModuleType.SQL_SCRIPT);

		try {
			rootModule.addAdditionalInfo(GenericMetricsUtil.executeAndGetResults(metricsContributor));
		} catch (final MetricException e) {
			LOG.error("Error while calculating metrics", e);
			rootModule.addError(Severity.ERROR, ErrorKey.METRICS_CALCULATION_ERROR, e.getMessage());
		}

		final AstModel model;
		try {
			model = parseResultProvider.getParseResult(sourceObject);
		} catch (final DiscoveryException e) {
			LOG.error("Error while parsing " + sourceObject.getPath(), e);
			rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, e.getMessage());
			/* unable to parse so nothing more to discover */
			return;
		} catch (final WorkerCancellationException e) {
			rootModule.addError(Severity.ERROR, ErrorKey.MODULE_ABORT, e.getMessage());
			return;
		}
		final Optional<AstNode> root = model.getRoot();
		if (root.isEmpty()) {
			LOG.error("Error while parsing " + sourceObject.getPath() + ": No root found.");
			rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, "Unable to parse SQL script. No root found.");
			/* root is missing (parse error) so nothing more to discover */
			return;
		}
		collectReferencedTables(model, rootModule, sourceObject.getContent().toString());
		collectDDLReferences((SqlLightweightNode) root.get(), rootModule);
		collectStoredProcedures(builder, (SqlLightweightNode) root.get());
		addSqlStatements(rootModule, (SqlLightweightNode) root.get(), model);
	}

	private void collectReferencedTables(final AstModel model, final ModuleBuilder rootModule, final String sourceContent) {
		model.getRoot().ifPresent(root -> root.getChildren().forEach(child -> {
			if (child instanceof SqlBegin || child instanceof CreateProcedureStatement || child instanceof CreateTriggerStatement) {
				final List<AstNode> sqlNodes = child.getChildren();
				sqlNodes.forEach(sqlNode -> collectReferencedTableDependencies(model, rootModule, sqlNode, sourceContent));
			} else {
				collectReferencedTableDependencies(model, rootModule, child, sourceContent);
			}
		}));
	}

	private void collectReferencedTableDependencies(final AstModel model, final ModuleBuilder rootModule, final AstNode sqlLightWeightNode,
			final String sourceContent) {
		if (sqlLightWeightNode instanceof SelectStatement || sqlLightWeightNode instanceof InsertStatement || sqlLightWeightNode instanceof DeleteStatement
				|| sqlLightWeightNode instanceof UpdateStatement || sqlLightWeightNode instanceof CteStatement) {
			final Map<String, List<DatabaseAccessType>> referencedTables;
			var sqlQuery = StringUtils.EMPTY;
			final var document = new Document(sourceContent);
			try {
				sqlQuery = model.getSource(sqlLightWeightNode);
				sqlQuery = removeTrailingNumbers(sqlQuery);
				referencedTables = SimpleSqlCollector.getSqlTables(sqlQuery);
				referencedTables.forEach((tableName, accessTypes) -> {
					final String entityName = SqlUtil.getEntityName(tableName);
					final var dependencyBuilder = DawnMetricsUtility.declareDependencyToSqlTableWithSchema(rootModule,
							entityName, ResolutionFlag.RESOLVE_CASE_INSENSITIVE);
					dependencyBuilder.setLocation(new ModuleLocation(sqlLightWeightNode.getStartOffset(), sqlLightWeightNode.getLength()));
					dependencyBuilder.addAttribute(ModelAttributeKey.DB_ACCESS_TYPE, accessTypes);
				});
			} catch (final SqlParseException e) {
				final var errorMessage = String.format( "Unable to parse statement %s for referenced table", StringUtils.normalizeSpace(sqlQuery));
				LOG.trace(errorMessage, e);
				final var offset = sqlLightWeightNode.getStartOffset();
				final var length = sqlLightWeightNode.getLength();
				final var lineNumbers = AstNodeLocationProvider.getLineNumbers(document, offset, length);
				final var moduleLocation = new AstNodeLocation(offset, length, offset, length, offset, length, lineNumbers.e1, lineNumbers.e2);
				rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, errorMessage,
						moduleLocation);
			}
		}
	}

	private void collectDDLReferences(final SqlLightweightNode sqlLightWeightNode, final ModuleBuilder rootModule) {
		sqlLightWeightNode.getChildren().forEach(child -> {
	        final SqlLightweightNode sqlNode = (SqlLightweightNode) child;
	        String entityName = null;
	        ModuleType moduleType = null;

	        if (sqlNode instanceof CreateSynonymOrAliasStatement) {
	            final CreateSynonymOrAliasStatement createSynonymStatement = (CreateSynonymOrAliasStatement) sqlNode;
	            entityName = SqlUtil.getEntityName(createSynonymStatement.getName());
	            moduleType = ModuleType.SQL_SYNONYM;
	        } else if (sqlNode instanceof CreateTableStatement) {
	            final CreateTableStatement createTableStatement = (CreateTableStatement) sqlNode;
	            entityName = SqlUtil.getEntityName(createTableStatement.getTableName());
	            moduleType = ModuleType.SQL_TABLE;
	        } else if (sqlNode instanceof CreateViewStatement) {
	            final CreateViewStatement createViewStatement = (CreateViewStatement) sqlNode;
	            entityName = SqlUtil.getEntityName(createViewStatement.getViewName());
	            moduleType = ModuleType.SQL_VIEW;
	        }

	        if (entityName != null && moduleType != null) {
	            final ModuleFilter tableFilter = new ModuleFilter()
	                    .setNames(entityName)
	                    .setTypes((moduleType != ModuleType.SQL_TABLE) ? moduleType : ModuleType.SQL_VIEW, ModuleType.SQL_TABLE);

	            rootModule.declareDependency(RelationshipType.ACCESSES, tableFilter, ResolutionFlag.RESOLVE_CASE_INSENSITIVE)
	                    .setBinding(Binding.LATE)
	                    .setLocation(new ModuleLocation(sqlNode.getStartOffset(), sqlNode.getLength()))
	                    .createIfMissing(entityName, moduleType);
	        }
		});
	}

	private void addSqlStatements(final ModuleBuilder rootModule, final SqlLightweightNode sqlLightWeightNode, final AstModel model) {
		new TopDownVisitor<SqlLightweightNode>(node -> {
			var sqlString = model.getSource(node);
			sqlString = removeTrailingNumbers(sqlString);
			if (node instanceof CreateTriggerStatement) {
				DawnExecStatementUtility.addSqlStatementForNonExecSql(sqlString, StatementType.CREATE_TRIGGER, rootModule);
				return true;
			}
			if (node instanceof CreateProcedureStatement) {
				DawnExecStatementUtility.addSqlStatementForNonExecSql(sqlString, StatementType.CREATE_PROCEDURE, rootModule);
				return true;
			}
			if (node instanceof SelectStatement || node instanceof InsertStatement
					|| node instanceof UpdateStatement || node instanceof DeleteStatement || node instanceof CteStatement 
					|| node instanceof CreateSynonymOrAliasStatement || node instanceof CreateTableStatement || node instanceof CreateViewStatement) {
			  	DawnExecStatementUtility.addSqlStatementForExecSql(sqlString, rootModule, true);
			}
			return true;
		}).visit(sqlLightWeightNode);
	}

	private void collectStoredProcedures(final DiscoveryBuilderFromSource builder, final SqlLightweightNode sqlLightWeightNode) {
		final List<CreateProcedureStatement> storedProcedures = sqlLightWeightNode.getChildrenDeep(CreateProcedureStatement.class);
		for (final CreateProcedureStatement procedure : storedProcedures) {
			final ModuleBuilder storedProcedureModule = builder.declareSubModule(procedure.getName(), ModuleType.SQL_STORED_PROCEDURE,
					new ModuleLocation(procedure.getStartOffset(), procedure.getLength()));

			final Optional<String> externalName = procedure.getExternalName();
			String extName;
			final ModuleType moduleType;
			if (externalName.isPresent()) {
				extName = externalName.get();
				if (procedure.getLanguage() == Language.JAVA) {
					extName = extractExternalNameForJava(extName);
					moduleType = ModuleType.JAVA_TYPE;
				} else {
					moduleType = ModuleType.COBOL_PROGRAM;
				}

				storedProcedureModule.declareDependency(RelationshipType.CALLS, new ModuleFilter().setNames(extName).setTypes(moduleType))
						.addAttribute(ModelAttributeKey.OUTBOUND_TARGETS, extName)
						.setLocation(new ModuleLocation(procedure.getStartOffset(), procedure.getLength())).setBinding(Binding.LATE);
			}
		}
	}
	
	private String extractExternalNameForJava(final String externalName) {
		final var nameStartIndex = externalName.indexOf(":") + 1;
		return externalName.substring(nameStartIndex, externalName.lastIndexOf("."));
	}

	private String removeTrailingNumbers(final String sql) {
		final var stringBuilder = new StringBuilder();
		final String[] lines = LINE_DELIMITER_PATTERN.split(sql);
		for (final String line : lines) {
			final String[] words = SPACE_PATTERN.split(line.trim());
			final int length = words.length;
			if (length < 2) {
				stringBuilder.append(line);
				stringBuilder.append(" ");
				continue;
			}
			final String lastWord = words[length - 1];
			final String secondLastWord = words[length - 2];
			if (isNumeric(lastWord) && ! KEYWORDS.contains(secondLastWord.toUpperCase())) {
				words[length - 1] = "";
			}
			final var processedLine = String.join(" ", words).trim();
			stringBuilder.append(StringUtils.normalizeSpace(processedLine));
			stringBuilder.append(LINE_SEPARATOR);
		}
		return stringBuilder.toString().trim();
	}

	private boolean isNumeric(final String string) {
		return DIGIT_PATTERN.matcher(string).matches();
	}
}

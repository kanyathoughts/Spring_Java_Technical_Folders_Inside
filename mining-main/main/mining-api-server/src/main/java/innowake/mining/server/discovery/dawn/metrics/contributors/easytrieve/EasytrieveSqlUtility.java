/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.easytrieve;

import static innowake.mining.server.discovery.metrics.exec.ExecStatementUtility.getDbAccess;

import java.util.Optional;

import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.DependencyBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.contributors.DawnExecStatementUtility;
import innowake.mining.server.discovery.dawn.metrics.contributors.DawnMetricsUtility;
import innowake.mining.server.discovery.metrics.exec.ExecSqlCollector;
import innowake.mining.server.discovery.parser.easytrieve.EasytrieveAntlrParseResult;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.antlr.easytrieve.ast.EasytrieveHostVariableNode;
import innowake.ndt.antlr.easytrieve.ast.EasytrieveReferenceNode;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.ast.model.exec.ExecNode;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlCall;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDeclareSchema;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDeclareTempTable;

/**
 * Utility class for easytrieve SQL resources and dependencies.
 */
public class EasytrieveSqlUtility {

	private EasytrieveSqlUtility() {
		/*  Private constructor to hide implicit one of the Util class. */
	}

	/**
	 * Collect sql resources.
	 *
	 * @param easytrieveAntlrParseResult the EasytrieveAntlrParseResult
	 * @param rootNode the AstNode of parser result
	 * @param builder the generic for DiscoveryBuilder or DiscoveryBuilderFromSource
	 * @param rootModule the module of the source object
	 */
	public static <T extends DiscoveryBuilder> void collectSqlResources(final EasytrieveAntlrParseResult easytrieveAntlrParseResult, final AstNode rootNode,
			final T builder, final ModuleBuilder rootModule) {
		rootNode.getChildrenDeep(ExecNode.class).forEach(sqlNode -> {
			try {
				DawnExecStatementUtility.addSqlStatementForExecSql(sqlNode, rootModule);
				if ( ! (sqlNode instanceof ExecSqlCall)) {
					final var sqlCollector = new ExecSqlCollector();
					sqlCollector.handleExecSql(sqlNode);
					sqlCollector.getReferencedTables().keySet().forEach(table -> {
						if (sqlNode instanceof ExecSqlDeclareTempTable) {
							builder.declareExternalModule(table, ModuleType.SQL_TEMPORARY_TABLE);
						} else if (sqlNode instanceof ExecSqlDeclareSchema) {
							builder.declareExternalModule(table, ModuleType.RDB_DATABASE);
						}
					});
				} else {
					collectSqlStoredProcedures(easytrieveAntlrParseResult, (ExecSqlCall<?>) sqlNode, builder);
				}
			} catch (final Exception e) {
				rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, e.getMessage());
			}
		});
	}

	private static <T extends DiscoveryBuilder> void collectSqlStoredProcedures(final EasytrieveAntlrParseResult easytrieveAntlrParseResult,
			final ExecSqlCall<?> call, final T builder) {
		final AstNode procedureNameReference = call.getProcedureName();
		if (procedureNameReference instanceof EasytrieveReferenceNode) {
			final EasytrieveReferenceNode referenceNode = (EasytrieveReferenceNode) procedureNameReference;
			builder.declareExternalModule(referenceNode.getContext().getText(), ModuleType.SQL_STORED_PROCEDURE);
		} else if (procedureNameReference instanceof EasytrieveHostVariableNode) {
			final EasytrieveHostVariableNode hostVariableNode = (EasytrieveHostVariableNode) procedureNameReference;
			final var variableString = hostVariableNode.getContext().getText();
			if (easytrieveAntlrParseResult.getAssignmentValues().containsKey(variableString)) {
				for (final String s : easytrieveAntlrParseResult.getAssignmentValues().get(variableString)) {
					builder.declareExternalModule(s, ModuleType.SQL_STORED_PROCEDURE);
				}
			}
		}
	}

	/**
	 * Collect sql dependencies.
	 *
	 * @param rootModule the module of the source object
	 * @param parseResult the EasytrieveAntlrParseResult
	 */
	public static void collectSqlDependencies(final ModuleBuilder rootModule, final EasytrieveAntlrParseResult parseResult) {
		final Optional<AstNode> rootNodeOptional = parseResult.getAstModel().getRoot();
		rootNodeOptional.ifPresent(rootNode -> 
		rootNode.getChildrenDeep(ExecNode.class).forEach(sqlNode -> {
			try {
				if (sqlNode instanceof ExecSqlCall) {
					collectSqlStoredProcedureDependency(rootModule, parseResult, (ExecSqlCall<?>) sqlNode);
				} else {
					final boolean isInstanceOfExecSqlDeclareSchema = sqlNode instanceof ExecSqlDeclareSchema;
					final boolean isInstanceOfExecSqlDeclareTempTable = sqlNode instanceof ExecSqlDeclareTempTable;
					final var sqlCollector = new ExecSqlCollector();
					sqlCollector.handleExecSql(sqlNode);
					sqlCollector.getReferencedTables().forEach((key, value) -> {
						final ModuleFilter moduleFilter;
						final RelationshipType relationship;
						final DependencyBuilder dependencyBuilder;
						if (isInstanceOfExecSqlDeclareTempTable) {
							final var targetType = ModuleType.fromTechnologyAndType(Technology.SQL, Type.TABLE);
							moduleFilter = new ModuleFilter().setNames(key).setTypes(targetType);
							relationship = RelationshipType.from(Technology.SQL, Type.TABLE);

							dependencyBuilder = rootModule.declareDependency(relationship, moduleFilter)
									.addAttribute(ModelAttributeKey.DB_ACCESS_TYPE, getDbAccess(value));
						} else if (isInstanceOfExecSqlDeclareSchema) {
							final var targetType = ModuleType.fromTechnologyAndType(Technology.ORACLE, Type.RDB_DATABASE);
							moduleFilter = new ModuleFilter().setNames(key).setTypes(targetType);
							relationship = RelationshipType.from(Technology.ORACLE, Type.RDB_DATABASE);

							dependencyBuilder = rootModule.declareDependency(relationship, moduleFilter)
									.addAttribute(ModelAttributeKey.DB_ACCESS_TYPE, getDbAccess(value));
						} else {
							dependencyBuilder = DawnMetricsUtility.declareDependencyToSqlTableWithSchema(rootModule, key, ResolutionFlag.RESOLVE_CASE_INSENSITIVE);
							dependencyBuilder.addAttribute(ModelAttributeKey.DB_ACCESS_TYPE, getDbAccess(value));
						}
						dependencyBuilder.setBinding(Binding.LATE);
					});
				}
			} catch (final Exception e) {
				rootModule.addError(Severity.ERROR, ErrorKey.METRICS_CALCULATION_ERROR, e.getMessage());
			}
		}));
	}

	private static void collectSqlStoredProcedureDependency(final ModuleBuilder rootModule, final EasytrieveAntlrParseResult parseResult,
			final ExecSqlCall<?> call) {
		final AstNode procedureNameReference = call.getProcedureName();
		if (procedureNameReference instanceof EasytrieveReferenceNode) {
			final EasytrieveReferenceNode referenceNode = (EasytrieveReferenceNode) procedureNameReference;
			final var targetType = ModuleType.fromTechnologyAndType(Technology.SQL, Type.STORED_PROCEDURE);
			final var moduleFilter = new ModuleFilter().setNames(referenceNode.getContext().getText()).setTypes(targetType);
			final var relationship = RelationshipType.from(Technology.SQL, Type.STORED_PROCEDURE);

			rootModule.declareDependency(relationship, moduleFilter).setBinding(Binding.LATE);

		} else if (procedureNameReference instanceof EasytrieveHostVariableNode) {
			final EasytrieveHostVariableNode hostVariableNode = (EasytrieveHostVariableNode) procedureNameReference;
			final var variableString = hostVariableNode.getContext().getText();
			if (parseResult.getAssignmentValues().containsKey(variableString)) {
				for (final String variableValue : parseResult.getAssignmentValues().get(variableString)) {
					final var targetType = ModuleType.fromTechnologyAndType(Technology.SQL, Type.STORED_PROCEDURE);
					final var moduleFilter = new ModuleFilter().setNames(variableValue).setTypes(targetType);
					final var relationship = RelationshipType.from(Technology.SQL, Type.STORED_PROCEDURE);

					rootModule.declareDependency(relationship, moduleFilter).setBinding(Binding.LATE);
				}
			}
		}
	}
}

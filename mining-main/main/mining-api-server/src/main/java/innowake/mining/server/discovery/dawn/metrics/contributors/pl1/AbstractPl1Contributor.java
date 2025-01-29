/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.contributors.pl1;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.model.discovery.ModelAttributeMap;
import innowake.mining.server.discovery.dawn.metrics.contributors.DawnExecStatementUtility;
import innowake.mining.server.discovery.metrics.exec.ExecSqlCollector;
import innowake.mining.server.discovery.parser.pl1.Pl1ExternalFunctionCallExtractor;
import innowake.mining.server.discovery.parser.pl1.Pl1ParseResultProvider;
import innowake.mining.server.discovery.parser.pl1.Pl1ParseResultProvider.Pl1ParseResult;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.utility.UtilityList;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.StatementType;
import innowake.ndt.core.assembling.IAssembling;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.ast.model.exec.ExecNode;
import innowake.ndt.core.parsing.ast.model.exec.ExecSqlNode;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlCall;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDeclareSchema;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDeclareTempTable;
import innowake.ndt.parsing.parser.dependency.ast.BaseDependencyNode;
import innowake.ndt.parsing.parser.dependency.ast.pl1.AbstractPl1ProcedureNode;
import innowake.ndt.parsing.parser.dependency.ast.pl1.NamedVariableDeclaration;
import innowake.ndt.parsing.parser.dependency.ast.pl1.Pl1CallNode;
import innowake.ndt.pl1parser.parser.ExecSqlParboiledNodeProducer;
import org.apache.commons.collections4.MultiValuedMap;

import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * Abstract class housing common code used in pl1ExperimentalLightWeightContributor and pl1LightWeightContributor.
 *
 * @param <T> Type of procedure node
 */
public abstract class AbstractPl1Contributor<T extends AbstractPl1ProcedureNode> {

	protected final AstModel pl1LightweightModel;
	private final AstModel pl1IncludeModel;
	private final AstModel pl1ExternalCallsModel;
	protected final AstModel pl1InternalModel;
	private final Map<String, ModuleType> externalEntries;
	private final IAssembling<SourcePojo> assembling;
	private final Pl1DependencyUtility<T> pl1DependencyUtility;
	private final Config config;
	private final Class<T> procedureNodeClass;
	private static final String SQLCA = "SQLCA";


	AbstractPl1Contributor(final Pl1ParseResult parseResult, final Pl1DependencyUtility<T> pl1DependencyUtility, final Config config)
			throws DiscoveryException {
		this.pl1LightweightModel = parseResult.getPl1LightweightModel();
		this.pl1IncludeModel = parseResult.getPl1IncludeModel();
		externalEntries = parseResult.getExternalEntries();
		pl1ExternalCallsModel = parseResult.getPl1ExternalCallsModel();
		pl1InternalModel = parseResult.getPl1InternalCallsModel();
		assembling = parseResult.getAssembling();
		this.pl1DependencyUtility = pl1DependencyUtility;
		this.config = config;
		this.procedureNodeClass = getProcedureClass();
	}

	/**
	 * Performs the collection of the metrics.
	 *
	 */
	void collectMetrics(final MultiValuedMap<String, Tuple2<ModuleLocation, ModelAttributeMap<Object>>> imsUtilities) {
		findCopies();
		findResources();
		findExternalCalls(imsUtilities);
	}

	private void findCopies() {
		pl1IncludeModel.getRoot().ifPresent(root -> root.getChildrenDeep(BaseDependencyNode.class).forEach(this::handleInclude));
	}

	private void findExternalCalls(final MultiValuedMap<String, Tuple2<ModuleLocation, ModelAttributeMap<Object>>> imsUtilities) {
		final Optional<AstNode> root = pl1LightweightModel.getRoot();
		final Optional<AstNode> externalCallsRoot = pl1ExternalCallsModel.getRoot();
		if (root.isEmpty() || externalCallsRoot.isEmpty()) {
			return;
		}
		root.get()
				.getChildrenDeep(NamedVariableDeclaration.class,
						dcl -> Pl1ParseResultProvider.isNamedDeclarationOfType((NamedVariableDeclaration) dcl, "ENTRY"))
				.forEach(dcl ->
					pl1DependencyUtility.createModelDependencies(dcl.getVariableName(), dcl, findTopLevelParentProcedure(dcl),
							Binding.LATE, RelationshipType.CALLS, true, new ModelAttributeMap<>(), false,
							dcl.getDataFormatTypes().contains("RETURNS") ? ModuleType.PL1_FUNCTION : ModuleType.PL1_SUBROUTINE)
				);
		externalCallsRoot.get().getChildren(Pl1ExternalFunctionCallExtractor.Pl1ExternalCallNode.class).forEach(node -> {
					final T parentProcedure = findTopLevelParentProcedure(node.getStartOffset());
					node.getExternalCalls().forEach((targetName, moduleType) -> pl1DependencyUtility.createModelDependencies(targetName,
							node, parentProcedure, Binding.LATE, RelationshipType.CALLS, true,  new ModelAttributeMap<>(), false,
							moduleType));
				});
		final var possibleExternalTargets = externalEntries.keySet();
		root.get().getChildrenDeep(Pl1CallNode.class).forEach(call -> createCallNodeDependencies(imsUtilities, call, possibleExternalTargets));
	}

	private void createCallNodeDependencies(final MultiValuedMap<String, Tuple2<ModuleLocation, ModelAttributeMap<Object>>> imsUtilities, final Pl1CallNode call,
			final Set<String> possibleExternalTargets) {
		final T procedure = findTopLevelParentProcedure(call);
		call.getTargets().forEach(targetName -> {
			/* Create a Utility dependency with attributes if it is present in imsUtilities */
			if (imsUtilities.containsKey(targetName)) {
				final var location = pl1DependencyUtility.getOriginResolver().resolveLocation(call);
				final var dependency =
						imsUtilities.get(targetName).stream().filter(property -> property.e1.getOffset().equals(location.getOffset())).findFirst();
				dependency.ifPresent(imsDependency -> {
					pl1DependencyUtility.discoveryBuilder.declareExternalModule(targetName, ModuleType.UNKNOWN_UTILITY);
					pl1DependencyUtility.createModelDependencies(targetName, call, procedure, Binding.LATE,
							RelationshipType.CALLS, true, imsDependency.e2, false, ModuleType.UNKNOWN_UTILITY);
				});
				return;
			}
			if (possibleExternalTargets.contains(targetName)) {
				pl1DependencyUtility.createModelDependencies(targetName, call, procedure, Binding.LATE,RelationshipType.CALLS, true, new ModelAttributeMap<>(),
						false, ModuleType.PL1_SUBROUTINE);
				return;
			}
			if (isUtility(targetName, config.getUtilityList())) {
				pl1DependencyUtility.discoveryBuilder.declareExternalModule(targetName, ModuleType.UNKNOWN_UTILITY);
				pl1DependencyUtility.createModelDependencies(targetName, call, procedure, Binding.LATE,RelationshipType.CALLS, true, new ModelAttributeMap<>(),
						false, ModuleType.UNKNOWN_UTILITY);
			}
		});
	}

	private void handleInclude(final BaseDependencyNode node) {
		final var includeTargets = node.getTargets();
		final int startOffset = node.getStartOffset();

		/* offset -1 since the actual the include is replaced it does not have a location in the assembled code, but the whitespace character
		 * directly before the replaced statement should still exist */
		final int offsetInAssembledSource = assembling.getOffsetAssembled(startOffset - 1, null);

		final T parentProcedure = findTopLevelParentProcedure(offsetInAssembledSource);
		for (final var includeTarget : includeTargets) {
			final var relationship = isUtility(includeTarget, config.getUtilityList()) ? RelationshipType.CALLS : RelationshipType.INCLUDES;
			final var isSqlCa = includeTarget.equalsIgnoreCase(SQLCA);
			final var moduleType = isSqlCa ? ModuleType.COBOL_COPYBOOK : ModuleType.PL1_COPYBOOK;
			pl1DependencyUtility.createModelDependencies(includeTarget, node, parentProcedure, Binding.EARLY, relationship, false,
					new ModelAttributeMap<>(), isSqlCa, moduleType);
		}
	}

	private void findResources() {
		final var root = pl1LightweightModel.getRoot();
		if (root.isEmpty()) {
			return;
		}

		root.get()
				.getChildrenDeep(ExecSqlParboiledNodeProducer.ExecSqlHeavyWeightNode.class)
				.forEach(exec -> {
					final ExecNode<?> genericType = exec.getGenericType(ExecNode.class);
					if (genericType != null) {
						resolveSqlStatement(genericType, exec);
						collectSqlMetrics(genericType);
					}
				});
	}

	private void resolveSqlStatement(final ExecNode<?> sqlNode, final ExecSqlParboiledNodeProducer.ExecSqlHeavyWeightNode exec) {
		if ( ! (sqlNode instanceof ExecSqlCall)) {
			final ExecSqlCollector sqlCollector = new ExecSqlCollector();
			sqlCollector.handleExecSql(sqlNode);
			sqlCollector.getReferencedTables().keySet()
					.forEach(table -> {
						if (sqlNode instanceof ExecSqlDeclareTempTable) {
							pl1DependencyUtility.createModelDependencies(table, exec, findTopLevelParentProcedure(exec), Binding.LATE, RelationshipType.ACCESSES, true,
									new ModelAttributeMap<>(), true, ModuleType.SQL_TEMPORARY_TABLE);
						} else if(sqlNode instanceof ExecSqlDeclareSchema) {
							pl1DependencyUtility.createModelDependencies(table, exec, findTopLevelParentProcedure(exec), Binding.LATE, RelationshipType.ACCESSES, true,
									new ModelAttributeMap<>(), true, ModuleType.RDB_DATABASE);
						} else {
							pl1DependencyUtility.createModelDependencies(table, exec, findTopLevelParentProcedure(exec), Binding.LATE, RelationshipType.ACCESSES, true,
									new ModelAttributeMap<>(), true, ModuleType.SQL_TABLE, ModuleType.SQL_VIEW);
						}
					});
		}
	}

	/**
	 * Used to collect SQL Metrics.
	 *
	 * @param sqlNode the {@link ExecNode}
	 */
	private void collectSqlMetrics(final ExecNode<?> sqlNode) {
		final var actualSource = pl1DependencyUtility.getActualSource(sqlNode, true);
		if (sqlNode instanceof ExecSqlDeclareTempTable) {
			DawnExecStatementUtility.addSqlStatementForNonExecSql(sqlNode, StatementType.DECLARE_TEMP_TABLE, actualSource);
			return;
		}
		if (sqlNode instanceof ExecSqlDeclareSchema) {
			DawnExecStatementUtility.addSqlStatementForNonExecSql(sqlNode, StatementType.DECLARE_SCHEMA, actualSource);
			return;
		}
		if (sqlNode instanceof ExecSqlNode) {
			DawnExecStatementUtility.addSqlStatementForExecSql(sqlNode, actualSource);
		}
	}

	/**
	 * Returns the top level parent procedure.
	 *
	 * @param startOffset the startOffset of the procedure
	 * @return the top level parent procedure
	 */
	@Nullable
	T findTopLevelParentProcedure(final int startOffset) {
		final var nodes = pl1LightweightModel.getNode(startOffset);
		if ( nodes.isEmpty()) {
			return null;
		}
		var closestNode = nodes.stream().filter(n -> n.getGenericType(procedureNodeClass) != null).findFirst().orElse(nodes.get(0));
		while (closestNode != null && closestNode.getEndOffset() < startOffset) {
			closestNode = closestNode.getNullableParent();
		}
		return closestNode != null ? findTopLevelParentProcedure(closestNode) : null;
	}

	/**
	 * Returns the top level parent procedure.
	 * @param node the {@link AstNode}
	 * @return the top level parent procedure
	 */
	@Nullable
	T findTopLevelParentProcedure(final AstNode node) {
		T parentProcedure = node.getGenericType(procedureNodeClass);
		for (var currentNode = node; currentNode != null; currentNode = currentNode.getNullableParent()) {
			final T maybeProcedure = currentNode.getGenericType(procedureNodeClass);
			if (maybeProcedure != null) {
				parentProcedure = maybeProcedure;
			}
		}
		return parentProcedure;
	}

	static boolean isUtility(final String name, final UtilityList utilityList) {
		final var upperCaseName = name.toUpperCase().trim();
		return utilityList.isUtility(upperCaseName);
	}

	/**
	 * Returns the class of the procedure.
	 *
	 * @return the class of the procedure
	 */
	abstract Class<T> getProcedureClass();
}

/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.c;

import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.TYPE_REFERENCE_TYPE;
import static innowake.mining.data.model.discovery.attribute.ModelAttributeValue.TypeReferenceType.IMPLEMENT;
import static innowake.mining.server.discovery.metrics.exec.ExecStatementUtility.getDbAccess;

import com.google.common.collect.Streams;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.model.discovery.ModelAttributeMap;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.data.model.discovery.attribute.ModelAttributeValue;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DeferredAction;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.contributors.AstInputProvider;
import innowake.mining.server.discovery.dawn.metrics.contributors.AstNodeLocationProvider;
import innowake.mining.server.discovery.dawn.metrics.contributors.DawnExecStatementUtility;
import innowake.mining.server.discovery.dawn.metrics.contributors.DawnMetricsUtility;
import innowake.mining.server.discovery.dawn.metrics.contributors.GenericMetricsUtil;
import innowake.mining.server.discovery.metrics.exec.ExecSqlCollector;
import innowake.mining.server.discovery.metrics.generic.GenericMetricsContributor;
import innowake.mining.server.discovery.metrics.generic.MetricException;
import innowake.mining.server.discovery.metrics.generic.MetricFactory;
import innowake.mining.server.discovery.metrics.generic.MetricType;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.server.discovery.parser.c.CAntlrParseResult;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.entities.DependencyDefinitionPojo;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.AstNodeLocation;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.antlr.base.BaseAntlrParserError;
import innowake.ndt.antlr.c.ast.CFileAccessStatement;
import innowake.ndt.antlr.c.ast.CFunctionDefinition;
import innowake.ndt.antlr.c.ast.CFunctionReference;
import innowake.ndt.antlr.c.ast.HostVariableNode;
import innowake.ndt.antlr.c.ast.ReferenceNode;
import innowake.ndt.core.parsing.ILocation;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.ast.model.exec.ExecNode;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlCall;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDeclareCursor;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDeclareSchema;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDeclareTempTable;
import innowake.ndt.core.parsing.ast.model.statement.CallExternalStatement;
import innowake.ndt.core.parsing.ast.model.statement.FileAccessStatement.FileAccessType;
import innowake.ndt.core.parsing.spi.Document;
import innowake.ndt.parsing.scanner.c.CLexerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Contributor for C files.
 */
@Component
public class CContributor implements DiscoveryContributorFromSource {

	private static final Logger LOG = LoggerFactory.getLogger(CContributor.class);

	@Autowired
	private ParserProviderService parserProvider;
	@Autowired
	private ModuleService moduleService;

	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return sourceObject.getTechnology() == Technology.C && (sourceObject.getType() == Type.HEADER || sourceObject.getType() == Type.PROGRAM);
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		final var cAntlrParseResultProvider = parserProvider.createCParser(context);
		final var rootModule = builder.declareRootModule(sourceObject.getName(),
				ModuleType.fromTechnologyAndType(sourceObject.getTechnology(), sourceObject.getType()));

		try {
			final var cAntlrParseResult = cAntlrParseResultProvider.getParseResult(sourceObject);
			final var astModel = cAntlrParseResult.getAstModel();
			final var root = astModel.getRoot();

			if (root.isEmpty()) {
				final var noRootFound = "Root not found for the C" + sourceObject.getType() + "with" + sourceObject.getPath();
				LOG.error(() -> noRootFound);
				rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, noRootFound);
				/* root is missing (parse error) so nothing more to discover */
				return;
			}
			final var collectParseErrors = context.getFeatureMap().get(FeatureId.COLLECT_PARSER_ERRORS).booleanValue();
			calculateGenericMetrics(rootModule, sourceObject, collectParseErrors, cAntlrParseResult, astModel);
			final var rootNode = root.get();
			collectSqlResources(cAntlrParseResult, rootNode, builder, rootModule);
			collectResourceFileDependencies(builder, rootNode, rootModule);
			collectDependencies(cAntlrParseResult, builder, rootModule, sourceObject.getPath());
			/* The transitive metrics creates dependency only if there is function dependency with implements attribute present. As only header files
			 *  creates the dependency to the function with implements attribute, we are adding deferred action to only header source files */
			if (sourceObject.getType() == Type.HEADER) {
				rootModule.deferAction("collectTransitiveMetrics");
			}
		} catch (final DiscoveryException e) {
			final var errorMessage = String.format("Error while parsing C %s", sourceObject.getPath());
			LOG.error(() -> errorMessage, e);
			rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, errorMessage, e);
			/* unable to parse so nothing more to discover */
		}
	}

	private void calculateGenericMetrics(final ModuleBuilder rootModule, final SourcePojo sourceObject, final boolean collectParseError,
			final CAntlrParseResult cAntlrParseResult, final AstModel astModel) {
		collectSourceMetrics(rootModule, sourceObject, astModel);
		collectErrors(collectParseError, cAntlrParseResult, rootModule, sourceObject.getContent().toString());
	}

	private void collectSourceMetrics(final ModuleBuilder rootModule, final SourcePojo sourceObject, final AstModel astModel) {
		final var metricsContributor =  new GenericMetricsContributor(new AstInputProvider(sourceObject, CLexerFactory.get(), astModel));
		metricsContributor.enable(MetricFactory.get(MetricType.LOC_LOC));
		metricsContributor.enable(MetricFactory.get(MetricType.MCCABE_COMPLEXITY));
		try {
			rootModule.addAdditionalInfo(GenericMetricsUtil.executeAndGetResults(metricsContributor));
		} catch (final MetricException e) {
			LOG.error("Error while collecting metrics for C", e.getMessage());
			rootModule.addError(Severity.ERROR, ErrorKey.METRICS_CALCULATION_ERROR, e.getMessage());
		}
	}

	private void collectErrors(final boolean collectParseError, final CAntlrParseResult cAntlrParseResult, final ModuleBuilder rootModule,
			final String sourceContent) {
		final var errors = cAntlrParseResult.getErrors();
		if ( ! errors.isEmpty()) {
			if (collectParseError) {
				final var document = new Document(sourceContent);
				errors.forEach(error -> rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, error.toString(), setModuleLoc(error, document)));
			} else {
				rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, String.format("%d parse errors were found in source file.", errors.size()));
			}
		}
	}

	private AstNodeLocation setModuleLoc(final BaseAntlrParserError error, final Document document) {
		final Optional<ILocation> loc = error.getLocation();
		if (loc.isPresent()) {
			final var offset = loc.get().getOffset();
			final var length = loc.get().getLength();
			final var lineNumbers = AstNodeLocationProvider.getLineNumbers(document, offset, length);
			return new AstNodeLocation(offset, length, offset, length, offset, length, lineNumbers.e1, lineNumbers.e2);
		}
		return new AstNodeLocation();
	}

	private void collectSqlResources (final CAntlrParseResult cAntlrParseResult, final AstNode root, final DiscoveryBuilderFromSource builder,
			final ModuleBuilder rootModule) {
		root.getChildrenDeep(ExecNode.class).forEach(sqlNode -> {
			final var isInstanceOfExecSqlDeclareCursor = sqlNode instanceof ExecSqlDeclareCursor;
			final var isInstanceOfExecSqlCall = sqlNode instanceof ExecSqlCall;
			try {
				if ( ! isInstanceOfExecSqlDeclareCursor) {
					DawnExecStatementUtility.addSqlStatementForExecSql(sqlNode, rootModule);
				}
				if ( ! isInstanceOfExecSqlCall && ! isInstanceOfExecSqlDeclareCursor) {
					final var execSqlCollector = new ExecSqlCollector();
					execSqlCollector.handleExecSql(sqlNode);
					collectSqlTableDependencies(builder, rootModule, execSqlCollector, sqlNode);
				} else if (isInstanceOfExecSqlCall) {
					collectSqlStoredProcedures(cAntlrParseResult, (ExecSqlCall<?>) sqlNode, rootModule, builder);
				}
			} catch (final Exception e) {
				rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, e.getMessage());
			}
		});
	}

	private void collectSqlTableDependencies(final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule, final ExecSqlCollector execSqlCollector,
			final ExecNode<?> sqlNode) {
		final Set<String> sqlTables = new HashSet<>();
		try {
			execSqlCollector.getReferencedTables().entrySet().stream()
			.filter(entry -> ! sqlTables.contains(entry.getKey()))
			.forEach(referencedTables -> {
				if (sqlNode instanceof ExecSqlDeclareTempTable) {
					final var moduleBuilder = builder.declareExternalModule(referencedTables.getKey(), ModuleType.SQL_TEMPORARY_TABLE);
					rootModule.declareDependency(RelationshipType.ACCESSES, moduleBuilder)
					.addAttribute(ModelAttributeKey.DB_ACCESS_TYPE, getDbAccess(referencedTables.getValue()))
					.setBinding(Binding.LATE);
				} else if (sqlNode instanceof ExecSqlDeclareSchema) {
					final var moduleBuilder = builder.declareExternalModule(referencedTables.getKey(), ModuleType.RDB_DATABASE);
					rootModule.declareDependency(RelationshipType.REFERENCES, moduleBuilder)
					.addAttribute(ModelAttributeKey.DB_ACCESS_TYPE, getDbAccess(referencedTables.getValue()))
					.setBinding(Binding.LATE);
				} else {
					DawnMetricsUtility.declareDependencyToSqlTableWithSchema(rootModule, referencedTables.getKey())
							.addAttribute(ModelAttributeKey.DB_ACCESS_TYPE, getDbAccess(referencedTables.getValue()));
				}
				sqlTables.add(referencedTables.getKey());
			});
		} catch (final Exception e) {
			rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, e.getMessage());
		}
	}


	private void collectSqlStoredProcedures(final CAntlrParseResult cAntlrParseResult, final ExecSqlCall<?> execSqlCall, final ModuleBuilder rootModule,
			final DiscoveryBuilderFromSource builder) {
		final var procedureNameReference = execSqlCall.getProcedureName();
		final Set<String> storedProcedures = new HashSet<>();
		if (procedureNameReference instanceof ReferenceNode) {
			final var referenceNode = (ReferenceNode) procedureNameReference;
			final var moduleBuilder = builder.declareExternalModule(referenceNode.getContext().getText(), ModuleType.SQL_STORED_PROCEDURE);
			rootModule.declareDependency(RelationshipType.CALLS, moduleBuilder).setBinding(Binding.LATE);
		} else if (procedureNameReference instanceof HostVariableNode) {
			final var hostVariableNode = (HostVariableNode) procedureNameReference;
			final var hostVariable = hostVariableNode.getContext().getText();
			cAntlrParseResult.getAssignmentValues().entries().stream()
			.filter(entry -> entry.getKey().equals(hostVariable))
			.map(Map.Entry::getValue)
			.filter(variableValue -> ! storedProcedures.contains(variableValue))
			.forEach(variableValue -> {
				final var moduleBuilder = builder.declareExternalModule(variableValue, ModuleType.SQL_STORED_PROCEDURE);
				rootModule.declareDependency(RelationshipType.CALLS, moduleBuilder).setBinding(Binding.LATE);
				storedProcedures.add(variableValue);
			});
		}
	}

	private void collectResourceFileDependencies(final DiscoveryBuilderFromSource builder, final AstNode root, final ModuleBuilder rootModule) {
		root.getChildrenDeep(CFileAccessStatement.class)
		.stream()
		.collect(Collectors.groupingBy(statement -> statement,
				Collectors.mapping(statement -> statement.getAccessType() == FileAccessType.READ ? ModelAttributeValue.FileAccess.READ :
					ModelAttributeValue.FileAccess.WRITE,
					Collectors.toSet())))
		.forEach((file, accessTypes) -> {
			final var moduleBuilder = builder.declareExternalModule(file.getFileName(), ModuleType.RESOURCE_FILE);
			rootModule.declareDependency(RelationshipType.ACCESSES, moduleBuilder)
			.setBinding(Binding.EARLY)
			.setLocation(new ModuleLocation(file.getStartOffset(), file.getLength()))
			.addAttribute(ModelAttributeKey.FILE_ACCESS_TYPE, accessTypes);
		});
	}

	private void collectDependencies(final CAntlrParseResult cAntlrParseResult, final DiscoveryBuilderFromSource builder,
			final ModuleBuilder rootModule, final String path) {
		final var headerDependencies = cAntlrParseResult.getHeaderDependencies();
		final var functionDependencies = cAntlrParseResult.getFunctionDependencies();
		final var otherDependencies = cAntlrParseResult.getUnvalidatedDependencies();

		/* Declare the dependencies to header modules and others modules like COBOL */
		Streams.concat(otherDependencies.stream(), headerDependencies.stream())
				.forEach(dependency -> {
					final var dependencyBuilder = rootModule.declareDependency(dependency.getRelationshipType(),
							dependency.getModuleFilters().iterator().next(), ResolutionFlag.RESOLVE_CASE_INSENSITIVE);
					dependencyBuilder.setBinding(dependency.getBindingType());
					dependency.getLocation().ifPresent(dependencyBuilder::setLocation);
				});

		final var headerFunctionNames = headerDependencies.stream().map(dep -> getNameAndType(dep).e1).collect(Collectors.toSet());
		functionDependencies.forEach(dependency -> addImplementingCFunctionDependency(builder, dependency, headerFunctionNames));

		/* Add dependencies to the main function  */
		final Optional<AstNode> rootNodeOptional = cAntlrParseResult.getAstModel().getRoot();
		rootNodeOptional.ifPresent(rootNode -> {
			final Optional<CFunctionDefinition> mainFunction = rootNode.getChildrenDeep(CFunctionDefinition.class).stream()
					.filter(CFunctionDefinition::isMain)
					.findAny();
			mainFunction.ifPresent(main -> main.getChildrenDeep(CFunctionReference.class)
					.forEach(cFunctionReference -> addDependenciesToMainFunction(builder, cFunctionReference, main, headerFunctionNames, path)));
		});
	}

	private void addDependenciesToMainFunction(final DiscoveryBuilder builder, final CFunctionReference cFunctionReference,
			final CFunctionDefinition mainFunction, final Set<String> headerFunctionNames, final String path) {
		final CallExternalStatement<?> callExternalStatement = cFunctionReference.getGenericType(CallExternalStatement.class);
		if (callExternalStatement == null) {
			return;
		}
		/* Main Function anchored moduleBuilder */
		final var anchoredMainFunction = builder.anchorTo(new ModuleFilter().setNames(mainFunction.getFunctionName())
				.setTypes(ModuleType.C_FUNCTION).setContainedIn(new ModuleFilter().setPaths(path)), ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY);

		final var functionInsideHeaderFilter = new ModuleFilter().setNames(callExternalStatement.getModuleName())
				.setTypes(ModuleType.C_FUNCTION).setContainedIn(new ModuleFilter().setTypes(ModuleType.C_HEADER).setNames(headerFunctionNames));
		final var globalFunctionFilter = new ModuleFilter().setNames(callExternalStatement.getModuleName())
				.setTypes(ModuleType.C_FUNCTION);

		anchoredMainFunction.declareDependency(RelationshipType.CALLS, List.of(functionInsideHeaderFilter, globalFunctionFilter))
				.setBinding(Binding.LATE);
	}

	/**
	 * This method creates a dependency from the C_FUNCTION module to the FUNCTIONS with the same in contained a C_HEADER module.
	 * <br> <b>For example</b>: <br>
	 *  TestProgram.c (C_PROGRAM) program file ---> testA() (C_FUNCTION) implementation <br>
	 *  TestHeader.h (C_HEADER) header file ---> testA() (C_FUNCTION) function declaration <br>
	 * We are declaring dependency from function module in C program to the function module in C header.<br>
	 * testA() of TestProgram.c ---<i>IMPLEMENTS</i>----> testA() of TestHeader.h
	 *
	 * @param builder the discovery builder
	 * @param dependency the dependency definition
	 * @param functionDeclarationsInHeader the set of function declarations in the header
	 */

	private static void addImplementingCFunctionDependency(final DiscoveryBuilderFromSource builder, final DependencyDefinitionPojo dependency,
			final Set<String> functionDeclarationsInHeader) {
		final var nameAndType = getNameAndType(dependency);
		final var functionModule = builder.declareSubModule(nameAndType.e1, ModuleType.C_FUNCTION);
		if (functionDeclarationsInHeader.isEmpty()) {
			return;
		}
		final var headerDependencyFilter = new ModuleFilter().setNames(nameAndType.e1).setTypes(nameAndType.e2).setContainedIn(new ModuleFilter()
				.setTypes(ModuleType.C_HEADER).setNames(functionDeclarationsInHeader).setPhysical(true));
		final var headerFunctionToProgramFunctionDependency = functionModule.declareDependency(RelationshipType.CALLS,
						headerDependencyFilter, ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL).setBinding(Binding.EARLY);

		final ModelAttributeMap<Object> attributes = new ModelAttributeMap<>();
		attributes.put(TYPE_REFERENCE_TYPE, IMPLEMENT);
		headerFunctionToProgramFunctionDependency.setAttributes(attributes);
		dependency.getLocation().ifPresent(headerFunctionToProgramFunctionDependency::setLocation);
	}

	/**
	 * Deferred action method to collect transitive metrics for C.
	 * @param builder the discovery builder
	 * @param context the discovery context
	 * @param module the module
	 */
	@DeferredAction
	public void collectTransitiveMetrics(final DiscoveryBuilder builder, final DiscoveryContext context, final ModuleLightweightPojo module) {
		new CTransitiveMetricsContributor(context, builder, module.identity(), moduleService).calculateTransitiveMetrics();
	}

	private static Tuple2<String, ModuleType> getNameAndType(final DependencyDefinitionPojo dependency) {
		final var moduleFilter = dependency.getModuleFilters().iterator().next();
		final var name = moduleFilter.getNames().iterator().next();
		final var type = moduleFilter.getTypes().iterator().next();
		return new Tuple2<>(name,type);
	}

}

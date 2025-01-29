/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.contributors.cobol;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import innowake.mining.server.discovery.dawn.metrics.contributors.AstNodeLocationProvider;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.model.discovery.ModelStatement;
import innowake.mining.extensions.export.callchain.CallChainService;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DeferredAction;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.contributors.AstInputProvider;
import innowake.mining.server.discovery.dawn.metrics.contributors.DawnExecStatementUtility;
import innowake.mining.server.discovery.dawn.metrics.contributors.GenericMetricsUtil;
import innowake.mining.server.discovery.dawn.metrics.contributors.InputProvider;
import innowake.mining.server.discovery.dawn.metrics.contributors.resolver.cobol.DefaultCobolReferenceResolver;
import innowake.mining.server.discovery.metrics.DawnOriginResolver;
import innowake.mining.server.discovery.metrics.cobol.CobolImsCollector;
import innowake.mining.server.discovery.metrics.cobol.CobolSourceObjectManager;
import innowake.mining.server.discovery.metrics.cobol.dependency.CobolDependencyNodeCollector;
import innowake.mining.server.discovery.metrics.cobol.reference.CobolLabelReferenceCollector;
import innowake.mining.server.discovery.metrics.exec.ExecSqlCollector;
import innowake.mining.server.discovery.metrics.generic.GenericMetricsContributor;
import innowake.mining.server.discovery.metrics.generic.MetricFactory;
import innowake.mining.server.discovery.metrics.generic.MetricType;
import innowake.mining.server.discovery.metrics.ims.DawnImsTransitiveMetricsCollector;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.server.discovery.parser.cobol.CobolParseResultProvider;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.StatementType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.cobol.parser.CobolToken;
import innowake.ndt.cobol.parser.ast.model.CobolModel;
import innowake.ndt.cobol.parser.ast.model.CobolNode;
import innowake.ndt.core.assembling.IAssembling;
import innowake.ndt.core.parsing.ast.model.exec.ExecNode;
import innowake.ndt.core.parsing.ast.model.exec.ExecSqlNode;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlCall;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDeclareSchema;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDeclareTempTable;
import innowake.ndt.core.parsing.cobol.CobolLexerFactory;

/**
* Contributor for Cobol files.
*/
@Component
public class CobolContributor implements DiscoveryContributorFromSource {

	private static final Logger LOG = LoggerFactory.getLogger(CobolContributor.class);

	@Autowired
	private ParserProviderService parserProviderService;

	@Autowired
	private ModuleService moduleService;

	@Autowired
	private GenericConfigProperties configProperties;
	
	@Autowired
	private SourceCachingService sourceService;
	
	@Autowired
	private CallChainService callChainService;
	
	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return sourceObject.getTechnology() == Technology.COBOL && (sourceObject.getType() == Type.PROGRAM || sourceObject.getType() == Type.COPYBOOK);
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		String name = sourceObject.getName();
		final var config = context.getConfig();
		final CobolParseResultProvider cobolParserResultProvider = parserProviderService.createCobolParser(config, context.getTimedWorker(),
				context.getSearchOrders(), context.getJobId());
		try {
			final Tuple2<CobolModel, IAssembling<SourcePojo>> parseResult = cobolParserResultProvider.getParseResult(sourceObject);
			final CobolModel model = parseResult.e1;
			final var astNodeLocationProvider = new AstNodeLocationProvider<>(parseResult.e2, sourceObject.getContent().toString());
			final var dependencyCollector = new CobolDependencyNodeCollector();
			dependencyCollector.setDebugging(model.isDebugging());
			dependencyCollector.visit(model);
			final var dawnCobolReferenceResolver = new DefaultCobolReferenceResolver(dependencyCollector.getMoveStatements(),
					dependencyCollector.getDataFields(), dependencyCollector.getSetStmts(), sourceObject, astNodeLocationProvider);

			final var identificationDivision = model.getCobolProgram().getIdentificationDivision();
			if (config.isCobolPgmIdAsName() && identificationDivision != null && identificationDivision.getProgramID() != null) {
				name = assertNotNull(identificationDivision).getProgramID();
			}
			
			final ModuleBuilder rootModule = calculateSourceMetrics(name, sourceObject, Optional.of(model), builder);
			if (sourceObject.getContent().toString().isBlank()) {
				rootModule.addError(Severity.WARNING, ErrorKey.EMPTY_FILE, "Found empty file: " + sourceObject.getName());
				return;
			}
			collectModules(dependencyCollector, builder);
			final var collectParseErrors = context.getFeatureMap().get(FeatureId.COLLECT_PARSER_ERRORS).booleanValue();
			collectErrors(rootModule, collectParseErrors, dependencyCollector, astNodeLocationProvider);
			collectStatements(rootModule, dependencyCollector.getStatements());
			collectDeadCode(rootModule, model, astNodeLocationProvider);
			
			collectBindings(context, builder, rootModule, sourceObject, parseResult.e2, dependencyCollector, dawnCobolReferenceResolver);
			if (config.isCobolImsDependencyEnabled()) {
				rootModule.deferAction("calculateTransitiveMetricsForIms");
			}
		} catch (final DiscoveryException e) {
			LOG.error(String.format("[%s] %s", name, e.getMessage()), e);
			LOG.debug(() -> ExceptionUtils.getFullStackTrace(e));
			final ModuleBuilder rootModule = calculateSourceMetrics(name, sourceObject, Optional.empty(), builder);
			rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, e.getMessage());
			/* unable to parse so nothing more to discover */
		} catch (final Exception e) {
			LOG.error("Exception occured while parsing" + sourceObject.getPath(), e);
			final ModuleBuilder rootModule = calculateSourceMetrics(name, sourceObject, Optional.empty(), builder);
			rootModule.addError(Severity.ERROR, ErrorKey.MODULE_ABORT, e.getMessage());
		} catch (final Throwable e) {
			LOG.error("Unxpected error occured while parsing" + sourceObject.getPath(), e);
			final ModuleBuilder rootModule = calculateSourceMetrics(name, sourceObject, Optional.empty(), builder);
			rootModule.addError(Severity.ERROR, ErrorKey.MODULE_ABORT, e.getMessage());
		}
	}

	private void collectModules(final CobolDependencyNodeCollector dependencyCollector, final DiscoveryBuilderFromSource builder) {
		dependencyCollector.getSqlCursors().forEach(sqlDeclareCursor -> resolveSqlStatement(sqlDeclareCursor, builder, dependencyCollector.isDebugging()));
		dependencyCollector.getSqlDeclareTables().forEach(sqlDeclareTable -> resolveSqlStatement(sqlDeclareTable, builder, dependencyCollector.isDebugging()));
		dependencyCollector.getSqlDeletes().forEach(sqlDelete -> resolveSqlStatement(sqlDelete, builder, dependencyCollector.isDebugging()));
		dependencyCollector.getSqlInserts().forEach(sqlInsert -> resolveSqlStatement(sqlInsert, builder, dependencyCollector.isDebugging()));
		dependencyCollector.getSqlLockTables().forEach(sqlLockTable -> resolveSqlStatement(sqlLockTable, builder, dependencyCollector.isDebugging()));
		dependencyCollector.getSqlSelects().forEach(sqlSelect -> resolveSqlStatement(sqlSelect, builder, dependencyCollector.isDebugging()));
		dependencyCollector.getSqlUpdates().forEach(sqlUpdate -> resolveSqlStatement(sqlUpdate, builder, dependencyCollector.isDebugging()));
	}

	private void resolveSqlStatement(final ExecNode<?> sqlNode, final DiscoveryBuilderFromSource builder, final boolean isDebugging) {
		if (sqlNode instanceof ExecSqlCall || sqlNode.getParent() instanceof ExecSqlNode) {
			return;
		}
		final var sqlCollector = new ExecSqlCollector();
		sqlCollector.setDebugging(isDebugging);
		sqlCollector.handleExecSql(sqlNode);
		sqlCollector.getReferencedTables().keySet().forEach(table -> {
			if (sqlNode instanceof ExecSqlDeclareTempTable) {
				builder.declareExternalModule(table, ModuleType.SQL_TEMPORARY_TABLE);
			} else if (sqlNode instanceof ExecSqlDeclareSchema) {
				builder.declareExternalModule(table, ModuleType.RDB_DATABASE);
			}
		});
	}

	private ModuleBuilder calculateSourceMetrics(final String name, final SourcePojo sourceObject, final Optional<CobolModel> cobolModel,
			final DiscoveryBuilderFromSource builder) {
		final ModuleBuilder rootModule = builder.declareRootModule(name.toUpperCase(),
				ModuleType.fromTechnologyAndType(sourceObject.getTechnology(), sourceObject.getType()));
		GenericMetricsContributor metricsContributor;
		if (sourceObject.getType() == Type.PROGRAM && cobolModel.isPresent()) {
			metricsContributor = new GenericMetricsContributor(new AstInputProvider(sourceObject, CobolLexerFactory.get(), cobolModel.get()));
			metricsContributor.enable(MetricFactory.get(MetricType.MCCABE_COMPLEXITY));
		} else {
			metricsContributor = new GenericMetricsContributor(new InputProvider(sourceObject, CobolLexerFactory.get()));
		}
		metricsContributor.enable(MetricFactory.get(MetricType.LOC_LOC));

		try {
			final var sourceMetrics = GenericMetricsUtil.executeAndGetResults(metricsContributor);
			/*
			 * We are adding a positive default value 0 for dead code lines and complexity. If we remove this we need to adjust the feature matrix as this
			 * technology won't support the dead code calculation and complexity, which is not correct.
			 */
			if (sourceMetrics.getDeadCodeLines() == -1) {
				sourceMetrics.setDeadCodeLines(0);
			}
			if (sourceMetrics.getComplexityMcCabe() == null || sourceMetrics.getComplexityMcCabe() == -1) {
				sourceMetrics.setComplexityMcCabe(0);
			}
			rootModule.addAdditionalInfo(sourceMetrics);
		} catch (final Exception e) {
			LOG.error(String.format("Error while calculating metrics of %s.", sourceObject.getPath()), e);
			rootModule.addError(Severity.ERROR, ErrorKey.METRICS_CALCULATION_ERROR, e.getMessage());
		}
		return rootModule;
	}

	private void collectErrors(final ModuleBuilder rootModule, final boolean collectParseErrors, final CobolDependencyNodeCollector dependencyCollector,
			final AstNodeLocationProvider<SourcePojo> astNodeLocationProvider) {
		if (collectParseErrors) {
			dependencyCollector.getCobolUnknownTokens()
					.forEach(cobolUnknownToken -> rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR,
							String.format("Unknown cobol token %s", cobolUnknownToken.getToken()),
							astNodeLocationProvider.getAstNodeLocation(cobolUnknownToken)));

			dependencyCollector.getAdaUnknownStmts()
					.forEach(adaUnknownStmts -> rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR,
							String.format("Unknown adaprep statement %s", adaUnknownStmts.getToken()),
							astNodeLocationProvider.getAstNodeLocation(adaUnknownStmts)));

			dependencyCollector.getAdaUnknownTokens()
					.forEach(adaUnknownToken -> rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR,
							String.format("Unknown adaprep token %s", adaUnknownToken.getToken()),
							astNodeLocationProvider.getAstNodeLocation(adaUnknownToken)));

			dependencyCollector.getExecUnknownTokens().forEach(execUnknownToken -> rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR,
					String.format("Unknown exec token %s", execUnknownToken.getStartToken())));

			dependencyCollector.getCobolParserErrors()
					.forEach(cobolParseErrors -> rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR,
							String.format("Cobol Parser error on token %s", cobolParseErrors.getStartToken()),
							astNodeLocationProvider.getAstNodeLocation(cobolParseErrors)));
		} else {
			final int errorCount = dependencyCollector.getCobolUnknownTokens().size() + dependencyCollector.getAdaUnknownStmts().size()
					+ dependencyCollector.getAdaUnknownTokens().size() + dependencyCollector.getExecUnknownTokens().size()
					+ dependencyCollector.getCobolParserErrors().size();
			rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, String.format("%d parse errors were found in source file.", errorCount));
		}
	}

	private void collectStatements(final ModuleBuilder rootModule, final List<ModelStatement> statements) {
		final Set<String> processedStatements = new HashSet<>();
		for (final ModelStatement statement : statements) {
			final var statementString = statement.getString();
			final var statementType = statement.getStatementType();
			if (statementType != StatementType.EXEC_SQL) {
				if (processedStatements.add(statementString)) {
					rootModule.declareStatement(statementType).setText(statementString);
				}
				if (statementType == StatementType.RDB_DATABASE) {
					DawnExecStatementUtility.addSqlStatementForNonExecSql(statementString, StatementType.DECLARE_SCHEMA, rootModule);
				} else if (statementType == StatementType.DECLARE_TEMP_TABLE) {
					DawnExecStatementUtility.addSqlStatementForNonExecSql(statementString, StatementType.DECLARE_TEMP_TABLE, rootModule);
				}
			} else {
				DawnExecStatementUtility.addSqlStatementForExecSql(statementString, rootModule, true);
			}
		}
	}

	private void collectDeadCode(final ModuleBuilder rootModule, final CobolModel model, final AstNodeLocationProvider<SourcePojo> astNodeLocationProvider) {
		CobolLabelReferenceCollector.findUnreferencedLables(model).forEach(cobolLabelStmt -> {
			final String labelName = CobolLabelReferenceCollector.buildLabelName(cobolLabelStmt);
			final CobolToken startToken = cobolLabelStmt.getStartToken();
			final int startLineCount = startToken.getLine();
			int endLineCount = startLineCount;
			if ( ! cobolLabelStmt.getChildren().isEmpty()) {
				final var lastNode = cobolLabelStmt.getChildren().get(cobolLabelStmt.getChildren().size() - 1);
				if (lastNode instanceof CobolNode) {
					endLineCount = ((CobolNode) lastNode).getEndToken().getLine();
				} else if (lastNode instanceof ExecSqlNode) {
					endLineCount = ((ExecSqlNode<?>) lastNode).getEndToken().getLine();
				} else {
					final var errorMessage = String.format("Unhandled AstNode node %s", lastNode.getAstNodeName());
					rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, errorMessage, astNodeLocationProvider.getAstNodeLocation(lastNode));
				}
			}
			rootModule.addDeadCode(labelName, startLineCount, endLineCount - startLineCount + 1);
		});
	}

	private void collectBindings(final DiscoveryContext context, final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule,
			final SourcePojo object, final IAssembling<SourcePojo> parseResult, final CobolDependencyNodeCollector dependencyCollector,
			final DefaultCobolReferenceResolver dawnCobolReferenceResolver) {
		final var sourceObjectResolver = context.getSourceObjectResolver();
		final var sourceObjectManager = new CobolSourceObjectManager(sourceObjectResolver, configProperties);
		final var dawnOriginResolver = new DawnOriginResolver(parseResult);
		new CobolDependencyAnalyzer(context, sourceObjectResolver, sourceObjectManager, dawnOriginResolver, dependencyCollector,
				dawnCobolReferenceResolver, moduleService).analyze(builder, rootModule, object);
	}
	
	/**
	 * Calculates the transitive metrics between Cobol programs/copy books, JCL jobs and involved IMS resources for second cycle.
	 *
	 * @param context the DiscoveryContext
	 * @param sourceObject the {@link SourcePojo}
	 * @param builder the {@link DiscoveryBuilder}
	 * @param rootModuleBuilder the ModuleBuilder
	 * @param module the source module
	 */
	@DeferredAction
	public void calculateTransitiveMetricsForIms(final DiscoveryContext context, final SourcePojo sourceObject, final DiscoveryBuilder builder,
			final ModuleBuilder rootModuleBuilder, final ModuleLightweightPojo module) {
		
		final CobolParseResultProvider cobolParserResultProvider = parserProviderService.createCobolParser(context.getConfig(), context.getTimedWorker(),
				context.getSearchOrders(), context.getJobId());
		try {
			final Tuple2<CobolModel, IAssembling<SourcePojo>> parseResult = cobolParserResultProvider.getParseResult(sourceObject);
			final CobolModel model = parseResult.e1;
			final var dawnOriginResolver = new DawnOriginResolver(parseResult.e2);
			final var dawnCobolImsCollector = new CobolImsCollector(sourceObject, builder, rootModuleBuilder, cobolParserResultProvider,
					model, dawnOriginResolver, moduleService, sourceService);
			new DawnImsTransitiveMetricsCollector(rootModuleBuilder, moduleService, module, dawnCobolImsCollector, context.getProjectId(),
					callChainService)
					.calculateTransitiveMetrics();
		} catch (final DiscoveryException e) {
			LOG.error(String.format("Error while parsing of %s while calculating transitive metrics for COBOL and IMS.", sourceObject.getPath()), e);
			rootModuleBuilder.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, e.getMessage());
		}	
	}
}

/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.contributors.pl1;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.extensions.export.callchain.CallChainService;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DeferredAction;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.contributors.AstInputProvider;
import innowake.mining.server.discovery.dawn.metrics.contributors.GenericMetricsUtil;
import innowake.mining.server.discovery.dawn.metrics.contributors.SafeExecUtils;
import innowake.mining.server.discovery.metrics.DawnOriginResolver;
import innowake.mining.server.discovery.metrics.generic.GenericMetricsContributor;
import innowake.mining.server.discovery.metrics.generic.MetricFactory;
import innowake.mining.server.discovery.metrics.generic.MetricType;
import innowake.mining.server.discovery.metrics.ims.DawnImsTransitiveMetricsCollector;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.server.discovery.parser.pl1.Pl1ParseResultProvider;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.SourceMetrics;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.core.parsing.ITokenPartitioning;
import innowake.ndt.core.parsing.TokenPartitioner2;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.pl1.Pl1LexerConfiguration;
import innowake.ndt.core.parsing.spi.StringContent;
import innowake.ndt.parsing.parser.dependency.ast.pl1.AbstractPl1ProcedureNode;
import innowake.ndt.parsing.parser.dependency.ast.pl1.Pl1PackageStatement;
import innowake.ndt.parsing.parser.dependency.ast.pl1.Pl1ProcedureNode;
import innowake.ndt.parsing.scanner.generic.LocHelper;
import innowake.ndt.parsing.scanner.pl1.PL1LexerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Optional;

/**
 * Contributor for Pl1 Language
 */
@Component
public class Pl1Contributor implements DiscoveryContributorFromSource {

	private static final Logger LOG = LoggerFactory.getLogger(Pl1Contributor.class);
	@Autowired
	private ParserProviderService parserProvider;
	@Autowired
	private ModuleService moduleService;
	@Autowired
	private CallChainService callChainService;
	@Autowired
	private SourceCachingService sourceService;

	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return sourceObject.getTechnology() == Technology.PL1 &&
				(sourceObject.getType() == Type.MAINPROGRAM
						|| sourceObject.getType() == Type.PROGRAM
						|| sourceObject.getType() == Type.COPYBOOK);
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		final var pl1ParserResultProvider = parserProvider.createPl1Parser(context);
		final var rootModule = builder.declareRootModule(sourceObject.getName(),
				ModuleType.fromTechnologyAndType(sourceObject.getTechnology(), sourceObject.getType()));
		final var parseResult = SafeExecUtils.safeExecute(pl1ParserResultProvider::getParseResult, sourceObject, rootModule);

		final var errorMsg = String.format("failed to calculate source metrics for the Pl1 file %s", sourceObject.getPath());
		final Optional<AstModel> programModel = parseResult.isPresent() ?
				SafeExecUtils.safeExecute(() -> parseResult.get().getProgramModel(), rootModule, ErrorKey.METRICS_CALCULATION_ERROR, Optional.of(errorMsg))
				: Optional.empty();
		final var sourceMetrics = getSourceMetrics(context, sourceObject, programModel, rootModule);
		if (sourceMetrics.getDeadCodeLines() == -1) {
			sourceMetrics.setDeadCodeLines(0);
		}
		rootModule.addAdditionalInfo(sourceMetrics);
		if (parseResult.isEmpty()) {
			/* Parsing failed, nothing to collect */
			return;
		}

		if (sourceObject.getType() != Type.MAINPROGRAM && sourceObject.getType() != Type.PROGRAM) {
			/* collect metrics only for MAIN PROGRAM AND PROGRAM */
			return;
		}
		final var model = SafeExecUtils.safeExecute( ()  -> parseResult.get().getPl1LightweightModel(), rootModule,
				ErrorKey.PARSE_ERROR, Optional.empty());
		model.ifPresent(astModel -> collectProcedures(context.getConfig(), astModel, builder, parseResult.get()));
		collectLightWeightMetrics(builder, context.getFeatureMap().get(FeatureId.PL1_EXPERIMENTAL).booleanValue(), sourceObject, parseResult.get(),
				rootModule, context.getConfig());
		/* Add a deferred action to collect transitive metrics */
		rootModule.deferAction("calculateTransitiveMetrics");
	}

	private static void collectLightWeightMetrics(final DiscoveryBuilderFromSource builder, final boolean useExperimental, final SourcePojo sourceObject,
			final Pl1ParseResultProvider.Pl1ParseResult parseResult, final ModuleBuilder rootModule, final Config config) {
		try {
			final var originResolver = new DawnOriginResolver(parseResult.getAssembling());
			/* Collect IMS UTILITY dependencies */
			final var imsCollector = new Pl1ImsCollector(rootModule, builder, parseResult.getProgramModel(), originResolver, config);
			imsCollector.collectMetrics();
			final var imsUtilities = imsCollector.getImsUtilities();
			if (useExperimental) {
				final var dependencyUtilityForExperimental = new Pl1DependencyUtility<Pl1ProcedureNode>(originResolver, builder, rootModule,
						sourceObject.getPath());
				new Pl1ExperimentalLightweightContributor(parseResult, dependencyUtilityForExperimental, config)
						.collectMetrics(imsUtilities);
			} else {
				final var dependencyUtility = new Pl1DependencyUtility<AbstractPl1ProcedureNode>(originResolver, builder, rootModule, sourceObject.getPath());
				new Pl1LightWeightContributor(parseResult, dependencyUtility, config)
						.collectMetrics(imsUtilities);
			}
		} catch (final DiscoveryException e) {
			LOG.error(String.format("Error while calculating Metrics for sourceObject: %s", sourceObject.getPath()) , e);
			rootModule.addError(Severity.ERROR, ErrorKey.METRICS_CALCULATION_ERROR, e.getMessage());
		} catch (final Throwable e) {
			LOG.error("Unexpected error occurred while parsing" + sourceObject.getPath(), e);
			rootModule.addError(Severity.ERROR, ErrorKey.MODULE_ABORT, e.getMessage());
		}
	}

	private final void collectProcedures(final Config config, final AstModel model, final DiscoveryBuilderFromSource builder,
			final Pl1ParseResultProvider.Pl1ParseResult parseResult) {
		final Optional<AstNode> root = model.getRoot();
		if (root.isPresent()) {
			/* root has either a package or one or more procedures */
			final AstNode searchStartNode = root.get()
					.getChildren(Pl1PackageStatement.class)
					.stream().findFirst()
					.map(AstNode.class::cast)
					.orElse(root.get());
			final var originResolver = new DawnOriginResolver(parseResult.getAssembling());
			searchStartNode
					.getChildren(Pl1ProcedureNode.class)
					.stream()
					.filter(procedureBlock -> ! AbstractPl1Contributor.isUtility(procedureBlock.getPrimaryEntryLabel(), config.getUtilityList()))
					.forEach(procedureBlock -> addProcedure(config, builder, procedureBlock, originResolver, parseResult));
		}
	}

	private final SourceMetrics getSourceMetrics(final DiscoveryContext context, final SourcePojo sourceObject,
			final Optional<AstModel> pl1Model, final ModuleBuilder rootModule) {
		final var pl1Partitioning = getPartitioning(context.getConfig())
				.doPartitioning(new StringContent(sourceObject.getContent().toString()));
		final var sourceMetrics = new SourceMetrics();
		sourceMetrics.setCommentLines(LocHelper.countLinesOfComments(pl1Partitioning));
		sourceMetrics.setCodeLines(LocHelper.countLinesOfCode(pl1Partitioning));
		sourceMetrics.setPhysicalLines(LocHelper.countPhysicalLines(pl1Partitioning));
		if (pl1Model.isEmpty()) {
			/* Cannot calculate complexity without the model present */
			return sourceMetrics;
		}
		if (sourceObject.getType() == Type.MAINPROGRAM || sourceObject.getType() == Type.PROGRAM) {
			/* Calculate the complexity */
			final GenericMetricsContributor metricsContributor = new GenericMetricsContributor(new AstInputProvider(sourceObject, PL1LexerFactory.get(),
					pl1Model.get()));
			metricsContributor.enable(MetricFactory.get(MetricType.MCCABE_COMPLEXITY));
			final var complexityResult = SafeExecUtils.safeExecute(GenericMetricsUtil::executeAndGetResults, metricsContributor,
					rootModule, ErrorKey.METRICS_CALCULATION_ERROR, Optional.of("Error occurred while calculating complexity"));
			final var complexity = complexityResult.map(SourceMetrics::getComplexityMcCabe)
					.orElse(-1);
			sourceMetrics.setComplexityMcCabe(complexity);
		}
		return sourceMetrics;
	}

	private void addProcedure(final Config config, final DiscoveryBuilderFromSource builder, final AbstractPl1ProcedureNode procedureBlock,
			final DawnOriginResolver originResolver, final Pl1ParseResultProvider.Pl1ParseResult parseResult) {
		final ModuleLocation moduleLocation = originResolver.resolveLocation(procedureBlock);
		final ITokenPartitioning pl1Partitioning = getPl1Partitioning(originResolver.resolveSourceObject(procedureBlock)
				.orElse(parseResult.getSourceObject()).getContent().toString(), moduleLocation, config);
		final int commentLines = LocHelper.countLinesOfComments(pl1Partitioning);
		final int codeLines = LocHelper.countLinesOfCode(pl1Partitioning);
		final var procedureType = procedureBlock.hasReturns() ? ModuleType.PL1_FUNCTION : ModuleType.PL1_SUBROUTINE;
		final var procedureModule = builder.declareSubModule(procedureBlock.getPrimaryEntryLabel(), procedureType, moduleLocation);
		final var additionalInfo = new SourceMetrics();
		additionalInfo.setCodeLines(codeLines);
		additionalInfo.setCommentLines(commentLines);
		procedureModule.addAdditionalInfo(additionalInfo);
	}

	/**
	 * Examines a target Pl1 program, and detects all file READ or WRITE accessed
	 * performed by the program. Also resolves the write accesses back to the actual
	 * RESOURCE_FILE module and adds a dependency edge between the Pl1 program and
	 * the file from Batch.
	 * Also calculates the transitive metrics between Pl1 program and IMS.
	 *
	 * @param context the DiscoveryContext object
	 * @param sourceObject the sourceObject
	 * @param builder the DiscoveryBuilder object
	 * @param moduleBuilder the ModuleBuilder object
	 * @param module the source module
	 */
	@DeferredAction
	public void calculateTransitiveMetrics(final DiscoveryContext context, final SourcePojo sourceObject, final DiscoveryBuilder builder,
			final DiscoveryBuilder.ModuleBuilder moduleBuilder, final ModuleLightweightPojo module) {

		final AstModel pl1Model;
		final AstModel programModel;
		final var pl1ParserProvider = parserProvider.createPl1Parser(context);
		final var parseResult = SafeExecUtils.safeExecute(pl1ParserProvider::getParseResult, sourceObject, moduleBuilder);
		if (parseResult.isEmpty()) {
			/* Parsing failed, nothing to collect */
			return;
		}
		try {
			pl1Model = parseResult.get().getPl1LightweightModel();
			programModel = parseResult.get().getProgramModel();
		} catch (final Exception e) {
			LOG.error("Error while parsing " + sourceObject.getPath(), e);
			return;
		}
		final var pl1DependencyUtility = new Pl1DependencyUtility<AbstractPl1ProcedureNode>(new DawnOriginResolver(parseResult.get().getAssembling()), builder,
				moduleBuilder, sourceObject.getPath());
		new Pl1ReadWriteAccessAnalyzer(pl1DependencyUtility, moduleService, callChainService).createFileDependencies(context, module, moduleBuilder, pl1Model);
		final var pl1LanguageSpecificContributor = new Pl1ImsTransitiveContributor(programModel, moduleBuilder, pl1DependencyUtility.discoveryBuilder,
				pl1DependencyUtility, module, sourceService, moduleService, pl1ParserProvider, context.getProjectId());
		new DawnImsTransitiveMetricsCollector(moduleBuilder, moduleService, module, pl1LanguageSpecificContributor, context.getProjectId(), callChainService)
				.calculateTransitiveMetrics();
	}

	private ITokenPartitioning getPl1Partitioning(final String source, final ModuleLocation moduleLocation, final Config config) {
		final int startOffset = moduleLocation.getOffset();
		final int endOffset = startOffset + moduleLocation.getLength() - 1;
		final String substring = source.substring(startOffset, endOffset);
		return getPartitioning(config).doPartitioning(new StringContent(substring));
	}

	private static TokenPartitioner2 getPartitioning(final Config configuration) {
		return TokenPartitioner2.create(PL1LexerFactory.get(new Pl1LexerConfiguration.Pl1LexerConfigurationBuilder()
				.setMargin(configuration.getPL1ParserMarginStart(), configuration.getPL1ParserMarginEnd(), configuration.getPL1ParserANS())
				.build()));
	}
}

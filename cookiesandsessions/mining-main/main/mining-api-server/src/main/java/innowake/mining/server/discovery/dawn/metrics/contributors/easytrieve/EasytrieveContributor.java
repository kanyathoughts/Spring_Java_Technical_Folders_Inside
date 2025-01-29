/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.contributors.easytrieve;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import innowake.mining.server.discovery.dawn.metrics.contributors.AstNodeLocationProvider;
import innowake.mining.shared.model.AstNodeLocation;
import innowake.ndt.core.parsing.spi.Document;
import org.antlr.v4.runtime.Token;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DeferredAction;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.DependencyBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.contributors.AstInputProvider;
import innowake.mining.server.discovery.dawn.metrics.contributors.GenericMetricsUtil;
import innowake.mining.server.discovery.dawn.metrics.contributors.InputProvider;
import innowake.mining.server.discovery.metrics.generic.GenericMetricsContributor;
import innowake.mining.server.discovery.metrics.generic.MetricException;
import innowake.mining.server.discovery.metrics.generic.MetricFactory;
import innowake.mining.server.discovery.metrics.generic.MetricType;
import innowake.mining.server.discovery.metrics.generic.input.MetricInputProvider;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.server.discovery.parser.easytrieve.EasytrieveAntlrParseResult;
import innowake.mining.server.discovery.parser.easytrieve.EasytrieveAntlrParseResultProvider;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.datapoints.LabelMappings;
import innowake.mining.shared.datapoints.LabelMappings.LabelType;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.SourceMetrics;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.antlr.base.BaseAntlrParserError;
import innowake.ndt.antlr.easytrieve.EasytrieveAntlrParser.AssembledEasytrieveProgram;
import innowake.ndt.antlr.easytrieve.tokenpartitioner.EasyTrieveTokenPartitioner;
import innowake.ndt.antlr.easytrieve.tokenpartitioner.EasyTrieveTokenPartitioningConfiguration;
import innowake.ndt.core.parsing.ILocation;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.parsing.scanner.easytrieve.EasytrieveLexerFactory;

/**
 * Contributor for Easytrieve files.
 */
@Component
public class EasytrieveContributor implements DiscoveryContributorFromSource {

	private static final Logger LOG = LoggerFactory.getLogger(EasytrieveContributor.class);

	private static final Map<LabelType, Map<String, String>> LABEL_MAPPINGS = LabelMappings.getLabelMappings();

	@Autowired
	private ParserProviderService parserProviderService;

	@Autowired
	private SourceCachingService sourceService;
	@Autowired
	private ModuleService moduleService;

	@Nullable
	private Set<SourcePojo> cachedMacroFiles = null;

	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return (sourceObject.getTechnology() == Technology.EASYTRIEVE && sourceObject.getType() == Type.PROGRAM)
				|| (sourceObject.getTechnology() == Technology.EASYTRIEVE && sourceObject.getType() == Type.MACRO_FILE);
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		final ModuleBuilder rootModule = builder.declareRootModule(sourceObject.getName(),
				ModuleType.fromTechnologyAndType(sourceObject.getTechnology(), sourceObject.getType()));

		final EasytrieveAntlrParseResultProvider parseResultProvider = parserProviderService.createEasytrieveParser(context);

		final AstModel easytrieveModel;
		try {
			final EasytrieveAntlrParseResult parseResult = parseResultProvider.getParseResult(sourceObject);
			easytrieveModel = parseResult.getAstModel();
			final Optional<AstNode> rootNodeOptional = easytrieveModel.getRoot();
			collectSourceMetrics(sourceObject, rootModule, Optional.of(easytrieveModel));
			if (rootNodeOptional.isPresent()) {
				EasytrieveSqlUtility.collectSqlResources(parseResult, rootNodeOptional.get(), builder, rootModule);
			} else {
				final String errorMessage = "Root node is not present and further processing of file" + sourceObject.getPath() + "can't continue.";
				LOG.error(() -> errorMessage);
				rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, errorMessage);
				return;
			}
		} catch (final DiscoveryException e) {
			collectSourceMetrics(sourceObject, rootModule, Optional.empty());
			LOG.error(() -> "Error while resolving " + sourceObject.getPath(), e);
			rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, "Unable to parse Easytrieve program. Cause unknown");
			return;
		} catch (final Exception e) {
			collectSourceMetrics(sourceObject, rootModule, Optional.empty());
			LOG.error("Exception occured while parsing" + sourceObject.getPath(), e);
			rootModule.addError(Severity.ERROR, ErrorKey.MODULE_ABORT, e.getMessage());
		} catch (final Throwable e) {
			LOG.error("Unxpected error occured while parsing" + sourceObject.getPath(), e);
			rootModule.addError(Severity.ERROR, ErrorKey.MODULE_ABORT, e.getMessage());
		}

		try {
			collectMetricsWithAntlrParser(builder, context, sourceObject, rootModule, parseResultProvider);
		} catch (final DiscoveryException e) {
			LOG.error(() -> "Error while collecting metrics using antlr parser", e);
			rootModule.addError(Severity.ERROR, ErrorKey.MODULE_ABORT, "Error while collecting metrics using antlr parser");
		}
	}

	private static void collectSourceMetrics(final SourcePojo sourceObject, final ModuleBuilder rootModule, final Optional<AstModel> optionalAstModel) {
		/* if macro... */
		if (sourceObject.getTechnology() == Technology.EASYTRIEVE && sourceObject.getType() == Type.MACRO_FILE) {
			/* no actual parsing for metrics is done on macros */
			/* add metrics to entry */
			final var sourceMetrics = new SourceMetrics();
			sourceMetrics.setCodeLines(-1);
			sourceMetrics.setCommentLines(-1);
			sourceMetrics.setComplexityMcCabe(-1);
			rootModule.addAdditionalInfo(sourceMetrics);
			return;
		}

		try {
			final EasyTrieveTokenPartitioningConfiguration configuration = (EasyTrieveTokenPartitioningConfiguration)
					new EasyTrieveTokenPartitioningConfiguration.EasyTrieveTokenPartitioningBuilder()
					.setDefaultStatementArea().tokenPartitioningforCommentsAndCode(Token.DEFAULT_CHANNEL, Token.MIN_USER_CHANNEL_VALUE)
					.build();
			final var easyTrieveTokenPartitioner = new EasyTrieveTokenPartitioner(configuration);
			final var iLexerFactory = EasytrieveLexerFactory.get();
			final MetricInputProvider inputProvider = optionalAstModel.isPresent()
					? new AstInputProvider(sourceObject, iLexerFactory, optionalAstModel.get(), easyTrieveTokenPartitioner)
					: new InputProvider(sourceObject, iLexerFactory, easyTrieveTokenPartitioner);
			final var metricsContributor = new GenericMetricsContributor(inputProvider);
			metricsContributor.enable(MetricFactory.get(MetricType.LOC_LOC));
			if (optionalAstModel.isPresent()) {
				metricsContributor.enable(MetricFactory.get(MetricType.MCCABE_COMPLEXITY));
			}
			rootModule.addAdditionalInfo(GenericMetricsUtil.executeAndGetResults(metricsContributor));
		} catch (final MetricException e) {
			LOG.error("Error while calculating metrics", e);
			rootModule.addError(Severity.ERROR, ErrorKey.METRICS_CALCULATION_ERROR, e.getMessage());
		}
	}

	private void collectMetricsWithAntlrParser(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject,
			final ModuleBuilder rootModule, final EasytrieveAntlrParseResultProvider antlrResultProvider) throws DiscoveryException {

		/* if macro... */
		if (sourceObject.getTechnology() == Technology.EASYTRIEVE && sourceObject.getType() == Type.MACRO_FILE) {
			/* no actual parsing for metrics is done on macros */
			return;
		}

		if (CollectionUtils.isEmpty(cachedMacroFiles)) {
			/* store all macro files from DB (need this for assembling ezt files */
			cachedMacroFiles = new HashSet<>(sourceService.find(q -> q.ofProject(context.getProjectId())
																		.withTechnology(Technology.EASYTRIEVE)
																		.withType(Type.MACRO_FILE)));
		}

		/* assemble ezt file */
		final AssembledEasytrieveProgram assembledProgram = antlrResultProvider.assembleFile(sourceObject, assertNotNull(cachedMacroFiles));

		/* parse assembled ezt file for metrics */
		final EasytrieveAntlrParseResult metrics = antlrResultProvider.parseText(assembledProgram.getAssembledText());

		resolveUnvalidatedDependencies(rootModule, metrics);

		EasytrieveSqlUtility.collectSqlDependencies(rootModule, metrics);

		/* add macro calls as well (have to do this separate because we remove them when we assemble) */
		resolveDependenciesForMacros(builder, sourceObject, rootModule, assembledProgram, metrics);

		collectErrors(context.getFeatureMap(), rootModule, assembledProgram, metrics, sourceObject.getContent().toString());
	}

	private void collectErrors(final Map<FeatureId, Boolean> featureMap, final ModuleBuilder rootModule, final AssembledEasytrieveProgram assembledProgram,
			final EasytrieveAntlrParseResult metrics, final String sourceContent) {
		metrics.getModelErrors().forEach(error -> {
			var moduleLocation = error.getModuleLocation();
			moduleLocation = moduleLocation == null ? new AstNodeLocation() : moduleLocation;
			rootModule.addError(error.getSeverity(), error.getKey(), error.getCause(), moduleLocation);
		});

		if (featureMap.get(FeatureId.COLLECT_PARSER_ERRORS).booleanValue()) {
			final var document = new Document(sourceContent);
			metrics.getErrors().forEach(error -> rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, error.toString(), setModuleLoc(error, document)));
			assembledProgram.getErrorsDuringAssembly().forEach(error -> rootModule.addError(Severity.WARNING, ErrorKey.PARSE_ERROR, error.toString()));
		} else {
			final int cnt = metrics.getErrors().size() + assembledProgram.getErrorsDuringAssembly().size();
			if (cnt > 0) {
				rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, String.format("%d parse errors were found in source file.", Integer.valueOf(cnt)));
			}
		}
	}

	private void resolveDependenciesForMacros(final DiscoveryBuilderFromSource builder, final SourcePojo sourceObject, final ModuleBuilder rootModule,
			final AssembledEasytrieveProgram assembledProgram, final EasytrieveAntlrParseResult metrics) {
		assembledProgram.getMacroCalls().forEach(macro -> {
			final var moduleFilter = new ModuleFilter().setNames(macro.macroName).setTypes(ModuleType.EASYTRIEVE_MACRO_FILE, ModuleType.COBOL_COPYBOOK);
			final DependencyBuilder dependencyForMacro = rootModule.declareDependency(RelationshipType.CALLS, moduleFilter).setBinding(Binding.EARLY);

			final var moduleFilterForEasytrieveProgram = new ModuleFilter().setNames(sourceObject.getName()).setPaths(sourceObject.getPath())
					.setTypes(ModuleType.EASYTRIEVE_PROGRAM);
			builder.anchorTo(moduleFilterForEasytrieveProgram).deferAction("addErrorForEasytrieveProgram");

			if ( ! metrics.getInlineMacroNames().contains(macro.macroName)) {
				dependencyForMacro.createIfMissing(macro.macroName, ModuleType.EASYTRIEVE_MACRO_FILE, Identification.MISSING);
			}
		});
	}

	@DeferredAction
	public void addErrorForEasytrieveProgram(final DiscoveryContext context, final DiscoveryBuilder.ModuleBuilder moduleBuilder, final ModuleLightweightPojo module) {
		final List<ModuleLightweightPojo> targetModules = moduleService.findModulesLightweight(q -> q.ofProject(context.getProjectId())
																									.withTechnology(Technology.COBOL)
																									.withType(Type.COPYBOOK)
																									.withSourceRelationshipsFrom(module.identity(), RelationshipType.CALLS));
		targetModules.forEach(targetModule -> {
			final var targetType = String.format("%s %s", LABEL_MAPPINGS.get(LabelMappings.LabelType.TECHNOLOGY_LABELS).get(Technology.COBOL.toString()),
					LABEL_MAPPINGS.get(LabelMappings.LabelType.TYPE_LABELS).get(Type.COPYBOOK.toString()));
			final var cause = String.format(
					"Cannot find Easytrieve Macro File with the name %s but found %s with the same name. " + "EASYTRIEVE Parser cannot translate the %s",
					targetModule.getName(), targetType, targetType);
			moduleBuilder.addError(Severity.WARNING, ErrorKey.DEPENDENCY_RESOLUTION_ERROR, cause);
		});
	}

	private void resolveUnvalidatedDependencies(final ModuleBuilder rootModule, final EasytrieveAntlrParseResult metrics) {
		metrics.getUnvalidatedDependencies().forEach(dependency -> {
			rootModule.declareDependency(dependency.getRelationshipType(), dependency.getModuleFilters().iterator().next())
				.setBinding(dependency.getBindingType());
		});
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
}

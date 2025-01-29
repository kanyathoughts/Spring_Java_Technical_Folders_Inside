/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.natural;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.discovery.metrics.IModuleRepository;
import innowake.mining.data.discovery.metrics.MetricsContributor;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.data.model.discovery.ErrorMarker;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.data.model.discovery.ModelDependency;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.server.discovery.metrics.generic.GenericMetricsContributor;
import innowake.mining.server.discovery.metrics.generic.MetricException;
import innowake.mining.server.discovery.metrics.generic.MetricFactory;
import innowake.mining.server.discovery.metrics.generic.MetricResult;
import innowake.mining.server.discovery.metrics.generic.MetricType;
import innowake.mining.server.discovery.metrics.generic.complexity.McCabeComplexityResult;
import innowake.mining.server.discovery.metrics.generic.input.AstModelProvider;
import innowake.mining.server.discovery.metrics.generic.input.TokenProvider;
import innowake.mining.server.discovery.parser.batch.JclParseResultProvider;
import innowake.mining.server.discovery.parser.natural.NaturalParseResultProvider;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.SourceMetrics;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.core.parsing.ITokenPartitioning;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.natural.INaturalParseResult;
import innowake.ndt.core.parsing.natural.NaturalLightweightParsing;
import innowake.ndt.naturalparser.INaturalModel;
import innowake.ndt.naturalparser.model.NaturalAstModel;

/**
 * Contributes Natural specific information.
 */
public class NaturalContributor implements MetricsContributor, AstModelProvider, TokenProvider {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.METRICS_COLLECTOR);

	private final IModuleRepository repo;
	private final JclParseResultProvider jclParseResultProvider;

	private final EntityId projectId;
	private final NaturalParseResultProvider naturalParserResultProvider;
	private final SourceCachingService sourceService;
	private final SourceObjectResolver sourceObjectResolver;
	private final NaturalSourceObjectManager sourceObjectManager;
	private final Map<FeatureId, Boolean> featureMap;
	private final Config config;

	public NaturalContributor(
			final IModuleRepository repo,
			final JclParseResultProvider jclParseResultProvider,
			final NaturalParseResultProvider naturalParserResultProvider,
			final EntityId projectId,
			final SourceCachingService sourceService,
			final SourceObjectResolver sourceObjectResolver,
			final NaturalSourceObjectManager sourceObjectManager,
			final Map<FeatureId, Boolean> featureMap,
			final Config config) {
		this.repo = repo;
		this.jclParseResultProvider = jclParseResultProvider;
		this.naturalParserResultProvider = naturalParserResultProvider;
		this.projectId = projectId;
		this.sourceService = sourceService;
		this.sourceObjectResolver = sourceObjectResolver;
		this.sourceObjectManager = sourceObjectManager;
		this.featureMap = featureMap;
		this.config = config;
	}

	/**
	 * Returns if this collector can process the given {@link ModelArtifact} in the given {@link Phase}.
	 *
	 * @param artifact the {@link ModelArtifact}
	 * @param phase the {@link Phase}
	 * @return {@code true} if this collector can process the given {@link ModelArtifact}; {@code false} otherwise
	 */
	public static boolean accept(final ModelArtifact artifact, final Phase phase) {
		/* !!!
		 * Dependent metrics are collected only if accept returns true for GENERIC_METRICS.
		 * Transitive metrics are collected only if accept returns true for GENERIC_METRICS and DEPENDENT_METRICS. */
		switch (phase) {
			case GENERIC_METRICS:
			case DEPENDENT_METRICS:
				return artifact.getType().getLanguage() == ResolveTarget.NATURAL &&
						artifact.getRepresentation() == Representation.PHYSICAL &&
						artifact.getType() != ResolveTarget.NATURAL_TEXT;
			default:
				return false;
		}
	}

	@Override
	public void calculateGenericMetrics(final ModelArtifact artifact) throws DiscoveryException {
		if (artifact.getRepresentation() != Representation.VIRTUAL && artifact.getType() != ResolveTarget.NATURAL_TEXT) {
			/* Set new instance of SourceMetrics to avoid lazy loading in LazyModelArtifact */
			artifact.setSourceMetrics(new SourceMetrics());
			/* Calculating the LOC metrics (Lines of code, Lines of comment, Physical lines of code) */
			collectLocMetrics(artifact);

			final SourcePojo sourceObject = getSourceObject(artifact);
			final ModelArtifact calculatedArtifact;
			if (supportsHeavyWeightParsing(artifact.getType())) {
				final INaturalModel model = naturalParserResultProvider.getParseResult(sourceObject).getHeavyweightModel();
				calculatedArtifact = new NaturalMetricProcessor().calculate(model, artifact);
				calculatedArtifact.setComplexity(calculateGenericCodeComplexity(calculatedArtifact));
			}
		}
	}

	@Override
	public void calculateDependentMetrics(final ModelArtifact artifact) throws DiscoveryException {
		if (artifact.getRepresentation() != Representation.VIRTUAL && artifact.getType() != ResolveTarget.NATURAL_TEXT) {
			final SourcePojo sourceObject = getSourceObject(artifact);
			final NaturalDependencyCollector collector = new NaturalDependencyCollector(repo, jclParseResultProvider, artifact, sourceObject,
					sourceObjectResolver, sourceObjectManager, naturalParserResultProvider, config);

			final List<ModelDependency> existingDependencies = artifact.getDependencies().collect(Collectors.toList());
			addDependenciesIfMissing(artifact, existingDependencies, collector.collectAvailableBindings());
			addDependenciesIfMissing(artifact, existingDependencies, collector.collectUnavailableBindings());

			try {
				addDependenciesIfMissing(artifact, existingDependencies, collector.collectAdditionalBindings());
			} catch (final Exception e) {
				LOG.error(() -> String.format("Error while parsing dependencies for object %s", artifact.getName()), e);
			}
			collector.collectDatabaseBindings();
			collector.collectWorkfileBindings();

			final NaturalProgrammingMode programmingMode = sourceObjectManager.getProgrammingMode(getSourceObject(artifact));
			if (programmingMode != NaturalProgrammingMode.REPORTING && supportsHeavyWeightParsing(artifact.getType())) {
				final INaturalModel model = naturalParserResultProvider.getParseResult(sourceObject).getHeavyweightModel();
				if (featureMap.get(FeatureId.COLLECT_PARSER_ERRORS).booleanValue()) {
					artifact.addErrors(NaturalErrorCollector.collectErrors(model));
				} else {
					NaturalErrorCollector.getParserResult(model).ifPresent(artifact::addError);
				}
			 }

		}
	}

	@Override
	public AstModel getAstModel(final ModelArtifact artifact) {
		try {
			final SourcePojo sourceObject = getSourceObject(artifact);
			final INaturalModel iNaturalModel = naturalParserResultProvider.getParseResult(sourceObject).getHeavyweightModel();
			return NaturalAstModel.from(iNaturalModel);
		} catch (final DiscoveryException e) {
			throw new IllegalStateException(e.getMessage(), e);
		}
	}

	private static void addDependenciesIfMissing(final ModelArtifact entry, final List<ModelDependency> existingDependencies,
			final List<ModelDependency> newDependencies) {
		newDependencies.stream()
						.filter(dep -> ! existingDependencies.contains(dep))
						.forEach(dependency -> {
							entry.addDependency(dependency);
							existingDependencies.add(dependency);
						});
	}

	private SourcePojo getSourceObject(final ModelArtifact entry) throws DiscoveryException {
		final String path = entry.getPath().orElseThrow(() -> new DiscoveryException("Entry file is not present: " + entry.getName()));
		try {
			return sourceService.cachingByProjectPath(projectId.getNid(), path);
		} catch (final MiningEntityNotFoundException e) {
			throw new DiscoveryException("Source object is unresolvable: " + path, e);
		}
	}

	/**
	 * This will calculate the complexity base on generic calculation logic
	 *
	 * @param artifact
	 * @return complexity
	 */
	private int calculateGenericCodeComplexity(final ModelArtifact artifact) {
		final GenericMetricsContributor metricsContributor = new GenericMetricsContributor(this);
		metricsContributor.enable(MetricFactory.get(MetricType.MCCABE_COMPLEXITY));
		try {
			final Map<MetricType<?>, MetricResult<?>> result = metricsContributor.execute(artifact);
			final McCabeComplexityResult ccResult = (McCabeComplexityResult) result.get(MetricType.MCCABE_COMPLEXITY);
			return ccResult.getComplexity();
		} catch (final Exception e) {
			LOG.error(() -> String.format("Error occured while calculating complexity for the source entry: %s ",
					artifact.getName()), e);
			final ErrorMarker modelError = new ErrorMarker().setKey(ErrorKey.METRICS_CALCULATION_ERROR)
															.setErrorSeverity()
															.setCause("Error occured while calculating complexity")
															.validate();
			artifact.addError(modelError);
			return -1;
		}
	}

	private boolean supportsHeavyWeightParsing(final ResolveTarget type) {
		return type == ResolveTarget.NATURAL_PROGRAM
				|| type == ResolveTarget.NATURAL_SUBROUTINE
				|| type == ResolveTarget.NATURAL_SUBPROGRAM
				|| type == ResolveTarget.NATURAL_FUNCTION
				|| type == ResolveTarget.NATURAL_HELP
				|| type == ResolveTarget.NATURAL_MAP;

	}
	
	private void collectLocMetrics(final ModelArtifact artifact) {
		final GenericMetricsContributor metricsContributor = new GenericMetricsContributor(this);
		metricsContributor.enable(MetricFactory.get(MetricType.LOC_LOC));
		try {
			metricsContributor.executeAndApplyResults(artifact);
		} catch (final MetricException e) {
			LOG.error(String.format("Error while calculating LOC metrics for %s", artifact.getName()), e);
			artifact.addError(new ErrorMarker(Severity.ERROR, ErrorKey.METRICS_CALCULATION_ERROR, e.getMessage(), null));
		}
	}

	@Override
	public ITokenPartitioning getTokens(final ModelArtifact artifact) {
		try {
			final SourcePojo sourceObject = getSourceObject(artifact);
			final INaturalParseResult parseResult = NaturalLightweightParsing.computeNaturalTokens(sourceObject.getContent().toString());
			return parseResult.getTokenPartitioning();
		} catch (final DiscoveryException e) {
			LOG.error(String.format("Error while getting the token partitioning for the given content of %s", artifact.getName()));
			throw new IllegalStateException(e);
		}
	}
}

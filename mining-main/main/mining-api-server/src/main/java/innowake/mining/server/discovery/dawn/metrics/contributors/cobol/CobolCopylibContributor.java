/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.cobol;

import org.apache.commons.lang.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.contributors.GenericMetricsUtil;
import innowake.mining.server.discovery.dawn.metrics.contributors.InputProvider;
import innowake.mining.server.discovery.metrics.generic.GenericMetricsContributor;
import innowake.mining.server.discovery.metrics.generic.MetricException;
import innowake.mining.server.discovery.metrics.generic.MetricFactory;
import innowake.mining.server.discovery.metrics.generic.MetricType;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.SourceMetrics;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.ndt.core.assembling.cobol.UnisysCobolCopyLibParser;
import innowake.ndt.core.parsing.cobol.CobolLexerFactory;

/**
 * Contributor for COBOL COPYLIB files.
 */
@Component
public class CobolCopylibContributor implements DiscoveryContributorFromSource {

	private static final Logger LOG = LoggerFactory.getLogger(CobolCopylibContributor.class);
	
	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return  sourceObject.getTechnology() == Technology.COBOL && sourceObject.getType() == Type.COPYLIB;
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		try {
			final ModuleBuilder rootModule = builder.declareRootModule(sourceObject.getName().toUpperCase(), ModuleType.COBOL_COPYLIB);
			calculateSourceMetrics(sourceObject, rootModule);
			if (sourceObject.getContent().toString().isBlank()) {
				rootModule.addError(Severity.WARNING, ErrorKey.EMPTY_FILE, "Found empty file: " + sourceObject.getName());
				return;
			}
			collectSubModules(sourceObject, builder);
		} catch (final Exception e) {
			LOG.error(() -> String.format("[%s] %s", sourceObject.getName(), e.getMessage()), e);
			LOG.debug(() -> ExceptionUtils.getFullStackTrace(e));
		}
	}

	private void collectSubModules(final SourcePojo sourceObject, final DiscoveryBuilderFromSource builder) {
		UnisysCobolCopyLibParser.parse(sourceObject.getContent().toString()).values().forEach(copyProc -> builder.declareSubModule(copyProc.getName(),
				ModuleType.COBOL_COPYPROC, new ModuleLocation(copyProc.getOffset(), copyProc.getLength())));
	}
	
	private void calculateSourceMetrics(final SourcePojo sourceObject, final ModuleBuilder rootModule) {
		final var metricsContributor = new GenericMetricsContributor(new InputProvider(sourceObject, CobolLexerFactory.get()));
		metricsContributor.enable(MetricFactory.get(MetricType.LOC_LOC));
		
		try {
			final SourceMetrics sourceMetrics = GenericMetricsUtil.executeAndGetResults(metricsContributor);
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
		} catch (final MetricException e) {
			LOG.error(String.format("Error while calculating metrics of %s.", sourceObject.getPath()), e);
			rootModule.addError(Severity.ERROR, ErrorKey.METRICS_CALCULATION_ERROR, e.getMessage());
		}
	}

}

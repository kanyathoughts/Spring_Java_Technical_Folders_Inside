/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.batch;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerCancellationException;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.contributors.GenericMetricsUtil;
import innowake.mining.server.discovery.dawn.metrics.contributors.InputProvider;
import innowake.mining.server.discovery.metrics.generic.GenericMetricsContributor;
import innowake.mining.server.discovery.metrics.generic.MetricException;
import innowake.mining.server.discovery.metrics.generic.MetricFactory;
import innowake.mining.server.discovery.metrics.generic.MetricType;
import innowake.mining.server.discovery.metrics.generic.input.InputType;
import innowake.mining.server.discovery.metrics.generic.loc.CustomRegionsLocMetric;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.server.discovery.parser.batch.DiscoveryJclContentProvider;
import innowake.mining.server.discovery.parser.batch.JclParseResultProvider;
import innowake.mining.server.discovery.parser.batch.JclParseResultProvider.JclParseResult;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.SourceMetrics;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.core.parsing.jcl.JclLexerFactory;
import innowake.ndt.core.parsing.jcl.JclRegionCategory;

/**
 * Default JCL contributor.
 */
@Component
public abstract class AbstractJclContributor implements DiscoveryContributorFromSource {
	private static final Logger LOG = LoggerFactory.getLogger(AbstractJclContributor.class);
	
	@Autowired
	protected transient ParserProviderService parserProviderService;
	
	protected void calculateSourceMetrics(final ModuleBuilder rootModule, final SourcePojo sourceObject, final Integer complexity) {
		try {
			rootModule.addAdditionalInfo(GenericMetricsUtil.executeAndGetResults(getGenericMetricsContributor(sourceObject), complexity));
		} catch (final MetricException e) {
			LOG.error("Error while calculating metrics: ", e);
			rootModule.addError(Severity.ERROR, ErrorKey.METRICS_CALCULATION_ERROR, e.getMessage());
		}
	}

	private GenericMetricsContributor getGenericMetricsContributor(final SourcePojo sourceObject) {
		final var metricsContributor = new GenericMetricsContributor(new InputProvider(sourceObject, JclLexerFactory.get()));
		metricsContributor.enable(MetricFactory.get(MetricType.LOC_LOC));
		metricsContributor.enable(new CustomRegionsLocMetric(InputType.TOKEN, JclRegionCategory.CATEGORY_COMMENT, JclRegionCategory.CATEGORY_STANDARD,
				JclRegionCategory.CATEGORY_JECL, JclRegionCategory.CATEGORY_STREAM));
		return metricsContributor;
	}
	
	protected JclParseResult getParseResult(final SourcePojo sourceObject, final DiscoveryContext context,
			final DiscoveryJclContentProvider jclContentProvider) throws WorkerCancellationException, DiscoveryException {
		final JclParseResultProvider jclParseProvider = parserProviderService.createJclParser(context, jclContentProvider);
		return jclParseProvider.getParseResult(sourceObject);
	}
	
	protected SourceMetrics calculateSourceMetrics(final ModuleBuilder rootModule, final SourcePojo sourceObject) {
		try {
			return GenericMetricsUtil.executeAndGetResults(getGenericMetricsContributor(sourceObject));
		} catch (final MetricException e) {
			LOG.error("Error while calculating metrics: ", e);
			rootModule.addError(Severity.ERROR, ErrorKey.METRICS_CALCULATION_ERROR, e.getMessage());
		}
		return new SourceMetrics();
	}
}

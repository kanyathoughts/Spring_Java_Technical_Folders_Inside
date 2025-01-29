/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.contributors.vb;

import org.springframework.stereotype.Component;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
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
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.parsing.scanner.vb6.Vb6LexerFactory;

/**
 * Dawn contributor for Vb6 files
 */
@Component
public class VbContributor implements DiscoveryContributorFromSource {
	
	private static final Logger LOG = LoggerFactory.getLogger(VbContributor.class);
	
	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return (sourceObject.getTechnology() == Technology.VB && sourceObject.getType() == Type.MODULE)
				|| (sourceObject.getTechnology() == Technology.VB && sourceObject.getType() == Type.CLASS)
				|| (sourceObject.getTechnology() == Technology.VB && sourceObject.getType() == Type.FORM)
				|| (sourceObject.getTechnology() == Technology.VB && sourceObject.getType() == Type.USER_CONTROL)
				|| (sourceObject.getTechnology() == Technology.VB && sourceObject.getType() == Type.DESIGNER_FILE)
				|| (sourceObject.getTechnology() == Technology.VB && sourceObject.getType() == Type.WORKSPACE)
				|| (sourceObject.getTechnology() == Technology.VB && sourceObject.getType() == Type.ACTIVEX_DOCUMENT)
				|| (sourceObject.getTechnology() == Technology.VB && sourceObject.getType() == Type.PROJECT);
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		final GenericMetricsContributor metricsContributor = new GenericMetricsContributor(new InputProvider(sourceObject, Vb6LexerFactory.get()));
		metricsContributor.enable(MetricFactory.get(MetricType.LOC_LOC));

		final ModuleBuilder rootModule = builder.declareRootModule(sourceObject.getName(),
				ModuleType.fromTechnologyAndType(sourceObject.getTechnology(), sourceObject.getType()));

		try {
			rootModule.addAdditionalInfo(GenericMetricsUtil.executeAndGetResults(metricsContributor));
		} catch (final MetricException e) {
			LOG.error("Error while calculating metrics", e);
			rootModule.addError(Severity.ERROR, ErrorKey.METRICS_CALCULATION_ERROR, e.getMessage());
		}
	}
}
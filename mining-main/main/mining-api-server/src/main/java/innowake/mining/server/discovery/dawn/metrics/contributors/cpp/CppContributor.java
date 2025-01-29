/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.contributors.cpp;

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
import innowake.ndt.parsing.scanner.c.CLexerFactory;

/**
 * Dawn contributor for C++ files.
 */
@Component
public class CppContributor implements DiscoveryContributorFromSource {

	private static final Logger LOG = LoggerFactory.getLogger(CppContributor.class);
	
	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return (sourceObject.getTechnology() == Technology.CPP && sourceObject.getType() == Type.PROGRAM)
				|| (sourceObject.getTechnology() == Technology.CPP && sourceObject.getType() == Type.HEADER);
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		/*
		 * We are using CLexerFactory to calculate the LOC metrics for C++ because the comment declaration is same in both C and C++.
		 */
		final var metricsContributor = new GenericMetricsContributor(new InputProvider(sourceObject, CLexerFactory.get()));
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

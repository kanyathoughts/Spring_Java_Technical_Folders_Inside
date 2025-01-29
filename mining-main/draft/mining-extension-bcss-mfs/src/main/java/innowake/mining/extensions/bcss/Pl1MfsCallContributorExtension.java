/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.bcss;

import static innowake.lib.core.lang.Assert.assertNotNull;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.discovery.PersistingSourceObjectResolver;
import innowake.mining.server.discovery.parser.pl1.Pl1ParseResultProvider;
import innowake.mining.server.service.ParseResultCacheService;
import innowake.mining.shared.model.discovery.ResolveTarget;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import innowake.mining.data.discovery.metrics.ContributorParameters;
import innowake.mining.data.discovery.metrics.MetricsContributor;
import innowake.mining.data.discovery.metrics.MetricsContributor.Phase;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.io.sourceobject.SourceObjectDao;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.extensions.MetricsContributorExtension;

import javax.annotation.PostConstruct;


/**
 * Contributor extension for PL/1 --[Calls]--> MFS dependency using BCSS specific implementation.
 */
@Component
public class Pl1MfsCallContributorExtension implements MetricsContributorExtension {

	private static final Logger LOG = LoggerFactory.getLogger(Pl1MfsCallContributorExtension.class);

	@Autowired
	private ParseResultCacheService parseResultCacheService;

	@PostConstruct
	public void init() {
		LOG.info("Pl1MfsCallContributorExtension loaded");
	}

	@Override
	public boolean accept(final ModelArtifact artifact, final Phase phase) {
		return phase == Phase.DEPENDENT_METRICS && (artifact.getType() == ResolveTarget.PL1_MAINPROGRAM || artifact.getType() == ResolveTarget.PL1_PROGRAM);
	}

	@Override
	public MetricsContributor init(final ContributorParameters parameters, final SourceObjectDao sourceObjectDao, final TimedWorker timedWorker) {
		final PersistingSourceObjectResolver objectResolver = new PersistingSourceObjectResolver(sourceObjectDao, assertNotNull(parameters.searchOrders));
		final Pl1ParseResultProvider parseResultProvider = new Pl1ParseResultProvider(parameters.config, assertNotNull(timedWorker), parameters.jobId,
				parseResultCacheService, objectResolver, parameters.featureMap);
		return new Pl1MfsCallContributor(parseResultProvider, parameters, sourceObjectDao);
	}
	
	@Override
	public boolean requiresTimedWorker() {
		return true;
	}
}

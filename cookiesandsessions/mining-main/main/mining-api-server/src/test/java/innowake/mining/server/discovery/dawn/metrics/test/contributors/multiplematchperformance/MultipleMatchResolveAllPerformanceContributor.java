/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.test.contributors.multiplematchperformance;

import innowake.mining.server.discovery.dawn.metrics.api.contributor.DeferredAction;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributor;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;

/**
 * Contributor that creates a large number of modules, each declaring a dependency on {@link #DEPENDENCY_TARGET_NAME} with
 * {@code MULTIPLE_MATCH_RESOLVE_ALL} flag.
 */
public class MultipleMatchResolveAllPerformanceContributor implements DiscoveryContributor {

	public static final int NUMBER_OF_MODULES = 10_000;
	public static final String DEPENDENCY_TARGET_NAME = "TestTarget";

	@Override
	public void contribute(final DiscoveryBuilder builder, final DiscoveryContext context) {
		/* declare 10000 modules with the same dependency */
		for (int i = 0; i < NUMBER_OF_MODULES; i++) {
			final DiscoveryBuilder.ModuleBuilder module = builder.declareExternalModule(String.format("TestSource%03d", i), ModuleType.UNKNOWN);
			module.declareDependency(RelationshipType.REFERENCES, new ModuleFilter().setNames(DEPENDENCY_TARGET_NAME),
					ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL);
			module.deferAction("deferredAction");
		}
	}

	@DeferredAction
	public void deferredAction(final DiscoveryBuilder builder) {
		/* add one more valid target */
		builder.declareExternalModule(DEPENDENCY_TARGET_NAME, ModuleType.COBOL_PROGRAM);
	}
}

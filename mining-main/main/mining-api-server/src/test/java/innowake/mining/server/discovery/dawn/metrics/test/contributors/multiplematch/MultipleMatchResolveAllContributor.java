/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.test.contributors.multiplematch;

import innowake.mining.server.discovery.dawn.metrics.api.contributor.DeferredAction;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributor;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;

/**
 * Contributor declaring a dependency with {@code MULTIPLE_MATCH_RESOLVE_ALL} flag, adding one valid dependency target in the
 * {@link #contribute(DiscoveryBuilder, DiscoveryContext)} method and a second valid target in {@link #deferredAction(DiscoveryBuilder)}.
 */
public class MultipleMatchResolveAllContributor implements DiscoveryContributor {
	public static final String DEPENDENCY_TARGET_NAME = "TestTarget";

	@Override
	public void contribute(final DiscoveryBuilder builder, final DiscoveryContext context) {
		/* define first target */
		builder.declareExternalModule(DEPENDENCY_TARGET_NAME, ModuleType.NATURAL_PROGRAM);

		final DiscoveryBuilder.ModuleBuilder module = builder.declareExternalModule("TestSource", ModuleType.UNKNOWN);
		module.declareDependency(RelationshipType.REFERENCES, new ModuleFilter().setNames(DEPENDENCY_TARGET_NAME),
				ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL);

		module.deferAction("deferredAction");
	}

	@DeferredAction
	public void deferredAction(final DiscoveryBuilder builder) {
		/* add one more valid target */
		builder.declareExternalModule(DEPENDENCY_TARGET_NAME, ModuleType.COBOL_PROGRAM);
	}
}

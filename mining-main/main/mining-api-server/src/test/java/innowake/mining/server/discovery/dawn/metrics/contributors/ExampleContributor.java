/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors;

import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.ModuleType;

/**
 * Fake contributor demonstrating how a contributor can be tested.
 */
public class ExampleContributor implements DiscoveryContributorFromSource {

	@Override
	public boolean accept(DiscoveryContext context, SourcePojo sourceObject) {
		return true;
	}

	@Override
	public void contribute(DiscoveryBuilderFromSource builder, DiscoveryContext context, SourcePojo sourceObject) {
		builder.declareRootModule(sourceObject.getName(), ModuleType.COBOL_PROGRAM);
	}
}

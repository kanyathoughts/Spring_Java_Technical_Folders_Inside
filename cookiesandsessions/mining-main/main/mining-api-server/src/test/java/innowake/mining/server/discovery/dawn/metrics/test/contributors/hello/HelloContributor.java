/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.test.contributors.hello;

import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributor;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.shared.model.ModuleType;

/**
 * Simple "no-source" contributor that creates an external module.
 */
public class HelloContributor implements DiscoveryContributor {
	@Override
	public void contribute(final DiscoveryBuilder builder, final DiscoveryContext context) {
		builder.declareExternalModule("Hello External", ModuleType.UNKNOWN);
	}
}

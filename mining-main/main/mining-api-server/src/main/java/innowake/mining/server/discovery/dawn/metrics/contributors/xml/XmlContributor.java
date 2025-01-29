/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.xml;

import org.springframework.stereotype.Component;

import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Contributor for XML files.
 */
@Component
public class XmlContributor implements DiscoveryContributorFromSource {

	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return (sourceObject.getTechnology() == Technology.XML && sourceObject.getType() == Type.UNKNOWN)
				|| (sourceObject.getTechnology() == Technology.XML && sourceObject.getType() == Type.XHTML);
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		builder.declareRootModule(sourceObject.getName(), ModuleType.fromTechnologyAndType(sourceObject.getTechnology(), sourceObject.getType()));
	}
}

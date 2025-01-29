/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.batch;

import org.springframework.stereotype.Component;

import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;

/**
 * Contributor for JCL_CONTROL_CARD and JCL_INCLUDE type files.
 */
@Component
public class JclControlCardIncludeContributor extends AbstractJclContributor {

	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return sourceObject.getTechnology() == Technology.JCL && (sourceObject.getType() == Type.CONTROLCARD || sourceObject.getType() == Type.INCLUDE);
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		final ModuleBuilder rootModule = builder.declareRootModule(sourceObject.getName(),
				ModuleType.fromTechnologyAndType(sourceObject.getTechnology(), sourceObject.getType()));
		calculateSourceMetrics(rootModule, sourceObject, 0);
		if (sourceObject.getContent().toString().isBlank()) {
			rootModule.addError(Severity.WARNING, ErrorKey.EMPTY_FILE, "Found empty file: " + sourceObject.getName());
		}
	}

}

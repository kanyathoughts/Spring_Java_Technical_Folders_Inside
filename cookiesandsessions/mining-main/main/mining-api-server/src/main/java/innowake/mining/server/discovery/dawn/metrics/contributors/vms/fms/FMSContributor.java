/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.contributors.vms.fms;

import java.util.regex.Pattern;

import org.springframework.stereotype.Component;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.contributors.DawnMetricsUtility;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Contributor for FMS files.
 */
@Component
public class FMSContributor implements DiscoveryContributorFromSource {

	private static final Logger LOG = LoggerFactory.getLogger(FMSContributor.class);

	private static final Pattern SPLITTER_NEWLINE = Pattern.compile("\n");
	private static final Pattern SPLITTER_EQUALS = Pattern.compile("=");
	private static final String FMS_COMMENT = "*";

	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return sourceObject.getTechnology() == Technology.VMS && sourceObject.getType() == Type.FMS_FORM;
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		final String resolvedName = resolveName(sourceObject);
		if (resolvedName != null) {
			final ModuleBuilder rootModule = builder.declareRootModule(resolvedName, ModuleType.FMS_FORM);
			DawnMetricsUtility.collectLinesOfCode(sourceObject, rootModule, FMS_COMMENT);
		} else {
			final var errorMessage = String.format("Could not resolve name from %s", sourceObject.getPath());
			LOG.error(errorMessage);
			throw new IllegalStateException(errorMessage);
		}
	}

	@Nullable
	private String resolveName(final SourcePojo sourceObject) {
		final String content = sourceObject.getContent().toString();
		final String[] splitContent = SPLITTER_NEWLINE.split(content);
		for (final String line : splitContent) {
			if (line.startsWith("FORM NAME=")) {
				final String[] splittedLine = SPLITTER_EQUALS.split(line);
				if (splittedLine.length >= 2) {
					return SPLITTER_EQUALS.split(line)[1].replace("'", "").replace("\r", "");
				}
			}
		}
		return null;
	}

}

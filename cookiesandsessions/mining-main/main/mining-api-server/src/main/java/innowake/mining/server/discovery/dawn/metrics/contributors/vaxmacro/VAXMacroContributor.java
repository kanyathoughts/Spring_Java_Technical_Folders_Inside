/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.vaxmacro;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.discovery.categorize.assembler.VAXMacroFileTypeDetection;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.contributors.DawnMetricsUtility;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ErrorMarkerPojoPrototype;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;

/**
 * Contributor for VAX MACRO files in VMS.
 */
@Component
public class VAXMacroContributor implements DiscoveryContributorFromSource {

	private static final Logger LOG = LoggerFactory.getLogger(VAXMacroContributor.class);
	private static final String COMMENT_IDENTIFIER = ";";
	private static final String TOKEN_ENTRY = ".ENTRY";
	private static final String NEW_LINE = "\n";

	@Autowired
	private ModuleService moduleService;

	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return sourceObject.getTechnology() == Technology.VMS && sourceObject.getType() == Type.VAX_MACRO;
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		final String[] splitContent = sourceObject.getContent().toString().split(NEW_LINE);
		final List<String> moduleNames = getSecondTokenFromSearchToken(splitContent, VAXMacroFileTypeDetection.TITLE_TOKEN);
		final ModuleBuilder rootModule;
		if (moduleNames.isEmpty()) {
			final String errorMessage = String.format("VAX_MACRO file not found in the given file : %s", sourceObject.getPath());
			moduleService.createErrorMarker(new ErrorMarkerPojoPrototype()
					.setProject(context.getProjectId())
					.setKey(ErrorKey.PARSE_ERROR)
					.setSeverity(Severity.ERROR)
					.setCause(errorMessage));
			LOG.error(errorMessage);
			return;
		}
		rootModule = builder.declareRootModule(moduleNames.get(0), ModuleType.VAX_MACRO);
		DawnMetricsUtility.collectLinesOfCode(sourceObject, rootModule, COMMENT_IDENTIFIER);
		collectDependencies(splitContent, rootModule, builder);
	}

	private static List<String> getSecondTokenFromSearchToken(final String[] splitLines, final String searchToken) {
		final List<String> findings = new ArrayList<>();
		for (final String line : splitLines) {
			final String trimmedLine = line.trim();
			if (trimmedLine.toUpperCase().startsWith(searchToken)) {
				final String[] spaceSeparatedLine = trimmedLine.split("\\s+");
				if (spaceSeparatedLine.length >= 2) {
					final int parmSeparator = spaceSeparatedLine[1].indexOf(',');
					final String editedToken = parmSeparator != -1 
						? spaceSeparatedLine[1].substring(0, parmSeparator) 
						: spaceSeparatedLine[1];
					findings.add(editedToken);
				}
			}
		}

		return findings;
	}

	private void collectDependencies(final String[] splitContent, final ModuleBuilder rootModule, final DiscoveryBuilderFromSource builder) {
		final List<String> tokenEntryModules = getSecondTokenFromSearchToken(splitContent, TOKEN_ENTRY);
		tokenEntryModules.forEach(tokenEntryModule -> {
			final ModuleBuilder externalTokenEntryModule = builder.declareExternalModule(tokenEntryModule, ModuleType.VAX_MACRO_ENTRY);

			rootModule.declareDependency(RelationshipType.CALLS, externalTokenEntryModule)
				.setBinding(Binding.EARLY);
		});
	}
}

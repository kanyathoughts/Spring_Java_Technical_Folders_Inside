/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.contributors.cdo;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.contributors.DawnMetricsUtility;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.server.discovery.parser.oracle.CDORecord;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;

/**
 * Contributor for Oracle Language files
 */
@Component
public class CDOContributor implements DiscoveryContributorFromSource {

	private static final Logger LOG = LoggerFactory.getLogger(CDOContributor.class);
	private static final String CDO_COMMENT = "!";

	@Autowired
	private ParserProviderService parserProvider;

	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return sourceObject.getTechnology() == Technology.ORACLE && sourceObject.getType() == Type.CDO_FILE;
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		final ModuleBuilder rootModule = builder.declareRootModule(sourceObject.getName(), ModuleType.CDO_FILE);
		try {
			DawnMetricsUtility.collectLinesOfCode(sourceObject, rootModule, CDO_COMMENT);
			final var cdoRecordParser = parserProvider.createCdoRecordParser();
			final List<CDORecord> cdoRecords = cdoRecordParser.parse(sourceObject.getContent().toString());
			if (cdoRecordParser.hasParseErrors()) {
				cdoRecordParser.getParseErrors().forEach(parseError -> {
					LOG.error(String.format("Error while parsing %s: %s", sourceObject.getPath(), parseError.getMessage()));
					rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, parseError.getMessage());
				});
			} else {
				processCdoRecords(cdoRecords, builder, rootModule);
			}
		} catch (final Exception e) {
			final var errorMessage = String.format("[%s] Could not analyze virtual resources for %s %s", sourceObject.getName(), sourceObject.getPath(),
					e.getMessage());
			LOG.error(() -> errorMessage, e);
			rootModule.addError(Severity.ERROR, ErrorKey.MODULE_ABORT, errorMessage);
		}
	}
	
	private void processCdoRecords(final List<CDORecord> cdoRecords, final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule) {
		cdoRecords.stream()
		.map(CDORecord::getName)
		.map(name -> builder.declareExternalModule(name, ModuleType.CDO_RECORD, ResolutionFlag.RESOLVE_CASE_INSENSITIVE))
		.forEach(cdoRecord -> rootModule.declareDependency(RelationshipType.REFERENCES, cdoRecord)
				.setBinding(Binding.EARLY));
	}
}

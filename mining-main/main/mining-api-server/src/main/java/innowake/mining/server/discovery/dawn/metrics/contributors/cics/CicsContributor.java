/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.contributors.cics;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import innowake.lib.core.IProgress;
import innowake.lib.core.lang.Assert;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.discovery.categorize.cobol.CobolIdentificationToken;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.contributors.GenericMetricsUtil;
import innowake.mining.server.discovery.dawn.metrics.contributors.InputProvider;
import innowake.mining.server.discovery.metrics.cobol.CobolStatementUtility;
import innowake.mining.server.discovery.metrics.generic.GenericMetricsContributor;
import innowake.mining.server.discovery.metrics.generic.MetricException;
import innowake.mining.server.discovery.metrics.generic.MetricFactory;
import innowake.mining.server.discovery.metrics.generic.MetricType;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.BinaryValue;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.hashing.CityHash;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.SourceMetrics;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.cobol.parser.bms.BMSParserAst;
import innowake.ndt.cobol.parser.bms.model.BmsDfhmdfNode;
import innowake.ndt.cobol.parser.bms.model.BmsDfhmdfNode.Attrb;
import innowake.ndt.cobol.parser.bms.model.BmsDfhmdiNode;
import innowake.ndt.cobol.parser.bms.model.BmsModel;
import innowake.ndt.core.parsing.assembler.AssemblerLexerConfiguration;
import innowake.ndt.core.parsing.assembler.AssemblerLexerFactory;

/**
 * Contributor for CICS files.
 */
@Component
public class CicsContributor implements DiscoveryContributorFromSource {

	private static final Logger LOG = LoggerFactory.getLogger(CicsContributor.class);

	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return sourceObject.getTechnology() == Technology.CICS && sourceObject.getType() == Type.BMS_MAPSET;
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		try {
			final String content = sourceObject.getContent().toString();
			final var bmsModel = new BMSParserAst(LOG, IProgress.DUMMY).parse(content);
			final ModuleBuilder rootModule = builder.declareRootModule(bmsModel.getBmsDfhmsdNode().getId(), ModuleType.CICS_BMS_MAPSET);
			calculateSourceMetricsForBmsMapset(rootModule, sourceObject);
			if (content.isBlank()) {
				rootModule.addError(Severity.WARNING, ErrorKey.EMPTY_FILE, "Found empty file: " + sourceObject.getName());
				return;
			}
			calculateMetricsForBmsMap(builder, sourceObject, rootModule, bmsModel);
		} catch (final Exception e) {
			final ModuleBuilder rootModule = builder.declareRootModule(sourceObject.getName(), ModuleType.CICS_BMS_MAPSET);
			calculateSourceMetricsForBmsMapset(rootModule, sourceObject);
			LOG.error(() -> String.format("[%s] %s", sourceObject.getName(), e.getMessage()), e);
			LOG.debug(() -> ExceptionUtils.getFullStackTrace(e));
		}
	}

	private void calculateSourceMetricsForBmsMapset(final ModuleBuilder rootModule, final SourcePojo sourceObject) {
		final SourceMetrics sourceMetrics = calculateSourceMetrics(sourceObject, rootModule);
		sourceMetrics.setComplexityMcCabe(Integer.valueOf(1));
		rootModule.addAdditionalInfo(sourceMetrics);
	}

	private void calculateMetricsForBmsMap(final DiscoveryBuilderFromSource builder, final SourcePojo sourceObject, final ModuleBuilder bmsMapSetModule,
			final BmsModel bmsModel) {
		for (final BmsDfhmdiNode mapNode : bmsModel.getBmsDfhmdiNode()) {
			final String name = mapNode.getId();
			final ModuleBuilder bmsMapModule = builder.declareSubModule(name, ModuleType.CICS_BMS_MAP);
			try {
				final var bmsMapSourceObject = getMapSourceObject(sourceObject, name);
				final SourceMetrics sourceMetricsOfBmsMap = calculateSourceMetrics(bmsMapSourceObject, bmsMapModule);
				final int complexity = calculateComplexity(bmsModel, name);
				sourceMetricsOfBmsMap.setComplexityMcCabe(Integer.valueOf(complexity));
				bmsMapModule.addAdditionalInfo(sourceMetricsOfBmsMap);
			} catch (final DiscoveryException e) {
				LOG.error(String.format("Error while calculating metrics of %s map", sourceObject.getPath()), e);
				bmsMapSetModule.addError(Severity.ERROR, ErrorKey.METRICS_CALCULATION_ERROR, e.getMessage());
			}

			bmsMapModule.declareDependency(RelationshipType.INCLUDES, bmsMapSetModule).setBinding(Binding.EARLY);
		}
	}

	private SourceMetrics calculateSourceMetrics(final SourcePojo sourceObject, final ModuleBuilder moduleBuilder) {
		final var metricsContributor = new GenericMetricsContributor(
				new InputProvider(sourceObject, AssemblerLexerFactory.get(AssemblerLexerConfiguration.DEFAULT)));
		metricsContributor.enable(MetricFactory.get(MetricType.LOC_LOC));
		var sourceMetrics = new SourceMetrics();
		try {
			sourceMetrics = GenericMetricsUtil.executeAndGetResults(metricsContributor);
		} catch (final MetricException e) {
			LOG.error(String.format("Error while calculating metrics of %s.", sourceObject.getPath()), e);
			moduleBuilder.addError(Severity.ERROR, ErrorKey.METRICS_CALCULATION_ERROR, e.getMessage());
		}
		/*
		 * We are adding a positive default value 0 as dead code lines. If we remove this we need to adjust the feature matrix as this technology won't support 
		 * the dead code calculation, which is not correct.
		 */
		if (sourceMetrics.getDeadCodeLines() == -1) {
			sourceMetrics.setDeadCodeLines(0);
		}
		
		return sourceMetrics;
	}

	/**
	 * Calculate the Complexity metric for maps
	 * Count every Field
	 *
	 * @param model The bms model to calculate the complexity for.
	 * @return The complexity value (count of code branches).
	 */
	private int calculateComplexity(final BmsModel model, final String mapName) {
		var fieldNodes = 0;
		final Set<Attrb> attributeSet = new HashSet<>(); /* save unique attributes */
		for (final BmsDfhmdiNode dfhmidNode : model.getBmsDfhmdiNode()) {
			/* skip if not map name */
			if ( ! dfhmidNode.getId().trim().equalsIgnoreCase(mapName.trim())) {
				continue;
			}
			/* Get field nodes and attributes (in future could analyze attributes more specifically) */
			for (final BmsDfhmdfNode node : dfhmidNode.getBmsDfhmdfNodes()) {
				fieldNodes++;
				attributeSet.addAll(Arrays.asList(node.getAttributes()));
			}
		}
		return 1 + fieldNodes + attributeSet.size();
	}
	private SourcePojo getMapSourceObject(final SourcePojo object, final String name) throws DiscoveryException {
		final String[] lines = CobolStatementUtility.getLines(object);
		final String content = Assert.assertNotNull(object.getContent().toString());
		int firstIndex = -1;
		int lastIndex = -1;

		for (final String line : lines) {
			if (line.contains(CobolIdentificationToken.DFHMDI.name()) && line.contains(name)) {
				firstIndex = content.indexOf(line);
			}

			if (firstIndex > 0 && (line.contains(CobolIdentificationToken.DFHMSD.name())
					|| line.contains(CobolIdentificationToken.DFHMDI.name()) && ! line.contains(name))) {
				lastIndex = content.indexOf(line);
				break;
			}
		}

		/* JWA: added this: if we get to the end of the content and don't get the expected end-token we just count this all as the map */
		if (firstIndex > lastIndex) {
			lastIndex = content.length() - 1;
		}
		final String mapContent  = firstIndex == -1 ? StringUtils.EMPTY : content.substring(firstIndex, lastIndex);
		return new SourcePojo(UUID.randomUUID(), -1l, object.getProject(), name, object.getPath(), Technology.CICS, Type.BMS_MAP, Long.valueOf(1),
				Long.valueOf(1), new BinaryValue(CityHash.EMPTY_CONTENT_HASH), () -> new BinaryString(mapContent), CustomPropertiesMap.empty());
	}
}

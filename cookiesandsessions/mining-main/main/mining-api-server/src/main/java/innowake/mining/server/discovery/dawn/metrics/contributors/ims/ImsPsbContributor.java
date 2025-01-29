/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.ims;

import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.IMS_DBD_NAME;
import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.IMS_DBD_SEGMENT_PARENT;
import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.IMS_PCB_PROCOPT;
import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.IMS_PCB_PROCSEQ;
import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.IMS_PCB_SENSEG;
import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.IMS_PCB_TYPE;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.model.discovery.ModelAttributeMap;
import innowake.mining.data.model.discovery.attribute.ModelAttributeComparator;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleMetadata;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.parsing.parser.ims.model.psbgen.ProgramControlBlock;
import innowake.ndt.parsing.parser.ims.model.psbgen.PsbGenModel;
import innowake.ndt.parsing.parser.ims.model.psbgen.SensitiveSegment;

/**
 * Contributor for IMS_PSB.
 */
@Component
public class ImsPsbContributor implements DiscoveryContributorFromSource {
	
	private static final Logger LOG = LoggerFactory.getLogger(ImsPsbContributor.class);
	
	@Autowired
	private ParserProviderService parserProviderService;

	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return sourceObject.getTechnology() == Technology.IMS && sourceObject.getType() == Type.PSB;
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		final var imsParseResultProvider = parserProviderService.createImsParser(context);
		try {
			final var imsParseResult = imsParseResultProvider.getParseResult(sourceObject);
			final var psbGenModel = imsParseResult.createPsbModel();
			/* the PSBNAME property defines the actual resource name for the environment if present, instead of the physical file name */
			String name = psbGenModel.getPsbgen().getPsbname();
			name  = StringUtils.isNotBlank(name) ? name : sourceObject.getName();
			final ModuleBuilder rootModule = builder.declareRootModule(name, ModuleType.IMS_PSB);
			calculateSourceMetrics(sourceObject, rootModule, Optional.of(psbGenModel));
			collectSubModules(builder, rootModule, psbGenModel, sourceObject.getPath());
		} catch (final DiscoveryException e) {
			LOG.error("[" + sourceObject.getName() + "] Error during parsing of PSB: " + sourceObject.getName(), e);
			ImsMetricsUtil.calculateSourceMetricsOnError(builder, sourceObject, e.getMessage(), ModuleType.IMS_PSB);
		} catch (final Throwable e) {
			LOG.error("Unexpected error occurred while parsing" + sourceObject.getPath(), e);
			ImsMetricsUtil.calculateSourceMetricsOnError(builder, sourceObject, e.getMessage(), ModuleType.IMS_PSB);
		}
		
	}
	
	private void calculateSourceMetrics(final SourcePojo sourceObject, final ModuleBuilder rootModule, final Optional<PsbGenModel> psbGenModel) {
		final var metricsV2 = ImsMetricsUtil.calculateSourceMetrics(sourceObject, rootModule);
		var complexity = 0;
		if (psbGenModel.isPresent()) {
			final List<ProgramControlBlock> pcbList = psbGenModel.get().getPCBs();
			complexity = pcbList.size() + 1 ;
		}
		metricsV2.setComplexityMcCabe(complexity);
		rootModule.addAdditionalInfo(metricsV2);
	}
	
	private void collectSubModules(final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule, final PsbGenModel psbGenModel,
			final String rootModulePath) {
		final String cmpat = psbGenModel.getPsbgen().getCmpat();
		if (StringUtils.isNotBlank(cmpat)) {
			final var metadata = new ModuleMetadata();
			metadata.put("CMPAT", cmpat.trim());
			rootModule.addAdditionalInfo(metadata);
		}
		final var pcbOrder = new AtomicInteger();
		psbGenModel.getPCBs()
		.stream()
		.filter(Objects::nonNull)
		.forEach(programControlBlock -> collectPcbModules(builder, rootModule, programControlBlock, pcbOrder, rootModulePath));
	}

	private void collectPcbModules(final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule, final ProgramControlBlock programControlBlock,
			final AtomicInteger pcbOrder, final String rootModulePath) {
		final String externalName = (String) programControlBlock.getProperty("EXTERNALNAME");
		final String pcbName = programControlBlock.getPcbname();
		final String dbdName = programControlBlock.getDbdname();
		String finalPcbName;
		ModuleBuilder pcbModuleBuilder = null;
		if (programControlBlock.getType().contains("TP")) {
			finalPcbName = pcbName != null ? pcbName : externalName;
			if (StringUtils.isBlank(finalPcbName)) {
				finalPcbName = "ALT_PCB";
			}
			pcbModuleBuilder = builder.declareSubModule(finalPcbName, ModuleType.IMS_ALT_PCB);
			if (StringUtils.isNoneBlank(pcbName, externalName)) {
				pcbModuleBuilder.addError(Severity.WARNING, ErrorKey.MODULE_ABORT, String
						.format("PCB statement contains both PCBNAME=%s and EXTERNALNAME=%s, hence EXTERNALNAME is discarded.", pcbName, externalName));
			}
		} else {
			/* if the PCB explicitly provides a name, we use that one and otherwise the DBD name if present */
			finalPcbName = pcbName != null ? pcbName : dbdName;
			if (StringUtils.isNotBlank(finalPcbName)) {
				pcbModuleBuilder = builder.declareSubModule(finalPcbName, ModuleType.IMS_PCB);
			}
		}
		if (pcbModuleBuilder != null) {
			collectDependenciesForPsb(rootModule, programControlBlock, pcbModuleBuilder, dbdName, pcbOrder, finalPcbName, rootModulePath);
		}
	}
	
	private void collectDependenciesForPsb(final ModuleBuilder rootModule, final ProgramControlBlock programControlBlock, final ModuleBuilder pcbModule,
			final String dbdName, final AtomicInteger pcbOrder, final String pcbName, final String rootModulePath) {

		/* we must be able to reconstruct the exact same order of the PCBs as declared in the PSB. */
		final var metadataForPcb = new ModuleMetadata();
		metadataForPcb.put("ORDER", String.valueOf(pcbOrder.getAndIncrement()));
		pcbModule.addAdditionalInfo(metadataForPcb);
		final ModelAttributeMap<Object> attributeMapForPcbDependency = new ModelAttributeMap<>(ModelAttributeComparator.getKeyComparator());
		attributeMapForPcbDependency.put(IMS_PCB_TYPE, programControlBlock.getType());
		ModuleFilter dbdModuleFilter = null;
		if (StringUtils.isNotBlank(dbdName)) {
			attributeMapForPcbDependency.put(IMS_DBD_NAME, dbdName);
			dbdModuleFilter = new ModuleFilter().setNames(dbdName).setTypes(ModuleType.IMS_DBD);
			final var hdamparmModuleFilter = new ModuleFilter().setNames(dbdName).setTypes(ModuleType.IMS_HDAMPARM);
			pcbModule.declareDependency(RelationshipType.REFERENCES, Arrays.asList(dbdModuleFilter, hdamparmModuleFilter), ResolutionFlag.MERGE_DUPLICATES)
					.setBinding(Binding.EARLY);

			rootModule.declareDependency(RelationshipType.REFERENCES, Arrays.asList(dbdModuleFilter, hdamparmModuleFilter), ResolutionFlag.MERGE_DUPLICATES)
					.setBinding(Binding.EARLY);
		}

		if (programControlBlock.getProcopt() != null && ! programControlBlock.getProcopt().isEmpty()) {
			attributeMapForPcbDependency.put(IMS_PCB_PROCOPT, programControlBlock.getProcopt());
		}
		if (programControlBlock.getProcseq() != null && ! programControlBlock.getProcseq().isEmpty()) {
			attributeMapForPcbDependency.put(IMS_PCB_PROCSEQ, programControlBlock.getProcseq());
		}

		collectSegments(rootModule, programControlBlock, pcbModule, pcbName, rootModulePath, attributeMapForPcbDependency, dbdModuleFilter);
	}

	private void collectSegments(final ModuleBuilder rootModule, final ProgramControlBlock programControlBlock, final ModuleBuilder pcbModule,
			final String pcbName, final String rootModulePath, final ModelAttributeMap<Object> attributeMapForPcbDependency,
			@Nullable final ModuleFilter dbdModuleFilter) {
		final List<SensitiveSegment> sensitiveSegments = programControlBlock.getSensitiveSegments();
		if ( ! sensitiveSegments.isEmpty()) {
			final int sensegsSize = sensitiveSegments.size();
			final var sb = new StringBuilder(sensegsSize * 9);
			for (int i = 0; i < sensegsSize; i++) {
				final var sensitiveSegment = sensitiveSegments.get(i);
				final String segmentName = sensitiveSegment.getName();

				sb.append(segmentName);
				if (i + 1 < sensegsSize) {
					sb.append(" ");
				}

				/* if the whole DBD could not be found, then we also treat all sensitive segments as missing. those are always coupled to that specific
				 * DBD referenced from this specific PCB, even though a segment with the same name may exist for a different DBD. */
				final ModelAttributeMap<Object> segmentAttributeMap = new ModelAttributeMap<>(ModelAttributeComparator.getKeyComparator());
				final String parent = sensitiveSegment.getParent();
				if (parent != null) {
					segmentAttributeMap.put(IMS_DBD_SEGMENT_PARENT, parent);
				}
				final var dbdSegmentFilter = new ModuleFilter().setNames(segmentName).setTypes(ModuleType.IMS_DBD_SEGMENT);
				if (dbdModuleFilter != null) {
					dbdSegmentFilter.setContainedIn(dbdModuleFilter);
				}
				pcbModule.declareDependency(RelationshipType.REFERENCES, dbdSegmentFilter, ResolutionFlag.MERGE_DUPLICATES)
						.setBinding(Binding.EARLY).setAttributes(segmentAttributeMap);
			}
			attributeMapForPcbDependency.put(IMS_PCB_SENSEG, sb.toString());
		}

		final ModuleType pcbTargetType = programControlBlock.getType().contains("TP") ? ModuleType.IMS_ALT_PCB : ModuleType.IMS_PCB;
		final var psbModuleFilter = new ModuleFilter().setPaths(rootModulePath).setTypes(ModuleType.IMS_PSB).setPhysical(true);
		final var pcbModuleFilter = new ModuleFilter().setNames(pcbName).setTypes(pcbTargetType).setContainedIn(psbModuleFilter);
		rootModule.declareDependency(RelationshipType.REFERENCES, pcbModuleFilter, ResolutionFlag.MERGE_DUPLICATES)
		.setBinding(Binding.EARLY)
		.setAttributes(attributeMapForPcbDependency);
	}
}

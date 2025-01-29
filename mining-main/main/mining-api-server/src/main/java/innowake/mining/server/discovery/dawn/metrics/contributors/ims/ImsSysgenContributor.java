/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.ims;

import java.util.List;
import java.util.Objects;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.AnchorToBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.server.discovery.parser.ims.ImsParseResultProvider;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.utility.UtilityList;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.ndt.parsing.parser.ims.sysgen.model.SysgenModel;
import innowake.ndt.parsing.parser.ims.sysgen.model.application.Transaction;

/**
 * Contributor for IMS_SYSGEN_EXPORT and its related subtypes.
 */
@Component
public class ImsSysgenContributor implements DiscoveryContributorFromSource {
	
	private static final Logger LOG = LoggerFactory.getLogger(ImsSysgenContributor.class);
	
	@Autowired
	private ParserProviderService parserProviderService;

	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return sourceObject.getTechnology() == Technology.IMS && sourceObject.getType() == Type.EXPORT;
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		try {
			final ModuleBuilder rootModule = builder.declareRootModule(sourceObject.getName(), ModuleType.IMS_SYSGEN_EXPORT);
			final ImsParseResultProvider parser = parserProviderService.createImsParser(context);
			final var imsParseResult = parser.getParseResult(sourceObject);
			final var sysgenModel = imsParseResult.createSysGenModel();
			/* Collecting source metrics */
			final var metricsV2 = ImsMetricsUtil.calculateSourceMetrics(sourceObject, rootModule);
			rootModule.addAdditionalInfo(metricsV2);
			/* Collecting dependencies */
			final var utilityList = context.getConfig().getUtilityList();
			collectDependencies(builder, sysgenModel, rootModule, utilityList);
		} catch (final DiscoveryException e) {
			LOG.error("[" + sourceObject.getName() + "] Error during parsing of SYSGEN: " + sourceObject.getName(), e);
			ImsMetricsUtil.calculateSourceMetricsOnError(builder, sourceObject, e.getMessage(), ModuleType.IMS_SYSGEN_EXPORT);
		} catch (final Throwable e) {
			LOG.error("Unexpected error occurred while parsing" + sourceObject.getPath(), e);
			ImsMetricsUtil.calculateSourceMetricsOnError(builder, sourceObject, e.getMessage(), ModuleType.IMS_SYSGEN_EXPORT);
		}
	}
	
	private void collectDependencies(final DiscoveryBuilderFromSource builder, final SysgenModel sysgenModel, final ModuleBuilder rootModule,
			final UtilityList utilityList) {
		sysgenModel.getTransactions().stream()
		.map(Transaction::getCode)
		.filter(CollectionUtils::isNotEmpty)
		.flatMap(List::stream)
		.filter(Objects::nonNull)
		.forEach(transaction -> builder.declareSubModule(transaction, ModuleType.IMS_SYSGEN_TRANSACTION));

		for (final var application : sysgenModel.getApplications()) {
			final String psbName = application.getPsb();
			if (psbName != null && StringUtils.isNotBlank(psbName)) {
				final boolean noUtilityFound = utilityList.findUtility(psbName).isEmpty();
				if (noUtilityFound) {

					final var applicationModuleBuilder = builder.declareSubModule(psbName, ModuleType.IMS_SYSGEN_APPLICATION);

					application.getTransactions().stream()
							.map(Transaction::getCode)
							.filter(CollectionUtils::isNotEmpty)
							.flatMap(List::stream)
							.forEach(code -> {
								final ModuleFilter transactionFilter = new ModuleFilter().setNames(code).setTypes(ModuleType.IMS_SYSGEN_TRANSACTION);
								final AnchorToBuilder transactionModuleBuilder = builder.anchorTo(transactionFilter, ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY);
								transactionModuleBuilder.declareDependency(RelationshipType.REFERENCES, applicationModuleBuilder).setBinding(Binding.EARLY);
							});

					final ModuleFilter psbFilter = new ModuleFilter().setNames(psbName).setTypes(ModuleType.IMS_PSB);
					applicationModuleBuilder.declareDependency(RelationshipType.REFERENCES, psbFilter).setBinding(Binding.EARLY);
					rootModule.declareDependency(RelationshipType.REFERENCES, psbFilter).setBinding(Binding.EARLY);

					final ModuleFilter programFilter = new ModuleFilter().setNames(psbName).setTypes(ImsMetricsUtil.IMS_PROGRAM_TYPES);
					applicationModuleBuilder.declareDependency(RelationshipType.CALLS, programFilter).setBinding(Binding.EARLY);
					rootModule.declareDependency(RelationshipType.CALLS, programFilter).setBinding(Binding.EARLY);
				}
			}
		}
	}
}

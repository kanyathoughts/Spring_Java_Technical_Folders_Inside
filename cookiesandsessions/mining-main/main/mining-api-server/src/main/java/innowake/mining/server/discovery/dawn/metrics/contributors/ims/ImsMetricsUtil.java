/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.ims;

import java.util.EnumSet;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.contributors.GenericMetricsUtil;
import innowake.mining.server.discovery.dawn.metrics.contributors.InputProvider;
import innowake.mining.server.discovery.metrics.generic.GenericMetricsContributor;
import innowake.mining.server.discovery.metrics.generic.MetricException;
import innowake.mining.server.discovery.metrics.generic.MetricFactory;
import innowake.mining.server.discovery.metrics.generic.MetricType;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.SourceMetrics;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.core.parsing.assembler.AssemblerLexerConfiguration;
import innowake.ndt.core.parsing.assembler.AssemblerLexerFactory;
import innowake.ndt.core.parsing.spi.Document;

/**
 * Utility class consist of all common methods for metrics collection for IMS.
 */
public class ImsMetricsUtil {
	
	private static final Logger LOG = LoggerFactory.getLogger(ImsMetricsUtil.class);
	private static final EnumSet<Type> TYPES_SUPPORTING_GENERIC_LOC = EnumSet.of(Type.PSB, Type.DBD);
	/**
	 * Possible {@link ModuleType} to be called from IMS.
	 */
	public static final ModuleType[] IMS_PROGRAM_TYPES = new ModuleType[] { ModuleType.COBOL_PROGRAM, ModuleType.PL1_MAINPROGRAM,
			ModuleType.C_PROGRAM, ModuleType.ASSEMBLER_PROGRAM, ModuleType.UNKNOWN_UTILITY };

	/**
	 * IMS utility implementations that can be for example called by Cobol or PL/I programs.
	 */
	public enum ImsUtility {
		AIBTDLI, CEETDLI, CTDLI, CBLTDLI, PLITDLI;
	}
	
	private ImsMetricsUtil() {
		
	}
	
	/**
	 * Calculates {@link SourceMetrics} for the given {@link SourcePojo}.
	 * 
	 * @param sourceObject {@link SourcePojo}
	 * @param rootModule root {@link ModuleBuilder}
	 * @return {@link SourceMetrics}
	 */
	public static SourceMetrics calculateSourceMetrics(final SourcePojo sourceObject, final ModuleBuilder rootModule) {
		var metricsV2 = new SourceMetrics();
		final var type = sourceObject.getType();
		/* Calculating LOC metrics (Lines of code , Lines of comments, Physical Lines of code) */
		if (TYPES_SUPPORTING_GENERIC_LOC.contains(type)) {
			final var inputProvider = new InputProvider(sourceObject, AssemblerLexerFactory.get(AssemblerLexerConfiguration.DEFAULT));
			final var metricsContributor = new GenericMetricsContributor(inputProvider);
			metricsContributor.enable(MetricFactory.get(MetricType.LOC_LOC));
			try {
				metricsV2 = GenericMetricsUtil.executeAndGetResults(metricsContributor);
			} catch (final MetricException e) {
				LOG.error("Error while calculating metrics", e);
				rootModule.addError(Severity.ERROR, ErrorKey.METRICS_CALCULATION_ERROR, e.getMessage());
			}
		} else {
			final var sourceObjectContent = sourceObject.getContent().toString();
			metricsV2.setPhysicalLines(new Document(sourceObjectContent).numberOfLines());
		}
		
		metricsV2.setComplexityMcCabe(0);
		return metricsV2;
	}
	
	/**
	 * Declare root module and calculates the source metrics on error for given {@link SourcePojo}.
	 * 
	 * @param builder {@link DiscoveryBuilderFromSource}
	 * @param sourceObject {@link SourcePojo}
	 * @param errorMessage error message
	 * @param type {@link ModuleType} of module
	 */
	public static void calculateSourceMetricsOnError(final DiscoveryBuilderFromSource builder, final SourcePojo sourceObject, final String errorMessage,
			final ModuleType type) {
		final ModuleBuilder rootModule = builder.declareRootModule(sourceObject.getName(), type);
		final var metricsV2 = ImsMetricsUtil.calculateSourceMetrics(sourceObject, rootModule);
		rootModule.addAdditionalInfo(metricsV2);
		rootModule.addError(Severity.ERROR, ErrorKey.MODULE_ABORT, errorMessage);
	}
	
	/**
	 * Resolves the {@link ImsUtility} based on the given {@code name}.
	 *
	 * @param name the name to check
	 * @return the {@link ImsUtility} if the name matches one or {@code null}
	 */
	@Nullable
	public static ImsUtility resolveImsUtility(final String name) {
		for (final ImsUtility util : ImsUtility.values()) {
			if (util.name().equalsIgnoreCase(name)) {
				return util;
			}
		}
		return null;
	}
}

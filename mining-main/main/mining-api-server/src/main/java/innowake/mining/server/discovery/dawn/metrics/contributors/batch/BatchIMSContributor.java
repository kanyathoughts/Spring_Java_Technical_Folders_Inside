/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.batch;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.contributors.ims.ImsMetricsUtil;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;

/**
 * Collects all the IMS related dependencies referred within the JCL
 */
public class BatchIMSContributor implements ExternalContributor {
	
	private static final Pattern WORD_PATTERN = Pattern.compile("(\\w+)");
	private static final Logger LOG = LoggerFactory.getLogger(BatchIMSContributor.class);
	final DiscoveryBuilder builder;
	final DiscoveryContext context;
	final ModuleBuilder sourceModule;
	final ParserProviderService parserProvider;
	private static final List<String> IMS_PROGRAM_REGIONS = Arrays.asList("MSG", "BMP", "DLI");
	private static final ModuleType[] IMS_PROGRAM_TYPES = ImsMetricsUtil.IMS_PROGRAM_TYPES;
	private static final String IMS_UTILITY_REGION = "ULU";
	private final List<String> targets = new ArrayList<>();
	
	BatchIMSContributor(final ParserProviderService parserProvider, final DiscoveryBuilder builder, final DiscoveryContext context,
			final ModuleBuilder sourceModule) {
		this.builder = builder;
		this.context = context;
		this.sourceModule = sourceModule;
		this.parserProvider = parserProvider;
	}

	@Override
	public void collectMetrics(final String... sourceContent) {
	    final var matcher = WORD_PATTERN.matcher(sourceContent[0]);
	    matcher.find(); /* Skip PARM */
	    matcher.find(); /* Region MSG, DLI, BMP, ULU, ... */
	    final String region  = matcher.group();
	    matcher.find(); /* program/utility name */
	    targets.add(matcher.group());
	    /* PSB/DBD name, if present */
	    if (matcher.find()) {
	    	targets.add(matcher.group());
	    } else {
	    	LOG.warn(() -> String.format("PSB/DBD name is missing in call: >%s<", sourceContent[0]));
	    }
	    
		if (IMS_PROGRAM_REGIONS.contains(region)) {
			/* executing anything in the MSG, BMP or DLI regions can either be a program or utility additionally also using a PSB as second parameter. */
			sourceModule.declareDependency(RelationshipType.CALLS, new ModuleFilter().setNames(targets.get(0)).setTypes(IMS_PROGRAM_TYPES))
					.setBinding(Binding.LATE);
			sourceModule.declareDependency(RelationshipType.REFERENCES, new ModuleFilter().setNames(targets.get(1)).setTypes(ModuleType.IMS_PSB))
					.setBinding(Binding.LATE);
		} else {
			final ModuleType[] moduleType = IMS_UTILITY_REGION.equals(region) ? new ModuleType[] {ModuleType.UNKNOWN_UTILITY} : IMS_PROGRAM_TYPES;
			sourceModule.declareDependency(RelationshipType.CALLS, new ModuleFilter().setNames(targets.get(0)).setTypes(moduleType))
					.setBinding(Binding.LATE);
			if (targets.size() > 1) {
				sourceModule.declareDependency(RelationshipType.REFERENCES,
						new ModuleFilter().setNames(targets.get(1)).setTypes(ModuleType.IMS_PSB, ModuleType.IMS_DBD)).setBinding(Binding.LATE);
			} else {
				final String errorMsg = "Unable to collect PSB or DBD binding. Second parameter is missing in PARM:" + sourceContent[0];
				this.sourceModule.addError(Severity.WARNING, ErrorKey.PARSE_ERROR, errorMsg);
			}
		}
		
	}
	
	/**
	 * Returns the IMS matched targets
	 *
	 * @return IMS matched targets
	 */
	public List<String> getMatchedTargets() {
		return targets;
	}
	
}

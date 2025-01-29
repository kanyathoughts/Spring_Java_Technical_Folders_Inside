/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.cobol;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import innowake.mining.extensions.export.callchain.CallChainService;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DeferredAction;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributor;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.ModuleType;

/**
 * Discovery 2.0 Cobol Read Write access contributor for handling transitive
 * phase of the discovery (Deferred action).
 */
@Component
public class CobolReadWriteAccessAnalyzerContributor implements DiscoveryContributor {

	@Autowired
	private ParserProviderService parserProviderService;

	@Autowired
	private ModuleService moduleService;
	
	@Autowired
	private CallChainService callChainService;

	@Override
	public void contribute(final DiscoveryBuilder builder, final DiscoveryContext context) {
		builder.anchorTo(new ModuleFilter().setTypes(ModuleType.COBOL_PROGRAM)
				/* fetch only changed modules for incremental scan. In a full scan all modules are created newly and will match */
				.setMetricsDate(context.getModuleParameters().getMetricsDate()), ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL,
				ResolutionFlag.RESOLVE_CASE_INSENSITIVE).deferAction("analyzeFileAccess");
	}

	/**
	 * Examines a target Cobol program, and detects all file READ or WRITE accessed
	 * performed by the program. Also resolves the write accesses back to the actual
	 * RESOURCE_FILE module and adds a dependency edge between the Cobol program and
	 * the file from JCL and CSD.
	 *
	 * @param context the DiscoveryContext object
	 * @param sourceObject {@link SourcePojo}
	 * @param builder the DiscoveryBuilder object
	 * @param moduleBuilder the ModuleBuilder object
	 * @param module the source module
	 */
	@DeferredAction
	public void analyzeFileAccess(final DiscoveryContext context, final SourcePojo sourceObject, final DiscoveryBuilder builder,
			final DiscoveryBuilder.ModuleBuilder moduleBuilder, final ModuleLightweightPojo module) {
		new CobolReadWriteAccessAnalyzer(parserProviderService, moduleService, callChainService).analyzeFileAccess(context, sourceObject, builder,
				moduleBuilder, module);
	}
}

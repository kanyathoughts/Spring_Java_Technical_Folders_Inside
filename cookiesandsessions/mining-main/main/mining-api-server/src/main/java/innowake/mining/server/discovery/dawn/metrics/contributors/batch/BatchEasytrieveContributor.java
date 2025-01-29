/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.batch;

import java.util.Optional;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.contributors.easytrieve.EasytrieveSqlUtility;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.server.discovery.parser.easytrieve.EasytrieveAntlrParseResult;
import innowake.mining.server.discovery.parser.easytrieve.EasytrieveAntlrParseResultProvider;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.core.parsing.ast.AstNode;

/**
 * Collects all the easytrieve dependencies referred within the JCL
 */
public class BatchEasytrieveContributor implements ExternalContributor {

	private static final Logger LOG = LoggerFactory.getLogger(BatchEasytrieveContributor.class);
	final DiscoveryBuilder builder;
	final DiscoveryContext context;
	final ModuleBuilder rootModule;
	final ParserProviderService parserProvider;

	BatchEasytrieveContributor(final ParserProviderService parserProvider, final DiscoveryBuilder builder, final DiscoveryContext context,
			final ModuleBuilder rootModule) {
		this.builder = builder;
		this.context = context;
		this.rootModule = rootModule;
		this.parserProvider = parserProvider;
	}

	@Override
	public void collectMetrics(final String... sourceContent) {
		final EasytrieveAntlrParseResultProvider eztParseProvider = parserProvider.createEasytrieveParser(context.getConfig(), context.getTimedWorker());
		for (final String inline : sourceContent) {
			try {
				final EasytrieveAntlrParseResult metrics = eztParseProvider.parseText(inline);
				metrics.getUnvalidatedDependencies().forEach(dependency -> {
					final var filter = dependency.getModuleFilters().iterator().next();
					builder.declareExternalModule(filter.getNames().iterator().next(), filter.getTypes().iterator().next());
					rootModule.declareDependency(dependency.getRelationshipType(), filter).setBinding(Binding.EARLY);
				});
				
				final Optional<AstNode> rootNodeOptional = metrics.getAstModel().getRoot();
				if (rootNodeOptional.isPresent()) {
					EasytrieveSqlUtility.collectSqlResources(metrics, rootNodeOptional.get(), builder, rootModule);
					EasytrieveSqlUtility.collectSqlDependencies(rootModule, metrics);
				}
			} catch (final DiscoveryException e) {
				final var errorMsg = String.format("Error while parsing the EasytrieveAntl parser when collecting metrics for batch contributor: %s",
						e.getMessage());
				LOG.error(errorMsg, e);
				rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, errorMsg);
			}
		}
	}
}

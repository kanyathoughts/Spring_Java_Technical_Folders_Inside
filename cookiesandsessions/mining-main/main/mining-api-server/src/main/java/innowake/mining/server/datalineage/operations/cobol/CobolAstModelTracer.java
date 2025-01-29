/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.datalineage.operations.cobol;

import innowake.mining.shared.entities.dataflow.ProxyContainerPojo;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;

import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.datalineage.astmodel.CobolDataLineage;
import innowake.mining.server.datalineage.context.DataLineageContext;
import innowake.mining.server.datalineage.operations.DataLineageTracer;
import innowake.mining.server.discovery.config.DiscoveryConfigAccessor;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.server.discovery.parser.cobol.CobolParseResultProvider;
import innowake.mining.server.service.ExecutorService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.datalineage.DataLineageResult;
import innowake.ndt.cobol.parser.ast.model.CobolModel;

/**
 * Data Lineage Tracer component tracing the internal data flow of a Cobol Module and creating Proxy Containers for input and output statements.
 * <p>
 * This implementation is using {@link CobolDataLineage} and is currently only a tech-preview.
 */
@Component
@ConditionalOnProperty(name = "configuration.enable-new-cobol-data-lineage")
public class CobolAstModelTracer implements DataLineageTracer {

	private ModuleService moduleService;
	private final SourceService sourceService;
	private final ExecutorService executorService;
	private final DiscoveryConfigAccessor configAccessor;
	private final ParserProviderService parserProviderService;

	/**
	 * Constructor
	 * @param moduleService Module data access service.
	 * @param sourceService SourceObject data access service.
	 * @param executorService provides entry points to execute various operations like storeAst
	 * @param configAccessor provides access to discovery config for a given job
	 * @param parserProviderService service to create new instances of parsers
	 */
	public CobolAstModelTracer(final ModuleService moduleService,
							   final SourceService sourceService,
							   final ExecutorService executorService,
							   final DiscoveryConfigAccessor configAccessor,
							   final ParserProviderService parserProviderService) {
		this.moduleService = moduleService;
		this.sourceService = sourceService;
		this.executorService = executorService;
		this.configAccessor = configAccessor;
		this.parserProviderService = parserProviderService;
	}

	@Override
	public boolean isSupported(final ModuleLightweightPojo module) {
		return module.getTechnology() == Technology.COBOL && module.getType() == Type.PROGRAM;
	}

	@Override
	public DataLineageResult trace(final DataLineageContext context, final ModuleLightweightPojo module) {
		ensureAsts(context.getProjectId(), module.identity());
		final CobolDataLineage cobolDataLineage = new CobolDataLineage(context, parseModule(context, module.getId()), module);
		return cobolDataLineage.compute();
	}

	@Override
	public DataLineageResult discoverProxyContainersOfType(final DataLineageContext context, final ModuleLightweightPojo module, final ProxyContainerPojo.Type type) {
		return trace(context, module);
	}

	private CobolModel parseModule(final DataLineageContext context, final Long moduleId) {
		final ModuleLightweightPojo module = moduleService.findAnyModuleLightweight(b -> b.byNid(moduleId))
				.orElseThrow(() -> new MiningEntityNotFoundException("While attempting to perform field tracing on Module " + moduleId + ": Module not found"));
		
		final String path = module.getPath();
		if (path == null) {
			throw new IllegalArgumentException("While attempting to perform field tracing on Module " + moduleId
					+ ": Only 'main modules' representing an entire source file can be traced."
					+ " This Module has no path and can therefore not be traced.");
		}

		final SourcePojo sourceObject = sourceService.findAny(q -> q.ofProject(context.getProjectId()).withPath(path))
				.orElseThrow(() -> new MiningEntityNotFoundException("Source not found for project: " + context.getProjectId() + " and path: " + path));

		final CobolParseResultProvider cobolParser = parserProviderService.createCobolParser(
				configAccessor.getConfig(context.getProjectId(), context.getJobId()),
				context.getTimedWorker(),
				configAccessor.getSearchOrders(context.getProjectId(), context.getJobId()),
				context.getJobId());
		try {
			return cobolParser.getModel(sourceObject);
		} catch (final DiscoveryException e) {
			throw new IllegalStateException("Failed to parse cobol module " + moduleId, e);
		}
	}

	private void ensureAsts(final EntityId projectId, final EntityId moduleId) {
		executorService.executeStoreAst(projectId, moduleId);
	}
}

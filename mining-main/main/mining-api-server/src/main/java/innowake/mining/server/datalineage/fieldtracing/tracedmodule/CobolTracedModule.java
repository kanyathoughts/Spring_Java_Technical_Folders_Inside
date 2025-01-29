/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.datalineage.fieldtracing.tracedmodule;

import java.util.ArrayList;
import java.util.List;

import innowake.mining.server.datalineage.context.DataLineageContext;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ModuleService.RelationshipField;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.RelationshipType;
import innowake.ndt.core.parsing.ITokenPartitioning;
import innowake.ndt.core.parsing.TokenPartitioner2;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.cobol.CobolLexerFactory;
import innowake.ndt.core.parsing.spi.StringContent;
import innowake.ndt.fieldtracing.OffsetModel;
import innowake.ndt.fieldtracing.ParsedProgram;
import innowake.ndt.fieldtracing.cobol.parser.CobolParserActions;
import innowake.ndt.fieldtracing.model.TracedModule;

/**
 * This class inherits from AbstractTracedModule. Provides functionality for retrieving incoming and outgoing dependencies.
 */
class CobolTracedModule extends AbstractTracedModule {

	private final ModuleService moduleService;
	private final CobolParserActions<ModuleLightweightPojo> parserActions;

	public CobolTracedModule(final DataLineageContext context, final ModuleLightweightPojo module, final SourcePojo sourceObject,
			final TracedModuleProvider tracedModuleProvider,
			final ModuleService moduleService,
			final CobolParserActions<ModuleLightweightPojo> parserActions) {
		super(context, module, sourceObject, tracedModuleProvider);
		this.moduleService = moduleService;
		this.parserActions = parserActions;
	}

	@Override
	public ParsedProgram<ModuleLightweightPojo, ? extends AstModel, ? extends AstNode, ? extends AstNode, ? extends AstNode, ? extends OffsetModel<?>> getParsedProgram() {
		context.getProgressMonitor().checkCanceled();

		return parserActions.parse(this);
	}

	@Override
	public List<TracedModule<ModuleLightweightPojo>> getIncomingDeps() {
		context.getProgressMonitor().checkCanceled();

		final List<TracedModule<ModuleLightweightPojo>> cachedIncomingDependencies = context.getCachedIncomingDependencies(module.getId());
		if (cachedIncomingDependencies != null) {
			return cachedIncomingDependencies;
		}
		final EntityId projectId = context.getProjectId();

		final List<ModuleRelationshipPojo> refs = moduleService.findRelationship(q -> q.ofProject(projectId)
				.ofDestination(module.identity())
				.withTypes(List.of(RelationshipType.INCLUDES))
				.distinct(RelationshipField.SOURCE, RelationshipField.DESTINATION));

		final List<TracedModule<ModuleLightweightPojo>> tracedList = new ArrayList<>();

		for (final var ref : refs) {
			final EntityId moduleNid = EntityId.of(ref.getSrcModule());
			if ( ! tracedModuleProvider.canTraceModule(projectId, moduleNid)) {
				continue;
			}
			/* getToModuleId used since the looked at module will always be the toModule due to EdgeDirection.IN*/
			tracedList.add(tracedModuleProvider.provideTracedModuleFromModule(moduleNid));
		}

		context.cacheIncomingDependencies(module.getId(), tracedList);
		return tracedList;
	}

	@Override
	public List<TracedModule<ModuleLightweightPojo>> getOutgoingDeps() {
		context.getProgressMonitor().checkCanceled();

		final List<TracedModule<ModuleLightweightPojo>> cachedDependencies = context.getCachedOutgoingDependencies(module.getId());
		if (cachedDependencies != null) {
			return cachedDependencies;
		}

		final EntityId projectId = context.getProjectId();

		final List<ModuleRelationshipPojo> refs = moduleService.findRelationship(q -> q.ofProject(projectId)
				.ofSource(module.identity())
				.withTypes(List.of(RelationshipType.INCLUDES))
				.distinct(RelationshipField.SOURCE, RelationshipField.DESTINATION));

		final List<TracedModule<ModuleLightweightPojo>> tracedList = new ArrayList<>();
		for(final var ref : refs) {
			final EntityId moduleNid = EntityId.of(ref.getDstModule());
			if ( ! tracedModuleProvider.canTraceModule(projectId, moduleNid)) {
				continue;
			}

			/* getToModuleId used since the looked at module will always be the fromModule due to EdgeDirection.OUT*/
			tracedList.add(tracedModuleProvider.provideTracedModuleFromModule(moduleNid));
		}

		context.cacheOutgoingDependencies(module.getId(), tracedList);
		return tracedList;
	}
	
	@Override
	public ITokenPartitioning getTokenPartitioning() {
		final TokenPartitioner2 partitioner = TokenPartitioner2.create(CobolLexerFactory.get());
		return partitioner.doPartitioning(new StringContent(getContent()));
	}

	@Override
	public String toString() {
		return "CobolTracedModule{" +
				"; path = " + sourceObject.getPath() +
				"; name = " + sourceObject.getName() + 
				"; id = " + sourceObject.getId() +
				'}';
	}	
	
	@Override
	public boolean equals(final Object o) {
		/* Was added to remove SonarLint issue */
		return super.equals(o);
	}
	
	@Override 
	public int hashCode() {
		/* Was added to remove SonarLint issue */
		return super.hashCode();
	}
}


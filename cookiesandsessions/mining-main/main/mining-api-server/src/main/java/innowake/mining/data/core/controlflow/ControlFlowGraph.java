/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow;

import java.util.Arrays;
import java.util.Optional;
import java.util.UUID;

import org.apache.commons.lang.time.StopWatch;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.core.controlflow.api.AstToControlFlow;
import innowake.mining.data.core.storeast.api.StoreAstExecutor;
import innowake.mining.server.service.MiningDataCoreService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ast.AstModuleRelationshipType;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.AstNodePojoPrototype;
import innowake.mining.shared.entities.ast.AstRelationshipType;
import innowake.mining.shared.lang.HashCache;

/**
 * Calculates control flow graphs.
 */
public class ControlFlowGraph {
	
	private static final Logger LOG = LoggerFactory.getLogger(ControlFlowGraph.class);
	
	private ControlFlowGraph() {}

	/**
	 * Starts the calculation of the control flow graph for the given {@code moduleId}.
	 *
	 * @param moduleId the Id of the module for which the control flow graph should be calculated
	 * @param storeAstExecutor the {@link StoreAstExecutor} to create the AST if it's not already present in the database
	 * @param core the {@link MiningDataCoreService} containing all entity services
	 * @return the identification result
	 */
	public static boolean calculate(final EntityId moduleId, final StoreAstExecutor storeAstExecutor, final MiningDataCoreService core) {
		final HashCache<UUID, AstNodePojo> astCache = new HashCache<>();
		final Optional<AstNodePojo> rootNode = core.getAstRootOrCreateExceptInclusions(moduleId, storeAstExecutor, astCache);
		if ( ! rootNode.isPresent()) {
			return false;
		}
		return calculateControlFlow(moduleId, rootNode.get(), core, astCache);
	}
	
	private static boolean calculateControlFlow(final EntityId moduleId, final AstNodePojo rootNode, final MiningDataCoreService core, final HashCache<UUID, AstNodePojo> astCache) {
		final StopWatch watch = new StopWatch();
		watch.start();
		final Optional<ModulePojo> module = core.moduleService.findAnyModule(q -> q.byId(moduleId));
		try {
			LOG.info(() -> String.format("Started the control flow calculation for the module with id %s", moduleId));
			if (module.isPresent()) {
				/* clear existing */
				core.astService.deleteRelationshipsByModule(moduleId, Arrays.asList(AstRelationshipType.FLOW));
				core.astService.deleteModuleRelationships(moduleId, AstModuleRelationshipType.CONTROL_FLOW_TERMINALS);
				/* (re)generate */
				final var cf = AstToControlFlow.getNewInstance(module.get().getTechnology(), rootNode);
				cf.calculateControlFlow();
				cf.getFlow().forEach(core.astService::createRelationship);
				cf.getTerminals().forEach(core.astService::createModuleRelationship);
				/* During the CF calculation some resolvers (Cobol) might calc & set additional super types (BranchStatement) which have to be stored */
				astCache.values().stream()
								 .filter(node -> node.getAdditionalSuperTypes().isPresent())
								 .forEach(node -> core.astService.update(new AstNodePojoPrototype().setId(node.getId()).setSuperTypes(node.getSuperTypes())));
			}
			watch.stop();
			LOG.info(() -> String.format("Finished the control flow calculation for the module with id %s. It took %s.", moduleId, watch.toString()));
		} catch (final Exception e) {
			LOG.error(() -> String.format("Failed to add control flow to AST for module with id %s.", moduleId), e);
			return false;
		}
		return true;
	}
	
}

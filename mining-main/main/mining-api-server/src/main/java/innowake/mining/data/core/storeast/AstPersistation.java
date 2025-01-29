/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.storeast;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import innowake.mining.data.core.api.AbstractTraverser;
import innowake.mining.server.service.MiningDataCoreService;
import innowake.mining.shared.entities.ast.AstModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.ast.AstRelationshipPojoPrototype;
import innowake.mining.shared.entities.ast.StoreAstPrototype;

/**
 * A traverser for persisting {@link StoreAstPrototype}s.
 */
public final class AstPersistation extends AbstractTraverser<StoreAstPrototype, UUID> {

	private final MiningDataCoreService core;
	private final List<AstRelationshipPojoPrototype> relAst = new ArrayList<>();
	private final List<AstModuleRelationshipPojoPrototype> relMod = new ArrayList<>();
	
	/**
	 * Constructor.
	 * 
	 * @param core data access service.
	 */
	public AstPersistation(final MiningDataCoreService core) {
		super(StoreAstPrototype::children);
		this.core = core;
	}
	
	@Override
	protected UUID visit(final StoreAstPrototype node) {
		final UUID nodeId = core.astService.create(node);
		node.id.set(nodeId);
		relAst.addAll(node.getRelationships());
		relMod.addAll(node.getModuleRelationships());
		return nodeId;
	}
	
	/**
	 * Creates all edges between the AST nodes.
	 * This has to be called after the whole AST has been traversed, since it has to be ensured that the FROM and TO node actually exist.
	 */
	public void createEdges() {
		relMod.stream().forEach(core.astService::createModuleRelationship);
		relAst.stream().forEach(core.astService::createRelationship);
	}
	
	@Override
	protected UUID traversed(final UUID node, final List<UUID> children) {
		/* this method is called from bottom to top */
		return node;
	}

}


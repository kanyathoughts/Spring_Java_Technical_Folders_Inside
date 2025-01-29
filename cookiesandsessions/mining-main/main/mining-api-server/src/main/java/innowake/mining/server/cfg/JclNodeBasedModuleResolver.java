/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.cfg;

import java.util.Optional;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.ndt.jcl.parser.model.ast.JclAstNode;

/**
 * JCL specific implementation of {@link NodeBasedModuleResolver}. Resolves EXEC_PGM module for a Control-Flow-AstNode.
 */
public class JclNodeBasedModuleResolver implements NodeBasedModuleResolver {
	
	private static final Logger LOG = LoggerFactory.getLogger(JclNodeBasedModuleResolver.class);

	private ModuleService moduleService;

	public JclNodeBasedModuleResolver(final ModuleService moduleService) {
		this.moduleService = moduleService;
	}
	
	@Override
	public Optional<ModulePojo> resolveModule(final AstNodePojo node) {
		final String qualifiedStepName = (String) node.getProperties().get(JclAstNode.QUALIFIED_STEP_NAME);
		if (qualifiedStepName == null) {
			return Optional.empty();
		}

		final var modules = moduleService.findModules(b -> b.withSourceRelationshipsFrom(node.getModule(), RelationshipType.CONTAINS)
										.withName(qualifiedStepName)
										.withTechnology(Technology.JCL)
										.withType(Type.EXEC_PGM)
										.sortName(SortDirection.ASCENDING));

		switch (modules.size()) {
			case 0:
				return Optional.empty();
			case 1:
				return Optional.of(modules.get(0));
			default:
				LOG.error("There is more than one step with the same qualifiedStepName in the same module!");
				return Optional.empty();
		}
	}
}

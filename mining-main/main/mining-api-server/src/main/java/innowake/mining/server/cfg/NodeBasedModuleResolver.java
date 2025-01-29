/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.cfg;

import java.util.Optional;

import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Used to resolve modules for corresponding modules.
 */
public interface NodeBasedModuleResolver {
	/**
	 * resolves the corresponding module to a {@link AstNodePojo}
	 *
	 * @param node corresponding module of this is resolved
	 * @return module
	 */
	public Optional<ModulePojo> resolveModule(final AstNodePojo node);
}

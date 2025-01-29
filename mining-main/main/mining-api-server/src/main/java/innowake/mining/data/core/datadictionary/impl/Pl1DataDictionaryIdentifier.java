/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.datadictionary.impl;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.server.service.MiningDataCoreService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.DefinedLocation;

/**
 * Identifies {@linkplain DataDictionaryPojoPrototype} for PL1 Modules.
 */
public class Pl1DataDictionaryIdentifier extends DataDictionaryIdentifier {

	private final AstNodePojo rootNode;
	private final ModuleLightweightPojo module;

	/**
	 * The constructor.
	 * 
	 * @param core the {@link MiningDataCoreService}
	 * @param module the Module
	 * @param rootNode the root {AstNode of the Module
	 */
	public Pl1DataDictionaryIdentifier(final MiningDataCoreService core, final ModuleLightweightPojo module, final AstNodePojo rootNode) {
		super(core);
		this.rootNode = rootNode;
		this.module = module;
	}
	
	@Override
	protected void collectLanguageSpecificProperties(final AstNodePojo field, final DataDictionaryPojoPrototype dde) {
		if (AstNodeUtils.STRUCTURAL_DECLARATION.equals(field.getType())) {
			dde.setFormat(null);
			dde.setLength(0L);
		}
	}

	@Override
	protected void assignScopesAccessTypes(final AstNodePojo field, final DataDictionaryPojoPrototype dde) {
		/* For now we don't assign scope access types*/
	}

	@Override
	protected void assignScopes(final AstNodePojo field, final DataDictionaryPojoPrototype dde) {
		/* For now we don't assign scopes*/
	}

	@Override
	public void setDefinedLocation(@Nullable final EntityId calleeModuleId, final DataDictionaryPojoPrototype dde, final AstNodePojo node) {
		AstNodePojo parent = node.getParent().orElse(null);
		boolean flag = true;
		while (parent != null && flag) {
			if (AstNodeUtils.PROCEDURE_BLOCK.equals(parent.getType()) || AstNodeUtils.PREPROCESSOR_PROCEDURE_BLOCK.equals(parent.getType())) {
				dde.setDefinedLocation(DefinedLocation.PROCEDURE);
				flag = false;
			} else if (AstNodeUtils.PACKAGE_BLOCK.equals(parent.getType())) {
				dde.setDefinedLocation(DefinedLocation.PACKAGE);
				flag = false;
			} else if (AstNodeUtils.BEGIN_BLOCK.equals(parent.getType())) {
				dde.setDefinedLocation(DefinedLocation.BEGIN);
				flag = false;
			}
			parent = parent.getParent().orElse(null);
		}
	}

	@Override
	protected AstNodePojo getRootNode() {
		return rootNode;
	}

	@Override
	protected EntityId getModuleId() {
		return module.identity();
	}

}

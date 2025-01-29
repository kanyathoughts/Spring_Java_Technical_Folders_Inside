/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.datadictionary.impl;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.service.MiningDataCoreService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.DefinedLocation;

/**
 * Identifies {@linkplain DataDictionaryPojoPrototype} for Natural Modules.
 */
public class NaturalDataDictionaryIdentifier extends DataDictionaryIdentifier {

	private final ModuleLightweightPojo module;
	private final AstNodePojo rootAstNode;

	/**
	 * The constructor.
	 * 
	 * @param core the {@link MiningDataCoreService}
	 * @param module the Module
	 * @param rootAstNode the root AstNode
	 */
	public NaturalDataDictionaryIdentifier(final MiningDataCoreService core, final ModuleLightweightPojo module, final AstNodePojo rootAstNode) {
		super(core);
		this.module = module;
		this.rootAstNode = rootAstNode;
	}
	
	@Override
	protected void collectLanguageSpecificProperties(final AstNodePojo field, final DataDictionaryPojoPrototype dde) {
		/* For now there is no language specific properties*/
	}

	@Override
	protected  void assignScopesAccessTypes(final AstNodePojo field, final DataDictionaryPojoPrototype dde) {
		/* For now we don't assign scope access types*/
	}

	@Override
	protected void assignScopes(AstNodePojo field, DataDictionaryPojoPrototype dde) {
		/* For now we don't assign scopes*/
	}
	
	@Override
	public void setDefinedLocation(final @Nullable EntityId calleeModuleId, final DataDictionaryPojoPrototype dde, final AstNodePojo node) {
		switch (module.getType()) {
			case SUBPROGRAM: {
				dde.setDefinedLocation(DefinedLocation.SUBPROGRAM);
				break;
			}
			case SUBROUTINE: {
				dde.setDefinedLocation(DefinedLocation.SUBROUTINE);
				break;
			}
			default:{
				dde.setDefinedLocation(DefinedLocation.PROGRAM);
				break;
			}
		}
	}

	@Override
	protected AstNodePojo getRootNode() {
		return rootAstNode;
	}

	@Override
	protected EntityId getModuleId() {
		return module.identity();
	}

}
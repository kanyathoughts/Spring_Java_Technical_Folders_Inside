/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.api;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.shared.entities.ModulePojo;
import innowake.ndt.core.assembling.IAssembling;
import innowake.ndt.core.parsing.ast.AstModel;

/**
 * The model containing the actual parser model and if available its assembling.
 */
public class DefaultModel implements Model {
	
	/**
	 * The actual parse model.
	 */
	public final AstModel parseModel;
	
	/**
	 * The corresponding assembling to the parse model.
	 */
	public final IAssembling<ModulePojo> assembling;
	
	/**
	 * Constructs a {@code Model} given a parse model and its assembling.
	 * 
	 * @param parseModel the parse model
	 * @param assembling the assembling of the model
	 */
	public DefaultModel(final AstModel parseModel, final IAssembling<ModulePojo> assembling) {
		this.parseModel = parseModel;
		this.assembling = Assert.assertNotNull(assembling);
	}
	
	@Override
	public AstModel getParseModel() {
		return parseModel;
	}
	
	@Override
	public Object getAssembling() {
		return assembling;
	}
	
	@Override
	@Nullable
	public String getAssembledContent() {
		return assembling.getAssembledContent();
	}
}

/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core;

import java.util.Optional;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.api.Model;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.Technology;

/**
 * The result object of the 'parse' function.
 */
public class OParseResult {
	
	private final EntityId moduleId;
	private final Optional<Model> model;
	private final Technology technology;
	
	/**
	 * Creates a new parse result.
	 * 
	 * @param moduleId the ID of the module
	 * @param model the parser model
	 * @param technology the technology of the result
	 */
	public OParseResult(final EntityId moduleId, @Nullable final Model model, final Technology technology) {
		this.moduleId = moduleId;
		this.model = Optional.ofNullable(model);
		this.technology = technology;
	}
	
	/**
	 * @return the model
	 */
	public Optional<Model> getModel() {
		return model;
	}
	
	/**
	 * @return the technology
	 */
	public Technology getTechnology() {
		return technology;
	}
	
	/**
	 * @return the module ID
	 */
	public EntityId getModuleId() {
		return moduleId;
	}

}

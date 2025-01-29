/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.taxonomy.assignment;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.TaxonomyPojo;

/**
 * Request model to get the {@linkplain TaxonomyPojo TaxonomyPojos} assigned to a selection of {@linkplain ModulePojo ModulePojos}.
 */
public class TaxonomyAssignmentsGetRequest {
	private ModuleMatcher modules;
	
	/**
	 * Creates an instance of {@linkplain TaxonomyAssignmentsGetRequest}
	 * 
	 * @param modules the {@linkplain ModuleMatcher} instance
	 */
	@JsonCreator
	public TaxonomyAssignmentsGetRequest(@JsonProperty(value = "modules") final ModuleMatcher modules) {
		this.modules = modules;
	}

	/**
	 * Get {@linkplain ModuleMatcher} instance
	 *
	 * @return the {@linkplain ModuleMatcher} instance
	 */
	public ModuleMatcher getModules() {
		return modules;
	}
}

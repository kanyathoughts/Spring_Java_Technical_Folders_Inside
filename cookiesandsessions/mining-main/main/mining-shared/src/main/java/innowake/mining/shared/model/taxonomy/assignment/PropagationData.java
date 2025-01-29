/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.taxonomy.assignment;

import java.io.Serializable;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.mining.shared.access.EntityId;

/**
 * Model Class hold the Module ID and a Set of taxonomy IDs to be assigned to the module
 */
public class PropagationData implements Serializable {
	
	private Long moduleId;
	
	private Set<EntityId> taxonomyIds;

	/**
	 * Constructor take moduleId and Set of taxonomyIds
	 * @param moduleId the module id
	 * @param taxonomyIds the TaxonomyIds for Particular {@link Module}
	 */
	@JsonCreator
	public PropagationData(@JsonProperty("moduleId") final Long moduleId, @JsonProperty("taxonomies") final Set<EntityId> taxonomyIds) {
		this.moduleId = moduleId;
		this.taxonomyIds = taxonomyIds;
	}

	/**
	 * Get the ModuleId
	 *
	 * @return moduleId the module id
	 */
	public Long getModuleId() {
		return moduleId;
	}
	
	/**
	 * Sets the module id.
	 *
	 * @param moduleId the module id
	 */
	public void setModuleId(final Long moduleId) {
		this.moduleId = moduleId;
	}
	
	/**
	 * Get the TaxonomyIds for Particular {@link Module}
	 *
	 * @return taxonomyIds for Particular {@link Module}
	 */
	public Set<EntityId> getTaxonomies() {
		return taxonomyIds;
	}
	
	/**
	 * Sets the TaxonomyIds for Particular {@link Module}
	 *
	 * @param taxonomyIds the List of the TaxonomyIds for module
	 */
	public void setTaxonomies(final Set<EntityId> taxonomyIds) {
		this.taxonomyIds = taxonomyIds;
	}
}

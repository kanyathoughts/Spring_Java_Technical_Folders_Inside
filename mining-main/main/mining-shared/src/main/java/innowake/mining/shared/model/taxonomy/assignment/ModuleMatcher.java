/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.taxonomy.assignment;
 
import java.io.Serializable;
import java.util.Collections;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
 
/**
 * Matches a selection of {@linkplain ModulePojo ModulePojos} to be used for {@linkplain TaxonomyAssignmentsGetRequest} and {@linkplain TaxonomyAssignmentsSetRequest}.
 */
public class ModuleMatcher implements Serializable {
	
	private static final long serialVersionUID = 2843605267234509556L;
	private final List<EntityId> ids;
	private final List<String> pathPatterns;
	
	/**
	 * Creates a new instance of {@linkplain ModuleMatcher} with parameters
	 * 
	 * @param ids the {@linkplain List} of {@linkplain Module} IDs
	 * @param pathPatterns the list of path patterns
	 */
	@JsonCreator
	public ModuleMatcher(@JsonProperty(value = "ids") @Nullable final List<EntityId> ids,
			@JsonProperty(value = "pathPatterns") @Nullable final List<String> pathPatterns) {
		this.ids = ids != null ? ids : Collections.emptyList();
		this.pathPatterns = pathPatterns != null ? pathPatterns : Collections.emptyList();
	}

	/**
	 * Get {@linkplain List} of {@linkplain Module} IDs
	 *
	 * @return {@linkplain List} of {@linkplain Module} IDs
	 */
	public List<EntityId> getIds() {
		return ids;
	}
	
	/**
	 * Get the list of path patterns.
	 *
	 * @return the list of path patterns
	 */
	public List<String> getPathPatterns() {
		return pathPatterns;
	}
	
}

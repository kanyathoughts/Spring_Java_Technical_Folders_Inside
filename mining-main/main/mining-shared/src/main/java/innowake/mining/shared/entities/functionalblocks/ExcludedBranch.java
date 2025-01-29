/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.shared.entities.functionalblocks;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import innowake.mining.shared.model.ModuleLocation;

/**
 * Model class denoting branches that are to be hidden in control flow graphs computed for blocks of type {@link FunctionalBlockType#FUNCTIONAL_GROUP}.
 * A list of these is stored in the {@link FunctionalBlockFlag#FB_EXCLUDED_BRANCHES} flag of the blocks. This class identifies an excluded branch by
 * <ul>
 *     <li>the ModuleLocation of the corresponding branch statement</li>
 *     <li>the label of the branch</li>
 * </ul>
 */
public class ExcludedBranch {

	private final ModuleLocation location;
	private final String branch;

	/**
	 * Create excluded branch
	 * @param location the location of the branch statement
	 * @param branch the label of the excluded branch
	 */
	@JsonCreator
	public ExcludedBranch(@JsonProperty("location") final ModuleLocation location, @JsonProperty("branch") final String branch) {
		this.location = location;
		this.branch = branch;
	}

	/**
	 * Returns the location of the branch statement.
	 * @return the location of the branch statement
	 */
	public ModuleLocation getLocation() {
		return location;
	}

	/**
	 * Returns the label of the excluded branch on the branch statement.
	 * @return the label of the excluded branch
	 */
	public String getBranch() {
		return branch;
	}
}

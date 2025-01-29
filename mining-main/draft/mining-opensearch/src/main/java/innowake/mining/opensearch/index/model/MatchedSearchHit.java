/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.opensearch.index.model;

import java.util.List;

/**
 * Model class for all occurrences found for a particular searchString
 */
public class MatchedSearchHit {
	public List<Integer> matchOffset;
	public String moduleId;
	public String searchString;
	
	public MatchedSearchHit(final List<Integer> matchOffset, final String string, final String searchWord) {
		super();
		this.matchOffset = matchOffset;
		this.moduleId = string;
		this.searchString = searchWord;
	}

	/**
	 * Returns the offset of the match
	 *
	 * @return Returns the offset of the match
	 */
	public List<Integer> getMatchOffset() {
		return matchOffset;
	}
	
	/**
	 * Sets the offset of the match
	 *
	 * @param matchOffset the offset of the match
	 */
	public void setMatchOffset(final List<Integer> matchOffset) {
		this.matchOffset = matchOffset;
	}
	
	/**
	 * Returns the id of the module
	 *
	 * @return Returns the id of the module
	 */
	public String getModuleId() {
		return moduleId;
	}
	
	/**
	 * Sets the id of the module
	 *
	 * @param moduleId the id of the module
	 */
	public void setModuleId(final String moduleId) {
		this.moduleId = moduleId;
	}

	
	/**
	 * Returns the searched String
	 *
	 * @return Returns the searched String
	 */
	public String getSearchString() {
		return searchString;
	}

	
	/**
	 * Sets the searched String
	 *
	 * @param searchString the searched String
	 */
	public void setSearchString(final String searchString) {
		this.searchString = searchString;
	}
	
	

}

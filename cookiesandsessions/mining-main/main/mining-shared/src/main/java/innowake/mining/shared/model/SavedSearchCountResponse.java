/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model;

import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.commons.lang.builder.ToStringStyle;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.SavedSearchPojo;

/**
 * Model class returns saved search with associated data count.
 */
public class SavedSearchCountResponse  {

	@Nullable
	private SavedSearchPojo savedSearch;

	@Nullable
	private Integer count;

	/**
	 * Constructor to make a SavedSearchCountResponse
	 * @param savedSearch of the the {@link SavedSearchPojo}
	 * @param count count of savedSearch
	 */
	@JsonCreator
	public SavedSearchCountResponse(@JsonProperty("savedSearch") final SavedSearchPojo savedSearch, @JsonProperty("count") final Integer count) {
		super();
		this.savedSearch = savedSearch;
		this.count = count;
	}

	/**
	 * Get the SavedSearch
	 *
	 * @return SavedSearch of the the {@link SavedSearchPojo}
	 */
	@Nullable
	public SavedSearchPojo getSavedSearch() {
		return savedSearch;
	}

	/**
	 * Sets the SavedSearch.
	 *
	 * @param savedSearch of the the {@link SavedSearchPojo}
	 */
	public void setSavedSearch(final SavedSearchPojo savedSearch) {
		this.savedSearch = savedSearch;
	}

	/**
	 * Get the count of the SavedSearch
	 *
	 * @return count of the the SavedSearch
	 */
	@Nullable
	public Integer getCount() {
		return count;
	}

	/**
	 * Sets the total number of SavedSearch
	 *
	 * @param count the total number of SavedSearch
	 */
	public void setCount(final Integer count) {
		this.count = count;
	}
	
	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.SHORT_PREFIX_STYLE)
				.append("savedSearch", savedSearch)
				.append("count", count)
				.toString();
	}
}

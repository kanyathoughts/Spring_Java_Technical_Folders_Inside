/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.keycloak.model;

import java.util.Collections;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * POJO to represent paginated content.
 * 
 * @param <T> The type of the content that has been paginated
 */
public class PaginatedResponse<T> {

	private final List<T> contentList;
	private final int totalSize;

	/**
	 * Constructor to instantiate a {@link PaginatedResponse Paginated Response}
	 * for the given content and the total number of records that actually exist.
	 * 
	 * @param contentList The content that is to be paginated
	 * @param totalSize The total number of records that actually exist
	 */
	@JsonCreator
	public PaginatedResponse(
			@JsonProperty("contentList") final List<T> contentList,
			@JsonProperty("totalSize") final int totalSize) {
		this.contentList = contentList;
		this.totalSize = totalSize;
	}

	/**
	 * Gets the content that has been paginated
	 *
	 * @return The paginated content
	 */
	public List<T> getContentList() {
		return Collections.unmodifiableList(contentList);
	}

	/**
	 * Gets the total number of records that actually exist,
	 * not just the size of the content.
	 *
	 * @return The total number of records that actually exist
	 */
	public int getTotalSize() {
		return totalSize;
	}
}

/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.shared.universalsearch;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Domain class for universal search result.
 */
public class UniversalSearchResult {
	
	public static final int RANK_DEFAULT = 0;
	public static final int RANK_TOP = Integer.MAX_VALUE;

	private final String providedBy;
	private final int rank;
	private final String title;
	private final String subTitle;
	private final String context;
	private final String type;
	private final List<UniversalSearchLink> links;
	
	/**
	 * Returns the matching search results with their links.
	 * 
	 * @param providedBy identifier for the type of provider
	 * @param rank the rank of the result
	 * @param title the matching result
	 * @param subTitle the sub title of matching result
	 * @param context the context of the result
	 * @param type type of the result
	 * @param links List of {@link UniversalSearchLink}
	 */
	@JsonCreator
	public UniversalSearchResult(@JsonProperty("providedBy") final String providedBy, @JsonProperty("rank") final int rank,
			@JsonProperty("title") final String title, @JsonProperty("subTitle") final String subTitle, @JsonProperty("context") final String context, 
			@JsonProperty("type") final String type, @JsonProperty("links") final List<UniversalSearchLink> links) {
		this.providedBy = providedBy;
		this.rank = rank;
		this.title = title;
		this.subTitle = subTitle;
		this.context = context;
		this.type = type;
		this.links = links;
	}

	/**
	 * Gets the unique identifier of the provider
	 *
	 * @return the unique identifier of the provider
	 */
	public String getProvidedBy() {
		return providedBy;
	}
	
	/**
	 * Gets the rank of the result.
	 *
	 * @return the rank of the result
	 */
	public int getRank() {
		return rank;
	}
	
	/**
	 * Gets the title of the matched result.
	 *
	 * @return the title of the matched result
	 */
	public String getTitle() {
		return title;
	}
	
	/**
	 * Gets the sub title of the matched result.
	 *
	 * @return the sub title of the matched result
	 */
	public String getSubTitle() {
		return subTitle;
	}

	/**
	 * Gets the context of the matched result.
	 *
	 * @return the context of the matched result
	 */
	public String getContext() {
		return context;
	}

	/**
	 * Gets the type of the result.
	 *
	 * @return the type of the result
	 */
	public String getType() {
		return type;
	}

	/**
	 * Gets the links to matched result.
	 *
	 * @return the links to matched result
	 */
	public List<UniversalSearchLink> getLinks() {
		return links;
	}
	
}

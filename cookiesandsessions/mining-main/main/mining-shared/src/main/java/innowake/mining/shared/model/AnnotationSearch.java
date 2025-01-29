/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.mining.shared.entities.AnnotationPojo;

/**
 * Result of an annotation search.
 */
@JsonIgnoreProperties({"currentSize"})
public class AnnotationSearch {
	
	private final List<AnnotationPojo> elements;
	private final Long overallSize;
	
	/**
	 * Creates a new search result with the given elements and the overall number of elements.
	 * 
	 * @param elements the elements corresponding to the search
	 * @param overallSize the overall number of elements corresponding to the search, this number can differ if the search was size bounded
	 */
	@JsonCreator
	public AnnotationSearch(@JsonProperty("elements") final List<AnnotationPojo> elements, @JsonProperty("overallSize") final Long overallSize) {
		this.elements = elements;
		this.overallSize = overallSize;
	}
	
	/**
	 * @return the number of retrieved elements
	 */
	public int getCurrentSize() {
		return getElements().size();
	}
	
	/**
	 * @return the overall number of elements corresponding to the search, this can differ with the current size if the search was size bounded
	 */
	public Long getOverallSize() {
		return overallSize;
	}
	
	/**
	 * @return {@code true} if there are overall more elements on the server than were retrieved by the search, e.g. when the search was size bounded
	 */
	public boolean hasMore() {
		return getCurrentSize() < overallSize.intValue();
	}

	/**
	 * @return the resulting elements
	 */
	public List<AnnotationPojo> getElements() {
		return elements;
	}

}

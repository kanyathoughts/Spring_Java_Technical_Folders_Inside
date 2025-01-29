/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.repository.query;

import java.util.Arrays;
import java.util.stream.Collectors;

/**
 * The class to represent an individual sorting clause which would be used in the sorting.
 */
public class SortClause {

	private final SortOrder.Direction direction;
	private final String[] properties;
	private String toString = "";

	SortClause(final SortOrder.Direction direction, final String... properties) {
		this.direction = direction;
		this.properties = properties;
	}

	/**
	 * Entity properties used for sorting.
	 * 
	 * @return the properties by which the individual entities would be sorted by
	 */
	public String[] getProperties() {
		return properties;
	}

	public SortOrder.Direction getDirection() {
		return direction;
	}

	/**
	 * Sort clause to be appended to the main query.
	 * 
	 * @return string representation of the sort clause which would be appended to the sql query
	 */
	String asString() {
		if (toString.isEmpty()) {
			toString = Arrays.stream(properties)
					.map(property -> (direction == SortOrder.Direction.ASC) ? property : property + " DESC")
					.collect(Collectors.joining(","));
		}
		return toString;
	}
}

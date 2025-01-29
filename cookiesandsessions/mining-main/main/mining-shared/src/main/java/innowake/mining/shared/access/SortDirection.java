/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.access;

/**
 * Sort direction specification
 */
public enum SortDirection {
	
	ASCENDING(false),
	DESCENDING(true);
	
	private final boolean descending;
	
	/**
	 * Specifies the sort direction
	 * @param descending Reverse order.
	 */
	private SortDirection(final boolean descending) {
		this.descending = descending;
	}
	
	/**
	 * Checks if the sort direction is ascending.
	 * @return Whether to sort form low to high.
	 */
	public boolean isAscending() {
		return ! descending;
	}
	
	/**
	 * Checks if the sort direction is descending.
	 * @return Whether to sort form high to low.
	 */
	public boolean isDescending() {
		return descending;
	}
	
	/**
	 * Derives sort order from vaarious case-insensitive strings. 
	 *
	 * @param direction Name of the sort direction.
	 * @return Sort direction specification
	 * @throws IllegalArgumentException If the direction string is not recognized.
	 */
	public static SortDirection of(final String direction) {
		final String directionUpper = direction.toUpperCase();
		try {
			switch (directionUpper) {
				case "ASC":
					return ASCENDING;
				case "DESC":
					return DESCENDING;
				default:
					return SortDirection.valueOf(directionUpper);
			}
		} catch (Exception e) {
			throw new IllegalArgumentException("Bad sort direction: " + direction, e);
		}
	}
	
}

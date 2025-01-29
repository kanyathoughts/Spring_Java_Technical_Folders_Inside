/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.repository.query;

import org.springframework.data.domain.Sort;

/**
 * Utility class to convert the Sorting configuration to a custom Sort Order instance used to actually
 * perform the sorting.
 */
public class SortingUtils {
	
	private SortingUtils() {}
	
	/**
	 * Extracts the properties and order of sorting by 
	 * those properties to create a custom SortOrder class instance which holds the list of all the sorting clauses.
	 *
	 * @param sort Instance of Sort class which holds the properties and the sorting order
	 * @return the custom SortOrder class instance which holds the list of all sorting clauses
	 */
	public static SortOrder convert(final Sort sort) {
		final SortOrder sortOrder = new SortOrder();
		if (sort != Sort.unsorted()) {
			for (final Sort.Order order : sort) {
				if (order.isAscending()) {
					sortOrder.add(order.getProperty());
				} else {
					sortOrder.add(SortOrder.Direction.DESC, order.getProperty());
				}
			}
		}
		return sortOrder;
	}
}

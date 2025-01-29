/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.datapoints.definition.usages.attributes.general;

import innowake.mining.shared.datapoints.definition.usages.Usages;

/**
 * Usage attributes for {@link Usages#SORT_BY}.
 */
public class SortByAttributes {

	/**
	 * SQL Fragment for the projection that is used in an ORDER BY clause for this data point.
	 */
	public static final String SQL_FRAGMENT_ORDER_BY = "sqlFragmentOrderBy";

	private SortByAttributes() { }
}

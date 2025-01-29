/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.datapoints.definition.usages.attributes.miningui;

import innowake.mining.shared.datapoints.definition.usages.Usages;

/**
 * Usage attributes for {@link Usages#MINING_UI_MODULES_TABLE} and {@link Usages#MINING_UI_ANNOTATIONS_TABLE}.
 */
public class TableAttributes {
	
	/**
	 * Determines the category under which a data point is shown in the column selection menu.
	 */
	public static final String CATEGORY = "category";
	
	/**
	 * Attribute determining that a data point should be included in the default set of columns for a table and its default column position.
	 * <p>
	 * It is set to a number value indicating the default position of the column. Columns should be ordered by this value in ascending order,
	 * with higher numbers appearing further to the right (in left-to-right layouts). The column numbers for a table should start at 0.
	 * <p>
	 * If this attribute is absent or set to an empty value, then the data point is not included in the default set of columns.
	 */
	public static final String DEFAULT_COLUMN_INDEX = "defaultColumnIndex";

	/**
	 * Attribute determining that a data point should be hidden from the customizable table unless the user manually selects it.
	 * <p>
	 *     If this attribute is present and set to a true value, the data point is hidden from the customizable table by default.
	 */
	public static final String HIDDEN_BY_DEFAULT = "hiddenByDefault";
}

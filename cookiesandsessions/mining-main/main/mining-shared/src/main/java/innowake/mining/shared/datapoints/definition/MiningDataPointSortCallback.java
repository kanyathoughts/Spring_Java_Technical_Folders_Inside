/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.datapoints.definition;

import innowake.mining.shared.access.SortDirection;

/**
 * A callback that is used to apply a sort operation on a data point.
 * @param <B> the type of the builder (or other object) received by the callback on which the sort operation is applied
 */
public interface MiningDataPointSortCallback<B> {

	/**
	 * Applies a sort operation using a provided {@code orderBuilder} and {@code sortDirection}.
	 * @param orderBuilder the builder (or other object) on which the sort operation is applied
	 * @param sortDirection the sort direction (ascending or descending)
	 */
	void apply(B orderBuilder, SortDirection sortDirection);
}

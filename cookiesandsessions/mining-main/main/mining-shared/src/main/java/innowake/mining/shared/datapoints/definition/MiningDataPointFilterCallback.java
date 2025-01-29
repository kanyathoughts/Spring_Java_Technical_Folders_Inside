/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.datapoints.definition;

/**
 * A callback that is used to apply a filter operation on a data point.
 * @param <B> the type of the builder (or other object) received by the callback on which the filter operation is applied
 * @param <T> the type of the filter value
 */
public interface MiningDataPointFilterCallback<B, T> {

	/**
	 * Applies a filter operation using a provided {@code inquiryBuilder} and {@code filterValue}.
	 * @param inquiryBuilder the builder (or other object) on which the filter operation is applied
	 * @param filterValue the value to filter by
	 */
	void apply(B inquiryBuilder, T filterValue);
}

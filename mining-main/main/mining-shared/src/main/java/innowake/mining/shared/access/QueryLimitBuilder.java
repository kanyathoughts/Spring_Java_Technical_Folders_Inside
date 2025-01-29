/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.access;

/**
 * Query limit builder for limiting the maximum amount of to be fetched entities when performing queries.
 * @param <B> type of query builder this limit builder is used for
 */
public interface QueryLimitBuilder<B> {

	/**
	 * Sets the limit for the to be fetched entities when performing queries.
	 *
	 * @param limit the limit, &gt 0
	 * @return the query builder for consecutive method calls
	 */
	B limit(int limit);
}

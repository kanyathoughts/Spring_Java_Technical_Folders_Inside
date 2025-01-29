/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.datapoints.builder;

import innowake.mining.data.datapoints.MiningDataPointBuilder;
import innowake.mining.data.datapoints.MiningDataPointBuilder.DataPointBuilder;

/**
 * Implementation of {@link DataPointBuilder} for queries.
 */
class QueryBuilderImpl extends DataPointBuilderImpl {

	/**
	 * Create new QueryBuilder. The defined query will be registered on the {@code parentBuilder}. The parent builder instance
	 * is also returned from {@link #add()}.
	 * 
	 * @param parentBuilder the parent builder
	 * @param name name of the query to define
	 */
	QueryBuilderImpl(final MiningDataPointBuilderImpl parentBuilder, final String name) {
		super(parentBuilder, name, "Query");
	}

	@Override
	public MiningDataPointBuilder add() {
		parentBuilder.addQueryDefinition(createDataPoint());
		return parentBuilder;
	}

}

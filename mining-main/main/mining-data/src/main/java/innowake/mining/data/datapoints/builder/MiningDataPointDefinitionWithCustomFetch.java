/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.datapoints.builder;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import graphql.schema.DataFetcher;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.datapoints.MiningDataPointBuilder.DynamicFieldBuilder;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;
import innowake.mining.shared.datapoints.definition.MiningDataPointFilterCallbacks;
import innowake.mining.shared.datapoints.definition.MiningDataPointSortCallback;

/**
 * A {@link MiningDataPointDefinition} which also allows to define custom fetch logic.
 */
/* This class extends the class from mining-shared and adds the customFetch property. That's because the DataFetcher type is not available in mining-shared */
public class MiningDataPointDefinitionWithCustomFetch extends MiningDataPointDefinition {

	@Nullable
	private DataFetcher<?> customFetch;
	private Map<String, DynamicFieldBuilder<?>> dynamicFieldBuilders = new HashMap<>();
	
	public MiningDataPointDefinitionWithCustomFetch(final MiningDataPointDefinition def) {
		this(def, null, Collections.emptyMap());
	}
	
	public MiningDataPointDefinitionWithCustomFetch(final MiningDataPointDefinition def,
			@Nullable final DataFetcher<?> customFetch, Map<String, DynamicFieldBuilder<?>> dynamicFieldBuilders) {
		super(def);
		this.customFetch = customFetch;
		this.dynamicFieldBuilders = dynamicFieldBuilders;
	}

	/**
	 * Sets the custom data fetcher for this data point.
	 *
	 * @param customFetch the data fetcher
	 */
	public void setCustomFetch(DataFetcher<?> customFetch) {
		this.customFetch = customFetch;
	}

	/**
	 * Gets the custom data fetcher defined for this data point.
	 *
	 * @return the data fetcher or {@code null} if none was defined
	 */
	@Nullable
	public DataFetcher<?> getCustomFetch() {
		return customFetch;
	}
	
	/**
	 * INTERNAL
	 * 
	 * For copying the filter callbacks onto the builder
	 * 
	 * @return the filterCallbacks
	 */
	Map<String, MiningDataPointFilterCallbacks> getFilterCallbacks() {
		return filterCallbacks;
	}

	/**
	 * INTERNAL
	 * 
	 * For copying the sort callbacks onto the builder
	 * 
	 * @return the sortCallbacks
	 */
	Map<String, MiningDataPointSortCallback<?>> getSortCallbacks() {
		return sortCallbacks;
	}
	
	/**
	 * @param <T> Type of the data access builder.
	 * @param queryName Name of the GraphQL query.
	 * @return Dynamic builder for acquiring the value at this data-point during query execution, if defined.
	 */
	@SuppressWarnings("unchecked")
	public <T> Optional<DynamicFieldBuilder<T>> getDynamicFieldBuilder(final String queryName) {
		return Optional.ofNullable((DynamicFieldBuilder<T>) dynamicFieldBuilders.get(queryName));
	}
}

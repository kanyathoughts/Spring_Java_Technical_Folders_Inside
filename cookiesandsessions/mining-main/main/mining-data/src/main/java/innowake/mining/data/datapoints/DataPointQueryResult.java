/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.datapoints;

import graphql.GraphQLError;
import org.apache.commons.lang3.StringUtils;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Holds the result of a data point query.
 */
public class DataPointQueryResult {

	private final List<GraphQLError> errors;
	
	private final Object data;

	public DataPointQueryResult(final List<GraphQLError> errors, final Object data) {
		this.errors = errors;
		this.data = data;
	}
	
	/**
	 * Returns the list of errors. It's empty if there were no errors.
	 *
	 * @return the list of errors
	 */
	public List<GraphQLError> getErrors() {
		return errors;
	}
	
	/**
	 * Returns the raw query result as a map.
	 *
	 * @return a map of the query results
	 */
	public Object getData() {
		return data;
	}
	
	/**
	 * Retrieves a data point from the query result. You must give the path to the data point from the root of the query result.
	 * <p>
	 * Note that this method always returns a {@link DataPointSelection}, even if the requested data point does not exist. However,
	 * subsequently trying to retrieve its {@linkplain DataPointSelection#getValue(int...) value} will yield {@code null}.
	 * <p>
	 * For convenience the path can be given either as a dot-separated string or as a list of strings:
	 * <pre>
	 * // these are equivalent: 
	 * getDataPoint("foo.bar.baz");
	 * getDataPoint("foo", "bar", "baz");
	 * </pre>
	 *
	 * @param path the path to the data point 
	 * @return a {@link DataPointSelection} object from which the value can be retrieved
	 */
	public DataPointSelection getDataPoint(final String... path) {
		final List<String> completePath = Arrays.stream(path)
				.flatMap(p -> Arrays.stream(p.split("\\.")))
				.filter(StringUtils::isNotEmpty)
				.collect(Collectors.toList());
		return new DataPointSelection(data, completePath);
	}

}

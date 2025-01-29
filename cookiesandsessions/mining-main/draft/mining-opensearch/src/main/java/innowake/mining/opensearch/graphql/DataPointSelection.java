/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.opensearch.graphql;

import innowake.lib.core.api.lang.Nullable;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Contains the value of a data point, selected from a query.
 * <p>
 * The data point can either be a single value or an array of values or even a multi-dimensional array of values.
 */
public class DataPointSelection {

	@Nullable
	private final Object data;
	private int dimensions;
	
	DataPointSelection(final Object data, final List<String> path) {
		this.data = getDataRecursively(data, path);
	}
	
	/**
	 * Retrieve the value of the data point, optionally resolving a number of indexes.
	 * <p>
	 * If the number of indexes provided is less that {@linkplain #getDimensions() the number of dimensions} then this
	 * method will return a {@link List}. Otherwise this method will return an Object of whatever type the data point has.
	 * <p>
	 * If the data point could not be found or if the provided index is out of range, this method returns {@code null}.
	 *
	 * @param <T> you decide what type the return value is; do keep in mind that this method will always return {@link List} (or {@code null})
	 *            if you provide less indexes than the number of dimensions
	 * @param indexes a list of indexes to resolve
	 * @return the resolved value
	 */
	@SuppressWarnings("unchecked")
	@Nullable
	public <T> T getValue(final int... indexes) {
		if (indexes.length > dimensions) {
			throw new IllegalArgumentException("found " + indexes.length + " indexes but the result only has " + dimensions + " dimensions");
		}
		Object ret = data;
		for (final int index : indexes) {
			if ( ! (ret instanceof List)) {
				return null;
			}
			final List<?> list = (List<?>) ret;
			if (list.size() <= index) {
				return null;
			}
			ret = list.get(index);
		}
		return (T) ret;
	}
	
	/**
	 * Get the number of array dimensions of this data point. If the data point represents a single value, this will return {@code 0}.
	 *
	 * @return the number of array dimensions
	 */
	public int getDimensions() {
		return dimensions;
	}
	
	@Nullable
	private Object getDataRecursively(final Object data, final List<String> path) {
		if (data == null) {
			return null;
		}
		if (data instanceof List) {
			dimensions++;
			return ((List<?>) data).stream()
					.map(item -> getDataRecursively(item, path))
					.collect(Collectors.toList());
		}
		if (path.isEmpty()) {
			return data;
		}
		if (data instanceof Map) {
			@SuppressWarnings("unchecked")
			final Map<String, Object> map = (Map<String, Object>) data;
			final Object obj = map.get(path.get(0));
			return getDataRecursively(obj, path.subList(1, path.size()));
		} else {
			throw new IllegalStateException("While extracting data from a data point query:" +
					" there are remaining path segments " + path + " for the data point so query data is expected to be either List or Map," +
					" but it was " + data.getClass());
		}
	}
}

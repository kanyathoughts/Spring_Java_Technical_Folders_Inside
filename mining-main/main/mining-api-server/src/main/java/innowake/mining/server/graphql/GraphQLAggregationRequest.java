/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.graphql;

import graphql.schema.SelectedField;
import innowake.mining.data.datapoints.MiningDataPointBuilder;
import innowake.mining.shared.model.aggregations.AggregationOperator;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Utility methods for working with {@link AggregationRequest} and {@link AggregationResult} in GraphQL.
 * @see MiningDataPointBuilder#defineAggregations(String, String, Class)
 */
public class GraphQLAggregationRequest {

	private GraphQLAggregationRequest() {
		/* utility class */
	}

	/**
	 * Converts a list of GraphQL field selections to {@link AggregationRequest}. The field selections need to be sub-selections of a data point
	 * defined by {@link MiningDataPointBuilder#defineAggregations(String, String, Class)}.
	 *
	 * @param selectedFields list of selected sub-fields of the "aggregations" field
	 * @param fieldNameClass the "field name" enum used when defining the aggregations
	 * @return the parsed AggregationRequest
	 * @param <F> field name enum type
	 */
	public static <F extends Enum<F>> AggregationRequest<F> toAggregationRequest(final List<SelectedField> selectedFields, final Class<F> fieldNameClass) {
		final Set<F> groupBy = new HashSet<>();
		final Map<F, AggregationOperator> fields = new HashMap<>();

		for (final SelectedField selectedField : selectedFields) {
			final String[] split = selectedField.getQualifiedName().split("/");
			if (split.length == 2 && "groupBy".equals(split[0])) {
				groupBy.add(Enum.valueOf(fieldNameClass, split[1]));
			} else if (split.length == 3 && "fields".equals(split[0])) {
				final F key = Enum.valueOf(fieldNameClass, split[1]);
				final AggregationOperator value = AggregationOperator.valueOf(split[2]);
				if (fields.containsKey(key)) {
					/* this is currently a limitation, because AggregationRequest only allows to define one operator for each field */
					throw new IllegalArgumentException("Field " + key + " is already used with aggregation operator " + fields.get(key) +
							". Can't also aggregate by " + value +
							": At the moment, you can not apply multiple different aggregation operators to the same field within the same aggregation." +
							" Please select multiple separate aggregations instead.");
				}
				fields.put(key, value);
			}
		}
		final AggregationRequest<F> aggregationRequest = new AggregationRequest<>();
		aggregationRequest.setFields(fields);
		aggregationRequest.setGroupBy(groupBy);
		return aggregationRequest;
	}

	/**
	 * Converts any {@link AggregationResult} to {@code AggregationResult<String>}. This is required or else GraphQL can not retrieve the keys - it always
	 * passes the keys as Strings.
	 * @param aggregationResult any aggregation result
	 * @return a new AggregationResult with all keys converted to Strings
	 */
	public static AggregationResult<String> toAggregationResultString(final AggregationResult<?> aggregationResult) {
		final AggregationResult<String> stringResult = new AggregationResult<>();

		stringResult.setGroup(keysToString(aggregationResult.getGroup()));
		stringResult.setFields(keysToString(aggregationResult.getFields()));

		return stringResult;
	}

	private static Map<String, Object> keysToString(final Map<?, ?> map) {
		final Map<String, Object> ret = new HashMap<>(map.size());

		for (final Map.Entry<?, ?> entry : map.entrySet()) {
			ret.put(entry.getKey().toString(), entry.getValue());
		}

		return ret;
	}
}

/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model.aggregations;

import java.util.function.Function;

/**
 * Operators to use when aggregating fields.
 * Unfortunately OrientDB does not support count(distinct()), see https://github.com/orientechnologies/orientdb/issues/9112
 */
public enum AggregationOperator {
	MIN((fieldName) ->		String.format("min(%s)", fieldName)),
	MAX((fieldName) ->		String.format("max(%s)", fieldName)),
	SUM((fieldName) ->		String.format("sum(%s)", fieldName)),
	AVG((fieldName) ->		String.format("avg(%s)", fieldName)),
	LIST((fieldName) ->		String.format("list(%s)", fieldName)),
	COUNT((fieldName) ->		String.format("count(%s)", fieldName)),
	COUNT_DISTINCT((fieldName) -> String.format("set(%s)", fieldName)),
	MODE((fieldName) ->		String.format("mode(%s)", fieldName)),
	MEDIAN((fieldName) ->		String.format("median(%s)", fieldName)),
	VARIANCE((fieldName) ->		String.format("variance(%s)", fieldName)),
	STDDEV((fieldName) ->		String.format("stddev(%s)", fieldName)),
	PERCENTILE_70((fieldName) ->	String.format("percentile(%s, 0.7)", fieldName)),
	PERCENTILE_80((fieldName) ->	String.format("percentile(%s, 0.8)", fieldName)),
	PERCENTILE_90((fieldName) ->	String.format("percentile(%s, 0.9)", fieldName)),
	PERCENTILE_95((fieldName) ->	String.format("percentile(%s, 0.95)", fieldName)),
	PERCENTILE_99((fieldName) ->	String.format("percentile(%s, 0.99)", fieldName));

	private final Function<String, String> toSqlFunction;

	private AggregationOperator(final Function<String, String> toSqlFunction) {
		this.toSqlFunction = toSqlFunction;
	}

	/**
	 * 
	 * Apply the function given in through the constructor
	 *
	 * @param fieldName field on which the function is applied
	 * @return output of the toSqlFunction
	 */
	public String toSql(final String fieldName) {
		return toSqlFunction.apply(fieldName);
	}
}

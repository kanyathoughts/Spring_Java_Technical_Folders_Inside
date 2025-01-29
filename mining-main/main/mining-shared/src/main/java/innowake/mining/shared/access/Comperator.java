/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.access;

import innowake.mining.shared.datapoints.definition.filters.FilterOperators;

/**
 * Basic comparison operators.
 */
public enum Comperator {
	
	EQUAL("="),
	UNEQUAL("<>"),
	GREATER(">"),
	GREATER_OR_EQUAL(">="),
	LESSER("<"),
	LESSER_OR_EQUAL("<=");
	
	private final String operator;
	
	private Comperator(final String operator) {
		this.operator = operator;
	}
	
	public String operator() {
		return operator;
	}

	/**
	 * Converts the given filter operator ({@linkplain FilterOperators} to a {@link Comperator}.
	 *
	 * @param filterOperator the string filter operator
	 * @return the matching Comperator
	 * @throws IllegalArgumentException if the given filter operator is not handled yet
	 */
	public static Comperator convert(final String filterOperator) {
		switch (filterOperator) {
			case FilterOperators.OPERATOR_EQ:
				return EQUAL;
			case FilterOperators.OPERATOR_NOT_EQ:
				return UNEQUAL;
			case FilterOperators.OPERATOR_GTE:
				return GREATER_OR_EQUAL;
			case FilterOperators.OPERATOR_GT:
				return GREATER;
			case FilterOperators.OPERATOR_LTE:
				return LESSER_OR_EQUAL;
			case FilterOperators.OPERATOR_LT:
				return LESSER;
			default:
				throw new IllegalArgumentException("Unknown filter operator: " + filterOperator);
		}
	}
}

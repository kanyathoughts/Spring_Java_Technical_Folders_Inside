/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.controller.rsql;

import cz.jirutka.rsql.parser.ast.ComparisonOperator;
import cz.jirutka.rsql.parser.ast.RSQLOperators;

/**
 * Enum that maps the internal {@link RSQLOperators}.
 */
public enum RsqlSearchOperation {
	
	EQUAL(RSQLOperators.EQUAL),
	NOT_EQUAL(RSQLOperators.NOT_EQUAL),
	GREATER_THAN(RSQLOperators.GREATER_THAN),
	GREATER_THAN_OR_EQUAL(RSQLOperators.GREATER_THAN_OR_EQUAL),
	LESS_THAN(RSQLOperators.LESS_THAN),
	LESS_THAN_OR_EQUAL(RSQLOperators.LESS_THAN_OR_EQUAL),
	IN(RSQLOperators.IN),
	NOT_IN(RSQLOperators.NOT_IN),
	IN_SIZE(new ComparisonOperator("=inSize=", false)),
	IGNORE_CASE(new ComparisonOperator("=ignoreCase=", false)),
	CONTAINS_ANY(new ComparisonOperator("=containsAny=", true)),
	CONTAINS_TEXT(new ComparisonOperator("=containsText=", true)),
	LENGTH(new ComparisonOperator("=length=", false)),
	LENGTH_GT(new ComparisonOperator("=lengthGt=", false)),
	LENGTH_GE(new ComparisonOperator("=lengthGe=", false)),
	LENGTH_LT(new ComparisonOperator("=lengthLt=", false)),
	LENGTH_LE(new ComparisonOperator("=lengthLe=", false));
	
	private final ComparisonOperator operator;
	
	/**
	 * Resolves the {@link RsqlSearchOperation} based on the provided RSQL {@link ComparisonOperator}.
	 * 
	 * @param operator the {@link ComparisonOperator}
	 * @return the {@link RsqlSearchOperation}
	 */
	public static RsqlSearchOperation fromOperator(final ComparisonOperator operator) {
		for (RsqlSearchOperation operation : values()) {
			if (operation.operator.equals(operator)) {
				return operation;
			}
		}
		throw new IllegalArgumentException("Unsupported RSQL operator: " + operator);
	}
	
	private RsqlSearchOperation(final ComparisonOperator operator) {
		this.operator = operator;
	}

	/**
	 * Returns the comparison operator.
	 *
	 * @return the underlying {@link ComparisonOperator Comparison Operator} for RsqlSearchOperation
	 */
	public ComparisonOperator getOperator() {
		return operator;
	}
}

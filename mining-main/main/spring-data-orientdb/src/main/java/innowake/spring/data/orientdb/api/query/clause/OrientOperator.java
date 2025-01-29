/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.api.query.clause;

import com.github.raymanrt.orientqb.query.Operator;

import innowake.spring.data.orientdb.commons.exception.UnsupportedQueryTypeException;

/**
 * Defines the operators using for building clauses.
 */
public enum OrientOperator {

	/**
	 * Equals.
	 */
	EQ,
	/**
	 * Not equals.
	 */
	NE,
	/**
	 * Less than.
	 */
	LT,
	/**
	 * Less than equals.
	 */
	LE,
	/**
	 * Greater than.
	 */
	GT,
	/**
	 * Greater than equals.
	 */
	GE,
	/**
	 * Like.
	 */
	LIKE,
	/**
	 * Instance of.
	 */
	INSTANCEOF,
	/**
	 * In.
	 */
	IN,
	/**
	 * Contains.
	 */
	CONTAINS,
	/**
	 * Contains key.
	 */
	CONTAINS_KEY,
	/**
	 * Contains value.
	 */
	CONTAINS_VALUE,
	/**
	 * Contains text.
	 */
	CONTAINS_TEXT,
	/**
	 * Matches.
	 */
	MATCHES,
	/**
	 * With in.
	 */
	WITHIN,
	/**
	 * Between.
	 */
	BETWEEN,
	/**
	 * Field.
	 */
	FIELD,
	/**
	 * Not.
	 */
	NOT,
	/**
	 * Not without parenthesis.
	 */
	NOT_WITHOUT_PARENTHESIS,
	/**
	 * Defined
	 */
	DEFINED,
	/**
	 * Not defined.
	 */
	NOT_DEFINED,
	/**
	 *  Null.
	 */
	NULL,
	/**
	 * Not Null.
	 */
	NOT_NULL;

	/**
	 * Returns the equivalent {@link Operator} type.
	 *
	 * @return the equivalent {@link Operator} type
	 */
	public Operator getOperator() {
		switch (this) {
			case EQ:
				return Operator.EQ;
			case NE:
				return Operator.NE;
			case LT:
				return Operator.LT;
			case LE:
				return Operator.LE;
			case GT:
				return Operator.GT;
			case GE:
				return Operator.GE;
			case LIKE:
				return Operator.LIKE;
			case INSTANCEOF:
				return Operator.INSTANCEOF;
			case IN:
				return Operator.IN;
			case CONTAINS:
				return Operator.CONTAINS;
			case CONTAINS_KEY:
				return Operator.CONTAINS_KEY;
			case CONTAINS_VALUE:
				return Operator.CONTAINS_VALUE;
			case CONTAINS_TEXT:
				return Operator.CONTAINS_TEXT;
			case MATCHES:
				return Operator.MATCHES;
			case WITHIN:
				return Operator.WITHIN;
			case BETWEEN:
				return Operator.BETWEEN;
			case FIELD:
				return Operator.FIELD;
			case NOT:
				return Operator.NOT;
			case NOT_WITHOUT_PARENTHESIS:
				return Operator.NOT_WITHOUT_PARENTHESIS;
			case DEFINED:
				return Operator.DEFINED;
			case NOT_DEFINED:
				return Operator.NOT_DEFINED;
			case NULL:
				return Operator.NULL;
			case NOT_NULL:
				return Operator.NOT_NULL;
			default:
				throw new UnsupportedQueryTypeException("Not a valid operator");
		}
	}

}

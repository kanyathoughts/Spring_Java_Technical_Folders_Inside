package innowake.mining.shared.datapoints.definition.filters;

/**
 * Defines the operators that can be used in a filter object.
 */
public class FilterOperators {

	/**
	 * Operator for equality comparison.
	 */
	public static final String OPERATOR_EQ = "eq";

	/**
	 * Operator for inequality comparison.
	 */
	public static final String OPERATOR_NOT_EQ = "notEq";

	/**
	 * Operator for "is null" comparison.
	 */
	public static final String OPERATOR_IS = "is";

	/**
	 * Operator for "is true" comparison.
	 */
	public static final String OPERATOR_IS_TRUE = "isTrue";

	/**
	 * Operator for "is false" comparison.
	 */
	public static final String OPERATOR_IS_FALSE = "isFalse";

	/**
	 * Operator for "is absent" comparison.
	 */
	public static final String OPERATOR_IS_ABSENT = "isAbsent";

	/**
	 * Operator for "is greater than or equal" comparison.
	 */
	public static final String OPERATOR_GTE = "gte";

	/**
	 * Operator for "is greater than or equal" comparison.
	 */
	public static final String OPERATOR_GT = "gt";

	/**
	 * Operator for "is less than or equal" comparison.
	 */
	public static final String OPERATOR_LTE = "lte";

	/**
	 * Operator for "is less than" comparison.
	 */
	public static final String OPERATOR_LT = "lt";

	/**
	 * Operator for "in" comparison.
	 */
	public static final String OPERATOR_IN = "in";

	/**
	 * Operator for "not in" comparison.
	 */
	public static final String OPERATOR_NOT_IN = "notIn";

	private FilterOperators() {
		// prevent instantiation
	}
}

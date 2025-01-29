/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.base;

/**
 * Utility methods for math.
 */
public final class MathUtil {

	private MathUtil() {}

	/**
	 * Compares a given {@code value} with {@code lower} and {@code upper} and returns {@code true} if the value is between or equal these.
	 *
	 * @param value the value to compare
	 * @param lower the lower bound to compare
	 * @param upper the upper bound to compare
	 * @return {@code true} if the value is between or equal
	 */
	public static boolean isBetweenOrEqual(final int value, final int lower, final int upper) {
		return value >= lower && value <= upper;
	}
}

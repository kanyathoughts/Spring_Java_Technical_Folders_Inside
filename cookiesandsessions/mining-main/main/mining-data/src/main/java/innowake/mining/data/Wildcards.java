/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data;

/**
 * Wildcard related utility methods.
 */
final class Wildcards {

	private Wildcards() {}
	
	/**
	 * Turns a wildcard expression with ? for any single character and * for any string into a corresponding regular expression.
	 * This regular expression is case insensitive.
	 *
	 * @param wildcardString the wildcard string
	 * @return the corresponding regular expression
	 */
	static String toRegEx(final String wildcardString) {
		final StringBuilder regEx = new StringBuilder("(?i)");
		boolean escaping = false;
		for (final char symbol : wildcardString.toCharArray()) {
			if (symbol == '*') {
				if (escaping) {
					regEx.append("\\E");
					escaping = false;
				}
				regEx.append(".*");
			} else if (symbol == '?') {
				if (escaping) {
					regEx.append("\\E");
					escaping = false;
				}
				regEx.append(".");
			} else {
				if ( ! escaping) {
					regEx.append("\\Q");
					escaping = true;
				}
				regEx.append(symbol);
			}
		}
		if (escaping) {
			regEx.append("\\E");
		}
		return regEx.toString();
	}
	
}

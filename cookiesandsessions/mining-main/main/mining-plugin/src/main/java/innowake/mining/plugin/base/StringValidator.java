/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.base;

/**
 * Validates a string against a wildcard pattern.
 */
public final class StringValidator {

	private StringValidator() {}
	
	/**
	 * Validates a string against a wildcard pattern:
	 * <p>
	 * Supporting ? for exactly one character or * for an arbitrary number of characters.
	 * 
	 * @param text text to test
	 * @param pattern wildcard pattern to test
	 * @return {@code true} if the text matches the wildcard pattern
	 */
	public static boolean match(final String text, final String pattern) {
	  return text.matches(pattern.replace("?", ".").replace("*", ".*?"));
	}
}

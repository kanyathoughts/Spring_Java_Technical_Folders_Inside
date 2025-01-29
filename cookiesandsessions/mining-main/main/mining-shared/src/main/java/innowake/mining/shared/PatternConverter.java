/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.shared;

/**
 * Helper functions for converting patterns.
 */
public final class PatternConverter {
	
	private PatternConverter() {}
	
	/**
	 * Converts the Ant pattern notation to Regex pattern notation.
	 *
	 * @param antPattern The Ant pattern.
	 * @return The Regex pattern.
	 */
	public static String convertAntToRegexPattern(final String antPattern) {
		final StringBuilder str = new StringBuilder(antPattern);
		final char dot = '.';
		final char questionMark = '?';
		final char asterisk = '*';
		final char forwardSlash = '/';

		int index = 0;
		while (index < str.length()) {
			final char chr = str.charAt(index);

			switch (chr) {
				case dot:
					/* dot character needs to be escaped in regex */
					str.replace(index, index + 1, "\\.");
					index = index + 3;
					break;

				case questionMark:
					/* question mark is replaced by dot, so it matches a single character */
					str.replace(index, index + 1, ".");
					index = index + 1;
					break;

				case asterisk:
					if (isSameChar(str, index + 1, asterisk)) {
						if (isSameChar(str, index + 2, forwardSlash)) {
							/* '**&#47;' is replaced by '.*' so it matches zero or more directory segments */
							str.replace(index, index + 3, ".*");
						} else {
							/* '**' is replaced by '.*' so it matches zero or more directory segments */
							str.replace(index, index + 2, ".*");
						}
						index = index + 2;
					} else {
						/* single asterisk ("*") is replaced by "[^/]*" so it matches all character but not path separators
						 * - i.e. it matches file names but not folders */
						str.replace(index, index + 1, "[^/]*");
						index = index + 6;
					}
					break;
				default : index ++;
			}
		}

		return str.toString();
	}
	
	/**
	 * Compares if the character matches with the character in the string at the specified index.
	 *
	 * @param str The StringBuilder object,
	 * @param index The index position to compare the give character.
	 * @param chr The Character for comparison.
	 * @return {@code true} if the character matches.
	 */
	private static boolean isSameChar(final StringBuilder str, final int index, final char chr) {
		return str.length() > index && str.charAt(index) == chr;
	}

}

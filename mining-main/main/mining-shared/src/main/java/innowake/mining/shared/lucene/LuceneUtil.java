/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.shared.lucene;

/**
 * Utility class for Lucene full text search.
 */
public class LuceneUtil {

	private LuceneUtil() {
		throw new UnsupportedOperationException();
	}

	/**
	 * <p>Escapes characters in the given {@code searchTerm} that are part of Lucene's query syntax adjusted for Orient DB.</p>
	 * 
	 * Adapted code from Lucene's QueryParser. Unlike the original code, this method does not escape the special character '*' in order to allow it to be used
	 * as a wildcard in the given {@code searchTerm}.
	 * 
	 * <p>If the escaped {@code searchTerm} is set directly into the query string then then {@code escapeTwice} must be {@code true} to avoid parser errors in
	 * OrientDB. If placeholders are used for {@code searchTerm} then {@code escapeTwice} must be {@code false}.</p>
	 * 
	 * @param searchTerm the search term to escape
	 * @param escapeTwice {@code true} to escape twice. Otherwise {@code false}.
	 * @return the search term with characters that are part of Lucene's query syntax escaped
	 */
	public static String escapeSearchTerm(final String searchTerm, final boolean escapeTwice) {
		final StringBuilder sb = new StringBuilder(searchTerm.length() + 8);
		for (int i = 0; i < searchTerm.length(); i++) {
			final char c = searchTerm.charAt(i);
			/* These characters are part of the query syntax and must be escaped. The special character '*' is not escaped. */
			switch(c) {
				case '\\':
				case '+':
				case '-':
				case '!':
				case '(':
				case ')':
				case ':':
				case '^':
				case '[':
				case ']':
				case '\"':
				case '{':
				case '}':
				case '~':
				case '?': 
				case '|':
				case '&':
				case '/':
					if (escapeTwice) {
						sb.append("\\\\");
					} else {
						sb.append('\\');
					}
					break;
				case '*':
				default:
					/* No escaping required */
					break;
			}
			sb.append(c);
		}

		return sb.toString();
	}
}

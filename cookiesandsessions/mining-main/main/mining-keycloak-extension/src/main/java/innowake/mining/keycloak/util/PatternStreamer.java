/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.keycloak.util;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.MatchResult;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * Utility class for matching pattern.
 */
public final class PatternStreamer {

	private final Pattern pattern;

	/**
	 * Constructor to compile the regular expression.
	 * 
	 * @param regex the regular expression
	 */
	public PatternStreamer(final String regex) {
		pattern = Pattern.compile(regex);
	}

	/**
	 * Matches the pattern with given input and and returns a stream of match results.
	 *
	 * @param input the input
	 * @return stream of matched values.
	 */
	public Stream<MatchResult> results(final CharSequence input) {
		final List<MatchResult> list = new ArrayList<>();
		for (final Matcher matcher = pattern.matcher(input); matcher.find();) {
			list.add(matcher.toMatchResult());
		}
		return list.stream();
	}
	
	/**
	 * Returns the pattern.
	 *
	 * @return the pattern
	 */
	public Pattern getPattern() {
		return this.pattern;
	}
}

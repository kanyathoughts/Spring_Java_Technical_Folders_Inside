/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.test.lucene;

import static org.junit.jupiter.api.Assertions.assertEquals;
import java.util.stream.Stream;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import innowake.mining.shared.lucene.LuceneUtil;

/**
 * Tests the {@link LuceneUtil}.
 */
@TestInstance(Lifecycle.PER_CLASS)
class LuceneUtilTest {

	/**
	 * @return stream of {@link Arguments} for parameterized test method {@code testEscapeSearchTerm(String, String, Boolean)}.
	 */
	public Stream<Arguments> escapeSearchTermTestCases() {
		return Stream.of(
				Arguments.arguments("\\\\", "\\\\\\\\\\\\", Boolean.TRUE),
				Arguments.arguments("\\\\", "\\\\\\\\", Boolean.FALSE),
				Arguments.arguments("+-!():^[]\"{}~?|&/*", "\\\\+\\\\-\\\\!\\\\(\\\\)\\\\:\\\\^\\\\[\\\\]\\\\\"\\\\{\\\\}\\\\~\\\\?\\\\|\\\\&\\\\/*", Boolean.TRUE),
				Arguments.arguments("+-!():^[]\"{}~?|&/*", "\\+\\-\\!\\(\\)\\:\\^\\[\\]\\\"\\{\\}\\~\\?\\|\\&\\/*", Boolean.FALSE),
				Arguments.arguments("1234abcd", "1234abcd", Boolean.TRUE),
				Arguments.arguments("1234abcd", "1234abcd", Boolean.FALSE),
				Arguments.arguments("brkg.trd.fixed(*", "brkg.trd.fixed\\\\(*", Boolean.TRUE),
				Arguments.arguments("brkg.trd.fixed(*", "brkg.trd.fixed\\(*", Boolean.FALSE),
				Arguments.arguments("*a*", "*a*", Boolean.TRUE),
				Arguments.arguments("*a*", "*a*", Boolean.FALSE));
	}

	/**
	 * Tests the {@link LuceneUtil#escapeSearchTerm(String)} method for correct escaping.
	 */
	@ParameterizedTest(name = "searchTerm: {0} | for Orient DB clause: {2} ")
	@MethodSource("escapeSearchTermTestCases")
	void testEscapeSearchTerm(final String searchTerm, final String expected, final Boolean forOrientClause) {
		assertEquals(expected, LuceneUtil.escapeSearchTerm(searchTerm, forOrientClause.booleanValue()));
	}
}

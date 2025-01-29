/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery;

import static innowake.mining.shared.PatternConverter.convertAntToRegexPattern;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

import java.util.regex.Pattern;
import org.springframework.util.AntPathMatcher;

import innowake.mining.shared.PatternConverter;

/**
 * Tests for {@link PatternConverter#convertAntToRegexPattern(String)}.
 */
public class AntPatternToRegexPatternConversionTest {
	
	private AntPathMatcher antPathMatcher = new AntPathMatcher();
	private static final String PATTERN1 = "/foo/bar/*.cbl";
	private static final String PATTERN2 = "/foo/bar/**/*.cbl";
	private static final String PATTERN3 = "/foo/**/baz/*.cbl";
	private static final String PATTERN4 = "/**/baz/**";
	private static final String PATTERN5 = "/foo/**/baz/*";
	private static final String PATTERN6 = "/foo/**";
	private static final String PATTERN7 = "/?oo/*/*.cbl";

	private static final String MATCH1 = "/foo/bar/A.cbl";
	private static final String MATCH2 =  "/foo/bar/baz/A.cbl";
	private static final String MATCH3 = "/foo/barABC/A.cbl";
	private static final String MATCH4 = "/foo/bar2/A.cbl";
	
	@Test
	public void testToMatchFileName() {
		assertTrue(Pattern.matches(convertAntToRegexPattern(PATTERN1), MATCH1));
		assertFalse(Pattern.matches(convertAntToRegexPattern(PATTERN1), MATCH2));
		
		assertTrue(antPathMatcher.match(PATTERN1, MATCH1));
		assertFalse(antPathMatcher.match(PATTERN1, MATCH2));
	}
	
	@Test
	public void testToMatchZeroOrMoreFolders() {
		assertTrue(Pattern.matches(convertAntToRegexPattern(PATTERN2), MATCH1));
		assertTrue(Pattern.matches(convertAntToRegexPattern(PATTERN2), MATCH2));
		
		assertTrue(antPathMatcher.match(PATTERN2, MATCH1));
		assertTrue(antPathMatcher.match(PATTERN2, MATCH2));
	}
	
	@Test
	public void testToMatchZeroOrMoreFolders2() {
		assertFalse(Pattern.matches(convertAntToRegexPattern(PATTERN3), MATCH1));
		assertTrue(Pattern.matches(convertAntToRegexPattern(PATTERN3), MATCH2));
		
		assertFalse(antPathMatcher.match(PATTERN3, MATCH1));
		assertTrue(antPathMatcher.match(PATTERN3, MATCH2));
	}
	
	@Test
	public void testToNotMatchTheDifferentSubFolder() {
		assertFalse(Pattern.matches(convertAntToRegexPattern(PATTERN2), MATCH3));
		assertFalse(Pattern.matches(convertAntToRegexPattern(PATTERN2), MATCH4));
		assertFalse(antPathMatcher.match(PATTERN2, MATCH3));
		assertFalse(antPathMatcher.match(PATTERN2, MATCH4));
	}
	
	@Test
	public void testToMatchZeroOrMoreFoldersBeforeAndAfter() {
		assertFalse(Pattern.matches(convertAntToRegexPattern(PATTERN4), MATCH1));
		assertTrue(Pattern.matches(convertAntToRegexPattern(PATTERN4), MATCH2));
		
		assertFalse(antPathMatcher.match(PATTERN4, MATCH1));
		assertTrue(antPathMatcher.match(PATTERN4, MATCH2));
	}
	
	@Test
	public void testToMatchZeroOrMoreFoldersInBetween() {
		assertFalse(Pattern.matches(convertAntToRegexPattern(PATTERN5), MATCH1));
		assertTrue(Pattern.matches(convertAntToRegexPattern(PATTERN5), MATCH2));
		
		assertFalse(antPathMatcher.match(PATTERN5, MATCH1));
		assertTrue(antPathMatcher.match(PATTERN5, MATCH2));
	}
	
	@Test
	public void testToMatchStartsWithGivenFolder() {
		assertTrue(Pattern.matches(convertAntToRegexPattern(PATTERN6), MATCH1));
		assertTrue(Pattern.matches(convertAntToRegexPattern(PATTERN6), MATCH2));
		
		assertTrue(antPathMatcher.match(PATTERN6, MATCH1));
		assertTrue(antPathMatcher.match(PATTERN6, MATCH2));
	}
	
	@Test
	public void testToMatchSingleChar() {
		assertTrue(Pattern.matches(convertAntToRegexPattern(PATTERN7), MATCH1));
		assertFalse(Pattern.matches(convertAntToRegexPattern(PATTERN7), MATCH2));
		
		assertTrue(antPathMatcher.match(PATTERN7, MATCH1));
		assertFalse(antPathMatcher.match(PATTERN7, MATCH2));
	}
}

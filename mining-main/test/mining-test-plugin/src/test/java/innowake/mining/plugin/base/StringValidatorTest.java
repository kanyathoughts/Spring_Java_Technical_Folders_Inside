/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.base;

import static innowake.mining.plugin.base.StringValidator.match;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * Tests {@link StringValidator}.
 */
public class StringValidatorTest {

	@Test
	public void test() {
		final String string = "HELLO WORLD";
		
		assertFalse(match(string, "ELLO"));
		assertFalse(match(string, "HELLO"));
		assertFalse(match(string, "*HELLO"));
		assertFalse(match(string, " *ELLO*"));
		assertFalse(match(string, "HELLO WORLD2"));
		assertFalse(match(string, "HELLO WORL"));
		assertFalse(match(string, "HELLO WORLD "));
		assertFalse(match(string, " HELLO WORLD"));
		assertFalse(match(string, "hello world"));
		 
		assertTrue(match(string, "*ELLO*"));
		assertTrue(match(string, "HELLO*"));
		assertTrue(match(string, "*WORLD"));
		assertTrue(match(string, "HELLO WORLD"));
		assertTrue(match(string, "*HELLO WORLD*"));

		assertFalse(match(string, " ?ELLO WORLD"));
		assertFalse(match(string, "HELLO WORLD?"));
		assertFalse(match(string, "?HELLO WORLD"));

		assertTrue(match(string, "?ELLO WORLD"));
		assertTrue(match(string, "HELLO WORL?"));
		assertTrue(match(string, "??LLO WORLD"));
		assertTrue(match(string, "?E?LO WORLD"));
		assertTrue(match(string, "????? ?????"));
		assertTrue(match(string, "?????*?????"));
		assertTrue(match(string, "???????????"));
	}
}

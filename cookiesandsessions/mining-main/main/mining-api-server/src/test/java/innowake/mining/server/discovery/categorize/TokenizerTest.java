/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.categorize;

import java.util.List;

import org.junit.Assert;
import org.junit.Test;

/**
 * Tests for the Tokenizer implementation
 */
public class TokenizerTest {
	
	@Test
	public void simpleTest() {
		Tokenizer t = new Tokenizer("01 MY-DATAAREA PIC 9(8).");
		Assert.assertEquals("01 MY-DATAAREA ", t.getToken("PIC"));
	}

	@Test
	public void getTokensTest() {
		Tokenizer t = new Tokenizer("01 MY-DATAAREA PIC 9(8).");
		List<String> escapedTokens = t.getTokens(" ");
		Assert.assertEquals("01", escapedTokens.get(0));
		Assert.assertEquals("MY-DATAAREA", escapedTokens.get(1));
		Assert.assertEquals("PIC", escapedTokens.get(2));
		Assert.assertEquals("9(8).", escapedTokens.get(3));
	}
}

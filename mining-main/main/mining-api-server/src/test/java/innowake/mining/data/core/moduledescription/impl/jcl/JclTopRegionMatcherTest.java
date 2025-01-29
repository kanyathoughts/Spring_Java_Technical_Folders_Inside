/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.data.core.moduledescription.impl.jcl;

import static org.junit.Assert.assertNotNull;

import org.junit.Assert;
import org.junit.Test;

import innowake.mining.data.core.TestWithResource;
import innowake.ndt.core.parsing.ILexScanner;
import innowake.ndt.core.parsing.ILexer;

/**
 * Tests for the {@link JclTopRegionMatcher}.
 */
public class JclTopRegionMatcherTest extends TestWithResource {

	@Test
	public void testWmin1173A() {
		testMatcher("WMIN1173A.job");
	}
	
	@Test
	public void testWmin1173B() {
		testMatcher("WMIN1173B.job");
	}
	
	@Test
	public void testWmin1173C() {
		testMatcher("WMIN1173C.job");
	}
	
	@Test
	public void testWmin1173D() {
		testMatcher("WMIN1173D.proc");
	}
	
	private void testMatcher(final String resourceName) {
		final String source = getResource(resourceName);
		final JclDescriptionExtractor extractor = new JclDescriptionExtractor();
		final ILexer lexer = extractor.createLexer();
		assertNotNull(lexer);
		final ILexScanner scanner = lexer.createScanner(source);
		final String region = JclTopRegionMatcher.INSTANCE.match(scanner, source);
		Assert.assertEquals(getResource(resourceName + ".region-exp"), region);
	}
	
}

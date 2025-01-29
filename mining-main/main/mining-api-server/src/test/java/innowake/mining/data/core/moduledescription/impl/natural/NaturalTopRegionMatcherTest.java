/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.data.core.moduledescription.impl.natural;

import static org.junit.Assert.assertNotNull;

import org.junit.Assert;
import org.junit.Test;

import innowake.mining.data.core.TestWithResource;
import innowake.ndt.core.parsing.ILexScanner;
import innowake.ndt.core.parsing.ILexer;

/**
 * Tests for the Natural matcher extracting the source from beginning till the first default code statement.
 */
public class NaturalTopRegionMatcherTest extends TestWithResource {

	@Test
	public void testMatcher() {
		final String source = getResource("WMIN1170A.nsp");
		final NaturalDescriptionExtractor extractor = new NaturalDescriptionExtractor();
		final ILexer lexer = extractor.createLexer();
		assertNotNull(lexer);
		final ILexScanner scanner = lexer.createScanner(source);
		final String region = NaturalTopRegionMatcher.INSTANCE.match(scanner, source);
		Assert.assertEquals(getResource("WMIN1170A.nsp.exp"), region);
	}
	
}

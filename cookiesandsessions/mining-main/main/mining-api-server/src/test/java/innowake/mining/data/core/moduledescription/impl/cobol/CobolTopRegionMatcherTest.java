/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.data.core.moduledescription.impl.cobol;

import static org.junit.Assert.assertNotNull;

import org.junit.Assert;
import org.junit.Test;

import innowake.mining.data.core.TestWithResource;
import innowake.ndt.core.parsing.ILexScanner;
import innowake.ndt.core.parsing.ILexer;

/**
 * Unit test for the Cobol matcher extracting the source from beginning till PROCEDURE DIVISION.
 */
public class CobolTopRegionMatcherTest extends TestWithResource {

	/**
	 * Simple test case of extraction with resource MatchCase01
	 */
	@Test
	public void testMatcher() {
		final String source = getResource("MatchCase01.cbl");
		final CobolDescriptionExtractor extractor = new CobolDescriptionExtractor();
		final ILexer lexer = extractor.createLexer();
		assertNotNull(lexer);
		final ILexScanner scanner = lexer.createScanner(source);
		final String region = CobolTopRegionMatcher.INSTANCE.match(scanner, source);
		Assert.assertEquals( getResource("MatchCase01.exp"), region);
	}
	

	/**
	 * Simple test case of extraction with resource WMIN7357 and test the ModuleDescription of it
	 */
	@Test
	public void testCommentsBeforeDataDivision() {
		final String source = getResource("WMIN7357.cbl");
		final CobolDescriptionExtractor extractor = new CobolDescriptionExtractor();
		final ILexer lexer = extractor.createLexer();
		assertNotNull(lexer);
		final ILexScanner scanner = lexer.createScanner(source);
		final String region = CobolTopRegionMatcher.INSTANCE.match(scanner, source);
		Assert.assertEquals( getResource("WMIN7357.exp"), region);
	}
}

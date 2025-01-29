/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.impl.cobol;

import static org.junit.Assert.assertNotNull;

import java.util.stream.Stream;

import org.junit.Assert;
import org.junit.Test;

import innowake.mining.data.core.TestWithResource;
import innowake.ndt.core.parsing.ILexScanner;
import innowake.ndt.core.parsing.ILexer;

/**
 * Unit tests for Cobol comment extraction. 
 */
public class CobolBlockCommentExtractorTest extends TestWithResource {

	/**
	 * Test the Cobol comment extraction with resource ExtractCase01.
	 */
	@Test
	public void testExtractComment() {
		final String source = getResource("ExtractCase01.cbl");
		final CobolDescriptionExtractor extractor = new CobolDescriptionExtractor();
		final ILexer lexer = extractor.createLexer();
		assertNotNull(lexer);
		final ILexScanner scanner = lexer.createScanner(source);
		final Stream<String> comment = CobolBlockCommentExtractor.INSTANCE.extract(scanner, source);
		
		final StringBuilder content = new StringBuilder();
		comment.forEach(line -> { content.append(line).append("\n"); });
		
		Assert.assertEquals(getResource("ExtractCase01.exp"), content.toString());
	}
}

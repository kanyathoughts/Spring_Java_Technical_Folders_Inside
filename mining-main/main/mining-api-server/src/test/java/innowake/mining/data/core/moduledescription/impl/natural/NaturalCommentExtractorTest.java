/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.impl.natural;

import static org.junit.Assert.assertNotNull;

import java.util.stream.Stream;

import org.junit.Assert;
import org.junit.Test;

import innowake.mining.data.core.TestWithResource;
import innowake.ndt.core.parsing.ILexScanner;
import innowake.ndt.core.parsing.ILexer;

/**
 * Unit tests for Natural comment extraction. 
 */
public class NaturalCommentExtractorTest extends TestWithResource {

	@Test
	public void testExtractComment() {
		final String source = getResource("WMIN1170B.nsp");
		final NaturalDescriptionExtractor extractor = new NaturalDescriptionExtractor();
		final ILexer lexer = extractor.createLexer();
		assertNotNull(lexer);
		final ILexScanner scanner = lexer.createScanner(source);
		final Stream<String> comment = NaturalCommentExtractor.INSTANCE.extract(scanner, source);
		
		final StringBuilder content = new StringBuilder();
		comment.forEach(line -> { content.append(line).append("\n"); });
		
		Assert.assertEquals(getResource("WMIN1170B.nsp.exp"), content.toString());
	}
}

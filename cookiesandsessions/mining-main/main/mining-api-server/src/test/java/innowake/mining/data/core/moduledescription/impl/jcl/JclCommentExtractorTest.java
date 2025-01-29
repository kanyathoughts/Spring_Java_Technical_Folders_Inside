/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.impl.jcl;

import static org.junit.Assert.assertNotNull;

import java.util.stream.Stream;

import org.junit.Assert;
import org.junit.Test;

import innowake.mining.data.core.TestWithResource;
import innowake.ndt.core.parsing.ILexScanner;
import innowake.ndt.core.parsing.ILexer;

/**
 * Unit tests for {@link JclCommentExtractor}.
 */
public class JclCommentExtractorTest extends TestWithResource {
	
	@Test
	public void testWmin1173A() {
		testExtractComments("WMIN1173A.job");
	}
	
	@Test
	public void testWmin1173B() {
		testExtractComments("WMIN1173B.job");
	}
	
	@Test
	public void testWmin1173C() {
		testExtractComments("WMIN1173C.job");
	}
	
	@Test
	public void testWmin1173D() {
		testExtractComments("WMIN1173D.proc");
	}

	private void testExtractComments(final String resourceName) {
		final String source = getResource(resourceName);
		final JclDescriptionExtractor extractor = new JclDescriptionExtractor();
		final ILexer lexer = extractor.createLexer();
		assertNotNull(lexer);
		final ILexScanner scanner = lexer.createScanner(source);
		final Stream<String> comment = JclCommentExtractor.INSTANCE.extract(scanner, source);
		
		final StringBuilder content = new StringBuilder();
		comment.forEach(line -> { content.append(line).append("\n"); });
		
		Assert.assertEquals(getResource(resourceName + ".comment-exp"), content.toString());
	}
}

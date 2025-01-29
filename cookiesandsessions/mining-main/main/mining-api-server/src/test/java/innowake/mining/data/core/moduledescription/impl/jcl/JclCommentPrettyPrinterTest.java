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
 * Tests for the {@link JclCommentPrettyPrinter}.
 */
public class JclCommentPrettyPrinterTest extends TestWithResource {
	
	@Test
	public void testWmin1173A() {
		testPrettyPrint("WMIN1173A.job");
	}
	
	@Test
	public void testWmin1173B() {
		testPrettyPrint("WMIN1173B.job");
	}
	
	@Test
	public void testWmin1173C() {
		testPrettyPrint("WMIN1173C.job");
	}
	
	@Test
	public void testWmin1173D() {
		testPrettyPrint("WMIN1173D.proc");
	}

	private void testPrettyPrint(final String resourceName) {
		final String source = getResource(resourceName);
		final JclDescriptionExtractor extractor = new JclDescriptionExtractor();
		final ILexer lexer = extractor.createLexer();
		assertNotNull(lexer);
		final ILexScanner scanner = lexer.createScanner(source);
		final Stream<String> comments = JclCommentExtractor.INSTANCE.extract(scanner, source);
		
		final String description = JclCommentPrettyPrinter.INSTANCE.print(comments);
		Assert.assertEquals(getResource(resourceName + ".pretty-exp"), description);
	}
}

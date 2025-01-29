/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.impl.cobol;

import org.junit.Assert;
import org.junit.Test;

import innowake.mining.data.core.TestWithResource;

/**
 * Test the description extraction for Cobol 
 */
public class CobolDescriptionExtractorTest extends TestWithResource {

	/**
	 * Test a simple Cobol module description extraction.
	 */
	@Test
	public final void testExtraction() {
		final CobolDescriptionExtractor extractor = new CobolDescriptionExtractor();
		final String description = extractor.getDescription(getResource("ExtractCase01.cbl"));
		Assert.assertEquals(getResource("PrettyPrinterCase01.exp"), description);
	}
	
}

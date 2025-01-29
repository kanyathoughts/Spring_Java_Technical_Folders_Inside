/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.impl.natural;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.Test;

import innowake.mining.data.core.TestWithResource;

/**
 * Tests the module description extraction for a Natural Program, Sub Program and Sub Routine.
 */
public class NaturalDescriptionExtractorTest extends TestWithResource {

	private final static String[] TEST_FILES = {"Prog_Desc.nsp", "Sub_Prog_Desc.nsn", "Subroutine_Desc.nss"};
	
	@Test
	public void testModuleDescriptionExtraction() {
		for (String testFile : TEST_FILES) {
			final String source = getResource(testFile);
			final NaturalDescriptionExtractor extractor = new NaturalDescriptionExtractor();
			final String content = extractor.getDescription(source);
			assertEquals(getResource(testFile + ".exp"), content);
		}
	}
	
	
}

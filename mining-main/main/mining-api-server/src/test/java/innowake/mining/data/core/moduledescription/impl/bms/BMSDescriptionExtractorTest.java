/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.impl.bms;

import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.Test;

import innowake.mining.data.core.TestWithResource;

/**
 * Tests the module description extraction for a BMS map.
 */
public class BMSDescriptionExtractorTest extends TestWithResource {
	
	private final static String[] TEST_MAPS = {"MEE4740B.map", "MEE6278A.map", "WNDT2670.map", "MEE4740BDBL.map"};
	private final static String CORRUPTED_MAP = "CORRUPTED.map";
	
	@Test
	public void testModuleDescriptionExtraction() {
		for (String testMap : TEST_MAPS) {
			final String source = getResource(testMap);
			final BMSDescriptionExtractor extractor = new BMSDescriptionExtractor();
			final String content = extractor.getDescription(source)
					.replace("\r\n", "\n"); /* normalize line endings */
			assertEquals(getResource(testMap + ".exp"), content);
		}
	}
	
	/**
	 * Ensure that a corrupted map does not lead to a NullPointerException and that we get a message
	 * about a corrupted map in the description.
	 */
	@Test
	public void testCorruptedMap() {
		final String source = getResource(CORRUPTED_MAP);
		final Stream<String> mapText = BMSMapToTextExtractor.INSTANCE.extract(source);
	
		final StringBuilder content = new StringBuilder();
		mapText.forEach(line -> { content.append(line).append(BMSDescriptionExtractor.LINE_BREAK); });
	
		assertTrue(content.toString().contains(BMSMapToTextExtractor.CORRUPTED_VIEW_MESSAGE));
	}
}

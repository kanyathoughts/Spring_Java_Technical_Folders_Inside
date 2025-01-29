/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.impl.basic;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.stream.Collectors;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.data.core.TestWithResource;
import innowake.mining.shared.TestResourceUtil;

/**
 * Tests regarding the extraction of BASIC Module descriptions based on the block comments.
 */
@RunWith(Parameterized.class)
public class BasicDescriptionExtractorTest extends TestWithResource {
	
	private static final Path RESOURCE_PATH = Paths.get(System.getProperty("user.dir"), TestResourceUtil.ROOT_FOLDER, "innowake/mining/data/core/moduledescription/impl/basic");

	private static final boolean WRITE_EXPECTED_FILES = false;
	
	/**
	 * The singular file name, for the parameterized test.
	 */
	@Parameter
	@Nullable
	public String fileName;

	/**
	 * Creates the test parameters based on the *.bas files in the test resource path.
	 *
	 * @return the file names of all test resources
	 * 
	 * @throws IOException see {@link Files#walk(Path, java.nio.file.FileVisitOption...)}
	 */
	@Parameters(name = "{index}: {0}")
	public static Iterable<? extends Object> data() throws IOException {
		return Files.walk(RESOURCE_PATH)
				.filter(path -> path.toString().endsWith("bas"))
				.map(Path::getFileName)
				.map(Path::toString)
				.collect(Collectors.toList());
	}
	
	/**
	 * Check the extracted description against an appropriate expected file.
	 * 
	 * @throws IOException if an I/O error occurs writing to or creating the expected file
	 */
	@Test
	public void checkFile() throws IOException {
		final String source = getResource(Assert.assertNotNull(fileName));
		final BasicDescriptionExtractor extractor = new BasicDescriptionExtractor();
		final String actualContent = extractor.getDescription(source);
		
		if (WRITE_EXPECTED_FILES) {
			writeExpected(fileName + ".exp", actualContent);
		} else {
			final String expectedContent = getResource(fileName + ".exp");
			assertEquals(String.format("Expected content for %s does not match", fileName), expectedContent, actualContent);
		}
	}
	
}

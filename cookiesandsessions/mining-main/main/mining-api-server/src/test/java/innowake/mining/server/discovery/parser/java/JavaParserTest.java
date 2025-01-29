/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.java;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;

import innowake.lib.job.api.NullProgressMonitor;
import innowake.mining.data.discovery.metrics.TimedWorkerImpl;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.server.service.ParseResultCacheService;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.searchorder.SearchOrder;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.testing.SourcePojoDummy;
import innowake.mining.shared.hashing.CityHash;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.ndt.parsing.parser.java.model.JavaModel;

/**
 * Tests for the {@link JavaParseResultProvider}
 */
class JavaParserTest {

	private static final String TEST_JAVA_PATH = "src/java/WMIN6412/Test.java";
	private static final String SOURCE_PATH = "test-resources/innowake/mining/server/discovery/parser/java";
	private static final String FILE_PATH = "./" + SOURCE_PATH + "/" + TEST_JAVA_PATH;
	private static final String PATTERN = "src/java/WMIN6412/**/*";
	private static final Long VERSION = Long.valueOf(0L);
	private static final EntityId PROJECT_ID = EntityId.of(5l);

	/**
	 * Tests that the Java parsing doesn't fail when source folders (search order target patterns) are duplicated.
	 *
	 * @throws DiscoveryException if parsing fails
	 * @throws IOException if reading of example file fails
	 */
	@Test
	void testFilterDuplicateTargetPatterns() throws DiscoveryException, IOException {
		final String content = Files.lines(Paths.get(FILE_PATH)).collect(Collectors.joining("\n"));
		final SourcePojo sourceObject = SourcePojoDummy.build(o -> o
				.setProject(PROJECT_ID)
				.setName("Test")
				.setPath(TEST_JAVA_PATH)
				.setTechnology(Technology.JAVA)
				.setType(Type.JOB)
				.setMetaDataRevision(VERSION)
				.setContentRevision(VERSION)
				.setContent(new BinaryString(content)));
		final List<SearchOrder> searchOrders = Arrays.asList(SearchOrder.fromPatterns("**/*", PATTERN, PATTERN, "./*", "**/*"));

		final JavaModel javaModel = getJavaParseResultProvider(searchOrders).getParseResult(sourceObject);
		assertNotNull(javaModel, "Java parse result must not be null");
		assertTrue(javaModel.getParserErrors().isEmpty(), "There must be no parser errors present");
	}

	/**
	 * Tests that the Java parsing doesn't fail when a source folder (search order target pattern) doesn't exists.
	 *
	 * @throws DiscoveryException if parsing failed
	 * @throws IOException if reading of example file fails
	 */
	@Test
	void testNoneExistingSourceFolder() throws IOException {
		final String content = Files.lines(Paths.get(FILE_PATH)).collect(Collectors.joining("\n"));
		final SourcePojo sourceObject = SourcePojoDummy.build(o -> o
				.setProject(PROJECT_ID)
				.setName("Test")
				.setPath(TEST_JAVA_PATH)
				.setTechnology(Technology.JAVA)
				.setType(Type.JOB)
				.setMetaDataRevision(VERSION)
				.setContentRevision(VERSION)
				.setContent(new BinaryString(content)));
		final List<SearchOrder> searchOrders = Arrays.asList(SearchOrder.fromPatterns("**/*", "missing/" + PATTERN, "./*", "**/*"));

		try {
			final JavaModel javaModel = getJavaParseResultProvider(searchOrders).getParseResult(sourceObject);
			assertNotNull(javaModel, "Java parse result must not be null");
			assertTrue(javaModel.getParserErrors().isEmpty(), "There must be no parser errors present");
		} catch (final DiscoveryException e) {
			fail("Source path validation must not fail with invalid target patterns");
		}
	}
	
	@SuppressWarnings("boxing")
	private static JavaParseResultProvider getJavaParseResultProvider(final List<SearchOrder> searchOrders) {
		final GenericConfigProperties config = new GenericConfigProperties(0, 0, 0, 0, null, 1000000, 0, 0, 0, 0L, true, false, 8, 5_000_000, -1L, 10D, 100,
				null, 1000, false, 0, 0, 0, 0, 0, null, 0, 0, 0, 0);
		return new JavaParseResultProvider(Config.getDefaultConfig(),
											new TimedWorkerImpl(new NullProgressMonitor(), null, null),
											Paths.get(SOURCE_PATH).toAbsolutePath().toString(),
											CityHash.cityHash128Hex("JavaParserTest"),
											new ParseResultCacheService(config),
											new SearchOrders(searchOrders));
	}
}

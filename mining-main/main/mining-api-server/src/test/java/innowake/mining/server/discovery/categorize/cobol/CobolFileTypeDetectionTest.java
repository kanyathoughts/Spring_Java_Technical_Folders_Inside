/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.categorize.cobol;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.lib.core.lang.Assert.fail;
import static org.apache.commons.lang3.math.NumberUtils.LONG_ONE;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.Test;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.data.io.sourceobject.FileExtension;
import innowake.mining.server.discovery.cache.DiscoveryJobCache;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.discovery.categorize.Identification.ID;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.testing.SourcePojoDummy;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ResolveTarget;

/**
 * CobolFileTypeDetection for CobolCopyLibs.
 */
class CobolFileTypeDetectionTest {

	private static final Path BASE_FOLDER = Paths.get("./test-resources/innowake/mining/server/discovery");
	protected static final Path SOURCE_FOLDER = BASE_FOLDER.resolve("source");
	private static final String FAILED_MESSAGE = "Failed to identify the dynamic tests. Exception occured : ";
	@Autowired
	private transient DiscoveryJobCache discoveryCache;
	
	private void compare(final Path path) throws IOException {
		final String content = new String(Files.readAllBytes(path), StandardCharsets.UTF_8);
		final ModuleType moduleType = FileExtension.resolve(path.toString());
		final SourcePojo sourceObject = SourcePojoDummy.build(o -> o
				.setNid(LONG_ONE)
				.setProject(EntityId.of(5l))
				.setName(path.getFileName().toString())
				.setPath(path.toString())
				.setTechnology(moduleType.getTechnology())
				.setType(moduleType.getType())
				.setMetaDataRevision(LONG_ONE)
				.setContentRevision(LONG_ONE)
				.setContent(new BinaryString(content)));
		final Config config = Config.getDefaultConfig();
		final DiscoverCodeJob jobId = new DiscoverCodeJob(EntityId.of(LONG_ONE));
		final CobolFileTypeDetection categorizer = new CobolFileTypeDetection(config, discoveryCache, jobId.getJobId());
		final Identification identification = categorizer.identifyByContent(sourceObject);
		assertEquals(ResolveTarget.COBOL_COPYLIB, assertNotNull(identification).getTarget());
	}

	/**
	 * Tests that the names of the two copybook in the Cobol module {@code Wmin2728.cbl} are set into the cache without quotation marks.
	 * <p>The following example must add {@code connora} and {@code utsysdatepe} to the list:</p>
	 * <pre>
	 * COPY 'connora'.
	 * COPY "utsysdatepe".
	 * </pre>
	 */
	@Test
	public void testCopyNameNormalization() {
		try {
			final String content = new String(Files.readAllBytes(SOURCE_FOLDER.resolve("WMIN2728").resolve("Wmin2728.cbl")), StandardCharsets.UTF_8);
			
			final SourcePojo sourceObject = SourcePojoDummy.build(o -> o
					.setNid(LONG_ONE)
					.setProject(EntityId.of(5l))
					.setName("Wmin2728.cbl")
					.setPath("dummy/Wmin2728.cbl")
					.setTechnology(Technology.COBOL)
					.setType(Type.PROGRAM)
					.setMetaDataRevision(LONG_ONE)
					.setContentRevision(LONG_ONE)
					.setContent(new BinaryString(content)));

			final DiscoverCodeJob jobId = new DiscoverCodeJob(EntityId.of(LONG_ONE));
			discoveryCache = Mockito.mock(DiscoveryJobCache.class);
			final CobolFileTypeDetection categorizer = new CobolFileTypeDetection(Config.getDefaultConfig(), discoveryCache, jobId.getJobId());

			categorizer.identifyDependencies(sourceObject);

			final ArgumentCaptor<String> jobCaptor = ArgumentCaptor.forClass(String.class);
			final ArgumentCaptor<String> keyCaptor = ArgumentCaptor.forClass(String.class);
			final ArgumentCaptor<String> copyCaptor = ArgumentCaptor.forClass(String.class);

			/* this already tests that the discoveryCache was called two times */
			Mockito.verify(discoveryCache, Mockito.times(2)).putMultiValue(jobCaptor.capture(), keyCaptor.capture(), copyCaptor.capture());

			for (String key : keyCaptor.getAllValues()) {
				assertEquals(CobolFileTypeDetection.CACHE_KEY, key, "DiscoveryJobCache.putMultiValue() must be called with key: " + CobolFileTypeDetection.CACHE_KEY);
			}

			assertEquals("utsysdatepe", copyCaptor.getAllValues().get(0), "First found copybook must be: utsysdatepe");
			assertEquals("connora", copyCaptor.getAllValues().get(1), "Second found copybook must be: connora");
		} catch (final IOException e) {
			fail(FAILED_MESSAGE + e.getMessage());
		}
	}
	
	/**
	 * Tests if COBOL Class Program is being identified correctly as COBOL.
	 *
	 */
	@Test
	public void testCobolFileIdentificationForIdYesOnWmin2812A() {
		testCobolFileIdentification("WMIN2812", "WMIN2812A", Identification.ID.YES);
	}

	/**
	 * Tests if COBOL Class Program is being identified maybe as COBOL.
	 *
	 */
	@Test
	public void testCobolFileIdentificationForIdMaybeOnWmin2812B() {
		testCobolFileIdentification("WMIN2812", "WMIN2812B", Identification.ID.MAYBE);
	}
	
	/**
	 * Tests if COBOL Class Program is being identified maybe as COBOL.
	 *
	 */
	@Test
	public void testCobolFileIdentificationForIdMaybeOnWmin2812C() {
		testCobolFileIdentification("WMIN2812", "WMIN2812C", Identification.ID.MAYBE);
	}
	
	/**
	 * Tests if COBOL Class Program is being identified correctly as COBOL.
	 *
	 */
	@Test
	public void testCobolFileIdentificationForIdYesOnWmin2812() {
		testCobolFileIdentification("WMIN2812", "WMIN2812", Identification.ID.YES);
	}

	/**
	 * Tests if COBOL Class Program is being identified correctly as COBOL.
	 *
	 */
	@Test
	public void testCobolFileIdentificationForIdYesOnWcfd861() {
		testCobolFileIdentification("WCFD861", "DFP8B001", Identification.ID.YES);
	}

	@TestFactory
	public Collection<DynamicTest> patternTest() {
		try (final Stream<Path> paths = Files.walk(SOURCE_FOLDER.resolve("WMIN2447"), 1)) {
			final List<Path> filePaths = paths.filter(Files::isRegularFile).collect(Collectors.toList());
			final Collection<DynamicTest> dynamicTests = new ArrayList<>();
			for (final Path path : filePaths) {
				final DynamicTest dt = DynamicTest.dynamicTest(path.getFileName().toString(),()-> compare(path));
				dynamicTests.add(dt);
			}
			return dynamicTests;
		} catch (final IOException e) {
			fail(FAILED_MESSAGE + e.getMessage());
			return Collections.emptyList();
		}
	}
	
	private Identification createProgramAndIdentify(final String name, final String path, final String content) {
		final SourcePojo sourceObject = SourcePojoDummy.build(o -> o
				.setNid(LONG_ONE)
				.setProject(EntityId.of(LONG_ONE))
				.setName(name)
				.setPath(path)
				.setTechnology(Technology.COBOL)
				.setType(Type.PROGRAM)
				.setMetaDataRevision(LONG_ONE)
				.setContentRevision(LONG_ONE)
				.setContent(new BinaryString(content)));
		
		final DiscoverCodeJob jobId = new DiscoverCodeJob(EntityId.of(LONG_ONE));
		final CobolFileTypeDetection categorizer = new CobolFileTypeDetection(Config.getDefaultConfig(), discoveryCache, jobId.getJobId());
		final Identification identification = assertNotNull(categorizer.identifyMainObject(sourceObject));
		return identification;
	}

	private void testCobolFileIdentification(final String testFolderName, final String testFileName, final ID id) {
		try {
			final String content = new String(Files.readAllBytes(SOURCE_FOLDER.resolve(testFolderName).resolve(testFileName)), StandardCharsets.UTF_8);
			final String testFileNameWithExtension = String.format("%s%s", testFileName, ".cbl");
			final String path = String.format("%s%s", "dummy/", testFileName);
			final Identification identification = createProgramAndIdentify(testFileNameWithExtension, path, content);

			assertEquals(ResolveTarget.COBOL, identification.getTarget().getLanguage(), "Identified Language should be COBOL");
			if (Identification.ID.YES.equals(id)) {
				assertEquals(Identification.ID.YES, identification.getId(), "Identification ID should be YES");
			} else {
				assertEquals(Identification.ID.MAYBE, identification.getId(), "Identification ID should be MAYBE");
			}
		} catch (final IOException e) {
			fail(FAILED_MESSAGE + e.getMessage());
		}
	}
}

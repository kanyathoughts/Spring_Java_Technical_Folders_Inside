/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.categorize.cpp;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.lib.core.lang.Assert.fail;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.io.FilenameUtils;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.io.sourceobject.FileExtension;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.BinaryValue;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.hashing.CityHash;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.discovery.ResolveTarget;

/**
 * JUnit test for C++ file categorization.
 */
class CppFileTypeDetectionTest {

	private static final Path BASE_FOLDER = Paths.get("./test-resources/innowake/mining/server/discovery");
	protected static final Path SOURCE_FOLDER = BASE_FOLDER.resolve("source");

	private void compare(final Path path) throws IOException {
		final String content = Files.readString(path);
		final String extension = FilenameUtils.getExtension(path.toString()).toUpperCase();
		final ModuleType moduleType = FileExtension.resolve(path.toString());
		final SourcePojo sourceObject = new SourcePojo(
				UUID.randomUUID(),
				1l,
				EntityId.of(5l), null, null,
				path.getFileName().toString(), 
				path.toString(), 
				moduleType.getTechnology(),
				moduleType.getType(), 
				Long.valueOf(1),
				Long.valueOf(1),
				new BinaryValue(CityHash.cityHash128Hex(content)),
				new BinaryString(content),
				CustomPropertiesMap.empty());

		final CppFileTypeDetection categorizer = new CppFileTypeDetection();
		final Identification identification = categorizer.identifyByExtension(sourceObject);
		assertResolveTarget(extension, identification);
	}

	private void assertResolveTarget(final String extension, @Nullable final Identification identification) {
		if ("CPP".equals(extension)) {
			assertEquals(ResolveTarget.CPP_PROGRAM, assertNotNull(identification).getTarget());
		} else if ("HPP".equals(extension)) {
			assertEquals(ResolveTarget.CPP_HEADER, assertNotNull(identification).getTarget());
		}
	}

	@TestFactory
	Collection<DynamicTest> fileDetection() {
		try (final Stream<Path> paths = Files.walk(SOURCE_FOLDER.resolve("WMIN9186"), 1)) {
			final List<Path> filePaths = paths.filter(Files::isRegularFile).collect(Collectors.toList());
			final Collection<DynamicTest> dynamicTests = new ArrayList<>();
			for (final Path path : filePaths) {
				final DynamicTest dt = DynamicTest.dynamicTest(path.getFileName().toString(), () -> compare(path));
				dynamicTests.add(dt);
			}
			paths.close();
			return dynamicTests;
		} catch (final IOException e) {
			fail("Fail to identify the dynamic tests. Exception occurred : " + e.getMessage());
			return Collections.emptyList();
		}
	}
}

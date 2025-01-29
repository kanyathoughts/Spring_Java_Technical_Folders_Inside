package innowake.mining.server.discovery.categorize.java;

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
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;

import innowake.mining.data.io.sourceobject.FileExtension;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.testing.SourcePojoDummy;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.discovery.ResolveTarget;

/**
 * JUnit test for JAVA file categorization
 */
class JavaFileTypeDetectionTest {

	private static final Path BASE_FOLDER = Paths.get("./test-resources/innowake/mining/server/discovery");
	protected static final Path SOURCE_FOLDER = BASE_FOLDER.resolve("source");
	
	private void compare(final Path path) throws IOException {
		final BinaryString content = new BinaryString(Files.readAllBytes(path));
		final ModuleType moduleType = FileExtension.resolve(path.toString());
		final SourcePojo sourceObject = SourcePojoDummy.build(o -> o 
				.setNid(1L)
				.setProject(EntityId.of(5L))
				.setName(path.getFileName().toString())
				.setPath(path.toString())
				.setTechnology(moduleType.getTechnology())
				.setType(moduleType.getType())
				.setContent(content));
		final JavaFileTypeDetection categorizer = new JavaFileTypeDetection();
		final Identification identification = categorizer.identifyByExtension(sourceObject);
		assertEquals(ResolveTarget.JAVA_COMPILATION_UNIT, assertNotNull(identification).getTarget());
	}

	@TestFactory
	Collection<DynamicTest> fileDetection() {
		try (final Stream<Path> paths = Files.walk(SOURCE_FOLDER.resolve("WDIS530"), 1)) {
			final List<Path> filePaths = paths.filter(Files::isRegularFile).collect(Collectors.toList());
			final Collection<DynamicTest> dynamicTests = new ArrayList<>();
			for (final Path path : filePaths) {
				final DynamicTest dt = DynamicTest.dynamicTest(path.getFileName().toString(),()-> compare(path));
				dynamicTests.add(dt);
			}
			paths.close();
			return dynamicTests;
		} catch (final IOException e) {
			fail("Fail to identify the dynamic tests. Exception occured : " + e.getMessage());
			return Collections.emptyList();
		}
	}
}

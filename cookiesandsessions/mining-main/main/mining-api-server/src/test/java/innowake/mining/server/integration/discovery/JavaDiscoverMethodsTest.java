/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.discovery;

import static org.junit.jupiter.params.provider.Arguments.arguments;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.stream.Stream;

import org.apache.poi.EncryptedDocumentException;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.BinaryValue;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.hashing.CityHash;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.ModuleType;

/**
 * Tests the collection of java method calls by Discover Metrics.
 */
@WithMockUser
class JavaDiscoverMethodsTest extends BaseDiscoveryTest {

	private static final String FEATURE_JAVA_COLLECT_METHOD_CALLS = FeatureId.JAVA_COLLECT_METHOD_CALLS.getId();

	@BeforeAll
	public void setFeature() {
		ff4j.enable(FEATURE_JAVA_COLLECT_METHOD_CALLS);
	}

	@AfterAll
	public void resetFeature() {
		ff4j.disable(FEATURE_JAVA_COLLECT_METHOD_CALLS);
	}

	private String testCase = "";

	@SuppressWarnings("unused")
	@DisplayName("Test Discover Metrics for Java method calls")
	@ParameterizedTest(name = "{0}")
	@MethodSource("testCases")
	void test(final String name, final String testCase) throws EncryptedDocumentException, InvalidFormatException, IOException {
		this.testCase = testCase;
		execute();
	}

	public Stream<Arguments> testCases() {
		return Stream.of(
			/* This is the nig run containing most of the various test cases */
			arguments("Test method calls in big run", "A"),

			/* Tests that method calls for unknown types are not collected.
			 * This is the Eclipse JDT default where for unknown types no {@link ITypeBinding} is available */
			arguments("Test with missing dependencies", "B"),

			/* Tests that method and method invocations for default methods are collected correctly */
			arguments("Test with interfaces", "C"),

			/* Tests that method calls for inherited and overridden methods are collected correctly
			 * Also tests that the types for method overloading are resolved correctly. */
			arguments("Test with inherited, overridden and overloaded methods", "D"),

			/* Tests that method calls with multiple inner classes using the same method names work as expected */
			arguments("Test inner classes", "E")
		);
	}
	
	@Override
	protected SourcePojoPrototype createSourceObject(final EntityId projectId, final Path path, final String content, final ModuleType moduleType) {
		final Path sourcePath = SOURCE_FOLDER.resolve(getTestFolder());
		return new SourcePojoPrototype()
			.setProject(projectId)
			.setName(path.getFileName().toString())
			.setPath(Paths.get("temp").resolve(sourcePath.relativize(path)).toString())
			.setTechnology(moduleType.getTechnology())
			.setType(moduleType.getType())
			.setMetaDataRevision(Long.valueOf(1))
			.setContentRevision(Long.valueOf(1))
			.setContent(new BinaryString(content))
			.setContentHash(new BinaryValue(CityHash.cityHash128Hex(content)));
	}

	@Override
	protected boolean normalizeFileContent() {
		return true;
	}

	@Override
	protected String getTestFolder() {
		return "WMIN7305" + "/" + testCase;
	}
}

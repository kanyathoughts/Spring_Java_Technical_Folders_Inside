/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.impl.contributor;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;

import org.junit.jupiter.api.Test;

import innowake.mining.server.discovery.dawn.metrics.api.model.ContributorResult;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Tests for {@link DiscoveryBuilderImpl}.
 */
class DiscoveryBuilderTest {

	private static final String FAKE_CLASS_NAME = "innowake.mining.server.discovery.dawn.metrics.contributors.TestContributor";

	@Test
	void testDeclareExternalModule() {
		final DiscoveryBuilderImpl builder = new DiscoveryBuilderImpl(FAKE_CLASS_NAME);

		builder.declareExternalModule("TESTMODULE", ModuleType.COBOL_PROGRAM);

		final List<ContributorResult> results = builder.buildResults();

		assertEquals(1, results.size());
		final ContributorResult result = results.get(0);

		assertEquals(ContributorResult.Type.EXTERNAL_MODULE, result.getType());

		assertEquals(Collections.singleton(ModuleType.COBOL_PROGRAM), result.getModuleFilter().getTypes());
		assertEquals(Collections.singleton("TESTMODULE"), result.getModuleFilter().getNames());
		assertEquals(Collections.emptySet(), result.getModuleFilter().getPaths());

		assertEquals("TESTMODULE", result.getModuleDefinition().name.get());
		assertEquals(Technology.COBOL, result.getModuleDefinition().technology.get());
        assertEquals(Type.PROGRAM, result.getModuleDefinition().type.get());
        assertFalse(result.getModuleDefinition().path.isPresent());
	}

	@Test
	void testDeclareRootModule() {
		final DiscoveryBuilderImpl builder = new DiscoveryBuilderImpl(FAKE_CLASS_NAME, "src/cobol/TESTMODULE.cbl");

		builder.declareRootModule("TESTMODULE", ModuleType.COBOL_PROGRAM);

		final List<ContributorResult> results = builder.buildResults();

		assertEquals(1, results.size());
		final ContributorResult result = results.get(0);

		assertEquals(ContributorResult.Type.ROOT_MODULE, result.getType());

		assertEquals(Collections.singleton(ModuleType.COBOL_PROGRAM), result.getModuleFilter().getTypes());
		assertEquals(Collections.singleton("TESTMODULE"), result.getModuleFilter().getNames());
		assertEquals(Collections.singleton("src/cobol/TESTMODULE.cbl"), result.getModuleFilter().getPaths());

		assertEquals("TESTMODULE", result.getModuleDefinition().name.get());
		assertEquals(Technology.COBOL, result.getModuleDefinition().technology.get());
		assertEquals(Type.PROGRAM, result.getModuleDefinition().type.get());
		assertEquals("src/cobol/TESTMODULE.cbl", result.getModuleDefinition().path.get());
	}

	@Test
	void testDeclareSubModule() {
		final DiscoveryBuilderImpl builder = new DiscoveryBuilderImpl(FAKE_CLASS_NAME, "src/cobol/TESTMODULE.cbl");

		builder.declareRootModule("TESTMODULE", ModuleType.JAVA_COMPILATION_UNIT);

		builder.declareSubModule("SUBMODULE", ModuleType.JAVA_TYPE);

		final List<ContributorResult> results = builder.buildResults();

		assertEquals(2, results.size());
		final ContributorResult result = results.get(1);

		assertEquals(ContributorResult.Type.SUB_MODULE, result.getType());

		assertEquals(Collections.singleton(ModuleType.JAVA_TYPE), result.getModuleFilter().getTypes());
		assertEquals(Collections.singleton("SUBMODULE"), result.getModuleFilter().getNames());
		assertEquals(Collections.singleton(ModuleType.JAVA_COMPILATION_UNIT), result.getModuleFilter().getContainedIn().get().getTypes());
		assertEquals(Collections.singleton("TESTMODULE"), result.getModuleFilter().getContainedIn().get().getNames());
		assertEquals(Collections.singleton("src/cobol/TESTMODULE.cbl"), result.getModuleFilter().getContainedIn().get().getPaths());

		assertEquals("SUBMODULE", result.getModuleDefinition().name.get());
		assertEquals(Technology.JAVA, result.getModuleDefinition().technology.get());
		assertEquals(Type.TYPE, result.getModuleDefinition().type.get());
	}

	@Test
	void testDeclareSubModuleWithoutRootModule() {
		final DiscoveryBuilderImpl builder = new DiscoveryBuilderImpl(FAKE_CLASS_NAME, "src/cobol/TESTMODULE.cbl");

		builder.declareSubModule("TESTMODULE", ModuleType.COBOL_PROGRAM);

		assertThrows(IllegalStateException.class, builder::buildResults);
	}

	@Test
	void testAnchorTo() {
		final DiscoveryBuilderImpl builder = new DiscoveryBuilderImpl(FAKE_CLASS_NAME);

		builder.anchorTo(new ModuleFilter().setNames("TESTMODULE").setTypes(ModuleType.COBOL_PROGRAM));

		final List<ContributorResult> results = builder.buildResults();

		assertEquals(1, results.size());
		final ContributorResult result = results.get(0);

		assertEquals(ContributorResult.Type.ANCHOR_TO, result.getType(), "Type must match");

		assertEquals(Collections.singleton(ModuleType.COBOL_PROGRAM), result.getModuleFilter().getTypes());
		assertEquals(Collections.singleton("TESTMODULE"), result.getModuleFilter().getNames());

		assertFalse(result.getModuleDefinition().name.isPresent());
		assertFalse(result.getModuleDefinition().path.isPresent());
	}

	@Test
	void testBlankModuleName() {
		final String path = "src/cobol/TESTMODULE.cbl";
		
		final Consumer<List<ContributorResult>> expectedErrorMarker = results -> assertEquals(1, results.get(results.size() - 1).getErrors().size());

		/* An exception should be thrown if rootModule name is blank */
		final var exception = assertThrows(IllegalStateException.class,
				() -> testValidateModuleName(builder -> builder.declareRootModule("",
						ModuleType.COBOL_PROGRAM),	path,
						results -> {},
				expectedErrorMarker));
		assertTrue(exception.getMessage().contains("Name cannot be blank for the declared root module"));

		testValidateModuleName(builder -> {
			builder.declareRootModule("ROOTMODULE", ModuleType.COBOL_PROGRAM);
			builder.declareSubModule("", ModuleType.COBOL_COPYPROC);
		}, path, results -> assertTrue(results.startsWith("ROOTMODULE")), expectedErrorMarker);

		testValidateModuleName(builder -> {
			builder.declareRootModule("Program1", ModuleType.COBOL_PROGRAM);
			builder.declareSubModule("", ModuleType.COBOL_COPYPROC);
		}, path, results -> assertTrue(results.startsWith("Program1")), expectedErrorMarker);

		assertThrows(IllegalStateException.class, () -> testValidateModuleName(builder -> {
			builder.declareRootModule("", ModuleType.COBOL_PROGRAM);
			builder.declareSubModule("", ModuleType.COBOL_COPYPROC);
		}, "", results -> {}, expectedErrorMarker));

	}

	private void testValidateModuleName(final Consumer<DiscoveryBuilderImpl> builderDeclaration, final String path, final Consumer<String> expectedString,
			final Consumer<List<ContributorResult>> expectedResults) {
		final DiscoveryBuilderImpl builder = new DiscoveryBuilderImpl(FAKE_CLASS_NAME, path);
		builderDeclaration.accept(builder);

		final List<ContributorResult> results = builder.buildResults();
		assertFalse(results.isEmpty());

		expectedString.accept(results.get(results.size() - 1).getModuleFilter().getNames().iterator().next());
		final Optional<String> moduleName = Optional.ofNullable(results.get(results.size() - 1).getModuleDefinition().name.get());
		assertTrue(moduleName.isPresent());
		expectedString.accept(moduleName.get());
		expectedResults.accept(results);
	}
}

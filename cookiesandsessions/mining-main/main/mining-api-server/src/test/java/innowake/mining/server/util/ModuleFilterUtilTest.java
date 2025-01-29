/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.util;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.config.searchorder.Candidate;
import innowake.mining.shared.discovery.config.searchorder.ContainedIn;
import innowake.mining.shared.discovery.config.searchorder.Source;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.discovery.ResolveTarget;

/**
 * Tests for the {@link ModuleFilterUtil}
 */
class ModuleFilterUtilTest extends DatabaseRelatedTest{

	@Autowired
	private ModuleFilterUtil moduleFilterUtil;
	
	/**
	 * Tests the {@link ModuleFilterUtil#toModuleFilter(Candidate, Optional)}.
	 */
	@Test
	void testToModuleFilterWithNullContainedInCandidate() {
		final Candidate candidate = new Source("test", "JAVA_COMPILATION_UNIT", "src/java/com/demo/Class.java", "src/com/demo/**/*.java", null);
		final ModuleFilter moduleFilter = moduleFilterUtil.toModuleFilter(candidate, Optional.empty());
		assertTrue(moduleFilter.getNames().contains("test"), "ModuleFilter should have test in names");
		assertTrue(moduleFilter.getTypes().contains(ModuleType.JAVA_COMPILATION_UNIT), "ModuleFilter should have JAVA_COMPILATION_UNIT in types");
		assertTrue(moduleFilter.getPaths().contains("src/java/com/demo/Class.java"), "ModuleFilter should have src/java/com/demo/Class.java in paths");
		assertTrue(moduleFilter.getPathPatterns().contains("src/com/demo/**/*.java"), "ModuleFilter should have src/com/demo/**/*.java in path pattern");
		assertFalse(moduleFilter.getContainedIn().isPresent(), "ModuleFilter shouldn't have containedIn");	
	}
	
	/**
	 * Tests the {@link ModuleFilterUtil#toModuleFilter(Candidate, Optional)} with invalid {@link Candidate}.
	 */
	@Test
	void testToModuleFilterWithInvalidCandidate() {
		final Candidate candidate = new Source(null, null, null, null, null);
		final ModuleFilter moduleFilter = moduleFilterUtil.toModuleFilter(candidate, Optional.empty());
		assertTrue(moduleFilter.getNames().isEmpty(), "Names should be empty");
		assertTrue(moduleFilter.getTypes().isEmpty(), "types should be empty");
		assertTrue(moduleFilter.getPaths().isEmpty(), "Paths should be empty");
		assertTrue(moduleFilter.getPathPatterns().isEmpty(), "Path pattern should be empty");
		assertFalse(moduleFilter.getContainedIn().isPresent(), "ContainedIn should be empty");	
	}
	
	/**
	 * Tests the {@link ModuleFilterUtil#toModuleFilter(Candidate, Optional)} with recursive {@link ContainedIn}.
	 */
	@Test
	void testToModuleFilterWithContainedInRecursively() {
		final ContainedIn containedIn2 = new ContainedIn("test2", "JAVA_COMPILATION_UNIT", "src/java/com/demo/Class2.java", "src/com/demo2/**/*.java", null);
		final ContainedIn containedIn1 = new ContainedIn("test1", "JAVA_COMPILATION_UNIT", "src/java/com/demo/Class1.java", "src/com/demo1/**/*.java",
				containedIn2);
		final Candidate candidate = new Source("test", "JAVA_COMPILATION_UNIT", "src/java/com/demo/Class.java", "src/com/demo/**/*.java", containedIn1);
		final ModuleFilter moduleFilter = moduleFilterUtil.toModuleFilter(candidate, Optional.of("src/java/com/demo"));
		
		assertTrue(moduleFilter.getNames().contains("test"), "ModuleFilter should have test in names");
		assertTrue(moduleFilter.getTypes().contains(ModuleType.JAVA_COMPILATION_UNIT), "ModuleFilter should have JAVA_COMPILATION_UNIT in types");
		assertTrue(moduleFilter.getPaths().contains("src/java/com/demo/Class.java"), "ModuleFilter should have src/java/com/demo/Class.java in paths");
		assertTrue(moduleFilter.getPathPatterns().contains("src/com/demo/**/*.java"), "ModuleFilter should have src/com/demo/**/*.java in path pattern");
		assertTrue(moduleFilter.getContainedIn().isPresent(), "ModuleFilter should have containedIn");	
		
		final ModuleFilter moduleFilter1 = moduleFilter.getContainedIn().get();
		
		assertTrue(moduleFilter1.getNames().contains("test1"), "ModuleFilter should have test1 in names");
		assertTrue(moduleFilter1.getTypes().contains(ModuleType.JAVA_COMPILATION_UNIT), "ModuleFilter should have JAVA_COMPILATION_UNIT in types");
		assertTrue(moduleFilter1.getPaths().contains("src/java/com/demo/Class1.java"), "ModuleFilter should have src/java/com/demo/Class1.java in paths");
		assertTrue(moduleFilter1.getPathPatterns().contains("src/com/demo1/**/*.java"), "ModuleFilter should have src/com/demo1/**/*.java in path pattern");
		assertTrue(moduleFilter1.getContainedIn().isPresent(), "ModuleFilter should have containedIn");	
		
		final ModuleFilter moduleFilter2 = moduleFilter1.getContainedIn().get();
		
		assertTrue(moduleFilter2.getNames().contains("test2"), "ModuleFilter should have test2 in names");
		assertTrue(moduleFilter2.getTypes().contains(ModuleType.JAVA_COMPILATION_UNIT), "ModuleFilter should have JAVA_COMPILATION_UNIT in types");
		assertTrue(moduleFilter2.getPaths().contains("src/java/com/demo/Class2.java"), "ModuleFilter should have src/java/com/demo/Class2.java in paths");
		assertTrue(moduleFilter2.getPathPatterns().contains("src/com/demo2/**/*.java"), "ModuleFilter should have src/com/demo2/**/*.java in path pattern");
		assertFalse(moduleFilter2.getContainedIn().isPresent(), "ModuleFilter should have containedIn");	
	}
	
	/**
	 * Tests the {@link ModuleFilterUtil#toModuleFilter(Candidate, Optional)} with path pattern containing ./ as a initial pattern.
	 */
	@Test
	void testToModuleFilterWithSpecialPathPattern() {
		final Candidate candidate = new Source(null, null, null, "./main/**/*.java", null);
		final ModuleFilter moduleFilter = moduleFilterUtil.toModuleFilter(candidate, Optional.of("src/java/com/demo/test.java"));
		assertTrue(moduleFilter.getPathPatterns().contains("src/java/com/demo/main/**/*.java"));
	}
	
	/**
	 * Tests the {@link ModuleFilterUtil#matches(ModuleFilter, ModelArtifact)} with same moduleIds.
	 */
	@Test
	void testMatchesWithValidModuleId() {
		final ModuleFilter moduleFilter = new ModuleFilter().setModuleIds(EntityId.of(0L));
		assertTrue(moduleFilterUtil.matches(moduleFilter, createModelArtifact()));
	}
	
	/**
	 * Tests the {@link ModuleFilterUtil#matches(ModuleFilter, ModelArtifact)} with different moduleIds.
	 */
	@Test
	void testMatchesWithInvalidModuleId() {
		final ModuleFilter moduleFilter = new ModuleFilter().setModuleIds(EntityId.of(1L));
		assertFalse(moduleFilterUtil.matches(moduleFilter, createModelArtifact()));
	}
	
	/**
	 * Tests the {@link ModuleFilterUtil#matches(ModuleFilter, ModelArtifact)} with same names.
	 */
	@Test
	void testMatchesWithValidName() {
		final ModuleFilter moduleFilter = new ModuleFilter().setNames("test");
		assertTrue(moduleFilterUtil.matches(moduleFilter, createModelArtifact()));
	}
	
	/**
	 * Tests the {@link ModuleFilterUtil#matches(ModuleFilter, ModelArtifact)} with different names.
	 */
	@Test
	void testMatchesWithInvalidName() {
		final ModuleFilter moduleFilter = new ModuleFilter().setNames("test1");
		assertFalse(moduleFilterUtil.matches(moduleFilter, createModelArtifact()));
	}
	
	/**
	 * Tests the {@link ModuleFilterUtil#matches(ModuleFilter, ModelArtifact)} with same types.
	 */
	@Test
	void testMatchesWithValidType() {
		final ModuleFilter moduleFilter = new ModuleFilter().setTypes(ModuleType.JAVA_COMPILATION_UNIT);
		assertTrue(moduleFilterUtil.matches(moduleFilter, createModelArtifact()));
	}
	
	/**
	 * Tests the {@link ModuleFilterUtil#matches(ModuleFilter, ModelArtifact)} with different types.
	 */
	@Test
	void testMatchesWithInvalidType() {
		final ModuleFilter moduleFilter = new ModuleFilter().setTypes(ModuleType.JAVA_ANNOTATION);
		assertFalse(moduleFilterUtil.matches(moduleFilter, createModelArtifact()));
	}
	
	/**
	 * Tests the {@link ModuleFilterUtil#matches(ModuleFilter, ModelArtifact)} with same paths.
	 */
	@Test
	void testMatchesWithValidPath() {
		final ModuleFilter moduleFilter = new ModuleFilter().setPaths("src/com/demo/main/Class.java");
		assertTrue(moduleFilterUtil.matches(moduleFilter, createModelArtifact()));
	}
	
	/**
	 * Tests the {@link ModuleFilterUtil#matches(ModuleFilter, ModelArtifact)} with different paths.
	 */
	@Test
	void testMatchesWithInvalidPath() {
		final ModuleFilter moduleFilter = new ModuleFilter().setPaths("src/com/demo/main/java/Class.java");
		assertFalse(moduleFilterUtil.matches(moduleFilter, createModelArtifact()));
	}
	
	/**
	 * Tests the {@link ModuleFilterUtil#matches(ModuleFilter, ModelArtifact)} with same path patterns.
	 */
	@Test
	void testMatchesWithValidPathPattern() {
		final ModuleFilter moduleFilter = new ModuleFilter().setPathPatterns("src/com/demo/**/*.java");
		assertTrue(moduleFilterUtil.matches(moduleFilter, createModelArtifact()));
	}
	
	/**
	 * Tests the {@link ModuleFilterUtil#matches(ModuleFilter, ModelArtifact)} with different path patterns.
	 */
	@Test
	void testMatchesWithInvalidPathPattern() {
		final ModuleFilter moduleFilter = new ModuleFilter().setPathPatterns("src/comm/demo1/**/*.java");
		assertFalse(moduleFilterUtil.matches(moduleFilter, createModelArtifact()));
	}
	
	/**
	 * Tests the {@link ModuleFilterUtil#matches(ModuleFilter, ModelArtifact)} with same contained in.
	 */
	@Test
	void testMatchesWithValidContainedIn() {
		final ModuleFilter moduleFilter1 = createModuleFilter(Long.valueOf(0L), "test1", ModuleType.JAVA_COMPILATION_UNIT, "src/com/demo/Class1.java",
				"src/com/demo/**/*.java");
		final ModuleFilter moduleFilter = createModuleFilter(Long.valueOf(0L), "test", ModuleType.JAVA_COMPILATION_UNIT, "src/com/demo/Class.java",
				"src/com/demo/**/*.java");
		moduleFilter.setContainedIn(moduleFilter1);
		
		final ModelArtifact modelArtifact1 = createModelArtifact("test1", ResolveTarget.JAVA_COMPILATION_UNIT, "src/com/demo/Class1.java", Long.valueOf(0L));
		final ModelArtifact modelArtifact = createModelArtifact("test", ResolveTarget.JAVA_COMPILATION_UNIT, "src/com/demo/Class.java", Long.valueOf(0L));
		modelArtifact.setParentModule(modelArtifact1);
		
		assertTrue(moduleFilterUtil.matches(moduleFilter, modelArtifact));
	}
	
	/**
	 * Tests the {@link ModuleFilterUtil#matches(ModuleFilter, ModelArtifact)} with different contained in.
	 */
	@Test
	void testMatchesWithInvalidContainedIn() {
		final ModuleFilter moduleFilter1 = createModuleFilter(Long.valueOf(0L), "test1", ModuleType.JAVA_COMPILATION_UNIT, "src/com/demo/Class1.java",
				"src/com/demo/**/*.java");
		final ModuleFilter moduleFilter = createModuleFilter(Long.valueOf(0L), "test2", ModuleType.JAVA_COMPILATION_UNIT, "src/com/demo/Class2.java",
				"src/com/demo/**/*.java");
		moduleFilter.setContainedIn(moduleFilter1);
		
		final ModelArtifact modelArtifact1 = createModelArtifact("test1", ResolveTarget.JAVA_COMPILATION_UNIT, "src/com/demo/Class1.java", Long.valueOf(0L));
		final ModelArtifact modelArtifact = createModelArtifact("test", ResolveTarget.JAVA_COMPILATION_UNIT, "src/com/demo/Class.java", Long.valueOf(0L));
		modelArtifact.setParentModule(modelArtifact1);
		
		assertFalse(moduleFilterUtil.matches(moduleFilter, modelArtifact));
	}
	
	/**
	 * Tests the {@link ModuleFilterUtil#matches(ModuleFilter, ModelArtifact)} with valid contained in and without parent {@link ModelArtifact}.
	 */
	@Test
	void testMatchesWithValidContainedInAndWithoutParentModule() {
		final ModuleFilter moduleFilter1 = createModuleFilter(Long.valueOf(0L), "test1", ModuleType.JAVA_COMPILATION_UNIT, "src/com/demo/Class1.java",
				"src/com/demo/**/*.java");
		final ModuleFilter moduleFilter = createModuleFilter(Long.valueOf(0L), "test2", ModuleType.JAVA_COMPILATION_UNIT, "src/com/demo/Class2.java",
				"src/com/demo/**/*.java");
		moduleFilter.setContainedIn(moduleFilter1);
		
		final ModelArtifact modelArtifact = createModelArtifact("test1", ResolveTarget.JAVA_COMPILATION_UNIT, "src/com/demo/Class1.java", Long.valueOf(0L));
		
		assertFalse(moduleFilterUtil.matches(moduleFilter, modelArtifact));
	}
	
	/**
	 * Tests the {@link ModuleFilterUtil#matches(ModuleFilter, ModelArtifact)} with all possible data and with invalid path pattern.
	 */
	@Test
	void testMatchesWithAllPossibleData() {
		final ModuleFilter moduleFilter1 = createModuleFilter(Long.valueOf(0L), "test1", ModuleType.JAVA_COMPILATION_UNIT, "src/com/demo/Class1.java",
				"src/com/demo/**/*.java");
		final ModuleFilter moduleFilter = createModuleFilter(Long.valueOf(0L), "test2", ModuleType.JAVA_COMPILATION_UNIT, "src/com/demo/Class2.java",
				"src/com/demo/**/*.java");
		moduleFilter.setContainedIn(moduleFilter1);
		
		final ModelArtifact modelArtifact1 = createModelArtifact("test1", ResolveTarget.JAVA_COMPILATION_UNIT, "src/com/demo/Class1.java", Long.valueOf(0L));
		final ModelArtifact modelArtifact = createModelArtifact("test2", ResolveTarget.JAVA_COMPILATION_UNIT, "src/com/test/Class.java", Long.valueOf(0L));
		modelArtifact.setParentModule(modelArtifact1);
		
		assertFalse(moduleFilterUtil.matches(moduleFilter, modelArtifact));
	}
	
	private ModuleFilter createModuleFilter(final Long moduleId, final String name, final ModuleType moduleType, final String path,
			final String pathPattern) {
		return new ModuleFilter()
				.setModuleIds(EntityId.of(moduleId))
				.setNames(name)
				.setTypes(moduleType)
				.setPathPatterns(pathPattern)
				.setPaths(path);
	}
	
	private ModelArtifact createModelArtifact() {
		return createModelArtifact("test", ResolveTarget.JAVA_COMPILATION_UNIT, "src/com/demo/main/Class.java", 0L);
	}
	
	private ModelArtifact createModelArtifact(final String name, final ResolveTarget resolvetarget, final String path, final Long moduleId) {
		return new ModelArtifact()
				.setName(name)
				.setType(resolvetarget)
				.setPath(path)
				.setModuleId(EntityId.of(UUID.randomUUID(), moduleId));
	}
}

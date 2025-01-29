/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.server.functionalblocks.generation;

import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.functionalblocks.service.FunctionalBlockGenerationService;
import innowake.mining.server.job.identification.AbstractIdentificationTest;
import innowake.mining.server.job.identification.IdentifyCandidatesJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.function.Executable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import java.util.*;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;

/**
 * This class is used to test the generation of structural functional blocks {@link StructuralFunctionalBlockGeneration}.
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@WithMockUser
class StructuralFunctionalBlockGenerationTest extends AbstractIdentificationTest {

	@Autowired
	private transient FunctionalBlockGenerationService functionalBlockGenerationService;
	@Autowired
	private FunctionalBlockService functionalBlockService;
	@Autowired
	private JobManager jobManager;
	private EntityId moduleId;

	/**
	 * This method is run before all tests.
	 * It initializes the modules and their relationships.
	 */
	@BeforeAll
	void init() {
		 moduleId = createModule(PROJECT_ID_1, "MMRS7101", "MMRS7101.cbl", RESOURCE_PATH, Technology.COBOL, Type.PROGRAM);
	}

	/**
	 * This test checks if the generate method throws an exception when the module ID is null.
	 */
	@Test
	@Order(1)
	void generateShouldThrowExceptionWhenModuleIdIsNull() {
		final Executable task = () -> functionalBlockGenerationService.generate(StructuralFunctionalBlockGeneration.class,
				new FunctionalBlockGenerationContext(PROJECT_ID_1), null);
		assertEquals("Cannot execute module block generation because the 'moduleId' is null",
				assertThrows(IllegalArgumentException.class, task).getMessage(),
				"Expected exception with message 'Cannot execute module block generation because the 'moduleId' is null'");
	}

	/**
	 * This test checks if the generate method throws an exception when the module is not found.
	 */
	@Test
	@Order(2)
	void generateShouldThrowExceptionWhenModuleNotFound() {
		final Executable task = () -> functionalBlockGenerationService.generate(StructuralFunctionalBlockGeneration.class,
				new FunctionalBlockGenerationContext(PROJECT_ID_1), EntityId.of(100L));
		assertEquals("Cannot execute module block generation because the 'module' is null",
				assertThrows(IllegalArgumentException.class, task).getMessage(),
				"Expected exception with message 'Cannot execute module block generation because the 'module' is null'");
	}

	/**
	 * This test checks if the generate method returns empty when the module block is not found.
	 */
	@Test
	@Order(3)
	void generateShouldReturnEmptyWhenModuleBlockNotFound() {
		assertTrue(functionalBlockGenerationService.generate(StructuralFunctionalBlockGeneration.class,
				new FunctionalBlockGenerationContext(PROJECT_ID_1), moduleId).isEmpty(),
				"Expected empty list when the module block is not found");
	}

	/**
	 * This test checks if the generate method returns a structural block.
	 */
	@Test
	@Order(4)
	void generateShouldReturnStructuralBlock() {
		submitJob(jobManager, new IdentifyCandidatesJob(PROJECT_ID_1, new ModuleMatcher(Collections.singletonList(moduleId), Collections.emptyList())));
		functionalBlockGenerationService.generate(ModuleBlockGeneration.class,
				new FunctionalBlockGenerationContext(PROJECT_ID_1), moduleId);
		final Collection<Pair<FunctionalBlockGenerationResult.Operation, UUID>> response = functionalBlockGenerationService.generate(
				StructuralFunctionalBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_1), moduleId);
		assertFalse(response.isEmpty(), "Expected creation of structural block");

		final List<FunctionalBlockPojo> structureBlocks = functionalBlockService.find(q -> q
				.ofProject(PROJECT_ID_1)
				.withType(FunctionalBlockType.FUNCTIONAL_GROUP)
				.withFlag(FunctionalBlockFlag.GENERATED_BY, "StructuralFunctionalBlockGeneration"));

		assertFalse(structureBlocks.isEmpty(), "Expected created structural blocks related to the module");
		final List<String> structureBlockNames = structureBlocks.stream().map(FunctionalBlockPojo::getName).collect(Collectors.toList());
		assertTrue(structureBlockNames.containsAll(Arrays.asList("MMRS7101", "TRUNCTEST")),
				"Expected that structural blocks are created for the module with the names MMRS7101 and TRUNCTEST");
	}
}

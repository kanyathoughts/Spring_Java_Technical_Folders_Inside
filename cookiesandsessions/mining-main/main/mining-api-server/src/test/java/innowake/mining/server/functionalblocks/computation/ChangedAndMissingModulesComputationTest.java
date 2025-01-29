/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.computation;

import innowake.lib.job.api.NullProgressMonitor;
import innowake.mining.server.functionalblocks.generation.ModuleBlockGeneration;
import innowake.mining.shared.access.BinaryValue;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.GeneratedFrom;
import innowake.mining.shared.entities.testing.ModulePojoDummy;
import innowake.mining.shared.hashing.CityHash;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import org.apache.calcite.avatica.org.apache.commons.codec.binary.Hex;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.mockito.Mockito;

import java.nio.ByteOrder;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Mocked Test class for {@link ChangedAndMissingModulesComputation}
 */
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@Tag("mocked")
class ChangedAndMissingModulesComputationTest {

	private static final Long PROJECT_ID = 1L;
	private FunctionalBlockPojo functionalBlock1;
	private ModulePojo module1;
	private GeneratedFrom generatedFrom1;
	private GeneratedFrom generatedFrom2;
	private GeneratedFrom generatedFrom3;

	@BeforeAll
	void init() {
		final UUID functionalblockUID1 = UUID.randomUUID();
		final UUID projectUUid = UUID.randomUUID();

		Map<String, Object> flags = new HashMap<>();
		flags.put(FunctionalBlockFlag.GENERATED_BY.name(), ModuleBlockGeneration.MODULE_BLOCK_GENERATION_ID);
		flags.put(FunctionalBlockFlag.TYPE.name(), FunctionalBlockType.MODULE);

		functionalBlock1 = new FunctionalBlockPojo(functionalblockUID1,
				null,
				EntityId.of(projectUUid, PROJECT_ID),
				Collections.emptyList(),
				null,
				Collections.emptyList(),
				"block 1",
				null,
				flags,
				null);

		final byte[] contentHash1 = CityHash.cityHash128(ByteOrder.BIG_ENDIAN, "ContentHash1".getBytes(StandardCharsets.UTF_8));
		final byte[] contentHash2 = CityHash.cityHash128(ByteOrder.BIG_ENDIAN, "ContentHash2".getBytes(StandardCharsets.UTF_8));

		module1 = ModulePojoDummy.build(new ModulePojoPrototype()
				.setLinkHash("LinkHash1")
				.setContentHash(new BinaryValue(contentHash1))
				.setDependencyHash("DependencyHash1")
				.setNid(1L)
				.setUid(UUID.randomUUID())
				.setOrigin(Origin.CUSTOM)
				.setType(Type.TYPE)
				.setStorage(Storage.DATABASE)
				.setTechnology(Technology.ASSEMBLER));

		generatedFrom1 =  GeneratedFrom.fromModule("LinkHash1", Hex.encodeHexString(contentHash1, true), "DependencyHash1");
		generatedFrom2 =  GeneratedFrom.fromModule("LinkHash1", Hex.encodeHexString(contentHash2, true), "DependencyHash1");
		generatedFrom3 = GeneratedFrom.fromModule("LinkHash1", Hex.encodeHexString(contentHash1, true), "DependencyHash2");
	}

	@Test
	void testNoChange() {
		/* test that GeneratedFrom of functional block is not updated when module is not modified or missing */
		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final var moduleService = Mockito.mock(ModuleService.class);

		when(functionalBlockService.getGeneratedFrom(anyList())).thenReturn(Map.of(functionalBlock1.getUid(),generatedFrom1));
		when(moduleService.findModules(any())).thenReturn(List.of(module1));

		final ChangedAndMissingModulesComputation reviewComputation = new ChangedAndMissingModulesComputation(functionalBlockService, moduleService);

		final Map<FunctionalBlockPojo, GeneratedFrom> computationResult = reviewComputation.computeBatched(List.of(functionalBlock1),
				new NullProgressMonitor());
		assertNull(computationResult.get(functionalBlock1), "expected no change for functionalBlock1");
	}

	@Test
	void testFunctionalBlockPojoPrototypeRequestReviewFlagComputation() {
		/* test that "contentChanged" timestamp is set and the content hash of "GeneratedFrom" is updated
		 * when the content hash of the module does not match the content hash recorded in "GeneratedFrom" */
		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final var moduleService = Mockito.mock(ModuleService.class);

		when(functionalBlockService.getGeneratedFrom(anyList())).thenReturn(Map.of(functionalBlock1.getUid(), generatedFrom2));
		when(moduleService.findModules(any())).thenReturn(List.of(module1));

		final ChangedAndMissingModulesComputation reviewComputation = new ChangedAndMissingModulesComputation(functionalBlockService, moduleService);

		final Instant now = Instant.now();
		final Map<FunctionalBlockPojo, GeneratedFrom> computationResult = reviewComputation.computeBatched(List.of(functionalBlock1),
				new NullProgressMonitor());

		final GeneratedFrom block1Result = computationResult.get(functionalBlock1);
		assertNotNull(block1Result);
		final Optional<String> newContentHash = block1Result.getModuleContentHash();
		assertTrue(newContentHash.isPresent());
		assertTrue(module1.getContentHash().isPresent());
		assertEquals(module1.getContentHash().get().toString(), newContentHash.get());
		final Optional<Instant> contentChanged = block1Result.getContentChanged();
		assertTrue(contentChanged.isPresent());
		assertThat(contentChanged.get(), greaterThanOrEqualTo(now));
		assertFalse(block1Result.isMissing());
	}


	@Test
	void testFunctionalBlockPojoPrototypeMissingFlagComputation() {
		/* test that "missingSince" timestamp is set when the module can not be found */
		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final var moduleService = Mockito.mock(ModuleService.class);

		when(functionalBlockService.getGeneratedFrom(anyList())).thenReturn(Map.of(functionalBlock1.getUid(), generatedFrom1));
		when(moduleService.findModules(any())).thenReturn(Collections.emptyList());

		final ChangedAndMissingModulesComputation reviewComputation = new ChangedAndMissingModulesComputation(functionalBlockService, moduleService);

		final Instant now = Instant.now();
		final Map<FunctionalBlockPojo, GeneratedFrom> computationResult = reviewComputation.computeBatched(List.of(functionalBlock1),
				new NullProgressMonitor());

		final GeneratedFrom block1Result = computationResult.get(functionalBlock1);
		assertNotNull(block1Result);
		assertTrue(block1Result.isMissing());
		final Optional<Instant> missingSince = block1Result.getMissingSince();
		assertTrue(missingSince.isPresent());
		assertThat(missingSince.get(), greaterThanOrEqualTo(now));
		final Optional<Instant> contentChanged = block1Result.getContentChanged();
		assertFalse(contentChanged.isPresent());
	}

	@Test
	void testFunctionalBlockPojoPrototypeDependencyChangedFlagComputation() {
		/* test that "dependencyChanged" timestamp is set and the dependency hash of "GeneratedFrom" is updated
		 * when the dependency hash of the module does not match the dependency hash recorded in "GeneratedFrom" */
		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final var moduleService = Mockito.mock(ModuleService.class);

		when(functionalBlockService.getGeneratedFrom(anyList())).thenReturn(Map.of(functionalBlock1.getUid(), generatedFrom3));
		when(moduleService.findModules(any())).thenReturn(List.of(module1));

		final ChangedAndMissingModulesComputation reviewComputation = new ChangedAndMissingModulesComputation(functionalBlockService, moduleService);

		final Instant now = Instant.now();
		final Map<FunctionalBlockPojo, GeneratedFrom> computationResult = reviewComputation.computeBatched(List.of(functionalBlock1),
				new NullProgressMonitor());

		final GeneratedFrom block1Result = computationResult.get(functionalBlock1);
		assertNotNull(block1Result);
		final Optional<String> newDependencyHash = block1Result.getModuleDependencyHash();
		assertTrue(newDependencyHash.isPresent());
		assertTrue(module1.getDependencyHash().isPresent());
		assertEquals(module1.getDependencyHash().get(), newDependencyHash.get());
		final Optional<Instant> dependencyChanged = block1Result.getDependencyChanged();
		assertTrue(dependencyChanged.isPresent());
		assertThat(dependencyChanged.get(), greaterThanOrEqualTo(now));
		assertFalse(block1Result.isMissing());
	}
}

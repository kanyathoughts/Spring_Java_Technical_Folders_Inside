/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.computation;

import innowake.lib.job.api.NullProgressMonitor;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.Map;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Mocked Test for ReachabilityBlockOutdatedComputation
 */
@Tag("mocked")
class ReachabilityBlockOudatedComputationTest {

	private static final Long PROJECT_ID = 1L;
	private static FunctionalBlockService functionalBlockService;
	private static FunctionalBlockPojo functionalBlockHavingNoGeneratedAtFlag;
	private static FunctionalBlockPojo changedAndMissingFunctionalBlock;
	private static FunctionalBlockPojo functionalBlock;

	@BeforeAll
	static void init() {
		final UUID projectUUid = UUID.randomUUID();
		functionalBlockHavingNoGeneratedAtFlag = new FunctionalBlockPojo(UUID.randomUUID(), null, EntityId.of(projectUUid, PROJECT_ID),
				Collections.emptyList(), null, Collections.emptyList(), "functionalBlockHavingNoGeneratedAtFlag",
				"functionalBlockHavingNoGeneratedAtFlag", Collections.emptyMap(),null);
		functionalBlock = new FunctionalBlockPojo(UUID.randomUUID(), null, EntityId.of(projectUUid, PROJECT_ID),
				Collections.emptyList(), null, Collections.emptyList(), "functionalBlockHavingChangedModules",
				"functionalBlockHavingChangedModules", Map.of(FunctionalBlockFlag.GENERATED_AT.name(), Long.valueOf("1709788846402")),null);
		changedAndMissingFunctionalBlock = new FunctionalBlockPojo(UUID.randomUUID(), null, EntityId.of(projectUUid, PROJECT_ID),
				Collections.emptyList(), null, Collections.emptyList(), "changedAndMissingFunctionalBlock",
				"changedFunctionalBlock", Collections.emptyMap(),null);
	}

	@Test
	void testWhenGeneratedAtIsNull() {
		functionalBlockService = mock(FunctionalBlockService.class);
		final ReachabilityBlockOutdatedComputation reachabilityBlockOutdatedComputation = new ReachabilityBlockOutdatedComputation(functionalBlockService);
		final FunctionalBlockPojoPrototype functionalBlockPojoPrototype = reachabilityBlockOutdatedComputation.compute(functionalBlockHavingNoGeneratedAtFlag,
				new NullProgressMonitor());
		assertNotNull(functionalBlockPojoPrototype);
		assertTrue((Boolean) functionalBlockPojoPrototype.flags.getNonNull().get("OUTDATED"));
	}

	@Test
	void testWhenGeneratedAtIsNotNullAndChangedModulesIsNotEmpty() {
		functionalBlockService = mock(FunctionalBlockService.class);
		when(functionalBlockService.findChildrenDeep(any(UUID.class), anyInt(), any(BuildingConsumer.class)))
				.thenReturn(Collections.singletonList(changedAndMissingFunctionalBlock));
		final ReachabilityBlockOutdatedComputation reachabilityBlockOutdatedComputation = new ReachabilityBlockOutdatedComputation(functionalBlockService);
		final FunctionalBlockPojoPrototype functionalBlockPojoPrototype = reachabilityBlockOutdatedComputation.compute(functionalBlock,
				new NullProgressMonitor());
		assertNotNull(functionalBlockPojoPrototype);
		assertTrue((Boolean) functionalBlockPojoPrototype.flags.getNonNull().get("OUTDATED"));
	}

	@Test
	void testWhenGeneratedAtIsNotNullAndChangedModulesIsEmptyAndMissingModulesIsEmpty() {
		functionalBlockService = mock(FunctionalBlockService.class);
		when(functionalBlockService.findChildrenDeep(any(UUID.class), anyInt(), any(BuildingConsumer.class)))
				.thenReturn(Collections.emptyList()).thenReturn(Collections.emptyList());
		final ReachabilityBlockOutdatedComputation reachabilityBlockOutdatedComputation = new ReachabilityBlockOutdatedComputation(functionalBlockService);
		assertNull(reachabilityBlockOutdatedComputation.compute(functionalBlock, new NullProgressMonitor()));
	}
}

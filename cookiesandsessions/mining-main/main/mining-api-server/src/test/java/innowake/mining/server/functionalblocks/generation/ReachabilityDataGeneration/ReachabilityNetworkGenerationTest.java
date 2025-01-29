/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.generation.ReachabilityDataGeneration;

import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationContext;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationResult;
import innowake.mining.server.functionalblocks.generation.ReachabilityNetworkGeneration;
import innowake.mining.server.functionalblocks.service.FunctionalBlockServiceImpl;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.lang.BuildingConsumer;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Mocked Test for ReachabilityNetworkGeneration
 */
@Tag("mocked")
class ReachabilityNetworkGenerationTest {

	private static final EntityId PROJECT_ID = EntityId.VOID;

	@Test
	void testReachabilityNetworkBlockGeneration() {
		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockServiceImpl.class);
		final ReachabilityNetworkGeneration reachabilityNetworkGeneration = new ReachabilityNetworkGeneration(functionalBlockService);

		final var mergeChild1 = new FunctionalBlockPojo(UUID.randomUUID(), null, PROJECT_ID, null, null, null, "MergeChild 1", "",
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name())), null);
		final var mergeChild2 = new FunctionalBlockPojo(UUID.randomUUID(), null, PROJECT_ID, null, null, null, "MergeChild 2", "",
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name())), null);
		final var mergeChild3 = new FunctionalBlockPojo(UUID.randomUUID(), null, PROJECT_ID, null, null, null, "MergeChild 3", "",
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name())), null);
		final var mergeChild4 = new FunctionalBlockPojo(UUID.randomUUID(), null, PROJECT_ID, null, null, null, "MergeChild 4", "",
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name())), null);

		final var block1 = new FunctionalBlockPojo(UUID.randomUUID(), null, PROJECT_ID, null, null, null, "Block 1", "",
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name())), null);
		final var block2 = new FunctionalBlockPojo(UUID.randomUUID(), null, PROJECT_ID, null, null, null, "Block 2", "",
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name())), null);

		final var mergeParent1 = new FunctionalBlockPojo(UUID.randomUUID(), null, PROJECT_ID, null, null, List.of(mergeChild1.getUid(), mergeChild2.getUid()),
				"MergeParent 1", "", Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.MERGE_PARENT.name(),
				FunctionalBlockType.RA_TOP_DOWN.name())), null);
		final var mergeParent2 = new FunctionalBlockPojo(UUID.randomUUID(), null, PROJECT_ID, null, null, List.of(mergeChild4.getUid(), mergeChild3.getUid())
				, "MergeParent 2", "", Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.MERGE_PARENT.name(),
				FunctionalBlockType.RA_TOP_DOWN.name())), null);

		when(functionalBlockService.find(any(BuildingConsumer.class))).thenReturn(List.of(mergeParent1, mergeParent2), List.of(block1, block2), List.of());

		final var result = reachabilityNetworkGeneration.generate(new FunctionalBlockGenerationContext(PROJECT_ID), null);

		verify(functionalBlockService, times(3)).find(any(BuildingConsumer.class));
		assertEquals(1, result.size());
		assertEquals(FunctionalBlockGenerationResult.Operation.CREATE, result.iterator().next().getOperation());
		final FunctionalBlockPojoPrototype resultPrototype = result.iterator().next().getFunctionalBlock();
		assertTrue(resultPrototype.flags.isDefined());
		assertEquals(List.of(FunctionalBlockType.REACHABILITY_NETWORK.name()), resultPrototype.flags.getNonNull().get(FunctionalBlockFlag.TYPE.name()));
		assertTrue(resultPrototype.children.isDefined());
		assertEquals(4, resultPrototype.children.getNonNull().size());
		assertTrue(resultPrototype.children.getNonNull().containsAll(List.of(mergeParent1.getUid(), mergeParent2.getUid(), block1.getUid(), block2.getUid())));
		assertFalse(resultPrototype.uid.isDefined());
	}

	@Test
	void testReachabilityNetworkBlockGenerationWhenBlockAlreadyExists() {
		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockServiceImpl.class);
		final ReachabilityNetworkGeneration reachabilityNetworkGeneration = new ReachabilityNetworkGeneration(functionalBlockService);

		final var networkBlock = new FunctionalBlockPojo(UUID.randomUUID(), null, PROJECT_ID, null, null, null,
				"Reachability Network", "", Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY_NETWORK.name())), null);

		final var mergeChild1 = new FunctionalBlockPojo(UUID.randomUUID(), null, PROJECT_ID, null, null, null, "MergeChild 1", "",
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name())), null);
		final var mergeChild2 = new FunctionalBlockPojo(UUID.randomUUID(), null, PROJECT_ID, null, null, null, "MergeChild 2", "",
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name())), null);
		final var mergeChild3 = new FunctionalBlockPojo(UUID.randomUUID(), null, PROJECT_ID, null, null, null, "MergeChild 3", "",
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name())), null);
		final var mergeChild4 = new FunctionalBlockPojo(UUID.randomUUID(), null, PROJECT_ID, null, null, null, "MergeChild 4", "",
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name())), null);

		final var block1 = new FunctionalBlockPojo(UUID.randomUUID(), null, PROJECT_ID, null, null, null, "Block 1", "",
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name())), null);
		final var block2 = new FunctionalBlockPojo(UUID.randomUUID(), null, PROJECT_ID, null, null, null, "Block 2", "",
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name())), null);

		final var mergeParent1 = new FunctionalBlockPojo(UUID.randomUUID(), null, PROJECT_ID, null, null, List.of(mergeChild1.getUid(), mergeChild2.getUid()),
				"MergeParent 1", "", Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.MERGE_PARENT.name(),
				FunctionalBlockType.RA_TOP_DOWN.name())), null);
		final var mergeParent2 = new FunctionalBlockPojo(UUID.randomUUID(), null, PROJECT_ID, null, null, List.of(mergeChild4.getUid(), mergeChild3.getUid())
				, "MergeParent 2", "", Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.MERGE_PARENT.name(),
				FunctionalBlockType.RA_TOP_DOWN.name())), null);

		when(functionalBlockService.find(any(BuildingConsumer.class))).thenReturn(List.of(mergeParent1, mergeParent2), List.of(block1, block2), List.of(networkBlock));

		final var result = reachabilityNetworkGeneration.generate(new FunctionalBlockGenerationContext(PROJECT_ID), null);

		verify(functionalBlockService, times(3)).find(any(BuildingConsumer.class));
		assertEquals(1, result.size());
		assertEquals(FunctionalBlockGenerationResult.Operation.UPDATE, result.iterator().next().getOperation());
		final FunctionalBlockPojoPrototype resultPrototype = result.iterator().next().getFunctionalBlock();
		assertTrue(resultPrototype.flags.isDefined());
		assertEquals(resultPrototype.flags.getNonNull().keySet(), Set.of(FunctionalBlockFlag.GENERATED_AT.name()));
		assertTrue(resultPrototype.children.isDefined());
		assertEquals(4, resultPrototype.children.getNonNull().size());
		assertTrue(resultPrototype.children.getNonNull().containsAll(List.of(mergeParent1.getUid(), mergeParent2.getUid(), block1.getUid(), block2.getUid())));
		assertTrue(resultPrototype.uid.isDefined());
		assertEquals(networkBlock.getUid(), resultPrototype.uid.getNonNull());
	}
}

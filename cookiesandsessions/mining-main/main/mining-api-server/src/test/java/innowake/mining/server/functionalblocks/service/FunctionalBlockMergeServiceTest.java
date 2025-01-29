/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.service;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.lang.BuildingConsumer;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Mocked Test For {@link FunctionalBlockMergeService}
 */
@Tag("mocked")
class FunctionalBlockMergeServiceTest {
	
	private static final Long PROJECT_ID = 1L;
	
	@Test
	void testMergeCallsCreateWhenMergeParentUidIsNotPresent() {
		final var functionalBlockService = mock(FunctionalBlockService.class);
		final var functionalBlockMergeService = new FunctionalBlockMergeService(functionalBlockService);

		final var mergeParentCreatedUuid = UUID.randomUUID();
		final var mergeParentPrototype = createFunctionalBlockPojoPrototype(EntityId.of(PROJECT_ID), Collections.emptyList(),
				"Merge Parent Prototype", "mergeParentPojoWithoutUid Description", null);
		final var commonParentBlock = new FunctionalBlockPojo(UUID.randomUUID(), null, EntityId.of(PROJECT_ID),
				null, null, List.of(), "Common Parent Block", "Common Parent Block Desc",
				Collections.emptyMap(), null);

		when(functionalBlockService.create(mergeParentPrototype)).thenReturn(mergeParentCreatedUuid);
		when(functionalBlockService.find(commonParentBlock.getUid())).thenReturn(Optional.of(commonParentBlock));

		functionalBlockMergeService.merge(commonParentBlock.getUid(), null, mergeParentPrototype,
				Arrays.asList(UUID.randomUUID(), UUID.randomUUID()), false);
		
		/* Verify that the merge method was called exactly once with any instance of FunctionalBlockService */
		verify(functionalBlockService, times(1)).merge(any(), any(), any());
		
		/* Verify that the create method was called exactly once with any instance of FunctionalBlockService */
		verify(functionalBlockService, times(1)).create(any());
		verify(functionalBlockService, times(1)).create(mergeParentPrototype);
		
		/* Verify that the update method was never called with on mergeParent but called once for common parent */
		final var updateArgumentCaptor = ArgumentCaptor.forClass(FunctionalBlockPojoPrototype.class);
		verify(functionalBlockService, times(1)).update(any());
		verify(functionalBlockService, times(0)).update(mergeParentPrototype);
		verify(functionalBlockService).update(updateArgumentCaptor.capture());
		final var updatedCommonParentPrototype = updateArgumentCaptor.getValue();
		Assertions.assertEquals(commonParentBlock.getUid(), updatedCommonParentPrototype.uid.getNonNull());
		Assertions.assertEquals(1, updatedCommonParentPrototype.children.getNonNull().size());
		Assertions.assertEquals(mergeParentCreatedUuid, updatedCommonParentPrototype.children.getNonNull().get(0));
	}

	@Test
	void testMergeDoesNotCallsUpdateOrCreateWhenMergeParentUidIsPresent() {
		final var functionalBlockService = mock(FunctionalBlockService.class);
		final var functionalBlockMergeService = new FunctionalBlockMergeService(functionalBlockService);
		final var mergeParentUid = UUID.randomUUID();
		
		functionalBlockMergeService.merge(UUID.randomUUID(), mergeParentUid, null,
				Arrays.asList(UUID.randomUUID(), UUID.randomUUID()), false);

		/* Verify that the merge method was called exactly once with any instance of FunctionalBlockService */
		verify(functionalBlockService, times(1)).merge(any(), any(), any());

		/* Verify that the update method was never called with any instance of FunctionalBlockService */
		verify(functionalBlockService, never()).update(any());

		/* Verify that the create method was never called with any instance of FunctionalBlockService */
		verify(functionalBlockService, never()).create(any());
	}

	@Test
	void testMergeAddsMergeParentTypeWhenItIsAbsent() {
		final var functionalBlockService = mock(FunctionalBlockService.class);
		final var functionalBlockMergeService = new FunctionalBlockMergeService(functionalBlockService);

		final var mergeParentUid = UUID.randomUUID();
		final var mergeParentWithoutFlagsPrototype = createFunctionalBlockPojoPrototype(EntityId.of(PROJECT_ID), Collections.emptyList(),
				"Merge Parent Prototype", "mergeParentPojoWithoutUid Description", null);
		final var commonParentBlock = new FunctionalBlockPojo(UUID.randomUUID(), null, EntityId.of(PROJECT_ID),
				null, null, List.of(), "Common Parent Block", "Common Parent Block Desc",
				Collections.emptyMap(), null);

		when(functionalBlockService.find(commonParentBlock.getUid())).thenReturn(Optional.of(commonParentBlock));
		when(functionalBlockService.create(mergeParentWithoutFlagsPrototype)).thenReturn(mergeParentUid);

		functionalBlockMergeService.merge(commonParentBlock.getUid(), null, mergeParentWithoutFlagsPrototype,
				Arrays.asList(UUID.randomUUID(), UUID.randomUUID()), false);

		/* Verify that the merge method was called exactly once with any instance of FunctionalBlockService */
		verify(functionalBlockService, times(1)).merge(any(), any(), any());

		/* Verify that the update method was called exactly once with any instance of FunctionalBlockService */
		verify(functionalBlockService, times(1)).update(any());

		/* Verify that the create method was called once for new merge parent creation with any instance of FunctionalBlockService */
		verify(functionalBlockService, times(1)).create(any());
		verify(functionalBlockService, times(1)).create(mergeParentWithoutFlagsPrototype);

		Assertions.assertTrue(mergeParentWithoutFlagsPrototype.flags.isDefined());
		Assertions.assertTrue(mergeParentWithoutFlagsPrototype.flags.getNonNull().containsKey(FunctionalBlockFlag.TYPE.name()));
		Assertions.assertTrue(((Collection<?>) mergeParentWithoutFlagsPrototype.flags.getNonNull().get(FunctionalBlockFlag.TYPE.name()))
				.contains(FunctionalBlockType.MERGE_PARENT.name()));
	}

	@Test
	void testMergeAddsMergeParentTypeWhenItIsAbsentButOtherTypesArePresent() {
		final var functionalBlockService = mock(FunctionalBlockService.class);
		final var functionalBlockMergeService = new FunctionalBlockMergeService(functionalBlockService);

		final var mergeParentUid = UUID.randomUUID();
		final var mergeParentWithFlagsPrototype = createFunctionalBlockPojoPrototype(EntityId.of(PROJECT_ID), Collections.emptyList(),
				"Merge Parent Prototype", "mergeParentPojoWithoutUid Description", null)
				.setFlags(new HashMap<>(Map.of(FunctionalBlockFlag.TYPE.name(), new ArrayList<>(List.of(FunctionalBlockType.FUNCTIONAL_UNIT.name())))));
		final var commonParentBlock = new FunctionalBlockPojo(UUID.randomUUID(), null, EntityId.of(PROJECT_ID),
				null, null, List.of(), "Common Parent Block", "Common Parent Block Desc",
				Collections.emptyMap(),  null);

		when(functionalBlockService.find(commonParentBlock.getUid())).thenReturn(Optional.of(commonParentBlock));
		when(functionalBlockService.create(mergeParentWithFlagsPrototype)).thenReturn(mergeParentUid);

		functionalBlockMergeService.merge(commonParentBlock.getUid(), null, mergeParentWithFlagsPrototype,
				Arrays.asList(UUID.randomUUID(), UUID.randomUUID()), false);

		/* Verify that the merge method was called exactly once with any instance of FunctionalBlockService */
		verify(functionalBlockService, times(1)).merge(any(), any(), any());

		/* Verify that the update method was called exactly once with any instance of FunctionalBlockService */
		verify(functionalBlockService, times(1)).update(any());

		/* Verify that the create method was called once for new merge parent creation with any instance of FunctionalBlockService */
		verify(functionalBlockService, times(1)).create(any());
		verify(functionalBlockService, times(1)).create(mergeParentWithFlagsPrototype);

		Assertions.assertTrue(mergeParentWithFlagsPrototype.flags.isDefined());
		Assertions.assertTrue(mergeParentWithFlagsPrototype.flags.getNonNull().containsKey(FunctionalBlockFlag.TYPE.name()));
		Assertions.assertTrue(((Collection<?>) mergeParentWithFlagsPrototype.flags.getNonNull().get(FunctionalBlockFlag.TYPE.name()))
				.contains(FunctionalBlockType.MERGE_PARENT.name()));
		Assertions.assertTrue(((Collection<?>) mergeParentWithFlagsPrototype.flags.getNonNull().get(FunctionalBlockFlag.TYPE.name()))
				.contains(FunctionalBlockType.FUNCTIONAL_UNIT.name()));
	}

	@Test
	void testMergeCallsUnmergeWhenChildrenIncludeMergeBlock() {
		final var functionalBlockService = mock(FunctionalBlockService.class);
		final var functionalBlockMergeService = new FunctionalBlockMergeService(functionalBlockService);

		final var childUuid1 = UUID.randomUUID();
		final var childUuid2 = UUID.randomUUID();
		final var childUuid3 = UUID.randomUUID();
		final var childUuid4 = UUID.randomUUID();
		final var mergeParentUid = UUID.randomUUID();

		final var childMergeParent1 = new FunctionalBlockPojo(UUID.randomUUID(), null, EntityId.of(PROJECT_ID), null, null,
				List.of(childUuid1, childUuid2), "Child Merge Parent 1", "Child Merge Parent Block 1 Description",
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.MERGE_PARENT.name())),  null);
		final var childMergeParent2 = new FunctionalBlockPojo(UUID.randomUUID(), null, EntityId.of(PROJECT_ID), null, null,
				List.of(childUuid3, childUuid4), "Child Merge Parent 1", "Child Merge Parent Block 1 Description",
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.MERGE_PARENT.name(), FunctionalBlockType.FUNCTIONAL_UNIT.name())),
				 null);
		final var simpleChild = new FunctionalBlockPojo(UUID.randomUUID(), null, EntityId.of(PROJECT_ID), null, null,
				List.of(), "Simple Child Block", "Simple Child Block Description",
				Collections.emptyMap(),  null);
		final List<UUID> mergeChildren = new ArrayList<>(Arrays.asList(childMergeParent1.getUid(), childMergeParent2.getUid(), simpleChild.getUid()));

		when(functionalBlockService.find(any(BuildingConsumer.class))).thenReturn(new ArrayList<>(
				List.of(childMergeParent1, childMergeParent2, simpleChild)));

		functionalBlockMergeService.merge(UUID.randomUUID(), mergeParentUid, null, mergeChildren, false);

		/* Verify that the merge method was called exactly once with any instance of FunctionalBlockService */
		verify(functionalBlockService, times(1)).merge(any(), any(), any());

		/* Verify that the un-merge method was called once for each child merged block */
		verify(functionalBlockService, times(2)).unmerge(any(), any(), any());
		verify(functionalBlockService, times(1)).unmerge(any(), eq(childMergeParent1.getUid()), any());
		verify(functionalBlockService, times(1)).unmerge(any(), eq(childMergeParent2.getUid()), any());

		/* Verify that the Delete method was not called */
		verify(functionalBlockService, never()).delete(any(UUID.class));
		Assertions.assertEquals(5, mergeChildren.size());
		Assertions.assertTrue(mergeChildren.containsAll(List.of(childUuid1, childUuid2, childUuid3, childUuid4, simpleChild.getUid())));
	}

	@Test
	void testMergeCallsUnmergeAndDeletesWhenChildrenIncludeMergeBlockAndRemoveEmptyBlocksIsTrue() {
		final var functionalBlockService = mock(FunctionalBlockService.class);
		final var functionalBlockMergeService = new FunctionalBlockMergeService(functionalBlockService);

		final var mergeParent = new FunctionalBlockPojo(UUID.randomUUID(), null, EntityId.of(PROJECT_ID),
				null, null, List.of(), "MergeParent Block", "Merge Parent Block Description", Map.of(), null);
		final var commonParent = new FunctionalBlockPojo(UUID.randomUUID(), null, EntityId.of(PROJECT_ID),
				null, null, List.of(), "Common Parent Block", "Common Parent Block Description", Map.of(), null);
		final var childMergeParent1 = new FunctionalBlockPojo(UUID.randomUUID(), null, EntityId.of(PROJECT_ID), null, null,
				List.of(), "Child Merge Parent 1", "Child Merge Parent Block 1 Description",
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.MERGE_PARENT.name())),  null);
		final var childMergeParent2 = new FunctionalBlockPojo(UUID.randomUUID(), null, EntityId.of(PROJECT_ID), null, null,
				List.of(), "Child Merge Parent 1", "Child Merge Parent Block 1 Description",
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.MERGE_PARENT.name(), FunctionalBlockType.FUNCTIONAL_UNIT.name())),
				 null);
		final var simpleChild = new FunctionalBlockPojo(UUID.randomUUID(), null, EntityId.of(PROJECT_ID), null, null,
				List.of(), "Simple Child Block", "Simple Child Block Description",
				Collections.emptyMap(),  null);

		when(functionalBlockService.find(any(BuildingConsumer.class))).thenReturn(new ArrayList<>(
				List.of(childMergeParent1, childMergeParent2, simpleChild)));

		functionalBlockMergeService.merge(commonParent.getUid(), mergeParent.getUid(), null, new ArrayList<>(
				Arrays.asList(childMergeParent1.getUid(), childMergeParent2.getUid())), true);

		/* Verify that the merge method was called exactly once with any instance of FunctionalBlockService */
		verify(functionalBlockService, times(1)).merge(any(), any(), any());

		/* Verify that the un-merge method was called once for each child merged block */
		verify(functionalBlockService, times(2)).unmerge(any(), any(), any());
		verify(functionalBlockService, times(1)).unmerge(any(), eq(childMergeParent1.getUid()), any());
		verify(functionalBlockService, times(1)).unmerge(any(), eq(childMergeParent2.getUid()), any());

		/* Verify that the Delete method was called */
		verify(functionalBlockService, times(2)).delete(any(UUID.class));
		verify(functionalBlockService, times(1)).delete(childMergeParent1.getUid());
		verify(functionalBlockService, times(1)).delete(childMergeParent2.getUid());
	}

	@Test
	void testUnmergeWhenRemoveEmptyBlockIsFalse() {
		final var functionalBlockService = mock(FunctionalBlockService.class);
		final var functionalBlockMergeService = new FunctionalBlockMergeService(functionalBlockService);
		final UUID mergeParent = UUID.randomUUID();

		functionalBlockMergeService.unmerge(UUID.randomUUID(), mergeParent, Collections.singletonList(UUID.randomUUID()), false);

		/* Verify that the un-merge method was called exactly once with any instance of FunctionalBlockService */
		verify(functionalBlockService, times(1)).unmerge(any(), any(), any());

		/* Verify that the Find method was not called with any instance of FunctionalBlockService */
		verify(functionalBlockService, never()).find(any(UUID.class));
		verify(functionalBlockService, never()).find(any(BuildingConsumer.class));

		/* Verify that the Delete method was not called with any instance of FunctionalBlockService */
		verify(functionalBlockService, never()).delete(any(UUID.class));
	}

	@Test
	void testUnmergeWhenRemoveEmptyBlockIsTrueAndMergeParentIsEmpty() {
		final var functionalBlockService = mock(FunctionalBlockService.class);
		final var functionalBlockMergeService = new FunctionalBlockMergeService(functionalBlockService);

		final var mergeParentBlock = new FunctionalBlockPojo(UUID.randomUUID(), null, EntityId.of(PROJECT_ID),
				null, null, List.of(), "Merge Parent Block", "Merge Parent Block Desc",
				Collections.emptyMap(),  null);

		when(functionalBlockService.find(mergeParentBlock.getUid())).thenReturn(Optional.of(mergeParentBlock));
		when(functionalBlockService.find(any(BuildingConsumer.class))).thenReturn(List.of());
		functionalBlockMergeService.unmerge(UUID.randomUUID(), mergeParentBlock.getUid(),
				Arrays.asList(UUID.randomUUID(), UUID.randomUUID()), true);

		/* Verify that the un-merge method was called exactly once with any instance of FunctionalBlockService */
		verify(functionalBlockService, times(1)).unmerge(any(), any(), any());

		/* Verify that the update method was called exactly once with any instance of FunctionalBlockService */
		verify(functionalBlockService, times(1)).delete(any(UUID.class));
		verify(functionalBlockService, times(1)).delete(mergeParentBlock.getUid());
	}

	@Test
	void testUnmergeWhenRemoveEmptyBlockIsTrueAndMergeParentIsNotEmpty() {
		final var functionalBlockService = mock(FunctionalBlockService.class);
		final var functionalBlockMergeService = new FunctionalBlockMergeService(functionalBlockService);

		final var mergeParentBlock = new FunctionalBlockPojo(UUID.randomUUID(), null, EntityId.of(PROJECT_ID),
				null, null, List.of(UUID.randomUUID()), "Merge Parent Block", "Merge Parent Block Desc",
				Collections.emptyMap(),  null);

		when(functionalBlockService.find(mergeParentBlock.getUid())).thenReturn(Optional.of(mergeParentBlock));
		when(functionalBlockService.find(any(BuildingConsumer.class))).thenReturn(List.of());
		functionalBlockMergeService.unmerge(UUID.randomUUID(), mergeParentBlock.getUid(),
				Collections.singletonList(UUID.randomUUID()), true);

		/* Verify that the un-merge method was called exactly once with any instance of FunctionalBlockService */
		verify(functionalBlockService, times(1)).unmerge(any(), any(), any());

		/* Verify that the Find method was called exactly once with any instance of FunctionalBlockService */
		verify(functionalBlockService, times(1)).find(any(UUID.class));

		/* Verify that the update method was never called with any instance of FunctionalBlockService */
		verify(functionalBlockService, times(0)).delete(any(UUID.class));
	}

	@Test
	void testUnmergeDeletesEmptyMergeParentsWichAreMergeChildrenWhenRemoveFlagIsTrue() {
		final var functionalBlockService = mock(FunctionalBlockService.class);
		final var functionalBlockMergeService = new FunctionalBlockMergeService(functionalBlockService);

		final var mergeParentBlock = new FunctionalBlockPojo(UUID.randomUUID(), null, EntityId.of(PROJECT_ID),
				null, null, List.of(UUID.randomUUID()), "Merge Parent Block", "Merge Parent Block Desc",
				Collections.emptyMap(),  null);
		final var mergeParentChildBlock1 = new FunctionalBlockPojo(UUID.randomUUID(), null, EntityId.of(PROJECT_ID),
				null, null, List.of(UUID.randomUUID()), "Merge Parent Child 1", "Merge Parent Block Desc",
				Collections.emptyMap(),  null);
		final var mergeParentChildBlock2 = new FunctionalBlockPojo(UUID.randomUUID(), null, EntityId.of(PROJECT_ID),
				null, null, List.of(), "Merge Parent Child 2", "Merge Parent Block Desc",
				Collections.emptyMap(),  null);

		when(functionalBlockService.find(mergeParentBlock.getUid())).thenReturn(Optional.of(mergeParentBlock));
		when(functionalBlockService.find(any(BuildingConsumer.class))).thenReturn(List.of(mergeParentChildBlock1, mergeParentChildBlock2));
		functionalBlockMergeService.unmerge(UUID.randomUUID(), mergeParentBlock.getUid(),
				Collections.singletonList(UUID.randomUUID()), true);

		/* Verify that the un-merge method was called exactly once with any instance of FunctionalBlockService */
		verify(functionalBlockService, times(1)).unmerge(any(), any(), any());

		/* Verify that the Find method was called exactly once with any instance of FunctionalBlockService */
		verify(functionalBlockService, times(1)).find(any(UUID.class));

		/* Verify that the update method was once called with any instance of FunctionalBlockService */
		verify(functionalBlockService, times(1)).delete(any(UUID.class));
		verify(functionalBlockService, times(1)).delete(mergeParentChildBlock2.getUid());

	}
	
	private static FunctionalBlockPojoPrototype createFunctionalBlockPojoPrototype(final EntityId project, final @Nullable List<UUID> children,
			final String name, final String description, final UUID uid) {
		final FunctionalBlockPojoPrototype functionalBlockPojoPrototype= new FunctionalBlockPojoPrototype();
		functionalBlockPojoPrototype.setProject(project);
		functionalBlockPojoPrototype.setName(name);
		functionalBlockPojoPrototype.setDescription(description);	
		if (children != null) {
			functionalBlockPojoPrototype.setChildren(children);
		}
		if (uid != null) {
			functionalBlockPojoPrototype.setUid(uid);
		}
		return functionalBlockPojoPrototype;
	}
}

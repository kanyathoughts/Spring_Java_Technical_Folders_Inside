package innowake.mining.server.functionalblocks.service;

import innowake.mining.data.access.postgres.FunctionalBlockPgDao;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLink;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.time.Instant;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Mock tests for FunctionalBlockServiceImpl
 */
@Tag("mocked")
class FunctionalBlockServiceImplMockTest {

	private static final Long PROJECT_ID = 1L;
	private final FunctionalBlockPgDao functionalBlockPgDao = mock(FunctionalBlockPgDao.class);
	private final FunctionalBlockServiceImpl functionalBlockService = new FunctionalBlockServiceImpl(functionalBlockPgDao);

	/**
	 * Test for setLinks method
	 */
	@SuppressWarnings("unchecked")
	@Test
	void testSetLinks() {
		final UUID reachabilityNetworkUid = UUID.randomUUID();
		final UUID child1Uid = UUID.randomUUID();
		final UUID child2Uid = UUID.randomUUID();

		final List<UUID> children = List.of(child1Uid, child2Uid);
		final FunctionalBlockPojo reachabilityNetwork = new FunctionalBlockPojo(reachabilityNetworkUid, mock(CustomPropertiesMap.class),
				EntityId.of(PROJECT_ID), Collections.emptyList(), Collections.emptyList(), children, "Reachability Network", "description",
				Collections.emptyMap(), Instant.now());
		when(functionalBlockPgDao.find(reachabilityNetworkUid)).thenReturn(Optional.of(reachabilityNetwork));

		final var functionalBlockLink = new FunctionalBlockLink(UUID.randomUUID(), reachabilityNetworkUid, child1Uid, child2Uid, null, Collections.emptyMap(),
				null);
		functionalBlockService.setLinks(reachabilityNetworkUid, Collections.singletonList(functionalBlockLink));

		final ArgumentCaptor<UUID> reachabilityNetworkUidCaptor = ArgumentCaptor.forClass(UUID.class);
		final ArgumentCaptor<List<FunctionalBlockLink>> childrenCaptor = ArgumentCaptor.forClass(List.class);

		verify(functionalBlockPgDao, times(1)).createFunctionalBlockLinks(reachabilityNetworkUidCaptor.capture(), childrenCaptor.capture());

		assertEquals(reachabilityNetworkUid, reachabilityNetworkUidCaptor.getValue());
		assertEquals(1, childrenCaptor.getValue().size());
		assertEquals(functionalBlockLink, childrenCaptor.getValue().get(0));
	}

	/**
	 * Test for setLinks method with invalid children and checks if it throws IllegalArgumentException
	 */
	@Test
	void testSetLinksHavingInvalidChild() {
		final UUID child1Uid = UUID.randomUUID();
		final UUID child2Uid = UUID.randomUUID();

		final UUID reachabilityNetworkWithInvalidChildA = UUID.randomUUID();
		final FunctionalBlockPojo reachabilityNetworkA = new FunctionalBlockPojo(reachabilityNetworkWithInvalidChildA, mock(CustomPropertiesMap.class),
				EntityId.of(PROJECT_ID), Collections.emptyList(), Collections.emptyList(), List.of(child1Uid), "Reachability Network", "description",
				Collections.emptyMap(), Instant.now());
		when(functionalBlockPgDao.find(reachabilityNetworkWithInvalidChildA)).thenReturn(Optional.of(reachabilityNetworkA));
		final var functionalBlockLinkA = new FunctionalBlockLink(UUID.randomUUID(), reachabilityNetworkWithInvalidChildA, child1Uid, child2Uid, null,
				Collections.emptyMap(), null);
		assertThrows(IllegalArgumentException.class,
				() -> functionalBlockService.setLinks(reachabilityNetworkWithInvalidChildA, Collections.singletonList(functionalBlockLinkA)));

		final UUID reachabilityNetworkWithInvalidChildB = UUID.randomUUID();
		final FunctionalBlockPojo reachabilityNetworkB = new FunctionalBlockPojo(reachabilityNetworkWithInvalidChildB, mock(CustomPropertiesMap.class),
				EntityId.of(PROJECT_ID), Collections.emptyList(), Collections.emptyList(), List.of(child1Uid), "Reachability Network", "description",
				Collections.emptyMap(), Instant.now());
		when(functionalBlockPgDao.find(reachabilityNetworkWithInvalidChildB)).thenReturn(Optional.of(reachabilityNetworkB));
		final var functionalBlockLinkB = new FunctionalBlockLink(UUID.randomUUID(), reachabilityNetworkWithInvalidChildB, child1Uid, child2Uid, null,
				Collections.emptyMap(), null);
		assertThrows(IllegalArgumentException.class,
				() -> functionalBlockService.setLinks(reachabilityNetworkWithInvalidChildB, Collections.singletonList(functionalBlockLinkB)));

		final UUID reachabilityNetworkWithNoChildren = UUID.randomUUID();
		final FunctionalBlockPojo reachabilityNetworkC = new FunctionalBlockPojo(reachabilityNetworkWithNoChildren, mock(CustomPropertiesMap.class),
				EntityId.of(PROJECT_ID), Collections.emptyList(), Collections.emptyList(), Collections.emptyList(), "Reachability Network", "description",
				Collections.emptyMap(), Instant.now());
		when(functionalBlockPgDao.find(reachabilityNetworkWithNoChildren)).thenReturn(Optional.of(reachabilityNetworkC));
		final var functionalBlockLinkC = new FunctionalBlockLink(UUID.randomUUID(), reachabilityNetworkWithNoChildren, child1Uid, child2Uid, null,
				Collections.emptyMap(), null);
		assertThrows(IllegalArgumentException.class,
				() -> functionalBlockService.setLinks(reachabilityNetworkWithNoChildren, Collections.singletonList(functionalBlockLinkC)));
	}

	/**
	 * Test for setLinks method with link's parent having grand child as the link's childA. It should have the parent of the grand child as the childA while
	 * create the link
	 */
	@Test
	void testSetLinksHavingMergedChildA() {
		final UUID reachabilityNetworkUid = UUID.randomUUID();
		final UUID child1Uid = UUID.randomUUID();
		final UUID child2Uid = UUID.randomUUID();
		final UUID grandChildUid = UUID.randomUUID();

		final List<UUID> children = List.of(child1Uid, child2Uid);

		final FunctionalBlockPojo reachabilityNetwork = new FunctionalBlockPojo(reachabilityNetworkUid, mock(CustomPropertiesMap.class),
				EntityId.of(PROJECT_ID), Collections.emptyList(), Collections.emptyList(), children, "Reachability Network", "description",
				Collections.emptyMap(), Instant.now());
		when(functionalBlockPgDao.find(reachabilityNetworkUid)).thenReturn(Optional.of(reachabilityNetwork));

		final FunctionalBlockPojo grandChild = new FunctionalBlockPojo(grandChildUid, mock(CustomPropertiesMap.class), EntityId.of(PROJECT_ID),
				Collections.emptyList(), Collections.singletonList(child1Uid), Collections.emptyList(), "Grand Child", "description", Collections.emptyMap(),
				Instant.now());
		when(functionalBlockPgDao.find(grandChildUid)).thenReturn(Optional.of(grandChild));

		final Map<String, Object> someFlags = new HashMap<>();
		someFlags.put("someFlag", true);
		final var functionalBlockLink = new FunctionalBlockLink(UUID.randomUUID(), reachabilityNetworkUid, grandChildUid, child2Uid, null, someFlags, null);
		functionalBlockService.setLinks(reachabilityNetworkUid, Collections.singletonList(functionalBlockLink));

		final ArgumentCaptor<UUID> reachabilityNetworkUidCaptor = ArgumentCaptor.forClass(UUID.class);
		final ArgumentCaptor<List<FunctionalBlockLink>> childrenCaptor = ArgumentCaptor.forClass(List.class);

		verify(functionalBlockPgDao, times(1)).createFunctionalBlockLinks(reachabilityNetworkUidCaptor.capture(), childrenCaptor.capture());

		assertEquals(reachabilityNetworkUid, reachabilityNetworkUidCaptor.getValue());
		assertEquals(1, childrenCaptor.getValue().size());

		final var expectedFlags = new HashMap<>(someFlags);
		expectedFlags.put(FunctionalBlockLinkFlag.MERGE_CHILD_A.name(), grandChildUid);
		final var expectedLink = new FunctionalBlockLink(functionalBlockLink.getUid(), reachabilityNetworkUid, child1Uid, child2Uid, null, expectedFlags,
				null);
		assertEquals(expectedLink, childrenCaptor.getValue().get(0));
	}

	/**
	 * Test for setLinks method with link's parent having grand child as the link's childB. It should have the parent of the grand child as the childB while
	 * create the link
	 */
	@Test
	void testSetLinksHavingMergedChildB() {
		final UUID reachabilityNetworkUid = UUID.randomUUID();
		final UUID child1Uid = UUID.randomUUID();
		final UUID child2Uid = UUID.randomUUID();
		final UUID grandChildUid = UUID.randomUUID();

		final List<UUID> children = List.of(child1Uid, child2Uid);

		final FunctionalBlockPojo reachabilityNetwork = new FunctionalBlockPojo(reachabilityNetworkUid, mock(CustomPropertiesMap.class),
				EntityId.of(PROJECT_ID), Collections.emptyList(), Collections.emptyList(), children, "Reachability Network", "description",
				Collections.emptyMap(), Instant.now());
		when(functionalBlockPgDao.find(reachabilityNetworkUid)).thenReturn(Optional.of(reachabilityNetwork));

		final FunctionalBlockPojo grandChild = new FunctionalBlockPojo(grandChildUid, mock(CustomPropertiesMap.class), EntityId.of(PROJECT_ID),
				Collections.emptyList(), Collections.singletonList(child2Uid), Collections.emptyList(), "Grand Child", "description", Collections.emptyMap(),
				Instant.now());
		when(functionalBlockPgDao.find(grandChildUid)).thenReturn(Optional.of(grandChild));

		final Map<String, Object> someFlags = new HashMap<>();
		someFlags.put("someFlag", true);
		final var functionalBlockLink = new FunctionalBlockLink(UUID.randomUUID(), reachabilityNetworkUid, child1Uid, grandChildUid, null, someFlags, null);
		functionalBlockService.setLinks(reachabilityNetworkUid, Collections.singletonList(functionalBlockLink));

		final ArgumentCaptor<UUID> reachabilityNetworkUidCaptor = ArgumentCaptor.forClass(UUID.class);
		final ArgumentCaptor<List<FunctionalBlockLink>> childrenCaptor = ArgumentCaptor.forClass(List.class);

		verify(functionalBlockPgDao, times(1)).createFunctionalBlockLinks(reachabilityNetworkUidCaptor.capture(), childrenCaptor.capture());

		assertEquals(reachabilityNetworkUid, reachabilityNetworkUidCaptor.getValue());
		assertEquals(1, childrenCaptor.getValue().size());

		final var expectedFlags = new HashMap<>(someFlags);
		expectedFlags.put(FunctionalBlockLinkFlag.MERGE_CHILD_B.name(), grandChildUid);
		final var expectedLink = new FunctionalBlockLink(functionalBlockLink.getUid(), reachabilityNetworkUid, child1Uid, child2Uid, null, expectedFlags,
				null);
		assertEquals(expectedLink, childrenCaptor.getValue().get(0));
	}

	/**
	 * Test for setLinks method with link's parent having grand child as the link's childA and childB. It should have the parent of the grand child as the
	 * childA and childB while
	 * create the link
	 */
	@Test
	void testSetLinksHavingMergedChildren() {
		final UUID reachabilityNetworkUid = UUID.randomUUID();
		final UUID child1Uid = UUID.randomUUID();
		final UUID child2Uid = UUID.randomUUID();
		final UUID grandChild1Uid = UUID.randomUUID();
		final UUID grandChild2Uid = UUID.randomUUID();

		final List<UUID> children = List.of(child1Uid, child2Uid);

		final FunctionalBlockPojo reachabilityNetwork = new FunctionalBlockPojo(reachabilityNetworkUid, mock(CustomPropertiesMap.class),
				EntityId.of(PROJECT_ID), Collections.emptyList(), Collections.emptyList(), children, "Reachability Network", "description",
				Collections.emptyMap(), Instant.now());
		when(functionalBlockPgDao.find(reachabilityNetworkUid)).thenReturn(Optional.of(reachabilityNetwork));

		final FunctionalBlockPojo grandChild1 = new FunctionalBlockPojo(grandChild1Uid, mock(CustomPropertiesMap.class), EntityId.of(PROJECT_ID),
				Collections.emptyList(), Collections.singletonList(child1Uid), Collections.emptyList(), "Grand ChildA", "description", Collections.emptyMap(),
				Instant.now());
		when(functionalBlockPgDao.find(grandChild1Uid)).thenReturn(Optional.of(grandChild1));
		final FunctionalBlockPojo grandChild2 = new FunctionalBlockPojo(grandChild2Uid, mock(CustomPropertiesMap.class), EntityId.of(PROJECT_ID),
				Collections.emptyList(), Collections.singletonList(child2Uid), Collections.emptyList(), "Grand ChildB", "description", Collections.emptyMap(),
				Instant.now());
		when(functionalBlockPgDao.find(grandChild2Uid)).thenReturn(Optional.of(grandChild2));

		final Map<String, Object> someFlags = new HashMap<>();
		someFlags.put("someFlag", true);
		final var functionalBlockLink = new FunctionalBlockLink(UUID.randomUUID(), reachabilityNetworkUid, grandChild1Uid, grandChild2Uid, null, someFlags,
				null);
		functionalBlockService.setLinks(reachabilityNetworkUid, Collections.singletonList(functionalBlockLink));

		final ArgumentCaptor<UUID> reachabilityNetworkUidCaptor = ArgumentCaptor.forClass(UUID.class);
		final ArgumentCaptor<List<FunctionalBlockLink>> childrenCaptor = ArgumentCaptor.forClass(List.class);

		verify(functionalBlockPgDao, times(1)).createFunctionalBlockLinks(reachabilityNetworkUidCaptor.capture(), childrenCaptor.capture());

		assertEquals(reachabilityNetworkUid, reachabilityNetworkUidCaptor.getValue());
		assertEquals(1, childrenCaptor.getValue().size());

		final var expectedFlags = new HashMap<>(someFlags);
		expectedFlags.put(FunctionalBlockLinkFlag.MERGE_CHILD_A.name(), grandChild1Uid);
		expectedFlags.put(FunctionalBlockLinkFlag.MERGE_CHILD_B.name(), grandChild2Uid);
		final var expectedLink = new FunctionalBlockLink(functionalBlockLink.getUid(), reachabilityNetworkUid, child1Uid, child2Uid, null, expectedFlags,
				null);
		assertEquals(expectedLink, childrenCaptor.getValue().get(0));
	}
}

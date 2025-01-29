/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.server.functionalblocks.computation.reachibility;

import innowake.lib.job.api.NullProgressMonitor;
import innowake.mining.server.functionalblocks.computation.ReachabilitySchedulerNetworkComputation;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.SchedulerInfoService;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkType;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.ResolvedModulePart;
import innowake.mining.shared.entities.scheduler.SchedulerEntryRelationshipPojo;
import innowake.mining.shared.lang.BuildingConsumer;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;

import java.time.Instant;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * test computation of reachability scheduler network.
 */
@Tag("mocked")
class ReachabilitySchedulerNetworkComputationTest {

	private static final Long PROJECT_ID = 1L;

	/**
	 *
	 *     module1 --------/
	 *                     +----------> module3
	 *     module2 --------/
	 */

	@SuppressWarnings("unchecked")
	@Test
	void testSimpleSchedulerNetwork() {
		final var reachabilityNetworkBlock = createFunctionalBlock(UUID.randomUUID(), "Reachability Network", List.of(), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY_NETWORK.name())));

		final var functionalBlockService = mock(FunctionalBlockService.class);

		final var schedulerInfoService = mock(SchedulerInfoService.class);
		final UUID module1 = UUID.randomUUID();
		final UUID module2 = UUID.randomUUID();
		final UUID module3 = UUID.randomUUID();

		final var reachabilityBlock1 = createFunctionalBlock(UUID.randomUUID(), "reachabilityBlock1", List.of(reachabilityNetworkBlock.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));
		final var reachabilityBlock2 = createFunctionalBlock(UUID.randomUUID(), "reachabilityBlock2", List.of(reachabilityNetworkBlock.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));
		final var reachabilityBlock3 = createFunctionalBlock(UUID.randomUUID(), "reachabilityBlock3", List.of(reachabilityNetworkBlock.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));

		final var upperBoundBlock1 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock1", List.of(reachabilityBlock1.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_UPPER_BOUND.name())));
		final var upperBoundBlock2 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock2", List.of(reachabilityBlock2.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_UPPER_BOUND.name())));
		final var upperBoundBlock3 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock3", List.of(reachabilityBlock3.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_UPPER_BOUND.name())));

		final UUID module1EntryUID = UUID.randomUUID();
		final UUID module2EntryUID = UUID.randomUUID();
		final UUID module3EntryUID = UUID.randomUUID();

		final SchedulerEntryRelationshipPojo relationship1 = new SchedulerEntryRelationshipPojo(UUID.randomUUID(), module1EntryUID, module3EntryUID, false,
				null, module1, module3);
		final SchedulerEntryRelationshipPojo relationship2 = new SchedulerEntryRelationshipPojo(UUID.randomUUID(), module2EntryUID, module3EntryUID, false,
				null, module2, module3);
		final SchedulerEntryRelationshipPojo relationship3 = new SchedulerEntryRelationshipPojo(UUID.randomUUID(), module2EntryUID, module3EntryUID, false,
				null, module2, module3);

		final var resolvedModulePart1 = new ResolvedModulePart(EntityId.of(module1));
		final var resolvedModulePart2 = new ResolvedModulePart(EntityId.of(module2));
		final var resolvedModulePart3 = new ResolvedModulePart(EntityId.of(module3));

		when(schedulerInfoService.findRelationships(any(), any())).thenReturn(Paged.ofContent(List.of(relationship1, relationship2, relationship3)));

		when(functionalBlockService.find(any(BuildingConsumer.class))).then(invocation -> {
			final BuildingConsumer<FunctionalBlockService.FunctionalBlockInquiryBuilder> builderConsumer = invocation.getArgument(0);
			final FunctionalBlockService.FunctionalBlockInquiryBuilder builder = Mockito.mock(FunctionalBlockService.FunctionalBlockInquiryBuilder.class);
			final ArgumentCaptor<List<EntityId>> resolvedModulePartCaptor = ArgumentCaptor.forClass(List.class);

			when(builderConsumer.prepare(builder)).thenReturn(builder);
			builderConsumer.prepare(builder);

			Mockito.verify(builder, atLeastOnce()).withResolvedModuleParts(resolvedModulePartCaptor.capture());
			if (resolvedModulePartCaptor.getValue().size() == 3 && resolvedModulePartCaptor.getValue()
					.containsAll(List.of(EntityId.of(module1), EntityId.of(module2), EntityId.of(module3)))) {
				return List.of(upperBoundBlock1, upperBoundBlock2, upperBoundBlock3);
			}
			return Collections.emptyList();
		});

		final Map<UUID, List<ResolvedModulePart>> resolvedModuleParts = Map.of(upperBoundBlock1.getUid(), Collections.singletonList(resolvedModulePart1),
				upperBoundBlock2.getUid(), Collections.singletonList(resolvedModulePart2), upperBoundBlock3.getUid(),
				Collections.singletonList(resolvedModulePart3));
		when(functionalBlockService.getResolvedModuleParts(any(List.class))).then(invocation -> {
			final List<UUID> blockUIDs = invocation.getArgument(0);
			if (blockUIDs.size() != 3) {
				fail("Unexpected block UIDs");
			}
			if (blockUIDs.containsAll(List.of(upperBoundBlock1.getUid(), upperBoundBlock2.getUid(), upperBoundBlock3.getUid()))) {
				return resolvedModuleParts;
			}
			fail("Unexpected block UIDs");
			return null;
		});

		final var computation = new ReachabilitySchedulerNetworkComputation(functionalBlockService, schedulerInfoService);

		final var links = assertNotNull(computation.compute(reachabilityNetworkBlock, new NullProgressMonitor()));

		assertEquals(2, links.size());

		links.forEach(link -> {
			assertEquals(reachabilityNetworkBlock.getUid(), link.getParent());
			assertEquals(List.of(FunctionalBlockLinkType.RA_FROM_SCHEDULER_INFO.name(), FunctionalBlockLinkType.DIRECTED.name()), link.getFlags()
					.get(FunctionalBlockLinkFlag.TYPE.name()));
			if (link.getChildA().equals(reachabilityBlock1.getUid())) {
				assertEquals(reachabilityBlock3.getUid(), link.getChildB());
			} else if (link.getChildA().equals(reachabilityBlock2.getUid())) {
				assertEquals(reachabilityBlock3.getUid(), link.getChildB());
			} else {
				fail("Unexpected link");
			}
		});
	}

	private static FunctionalBlockPojo createFunctionalBlock(final UUID uid, final String name, final List<UUID> parents, final List<UUID> children,
			final Map<String, Object> flags) {
		return new FunctionalBlockPojo(uid, null, EntityId.of(PROJECT_ID), Collections.emptyList(), parents, children, name, "", flags, Instant.now());
	}
}

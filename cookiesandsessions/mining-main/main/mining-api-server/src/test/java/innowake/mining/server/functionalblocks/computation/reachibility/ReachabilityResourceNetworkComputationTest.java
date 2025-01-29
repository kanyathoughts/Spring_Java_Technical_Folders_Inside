/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.functionalblocks.computation.reachibility;


import innowake.lib.job.api.NullProgressMonitor;
import innowake.mining.server.functionalblocks.computation.ReachabilityResourceNetworkComputation;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.FunctionalBlockService.FunctionalBlockInquiryBuilder;
import innowake.mining.shared.access.MockInquiryBuilder;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLink;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkType;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.FeatureId;
import org.apache.commons.lang3.tuple.Pair;
import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;

import static innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag.RA_ACCESS_TYPE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * test computation of reachability resource network.
 */
@Tag("mocked")
class ReachabilityResourceNetworkComputationTest {

	private static final Long PROJECT_ID = 1L;

	/**
	 *      rb1 -----------> ub1 --------R------> lb1 -------> mod1
	 *      rb2 -----------> ub2 --------W------> lb2 -------> mod2
	 */
	@Test
	void testNoLinksAreCreatedWhenNoSharedLowerBoundExists() {
		final var reachabilityNetworkBlock = createFunctionalBlock(UUID.randomUUID(), "Reachability Network", List.of(), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY_NETWORK.name())));

		final var reachabilityBlock1 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock1", List.of(reachabilityNetworkBlock.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));
		final var reachabilityBlock2 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock2", List.of(reachabilityNetworkBlock.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));

		final var lowerBoundBlock1 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundBlock1", List.of(reachabilityBlock1.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND.name()), RA_ACCESS_TYPE.name(), List.of("READ")));
		final var lowerBoundBlock2 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundBlock2", List.of(reachabilityBlock2.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND.name()), RA_ACCESS_TYPE.name(), List.of("WRITE")));

		final var upperBoundBlock1 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock1", List.of(reachabilityBlock1.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_UPPER_BOUND.name())));
		final var upperBoundBlock2 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock2", List.of(reachabilityBlock2.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_UPPER_BOUND.name())));

		final var lowerBoundModule1 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundModule1",
				List.of(lowerBoundBlock1.getUid()), List.of(),
				Map.of(FunctionalBlockType.MODULE.name(), List.of(FunctionalBlockType.MODULE.name())));
		final var lowerBoundModule2 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundModule2",
				List.of(lowerBoundBlock2.getUid()), List.of(),
				Map.of(FunctionalBlockType.MODULE.name(), List.of(FunctionalBlockType.MODULE.name())));

		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final ReachabilityResourceNetworkComputation reachabilityResourceNetworkComputation = new ReachabilityResourceNetworkComputation(
				functionalBlockService, mockFF4j(true));

		when(functionalBlockService.find(any(BuildingConsumer.class))).then(invocation -> {
			final BuildingConsumer<FunctionalBlockInquiryBuilder> builderConsumer = invocation.getArgument(0);

			return mockFind(builderConsumer,
					/* RA_TOP_DOWN -> RA_LOWER_BOUND */
					List.of(
							Pair.of(reachabilityBlock1, lowerBoundBlock1),
							Pair.of(reachabilityBlock2, lowerBoundBlock2)
					),
					/* RA_LOWER_BOUND -> MODULE */
					List.of(
							Pair.of(lowerBoundBlock1, lowerBoundModule1),
							Pair.of(lowerBoundBlock2, lowerBoundModule2)
					));
		});

		when(functionalBlockService.getLinks(Set.of(reachabilityBlock1.getUid())))
				.thenReturn(Map.of(reachabilityBlock1.getUid(),	List.of(
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock1.getUid(), upperBoundBlock1.getUid(), lowerBoundBlock1.getUid(), null,
								Map.of(FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name(), List.of("READ")), null)))
				);

		when(functionalBlockService.getLinks(Set.of(reachabilityBlock2.getUid())))
				.thenReturn(Map.of(reachabilityBlock2.getUid(),	List.of(
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock2.getUid(), upperBoundBlock2.getUid(), lowerBoundBlock2.getUid(), null,
						Map.of(FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name(), List.of("WRITE")), null)))
				);

		final List<FunctionalBlockLink> links = Objects.requireNonNull(reachabilityResourceNetworkComputation.compute(reachabilityNetworkBlock,
				new NullProgressMonitor()));
		assertEquals(0, links.size());
	}

	/**
	 *                                 +------R------> lb1 ----> mod1
	 *      rb1 -----------> ub1 ------+
	 *                                 +------R------> lb2 +
	 *                                                     +---> mod2
	 *                                 +------W------> lb3 +
	 *      rb2 -----------> ub2 ------+
	 *                                 +------W------> lb4 ----> mod3
	 */
	@Test
	void testLinkDirectionForLowerBoundBeingReadAndWritten() {
		final var reachabilityNetworkBlock = createFunctionalBlock(UUID.randomUUID(), "Reachability Network",
				List.of(), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY_NETWORK.name())));

		final var reachabilityBlock1 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock1",
				List.of(reachabilityNetworkBlock.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));
		final var reachabilityBlock2 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock2",
				List.of(reachabilityNetworkBlock.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));

		final var lowerBoundBlock1 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundBlock1",
				List.of(reachabilityBlock1.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND.name()),
						RA_ACCESS_TYPE.name(), List.of("READ")));
		final var lowerBoundBlock2 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundBlock2",
				List.of(reachabilityBlock1.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND.name()),
						RA_ACCESS_TYPE.name(), List.of("READ")));
		final var lowerBoundBlock3 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundBlock3",
				List.of(reachabilityBlock2.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND.name()),
						RA_ACCESS_TYPE.name(), List.of("WRITE")));
		final var lowerBoundBlock4 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundBlock4",
				List.of(reachabilityBlock2.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND.name()),
						RA_ACCESS_TYPE.name(), List.of("WRITE")));

		final var upperBoundBlock1 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock1",
				List.of(reachabilityBlock1.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_UPPER_BOUND.name())));
		final var upperBoundBlock2 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock2",
				List.of(reachabilityBlock2.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_UPPER_BOUND.name())));

		final var lowerBoundModule1 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundModule1",
				List.of(lowerBoundBlock1.getUid()), List.of(),
				Map.of(FunctionalBlockType.MODULE.name(), List.of(FunctionalBlockType.MODULE.name())));
		final var lowerBoundModule2 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundModule2",
				List.of(lowerBoundBlock2.getUid(), lowerBoundBlock3.getUid()), List.of(),
				Map.of(FunctionalBlockType.MODULE.name(), List.of(FunctionalBlockType.MODULE.name())));
		final var lowerBoundModule3 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundModule3",
				List.of(lowerBoundBlock4.getUid()), List.of(),
				Map.of(FunctionalBlockType.MODULE.name(), List.of(FunctionalBlockType.MODULE.name())));

		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final ReachabilityResourceNetworkComputation reachabilityResourceNetworkComputation =
				new ReachabilityResourceNetworkComputation(functionalBlockService, mockFF4j(true));

		when(functionalBlockService.find(any(BuildingConsumer.class))).then(invocation -> {
			final BuildingConsumer<FunctionalBlockInquiryBuilder> builderConsumer = invocation.getArgument(0);

			return mockFind(builderConsumer,
					/* RA_TOP_DOWN -> RA_LOWER_BOUND */
					List.of(
							Pair.of(reachabilityBlock1, lowerBoundBlock1),
							Pair.of(reachabilityBlock1, lowerBoundBlock2),
							Pair.of(reachabilityBlock2, lowerBoundBlock3),
							Pair.of(reachabilityBlock2, lowerBoundBlock4)
					),
					/* RA_LOWER_BOUND -> MODULE */
					List.of(
							Pair.of(lowerBoundBlock1, lowerBoundModule1),
							Pair.of(lowerBoundBlock2, lowerBoundModule2),
							Pair.of(lowerBoundBlock3, lowerBoundModule2),
							Pair.of(lowerBoundBlock4, lowerBoundModule3)
					));
		});

		when(functionalBlockService.getLinks(Set.of(reachabilityBlock1.getUid())))
				.thenReturn(Map.of(reachabilityBlock1.getUid(), List.of(
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock1.getUid(), upperBoundBlock1.getUid(), lowerBoundBlock1.getUid(), null,
								Map.of(FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name(), List.of("READ")), null),
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock1.getUid(), upperBoundBlock1.getUid(), lowerBoundBlock2.getUid(), null,
								Map.of(FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name(), List.of("READ")), null)))
				);
		when(functionalBlockService.getLinks(Set.of(reachabilityBlock2.getUid())))
				.thenReturn(Map.of(reachabilityBlock2.getUid(), List.of(
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock2.getUid(), upperBoundBlock2.getUid(), lowerBoundBlock3.getUid(), null,
								Map.of(FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name(), List.of("WRITE")), null),
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock2.getUid(), upperBoundBlock2.getUid(), lowerBoundBlock4.getUid(), null,
								Map.of(FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name(), List.of("WRITE")), null)))
				);

		final List<FunctionalBlockLink> links = Objects.requireNonNull(reachabilityResourceNetworkComputation.compute(reachabilityNetworkBlock,
				new NullProgressMonitor()));
		assertEquals(1, links.size());
		final FunctionalBlockLink link = links.get(0);
		assertEquals(reachabilityBlock1.getUid(), link.getChildB());
		assertEquals(reachabilityBlock2.getUid(), link.getChildA());
		assertEquals(Set.of(lowerBoundModule2.getUid()), link.getFlags().get(FunctionalBlockLinkFlag.RA_SHARED_RESOURCE_ID.name()));
		assertEquals(List.of(FunctionalBlockLinkType.RA_SHARED_RESOURCE.name(), FunctionalBlockLinkType.DIRECTED.name()),
				link.getFlags().get(FunctionalBlockLinkFlag.TYPE.name()));
	}

	/**
	 *                                 +------R------> lb1 -----> mod1
	 *      rb1 -----------> ub1 ------+
	 *                                 +------R------> lb2 +
	 *                                                     +----> mod2
	 *                                 +------R------> lb3 +
	 *      rb2 -----------> ub2 ------+
	 *                                 +------W------> lb4 -----> mod3
	 */
	@Test
	void testLinkDirectionForLowerBoundBeingReadByTwoBlocks() {
		final var reachabilityNetworkBlock = createFunctionalBlock(UUID.randomUUID(), "Reachability Network",
				List.of(), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY_NETWORK.name())));

		final var reachabilityBlock1 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock1",
				List.of(reachabilityNetworkBlock.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));
		final var reachabilityBlock2 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock2",
				List.of(reachabilityNetworkBlock.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));

		final var lowerBoundBlock1 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundBlock1",
				List.of(reachabilityBlock1.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND.name()),
						RA_ACCESS_TYPE.name(), List.of("READ")));
		final var lowerBoundBlock2 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundBlock2",
				List.of(reachabilityBlock1.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND.name()),
						RA_ACCESS_TYPE.name(), List.of("READ")));
		final var lowerBoundBlock3 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundBlock3",
				List.of(reachabilityBlock2.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND.name()),
						RA_ACCESS_TYPE.name(), List.of("READ")));
		final var lowerBoundBlock4 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundBlock4",
				List.of(reachabilityBlock2.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND.name()),
						RA_ACCESS_TYPE.name(), List.of("WRITE")));

		final var upperBoundBlock1 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock1",
				List.of(reachabilityBlock1.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_UPPER_BOUND.name())));
		final var upperBoundBlock2 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock2",
				List.of(reachabilityBlock2.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_UPPER_BOUND.name())));

		final var lowerBoundModule1 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundModule1",
				List.of(lowerBoundBlock1.getUid()), List.of(),
				Map.of(FunctionalBlockType.MODULE.name(), List.of(FunctionalBlockType.MODULE.name())));
		final var lowerBoundModule2 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundModule2",
				List.of(lowerBoundBlock2.getUid(), lowerBoundBlock3.getUid()), List.of(),
				Map.of(FunctionalBlockType.MODULE.name(), List.of(FunctionalBlockType.MODULE.name())));
		final var lowerBoundModule3 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundModule3",
				List.of(lowerBoundBlock4.getUid()), List.of(),
				Map.of(FunctionalBlockType.MODULE.name(), List.of(FunctionalBlockType.MODULE.name())));

		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final ReachabilityResourceNetworkComputation reachabilityResourceNetworkComputation =
				new ReachabilityResourceNetworkComputation(functionalBlockService, mockFF4j(true));

		when(functionalBlockService.find(any(BuildingConsumer.class))).then(invocation -> {
			final BuildingConsumer<FunctionalBlockInquiryBuilder> builderConsumer = invocation.getArgument(0);

			return mockFind(builderConsumer,
					/* RA_TOP_DOWN -> RA_LOWER_BOUND */
					List.of(
							Pair.of(reachabilityBlock1, lowerBoundBlock1),
							Pair.of(reachabilityBlock1, lowerBoundBlock2),
							Pair.of(reachabilityBlock2, lowerBoundBlock3),
							Pair.of(reachabilityBlock2, lowerBoundBlock4)
					),
					/* RA_LOWER_BOUND -> MODULE */
					List.of(
							Pair.of(lowerBoundBlock1, lowerBoundModule1),
							Pair.of(lowerBoundBlock2, lowerBoundModule2),
							Pair.of(lowerBoundBlock3, lowerBoundModule2),
							Pair.of(lowerBoundBlock4, lowerBoundModule3)
					));
		});

		when(functionalBlockService.getLinks(Set.of(reachabilityBlock1.getUid())))
				.thenReturn(Map.of(reachabilityBlock1.getUid(), List.of(
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock1.getUid(), upperBoundBlock1.getUid(), lowerBoundBlock1.getUid(), null,
								Map.of(FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name(), List.of("READ")), null),
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock1.getUid(), upperBoundBlock1.getUid(), lowerBoundBlock2.getUid(), null,
								Map.of(FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name(), List.of("READ")), null)))
				);

		when(functionalBlockService.getLinks(Set.of(reachabilityBlock2.getUid())))
				.thenReturn(Map.of(reachabilityBlock2.getUid(), List.of(
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock2.getUid(), upperBoundBlock2.getUid(), lowerBoundBlock3.getUid(), null,
								Map.of(FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name(), List.of("READ")), null),
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock2.getUid(), upperBoundBlock2.getUid(), lowerBoundBlock4.getUid(), null,
								Map.of(FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name(), List.of("WRITE")), null))
				));

		/* WMIN-13077 removed this functionality */
		final List<FunctionalBlockLink> links = Objects.requireNonNull(reachabilityResourceNetworkComputation.compute(reachabilityNetworkBlock,
				new NullProgressMonitor()));
		assertEquals(0, links.size());
	}

	/**
	 *                                 +------R------> lb1 ------> mod1
	 *      rb1 -----------> ub1 ------+
	 *                                 +------W------> lb2 +
	 *                                                     +-----> mod2
	 *                                 +------W------> lb3 +
	 *      rb2 -----------> ub2 ------+
	 *                                 +------W------> lb4 ------> mod3
	 */
	@Test
	void testLinkDirectionForLowerBoundBeingWrittenByTwoBlocks() {
		final var reachabilityNetworkBlock = createFunctionalBlock(UUID.randomUUID(), "Reachability Network", List.of(), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY_NETWORK.name())));

		final var reachabilityBlock1 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock1", List.of(reachabilityNetworkBlock.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));
		final var reachabilityBlock2 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock2", List.of(reachabilityNetworkBlock.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));

		final var lowerBoundBlock1 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundBlock1", List.of(reachabilityBlock1.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND.name()), RA_ACCESS_TYPE.name(), List.of("READ")));
		final var lowerBoundBlock2 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundBlock2",
				List.of(reachabilityBlock1.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND.name()), RA_ACCESS_TYPE.name(), List.of("WRITE")));
		final var lowerBoundBlock3 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundBlock3",
				List.of(reachabilityBlock2.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND.name()), RA_ACCESS_TYPE.name(), List.of("WRITE")));
		final var lowerBoundBlock4 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundBlock4",
				List.of(reachabilityBlock2.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND.name()), RA_ACCESS_TYPE.name(), List.of("WRITE")));

		final var upperBoundBlock1 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock1", List.of(reachabilityBlock1.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_UPPER_BOUND.name())));
		final var upperBoundBlock2 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock2", List.of(reachabilityBlock2.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_UPPER_BOUND.name())));

		final var lowerBoundModule1 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundModule1",
				List.of(lowerBoundBlock1.getUid()), List.of(),
				Map.of(FunctionalBlockType.MODULE.name(), List.of(FunctionalBlockType.MODULE.name())));
		final var lowerBoundModule2 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundModule2",
				List.of(lowerBoundBlock2.getUid(), lowerBoundBlock3.getUid()), List.of(),
				Map.of(FunctionalBlockType.MODULE.name(), List.of(FunctionalBlockType.MODULE.name())));
		final var lowerBoundModule3 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundModule3",
				List.of(lowerBoundBlock4.getUid()), List.of(),
				Map.of(FunctionalBlockType.MODULE.name(), List.of(FunctionalBlockType.MODULE.name())));

		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);

		final ReachabilityResourceNetworkComputation reachabilityResourceNetworkComputation = new ReachabilityResourceNetworkComputation(
				functionalBlockService, mockFF4j(true));

		when(functionalBlockService.find(any(BuildingConsumer.class))).then(invocation -> {
			final BuildingConsumer<FunctionalBlockInquiryBuilder> builderConsumer = invocation.getArgument(0);

			return mockFind(builderConsumer,
					/* RA_TOP_DOWN -> RA_LOWER_BOUND */
					List.of(
							Pair.of(reachabilityBlock1, lowerBoundBlock1),
							Pair.of(reachabilityBlock1, lowerBoundBlock2),
							Pair.of(reachabilityBlock2, lowerBoundBlock3),
							Pair.of(reachabilityBlock2, lowerBoundBlock4)
					),
					/* RA_LOWER_BOUND -> MODULE */
					List.of(
							Pair.of(lowerBoundBlock1, lowerBoundModule1),
							Pair.of(lowerBoundBlock2, lowerBoundModule2),
							Pair.of(lowerBoundBlock3, lowerBoundModule2),
							Pair.of(lowerBoundBlock4, lowerBoundModule3)
					));
		});
		when(functionalBlockService.getLinks(Set.of(reachabilityBlock1.getUid())))
				.thenReturn(Map.of(reachabilityBlock1.getUid(),List.of(
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock1.getUid(), upperBoundBlock1.getUid(), lowerBoundBlock1.getUid(), null,
								Map.of(FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name(), List.of("READ")), null),
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock1.getUid(), upperBoundBlock1.getUid(), lowerBoundBlock2.getUid(), null,
								Map.of(FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name(), List.of("WRITE")), null)))
				);

		when(functionalBlockService.getLinks(Set.of(reachabilityBlock2.getUid())))
				.thenReturn(Map.of(reachabilityBlock2.getUid(),List.of(
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock2.getUid(), upperBoundBlock2.getUid(), lowerBoundBlock3.getUid(), null,
								Map.of(FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name(), List.of("WRITE")), null),
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock2.getUid(), upperBoundBlock2.getUid(), lowerBoundBlock4.getUid(), null,
								Map.of(FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name(), List.of("WRITE")), null)))
				);

		/* WMIN-13077 removed this functionality */
		final List<FunctionalBlockLink> links = Objects.requireNonNull(reachabilityResourceNetworkComputation.compute(reachabilityNetworkBlock,
				new NullProgressMonitor()));
		assertEquals(0, links.size());
	}

	/**
	 *                                 +-------R-------> lb1 ---------------+
	 *      rb1 -----------> ub1 ------+                                    |
	 *                                 +-------W-------> lb2 +              |
	 *                                                       +----> mod2    +----> mod1
	 *                                 +-------R-------> lb4 +              |
	 *      rb2 -----------> ub2 ------+                                    |
	 *                                 +-------W-------> lb3 ---------------+
	 */
	@Test
	void testLinkDirectionForLowerBoundBeingReadAndWrittenByDifferentBlocks() {
		final var reachabilityNetworkBlock = createFunctionalBlock(UUID.randomUUID(), "Reachability Network", List.of(), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY_NETWORK.name())));

		final var reachabilityBlock1 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock1", List.of(reachabilityNetworkBlock.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));
		final var reachabilityBlock2 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock2", List.of(reachabilityNetworkBlock.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));

		final var lowerBoundBlock1 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundBlock1",
				List.of(reachabilityBlock1.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND.name()), RA_ACCESS_TYPE.name(), List.of("READ")));
		final var lowerBoundBlock2 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundBlock2",
				List.of(reachabilityBlock1.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND.name()), RA_ACCESS_TYPE.name(), List.of("WRITE")));
		final var lowerBoundBlock3 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundBlock3",
				List.of(reachabilityBlock2.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND.name()), RA_ACCESS_TYPE.name(), List.of("WRITE")));
		final var lowerBoundBlock4 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundBlock4",
				List.of(reachabilityBlock2.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND.name()), RA_ACCESS_TYPE.name(), List.of("READ")));

		final var upperBoundBlock1 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock1", List.of(reachabilityBlock1.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_UPPER_BOUND.name())));
		final var upperBoundBlock2 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock2", List.of(reachabilityBlock2.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_UPPER_BOUND.name())));

		final var lowerBoundModule1 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundModule1",
				List.of(lowerBoundBlock1.getUid(), lowerBoundBlock3.getUid()), List.of(),
				Map.of(FunctionalBlockType.MODULE.name(), List.of(FunctionalBlockType.MODULE.name())));
		final var lowerBoundModule2 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundModule2",
				List.of(lowerBoundBlock2.getUid(), lowerBoundBlock4.getUid()), List.of(),
				Map.of(FunctionalBlockType.MODULE.name(), List.of(FunctionalBlockType.MODULE.name())));

		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final ReachabilityResourceNetworkComputation reachabilityResourceNetworkComputation = new ReachabilityResourceNetworkComputation(
				functionalBlockService, mockFF4j(true));

		when(functionalBlockService.find(any(BuildingConsumer.class))).then(invocation -> {
			final BuildingConsumer<FunctionalBlockInquiryBuilder> builderConsumer = invocation.getArgument(0);

			return mockFind(builderConsumer,
					/* RA_TOP_DOWN -> RA_LOWER_BOUND */
					List.of(
							Pair.of(reachabilityBlock1, lowerBoundBlock1),
							Pair.of(reachabilityBlock1, lowerBoundBlock2),
							Pair.of(reachabilityBlock2, lowerBoundBlock3),
							Pair.of(reachabilityBlock2, lowerBoundBlock4)
					),
					/* RA_LOWER_BOUND -> MODULE */
					List.of(
							Pair.of(lowerBoundBlock1, lowerBoundModule1),
							Pair.of(lowerBoundBlock2, lowerBoundModule2),
							Pair.of(lowerBoundBlock3, lowerBoundModule1),
							Pair.of(lowerBoundBlock4, lowerBoundModule2)
					));
		});

		when(functionalBlockService.getLinks(Set.of(reachabilityBlock1.getUid())))
				.thenReturn(Map.of(reachabilityBlock1.getUid(),List.of(
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock1.getUid(), upperBoundBlock1.getUid(), lowerBoundBlock1.getUid(), null,
								Map.of(FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name(), List.of("READ")), null),
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock1.getUid(), upperBoundBlock1.getUid(), lowerBoundBlock2.getUid(), null,
								Map.of(FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name(), List.of("WRITE")), null)))
				);

		when(functionalBlockService.getLinks(Set.of(reachabilityBlock2.getUid())))
				.thenReturn(Map.of(reachabilityBlock2.getUid(),List.of(
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock2.getUid(), upperBoundBlock2.getUid(), lowerBoundBlock3.getUid(), null,
								Map.of(FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name(), List.of("WRITE")), null),
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock2.getUid(), upperBoundBlock2.getUid(), lowerBoundBlock4.getUid(), null,
								Map.of(FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name(), List.of("READ")), null)))
				);

		final List<FunctionalBlockLink> links = Objects.requireNonNull(reachabilityResourceNetworkComputation.compute(reachabilityNetworkBlock,
				new NullProgressMonitor()));
		assertEquals(2, links.size());
		final FunctionalBlockLink link1 = links.stream().filter(link -> link.getChildB().equals(reachabilityBlock1.getUid())).findFirst().orElseThrow();
		assertEquals(reachabilityBlock2.getUid(), link1.getChildA());
		assertEquals(Set.of(lowerBoundModule1.getUid()), link1.getFlags().get(FunctionalBlockLinkFlag.RA_SHARED_RESOURCE_ID.name()));
		assertEquals(List.of(FunctionalBlockLinkType.RA_SHARED_RESOURCE.name(), FunctionalBlockLinkType.DIRECTED.name()),
				link1.getFlags().get(FunctionalBlockLinkFlag.TYPE.name()));

		final FunctionalBlockLink link2 = links.stream().filter(link -> link.getChildB().equals(reachabilityBlock2.getUid())).findFirst().orElseThrow();
		assertEquals(reachabilityBlock1.getUid(), link2.getChildA());
		assertEquals(Set.of(lowerBoundModule2.getUid()), link2.getFlags().get(FunctionalBlockLinkFlag.RA_SHARED_RESOURCE_ID.name()));
		assertEquals(List.of(FunctionalBlockLinkType.RA_SHARED_RESOURCE.name(), FunctionalBlockLinkType.DIRECTED.name()),
				link2.getFlags().get(FunctionalBlockLinkFlag.TYPE.name()));
	}

	@Test
	void testAcceptReturnsFalseWhenFeatureNotEnabled() {
		final ReachabilityResourceNetworkComputation reachabilityResourceNetworkComputation = new ReachabilityResourceNetworkComputation(
				mock(FunctionalBlockService.class), mockFF4j(false));
		final var reachabilityNetworkBlock = createFunctionalBlock(UUID.randomUUID(), "Reachability Network", List.of(), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY_NETWORK.name())));

		assertFalse(reachabilityResourceNetworkComputation.accept(reachabilityNetworkBlock));
	}

	@Test
	void testAcceptReturnsTrueWhenNoFlagsPresent() {
		final ReachabilityResourceNetworkComputation reachabilityResourceNetworkComputation = new ReachabilityResourceNetworkComputation(
				mock(FunctionalBlockService.class), mockFF4j(true));
		final var reachabilityNetworkBlock = createFunctionalBlock(UUID.randomUUID(), "Reachability Network", List.of(), List.of(),
				Map.of(
						FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY_NETWORK.name())
				));

		assertTrue(reachabilityResourceNetworkComputation.accept(reachabilityNetworkBlock));
	}

	@Test
	void testAcceptReturnsTrueWhenGeneratedAtNotPresent() {
		final ReachabilityResourceNetworkComputation reachabilityResourceNetworkComputation = new ReachabilityResourceNetworkComputation(
				mock(FunctionalBlockService.class), mockFF4j(true));
		final var reachabilityNetworkBlock = createFunctionalBlock(UUID.randomUUID(), "Reachability Network", List.of(), List.of(),
				Map.of(
						FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY_NETWORK.name()),
						FunctionalBlockFlag.COMPUTED_AT.name(), LocalDate.parse("2024-07-01").atStartOfDay().toInstant(ZoneOffset.UTC).toEpochMilli()
				));

		assertTrue(reachabilityResourceNetworkComputation.accept(reachabilityNetworkBlock));
	}

	@Test
	void testAcceptReturnsTrueWhenComputedAtNotPresent() {
		final ReachabilityResourceNetworkComputation reachabilityResourceNetworkComputation = new ReachabilityResourceNetworkComputation(
				mock(FunctionalBlockService.class), mockFF4j(true));
		final var reachabilityNetworkBlock = createFunctionalBlock(UUID.randomUUID(), "Reachability Network", List.of(), List.of(),
				Map.of(
						FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY_NETWORK.name()),
						FunctionalBlockFlag.GENERATED_AT.name(), LocalDate.parse("2024-07-01").atStartOfDay().toInstant(ZoneOffset.UTC).toEpochMilli()
				));

		assertTrue(reachabilityResourceNetworkComputation.accept(reachabilityNetworkBlock));
	}

	@Test
	void testAcceptReturnsTrueWhenGeneratedAtAfterComputedAt() {
		final ReachabilityResourceNetworkComputation reachabilityResourceNetworkComputation = new ReachabilityResourceNetworkComputation(
				mock(FunctionalBlockService.class), mockFF4j(true));
		final var reachabilityNetworkBlock = createFunctionalBlock(UUID.randomUUID(), "Reachability Network", List.of(), List.of(),
				Map.of(
						FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY_NETWORK.name()),
						FunctionalBlockFlag.GENERATED_AT.name(), LocalDate.parse("2024-07-02").atStartOfDay().toInstant(ZoneOffset.UTC).toEpochMilli(),
						FunctionalBlockFlag.COMPUTED_AT.name(), LocalDate.parse("2024-07-01").atStartOfDay().toInstant(ZoneOffset.UTC).toEpochMilli()
				));

		assertTrue(reachabilityResourceNetworkComputation.accept(reachabilityNetworkBlock));
	}

	@Test
	void testAcceptReturnsFalseWhenComputedAtAfterGeneratedAt() {
		final ReachabilityResourceNetworkComputation reachabilityResourceNetworkComputation = new ReachabilityResourceNetworkComputation(
				mock(FunctionalBlockService.class), mockFF4j(true));
		final var reachabilityNetworkBlock = createFunctionalBlock(UUID.randomUUID(), "Reachability Network", List.of(), List.of(),
				Map.of(
						FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY_NETWORK.name()),
						FunctionalBlockFlag.GENERATED_AT.name(), LocalDate.parse("2024-07-01").atStartOfDay().toInstant(ZoneOffset.UTC).toEpochMilli(),
						FunctionalBlockFlag.COMPUTED_AT.name(), LocalDate.parse("2024-07-02").atStartOfDay().toInstant(ZoneOffset.UTC).toEpochMilli()
				));

		assertFalse(reachabilityResourceNetworkComputation.accept(reachabilityNetworkBlock));
	}

	@Test
	void testAcceptReturnsFalseWhenComputedAtEqualToGeneratedAt() {
		final ReachabilityResourceNetworkComputation reachabilityResourceNetworkComputation = new ReachabilityResourceNetworkComputation(
				mock(FunctionalBlockService.class), mockFF4j(true));
		final var reachabilityNetworkBlock = createFunctionalBlock(UUID.randomUUID(), "Reachability Network", List.of(), List.of(),
				Map.of(
						FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY_NETWORK.name()),
						FunctionalBlockFlag.GENERATED_AT.name(), LocalDate.parse("2024-07-01").atStartOfDay().toInstant(ZoneOffset.UTC).toEpochMilli(),
						FunctionalBlockFlag.COMPUTED_AT.name(), LocalDate.parse("2024-07-01").atStartOfDay().toInstant(ZoneOffset.UTC).toEpochMilli()
				));

		assertFalse(reachabilityResourceNetworkComputation.accept(reachabilityNetworkBlock));
	}

	private List<FunctionalBlockPojo> mockFind(final BuildingConsumer<FunctionalBlockInquiryBuilder> buildingConsumer,
			final Collection<Pair<FunctionalBlockPojo, FunctionalBlockPojo>> reachabilityBlockToLowerBound,
			final Collection<Pair<FunctionalBlockPojo, FunctionalBlockPojo>> lowerBoundToModule) {

		final MockInquiryBuilder<FunctionalBlockInquiryBuilder> mockInquiryBuilder = new MockInquiryBuilder<>(FunctionalBlockInquiryBuilder.class);
		mockInquiryBuilder.addMethodWithNestedBuilder("withChild");
		mockInquiryBuilder.addMethodWithNestedBuilder("withChildForMergedAndSingleReachabilityBlocks");
		mockInquiryBuilder.addMethodWithNestedBuilder("withParent");
		mockInquiryBuilder.addMethodWithNestedBuilder("notWithParent");

		buildingConsumer.prepare(mockInquiryBuilder.mockBuilder());

		final FunctionalBlockType withType = mockInquiryBuilder.getFirstArgument("withType");
		final UUID withChild = mockInquiryBuilder.getFirstArgument("withChild", "byUid");
		final UUID withChildForMergedAndSingleReachabilityBlocks = mockInquiryBuilder.getFirstArgument("withChildForMergedAndSingleReachabilityBlocks",
				"byUid");

		if (FunctionalBlockType.MODULE.equals(withType)) {
			/* it's searching for the lower bound modules */
			return lowerBoundToModule.stream().map(Pair::getRight).toList();
		} else if (FunctionalBlockType.RA_LOWER_BOUND.equals(withType)) {
			/* it's searching for the lower bound block of given module */
			return lowerBoundToModule.stream()
					.filter(p -> p.getRight().getUid().equals(withChild))
					.map(Pair::getLeft)
					.toList();
		} else if (FunctionalBlockType.RA_TOP_DOWN.equals(withType)) {
			return reachabilityBlockToLowerBound.stream()
					.filter(p -> p.getRight().getUid().equals(withChildForMergedAndSingleReachabilityBlocks))
					.map(Pair::getLeft)
					.toList();
		}

		return Collections.emptyList();
	}

	private static FunctionalBlockPojo createFunctionalBlock(final UUID uid, final String name, final List<UUID> parents, final List<UUID> children,
			final Map<String, Object> flags) {
		return new FunctionalBlockPojo(uid, null, EntityId.of(PROJECT_ID), Collections.emptyList(), parents, children,
				name, "", flags, Instant.now());
	}

	private FF4j mockFF4j(final boolean enableFeature) {
		final FF4j mock = mock(FF4j.class);
		when(mock.getFeature(FeatureId.REACHABILITY_RESOURCE_NETWORK.getId())).thenReturn(new Feature(FeatureId.REACHABILITY_RESOURCE_NETWORK.getId(), enableFeature));
		return mock;
	}
}

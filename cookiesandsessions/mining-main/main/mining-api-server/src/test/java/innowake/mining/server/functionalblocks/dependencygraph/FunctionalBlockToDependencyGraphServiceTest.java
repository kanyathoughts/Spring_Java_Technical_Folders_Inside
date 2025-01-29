/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.dependencygraph;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.datapoints.FilterObjectCoercionService;
import innowake.mining.data.datapoints.FilterObjectService;
import innowake.mining.server.functionalblocks.service.FunctionalBlockToDependencyGraphService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipBasePojo;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.model.functionalblocks.ReachabilityBlockGraphFilterRequest;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLink;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkType;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.GeneratedFrom;
import innowake.mining.shared.entities.functionalblocks.ResolvedModulePart;
import innowake.mining.shared.entities.testing.ModulePojoDummy;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.dependency.graph.DependencyGraph;
import innowake.mining.shared.model.functionalblocks.ReachabilityNetworkGraphFilterRequest;
import innowake.mining.shared.model.functionalblocks.TechnologyType;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;

import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

import static innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag.DELETED;
import static innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag.TYPE;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyCollection;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.atMost;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Mocked Test for FunctionalBlockToDependencyGraphService
 */
@Tag("mocked")
class FunctionalBlockToDependencyGraphServiceTest {

    private static final Long PROJECT_ID = 1L;

	private static FunctionalBlockPojo upperBoundBlock1;
	private static FunctionalBlockPojo upperBoundBlock2;
	private static FunctionalBlockPojo lowerBoundBlock1, lowerBoundBlock2;
	private static FunctionalBlockPojo upperBoundModule1;
	private static FunctionalBlockPojo upperBoundModule2;
	private static FunctionalBlockPojo lowerBoundModule1;
	private static FunctionalBlockPojo lowerBoundModule2;
	private static FunctionalBlockPojo lowerBoundModule3;
	private static FunctionalBlockPojo intermediateModule1;
	private static FunctionalBlockPojo accessModule1, accessModule2;
	private static UUID functionalBlockUid1;
	private static UUID functionalBlockUid2;
	private static UUID functionalBlockUid3;
	private static UUID functionalBlockUid4;
	private static UUID functionalBlockUid5;
	private static UUID functionalBlockUid6;
	private static UUID functionalBlockUid7;
	private static UUID functionalBlockUid8;

	private static ModulePojo modulePojo1;
	private static ModulePojo modulePojo2;
	private static ModulePojo modulePojo3;
	private static ModulePojo modulePojo4;
	private static ModulePojo modulePojo5;
	private static ModulePojo modulePojo6;
	private static ModulePojo modulePojo7;
	private static ModulePojo modulePojo8;
	private static FunctionalBlockPojo dependencyGraphFunctionalBlockPojo;
	private static GeneratedFrom generatedFrom1;
	private static GeneratedFrom generatedFrom2;
	private static GeneratedFrom generatedFrom3;
	private static GeneratedFrom generatedFrom4;
	private static GeneratedFrom generatedFrom5;
	private static GeneratedFrom generatedFrom6;
	private static GeneratedFrom generatedFrom7;
	private static GeneratedFrom generatedFrom8;

	private static ModuleRelationshipPojo moduleRelationshipPojo1;
	private static ModuleRelationshipPojo moduleRelationshipPojo2;
	private static ModuleRelationshipPojo moduleRelationshipPojo3;
	private static ModuleRelationshipPojo moduleRelationshipPojo4;
	private static ModuleRelationshipPojo moduleRelationshipPojo5;
	private static ModuleRelationshipPojo moduleRelationshipPojo6;

	private final FilterObjectService filterObjectServiceMock = mock(FilterObjectService.class);
	private final FilterObjectCoercionService filterObjectCoercionServiceMock = mock(FilterObjectCoercionService.class);

    @BeforeAll
    static void init() {
		upperBoundBlock1 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock1", List.of(), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_UPPER_BOUND.name())));
		upperBoundBlock2 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock2", List.of(), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_UPPER_BOUND.name())));

		upperBoundModule1 = createFunctionalBlock(UUID.randomUUID(), "upperBoundModule1",
				List.of(upperBoundBlock1.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.MODULE.name())));
		upperBoundModule2 = createFunctionalBlock(UUID.randomUUID(), "upperBoundModule2",
				List.of(upperBoundBlock2.getUid()), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.MODULE.name())));

		accessModule1 = createFunctionalBlock(UUID.randomUUID(), "accessModule1",
				List.of(), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.MODULE.name())));
		accessModule2 = createFunctionalBlock(UUID.randomUUID(), "accessModule2",
				List.of(), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.MODULE.name())));

		intermediateModule1 = createFunctionalBlock(UUID.randomUUID(), "intermediateModule1",
				List.of(), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.MODULE.name())));

		lowerBoundModule1 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundModule1",
				List.of(), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.MODULE.name())));
		lowerBoundModule2 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundModule2",
				List.of(), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.MODULE.name())));
		lowerBoundModule3 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundModule3",
				List.of(), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.MODULE.name())));

		lowerBoundBlock1 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundBlock1",
				List.of(), List.of(lowerBoundModule1.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND.name())));
		lowerBoundBlock2 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundBlock2",
				List.of(), List.of(lowerBoundModule2.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND.name())));
    }

	/*
	 *                                 +------R------> lb1
	 *      rb1 -----------> ub1 ------+
	 *                                 +----1R/2W----> lb2
	 *      rb2 -----------> ub2 ------+
	 *                                 +------W------> lb3
	 */
	@Test
	void testDependencyGraphIsCreatedForSimpleReachabilityNetwork() {
		final var reachabilityBlock1 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock1",
				List.of(), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));
		final var reachabilityBlock2 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock2",
				List.of(), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name()),
						FunctionalBlockFlag.OUTDATED.name(), true));

		final var reachabilityNetworkBlock = createFunctionalBlock(UUID.randomUUID(), "Reachability Network",
				List.of(), List.of(reachabilityBlock1.getUid(), reachabilityBlock2.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY_NETWORK.name())));

		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final ModuleService moduleService = mock(ModuleService.class);
		when(functionalBlockService.find(reachabilityNetworkBlock.getUid())).thenReturn(Optional.of(reachabilityNetworkBlock));
		when(functionalBlockService.find(any(BuildingConsumer.class))).then(invocation -> {
			final BuildingConsumer<FunctionalBlockService.FunctionalBlockInquiryBuilder> builderConsumer = invocation.getArgument(0);
			final FunctionalBlockService.FunctionalBlockInquiryBuilder builder = Mockito.mock(FunctionalBlockService.FunctionalBlockInquiryBuilder.class);

			final ArgumentCaptor<FunctionalBlockType> typeCaptor = ArgumentCaptor.forClass(FunctionalBlockType.class);
			final ArgumentCaptor<List<UUID>> uuidCaptor = ArgumentCaptor.forClass(List.class);

			when(builder.ofProject(any())).thenReturn(builder);
			when(builder.withType(any())).thenReturn(builder);
			when(builder.byUids(any())).thenReturn(builder);
			if (builder.withParent(any()) == null) {
				when(builder.withFlag(DELETED, true)).thenReturn(builder);
			} else {
				when(builder.withParent(any())).thenReturn(builder);
			}
			when(builderConsumer.prepare(builder)).thenReturn(builder);

			builderConsumer.prepare(builder);
			Mockito.verify(builder, atLeastOnce()).withType(typeCaptor.capture());
			Mockito.verify(builder, atMost(6)).byUids(uuidCaptor.capture());

			final List<FunctionalBlockPojo> returnValue;
			if (typeCaptor.getValue().equals(FunctionalBlockType.RA_TOP_DOWN) && uuidCaptor.getValue().containsAll(reachabilityNetworkBlock.getChildren())) {
				returnValue = List.of(reachabilityBlock1, reachabilityBlock2);
			} else {
				returnValue = List.of();
			}
			return new ArrayList<>(returnValue);
		});
		when(functionalBlockService.getLinks(any(BuildingConsumer.class))).then(invocation -> {
			final BuildingConsumer<FunctionalBlockService.FunctionalBlockLinkInquiryBuilder> builderConsumer = invocation.getArgument(0);
			final FunctionalBlockService.FunctionalBlockLinkInquiryBuilder builder = Mockito.mock(FunctionalBlockService.FunctionalBlockLinkInquiryBuilder.class);

			final ArgumentCaptor<UUID> uuidCaptor = ArgumentCaptor.forClass(UUID.class);

			when(builder.ofParent(any())).thenReturn(builder);
			when(builder.withFlag(any(), any())).thenReturn(builder);

			builderConsumer.prepare(builder);
			Mockito.verify(builder, atLeastOnce()).ofParent(uuidCaptor.capture());
			if (uuidCaptor.getValue().equals(reachabilityNetworkBlock.getUid())) {
				return List.of(new FunctionalBlockLink(UUID.randomUUID(), reachabilityNetworkBlock.getUid(), reachabilityBlock2.getUid(), reachabilityBlock1.getUid(), null,
						Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.RA_SHARED_RESOURCE.name(), FunctionalBlockLinkType.DIRECTED.name()),
								FunctionalBlockLinkFlag.RA_SHARED_RESOURCE_ID.name(), List.of(lowerBoundModule2.getUid())), null));
			} else {
				return List.of();
			}
		});

		final FunctionalBlockToDependencyGraphService service = new FunctionalBlockToDependencyGraphService(filterObjectServiceMock, functionalBlockService, filterObjectCoercionServiceMock, moduleService);
		final DependencyGraph dependencyGraph = service.toReachabilityNetworkGraph(reachabilityNetworkBlock.getUid(),
				new ReachabilityNetworkGraphFilterRequest(Map.of(), Set.of(), null)).orElseThrow();
		assertEquals(2, dependencyGraph.getModules().size());
		final var module1 = dependencyGraph.getModules().stream().filter(m -> m.getUid().equals(reachabilityBlock1.getUid())).findFirst().orElseThrow();
		final var module2 = dependencyGraph.getModules().stream().filter(m -> m.getUid().equals(reachabilityBlock2.getUid())).findFirst().orElseThrow();
		assertEquals(reachabilityBlock1.getFlags(), module1.getInfo().orElseThrow());
		assertTrue((Boolean) module2.getInfo().orElseThrow().get(FunctionalBlockFlag.OUTDATED.name()));
		assertEquals(1, dependencyGraph.getReferences().size());
		assertEquals(reachabilityBlock2.getUid(), dependencyGraph.getReferences().get(0).getSrcModule());
		assertEquals(reachabilityBlock1.getUid(), dependencyGraph.getReferences().get(0).getDstModule());
		assertEquals(RelationshipDirection.OUT, dependencyGraph.getReferences().get(0).getDirection().orElseThrow());
		assertEquals(2, dependencyGraph.getRootModuleIds().size());
		assertTrue(dependencyGraph.getRootModuleIds().containsAll(List.of(module1.getId(), module2.getId())));
	}

	/*
	 *      rb1 -----------> ub1 ------------R------> lb1
	 *      rb2 -----------> ub2 ------------W------> lb3
	 */
	@Test
	void testDependencyGraphIsCreatedForSimpleReachabilityNetworkWithNoSharedModule() {
		final var reachabilityBlock1 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock1",
				List.of(), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));
		final var reachabilityBlock2 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock2",
				List.of(), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));

		final var reachabilityNetworkBlock = createFunctionalBlock(UUID.randomUUID(), "Reachability Network",
				List.of(), List.of(reachabilityBlock1.getUid(), reachabilityBlock2.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY_NETWORK.name())));

		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final ModuleService moduleService = mock(ModuleService.class);
		when(functionalBlockService.find(reachabilityNetworkBlock.getUid())).thenReturn(Optional.of(reachabilityNetworkBlock));
		when(functionalBlockService.find(any(BuildingConsumer.class))).then(invocation -> {
			final BuildingConsumer<FunctionalBlockService.FunctionalBlockInquiryBuilder> builderConsumer = invocation.getArgument(0);
			final FunctionalBlockService.FunctionalBlockInquiryBuilder builder = Mockito.mock(FunctionalBlockService.FunctionalBlockInquiryBuilder.class);

			final ArgumentCaptor<FunctionalBlockType> typeCaptor = ArgumentCaptor.forClass(FunctionalBlockType.class);
			final ArgumentCaptor<List<UUID>> uuidCaptor = ArgumentCaptor.forClass(List.class);

			when(builder.ofProject(any())).thenReturn(builder);
			when(builder.withType(any())).thenReturn(builder);
			when(builder.byUids(any())).thenReturn(builder);
			if (builder.withParent(any()) == null) {
				when(builder.withFlag(DELETED, true)).thenReturn(builder);
			} else {
				when(builder.withParent(any())).thenReturn(builder);
			}
			when(builderConsumer.prepare(builder)).thenReturn(builder);

			builderConsumer.prepare(builder);
			Mockito.verify(builder, atLeastOnce()).withType(typeCaptor.capture());
			Mockito.verify(builder, atMost(6)).byUids(uuidCaptor.capture());

			final List<FunctionalBlockPojo> returnValue;
			if (typeCaptor.getValue().equals(FunctionalBlockType.RA_TOP_DOWN) && uuidCaptor.getValue().containsAll(reachabilityNetworkBlock.getChildren())) {
				returnValue = List.of(reachabilityBlock1, reachabilityBlock2);
			} else {
				returnValue = List.of();
			}
			return new ArrayList<>(returnValue);
		});
		when(functionalBlockService.getLinks(reachabilityNetworkBlock.getUid())).thenReturn(List.of());

		final FunctionalBlockToDependencyGraphService service = new FunctionalBlockToDependencyGraphService(filterObjectServiceMock, functionalBlockService, filterObjectCoercionServiceMock, moduleService);
		final DependencyGraph dependencyGraph = service.toReachabilityNetworkGraph(reachabilityNetworkBlock.getUid(),
				new ReachabilityNetworkGraphFilterRequest(Map.of(), Set.of(), null)).orElseThrow();
		assertEquals(2, dependencyGraph.getModules().size());
		final var moduleIds = dependencyGraph.getModules().stream().map(ModulePojo::identity).collect(Collectors.toList());
		assertTrue(moduleIds.contains(reachabilityBlock1.identity()));
		assertTrue(moduleIds.contains(reachabilityBlock2.identity()));
		assertEquals(0, dependencyGraph.getReferences().size());
		assertEquals(2, dependencyGraph.getRootModuleIds().size());
		assertTrue(dependencyGraph.getRootModuleIds().containsAll(moduleIds.stream().map(EntityId::getNid).collect(Collectors.toList())));
	}

	/*
 	 *                        mr
	 *    +-------------------------------------------------+
	 *    |                            +------R------> lb1  |
	 *    | rb1 -----------> ub1 ------+                    |
	 *    |                            +----1R/2W----> lb2  |
	 *    | rb2 -----------> ub2 ------+                    |
	 *    |                            +----2W/3R----> lb3  |
	 *    +----------------------------+--------------------+
	 *    rb3 -----------> ub3 --------+------W------> lb4
	 */
	@Test
	void testDependencyGraphIsCreatedForReachabilityNetworkWithMergedBlock() {
		final var reachabilityBlock1 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock1",
				List.of(), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));
		final var reachabilityBlock2 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock2",
				List.of(), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));
		final var reachabilityBlock3 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock3",
				List.of(), List.of(),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));
		final var mergeBlock = createFunctionalBlock(UUID.randomUUID(), "mergeBlock",
				List.of(), List.of(reachabilityBlock1.getUid(), reachabilityBlock2.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.MERGE_PARENT.name(),
						FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));

		final var reachabilityNetworkBlock = createFunctionalBlock(UUID.randomUUID(), "Reachability Network",
				List.of(), List.of(mergeBlock.getUid(), reachabilityBlock3.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY_NETWORK.name())));

		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final ModuleService moduleService = mock(ModuleService.class);
		when(functionalBlockService.find(reachabilityNetworkBlock.getUid())).thenReturn(Optional.of(reachabilityNetworkBlock));
		when(functionalBlockService.find(any(BuildingConsumer.class))).then(invocation -> {
			final BuildingConsumer<FunctionalBlockService.FunctionalBlockInquiryBuilder> builderConsumer = invocation.getArgument(0);
			final FunctionalBlockService.FunctionalBlockInquiryBuilder builder = Mockito.mock(FunctionalBlockService.FunctionalBlockInquiryBuilder.class);

			final ArgumentCaptor<FunctionalBlockType> typeCaptor = ArgumentCaptor.forClass(FunctionalBlockType.class);
			final ArgumentCaptor<List<UUID>> uuidCaptor = ArgumentCaptor.forClass(List.class);

			when(builder.ofProject(any())).thenReturn(builder);
			when(builder.withType(any())).thenReturn(builder);
			when(builder.byUids(any())).thenReturn(builder);
			when(builder.withParent(any())).thenReturn(builder);
			when(builderConsumer.prepare(builder)).thenReturn(builder);

			builderConsumer.prepare(builder);
			Mockito.verify(builder, atLeastOnce()).withType(typeCaptor.capture());
			Mockito.verify(builder, atMost(6)).byUids(uuidCaptor.capture());

			final List<FunctionalBlockPojo> returnValue;
			if (typeCaptor.getValue().equals(FunctionalBlockType.RA_TOP_DOWN) && uuidCaptor.getValue().containsAll(reachabilityNetworkBlock.getChildren())) {
				returnValue = List.of(mergeBlock, reachabilityBlock3);
			} else {
				returnValue = List.of();
			}
			return new ArrayList<>(returnValue);
		});
		when(functionalBlockService.getLinks(any(BuildingConsumer.class))).then(invocation -> {
			final BuildingConsumer<FunctionalBlockService.FunctionalBlockLinkInquiryBuilder> builderConsumer = invocation.getArgument(0);
			final FunctionalBlockService.FunctionalBlockLinkInquiryBuilder builder = Mockito.mock(FunctionalBlockService.FunctionalBlockLinkInquiryBuilder.class);

			final ArgumentCaptor<UUID> uuidCaptor = ArgumentCaptor.forClass(UUID.class);

			when(builder.ofParent(any())).thenReturn(builder);
			when(builder.withFlag(any(), any())).thenReturn(builder);

			builderConsumer.prepare(builder);
			Mockito.verify(builder, atLeastOnce()).ofParent(uuidCaptor.capture());
			if (uuidCaptor.getValue().equals(reachabilityNetworkBlock.getUid())) {
				return List.of(new FunctionalBlockLink(UUID.randomUUID(), reachabilityNetworkBlock.getUid(), mergeBlock.getUid(), reachabilityBlock3.getUid(),
						null,
						Map.of(FunctionalBlockLinkFlag.TYPE.name(),
								List.of(FunctionalBlockLinkType.RA_SHARED_RESOURCE.name(), FunctionalBlockLinkType.DIRECTED.name()),
								FunctionalBlockLinkFlag.RA_SHARED_RESOURCE_ID.name(), List.of(lowerBoundModule3.getUid())), null));
			} else {
				return List.of();
			}
		});

		final FunctionalBlockToDependencyGraphService service = new FunctionalBlockToDependencyGraphService(filterObjectServiceMock, functionalBlockService, filterObjectCoercionServiceMock, moduleService);
		final DependencyGraph dependencyGraph = service.toReachabilityNetworkGraph(reachabilityNetworkBlock.getUid(),
				new ReachabilityNetworkGraphFilterRequest(Map.of(), Set.of(), null)).orElseThrow();
		assertEquals(2, dependencyGraph.getModules().size());
		final var moduleIds = dependencyGraph.getModules().stream().map(ModulePojo::identity).collect(Collectors.toList());
		assertTrue(moduleIds.containsAll(List.of(reachabilityBlock3.identity(), mergeBlock.identity())));
		assertEquals(1, dependencyGraph.getReferences().size());
		assertEquals(mergeBlock.getUid(), dependencyGraph.getReferences().get(0).getSrcModule());
		assertEquals(RelationshipDirection.OUT, dependencyGraph.getReferences().get(0).getDirection().orElseThrow());
		assertEquals(reachabilityBlock3.getUid(), dependencyGraph.getReferences().get(0).getDstModule());
		assertEquals(2, dependencyGraph.getRootModuleIds().size());
		assertTrue(dependencyGraph.getRootModuleIds().containsAll(moduleIds.stream().map(EntityId::getNid).collect(Collectors.toList())));
	}

	/*
	 *                                 +-----1R------> lb1
	 *                                 +
	 *      rb1 -----------> ub1 ------+----1R/2R----> lb2
	 *                                 +
	 *      rb2 -----------> ub2 ------+----1W/2W----> lb3
	 *                                 +
	 *                                 +-----2W------> lb4
	 */
	@Test
	@SuppressWarnings("unchecked")
	void testDependencyGraphIsCreatedForReachabilityNetworkWithSharedModuleBeingReadOnly() {
		final var reachabilityBlock1 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock1",
				List.of(), List.of(upperBoundBlock1.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));
		final var reachabilityBlock2 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock2",
				List.of(), List.of(upperBoundBlock2.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));

		final var reachabilityNetworkBlock = createFunctionalBlock(UUID.randomUUID(), "Reachability Network",
				List.of(), List.of(reachabilityBlock1.getUid(), reachabilityBlock2.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY_NETWORK.name())));

		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final ModuleService moduleService = mock(ModuleService.class);
		when(functionalBlockService.find(reachabilityNetworkBlock.getUid())).thenReturn(Optional.of(reachabilityNetworkBlock));
		when(functionalBlockService.find(any(BuildingConsumer.class))).then(invocation -> {
			final BuildingConsumer<FunctionalBlockService.FunctionalBlockInquiryBuilder> builderConsumer = invocation.getArgument(0);
			final FunctionalBlockService.FunctionalBlockInquiryBuilder builder = Mockito.mock(FunctionalBlockService.FunctionalBlockInquiryBuilder.class);

			final ArgumentCaptor<FunctionalBlockType> typeCaptor = ArgumentCaptor.forClass(FunctionalBlockType.class);
			final ArgumentCaptor<List<UUID>> uuidCaptor = ArgumentCaptor.forClass(List.class);

			when(builder.ofProject(any())).thenReturn(builder);
			when(builder.withType(any())).thenReturn(builder);
			when(builder.byUids(any())).thenReturn(builder);
			when(builder.withParent(any())).thenReturn(builder);
			when(builderConsumer.prepare(builder)).thenReturn(builder);

			builderConsumer.prepare(builder);
			Mockito.verify(builder, atLeastOnce()).withType(typeCaptor.capture());
			Mockito.verify(builder, atMost(6)).byUids(uuidCaptor.capture());

			final List<FunctionalBlockPojo> returnValue;
			if (typeCaptor.getValue().equals(FunctionalBlockType.RA_TOP_DOWN) && uuidCaptor.getValue().containsAll(reachabilityNetworkBlock.getChildren())) {
				returnValue = List.of(reachabilityBlock1, reachabilityBlock2);
			} else {
				returnValue = List.of();
			}
			return new ArrayList<>(returnValue);
		});
		when(functionalBlockService.getLinks(any(BuildingConsumer.class))).then(invocation -> {
			final BuildingConsumer<FunctionalBlockService.FunctionalBlockLinkInquiryBuilder> builderConsumer = invocation.getArgument(0);
			final FunctionalBlockService.FunctionalBlockLinkInquiryBuilder builder = Mockito.mock(FunctionalBlockService.FunctionalBlockLinkInquiryBuilder.class);

			final ArgumentCaptor<UUID> uuidCaptor = ArgumentCaptor.forClass(UUID.class);

			when(builder.ofParent(any())).thenReturn(builder);
			when(builder.withFlag(any(), any())).thenReturn(builder);

			builderConsumer.prepare(builder);
			Mockito.verify(builder, atLeastOnce()).ofParent(uuidCaptor.capture());
			if (uuidCaptor.getValue().equals(reachabilityNetworkBlock.getUid())) {
				return List.of(new FunctionalBlockLink(UUID.randomUUID(), reachabilityNetworkBlock.getUid(), reachabilityBlock1.getUid(),
								reachabilityBlock2.getUid(), null, Map.of(FunctionalBlockLinkFlag.TYPE.name(),
								List.of(FunctionalBlockLinkType.RA_SHARED_RESOURCE.name(), FunctionalBlockLinkType.DIRECTED.name()),
									FunctionalBlockLinkFlag.RA_SHARED_RESOURCE_ID.name(), List.of(lowerBoundModule2.getUid())), null),
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityNetworkBlock.getUid(), reachabilityBlock2.getUid(),
								reachabilityBlock1.getUid(), null,  Map.of(FunctionalBlockLinkFlag.TYPE.name(),
								List.of(FunctionalBlockLinkType.RA_SHARED_RESOURCE.name(), FunctionalBlockLinkType.DIRECTED.name()),
										FunctionalBlockLinkFlag.RA_SHARED_RESOURCE_ID.name(), List.of(lowerBoundModule3.getUid())), null));
			} else {
				return List.of();
			}
		});

		final FunctionalBlockToDependencyGraphService service = new FunctionalBlockToDependencyGraphService(filterObjectServiceMock, functionalBlockService, filterObjectCoercionServiceMock, moduleService);
		final DependencyGraph dependencyGraph = service.toReachabilityNetworkGraph(reachabilityNetworkBlock.getUid(),
				new ReachabilityNetworkGraphFilterRequest(Map.of(), Set.of(), null)).orElseThrow();
		assertEquals(2, dependencyGraph.getModules().size());
		final var moduleIds = dependencyGraph.getModules().stream().map(ModulePojo::identity).collect(Collectors.toList());
		assertTrue(moduleIds.containsAll(List.of(reachabilityBlock1.identity(), reachabilityBlock2.identity())));
		assertEquals(1, dependencyGraph.getReferences().size());
		final ModuleRelationshipPojo reference = dependencyGraph.getReferences().get(0);
		final Collection<UUID> sharedResources = (Collection<UUID>) reference.getProperties().orElseThrow().get(FunctionalBlockLinkFlag.RA_SHARED_RESOURCE_ID.name());
		assertTrue(Set.of(reachabilityBlock1.getUid(), reachabilityBlock2.getUid()).contains(reference.getSrcModule()));
		assertTrue(Set.of(reachabilityBlock1.getUid(), reachabilityBlock2.getUid()).contains(reference.getDstModule()));
		assertNotEquals(reference.getSrcModule(), reference.getDstModule());
		assertEquals(RelationshipDirection.BOTH, reference.getDirection().orElseThrow());
		assertEquals(2, sharedResources.size());
		assertTrue(Set.of(lowerBoundModule2.getUid(), lowerBoundModule3.getUid()).containsAll(sharedResources));
		assertEquals(2, dependencyGraph.getRootModuleIds().size());
		assertTrue(dependencyGraph.getRootModuleIds().containsAll(moduleIds.stream().map(EntityId::getNid).collect(Collectors.toList())));
	}

	/*
	 *                            +--> im1 -------> am1 ----R----> lb1
	 *     rb1 --------> ub1 -----+
	 *                            +--> am2 ------W----> lb2
	 */
    @ParameterizedTest
	@ValueSource(booleans = {true, false})
    void testCreateDependencyGraphForCallChainTypes(final boolean withDeletedModules) {
		final ModulePojo ub1 = ModulePojoDummy.build(
				new ModulePojoPrototype().setUid(UUID.randomUUID()).setNid(3L).setLinkHash("ubLinkHash1").setTechnology(Technology.JCL)
						.setType(Type.JOB));
		final ModulePojo im1 = ModulePojoDummy.build(
				new ModulePojoPrototype().setUid(UUID.randomUUID()).setNid(4L).setLinkHash("imLinkHash1").setTechnology(Technology.JCL)
						.setType(Type.EXEC_PGM));
		final ModulePojo am1 = ModulePojoDummy.build(
				new ModulePojoPrototype().setUid(UUID.randomUUID()).setNid(5L).setLinkHash("amLinkHash1").setTechnology(Technology.COBOL)
						.setType(Type.PROGRAM));
		final ModulePojo am2 = ModulePojoDummy.build(
				new ModulePojoPrototype().setUid(UUID.randomUUID()).setNid(6L).setLinkHash("amLinkHash2").setTechnology(Technology.NATURAL)
						.setType(Type.PROGRAM));
		final ModulePojo lb1 = ModulePojoDummy.build(
				new ModulePojoPrototype().setUid(UUID.randomUUID()).setNid(7L).setLinkHash("lbLinkHash1").setTechnology(Technology.RESOURCE)
						.setType(Type.FILE));
		final ModulePojo lb2 = ModulePojoDummy.build(
				new ModulePojoPrototype().setUid(UUID.randomUUID()).setNid(8L).setLinkHash("lbLinkHash2").setTechnology(Technology.SQL)
						.setType(Type.TABLE));

		final var callChainBlock = createFunctionalBlock(UUID.randomUUID(), "Call Chain",
				List.of(), List.of(upperBoundModule1.getUid(), lowerBoundModule1.getUid(), lowerBoundModule2.getUid(), accessModule1.getUid(),
						accessModule2.getUid(), intermediateModule1.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.CALL_CHAIN.name())));

		final var refUb1Im1 = new ModuleRelationshipPojo(UUID.randomUUID(), ub1.getUid(), null, im1.getUid(), null,
				RelationshipType.CALLS, null, null, null, null, Collections.emptyList(),
				null, null, null);
		final var refIm1Am1 = new ModuleRelationshipPojo(UUID.randomUUID(), im1.getUid(), null, am1.getUid(), null,
				RelationshipType.CALLS, null, null, null, null, Collections.emptyList(),
				null, null, null);
		final var refAm1Lb1 = new ModuleRelationshipPojo(UUID.randomUUID(), am1.getUid(), null, lb1.getUid(), null,
				RelationshipType.ACCESSES, null, null, null, null, Collections.emptyList(), null, null, null);
		final var refUb1Am2 = new ModuleRelationshipPojo(UUID.randomUUID(), ub1.getUid(), null, am2.getUid(), null,
				RelationshipType.CALLS, null, null, null, null, Collections.emptyList(),
				null, null, null);
		final var refAm2Lb2 = new ModuleRelationshipPojo(UUID.randomUUID(), am2.getUid(), null, lb2.getUid(), null,
				RelationshipType.ACCESSES, null, null, null, null, Collections.emptyList(),
				null, null, null);

		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final ModuleService moduleService = mock(ModuleService.class);

		when(functionalBlockService.getLinks(callChainBlock.getUid())).thenReturn(List.of(
				new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), upperBoundModule1.getUid(), intermediateModule1.getUid(), null,
						Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN.name(), FunctionalBlockLinkType.DIRECTED.name()),
								FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(refUb1Im1.getRelationship())), null),
				new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), intermediateModule1.getUid(), accessModule1.getUid(), null,
						Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN.name(), FunctionalBlockLinkType.DIRECTED.name()),
								FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(refIm1Am1.getRelationship())), null),
				new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), accessModule1.getUid(), lowerBoundModule1.getUid(), null,
						Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN.name(), FunctionalBlockLinkType.DIRECTED.name()),
								FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(refAm1Lb1.getRelationship())), null),
				new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), upperBoundModule1.getUid(), accessModule2.getUid(), null,
						Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN.name(), FunctionalBlockLinkType.DIRECTED.name()),
								FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(refUb1Am2.getRelationship())), null),
				new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), accessModule2.getUid(), lowerBoundModule2.getUid(), null,
						Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN.name(), FunctionalBlockLinkType.DIRECTED.name()),
								FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(refAm2Lb2.getRelationship())), null)
		));
		when(functionalBlockService.getGeneratedFrom(anyCollection())).then(invocation -> {
			final Collection<UUID> argument = invocation.getArgument(0);
			if (argument.containsAll(List.of(upperBoundModule1.getUid(), lowerBoundModule1.getUid(), lowerBoundModule2.getUid(), accessModule1.getUid(),
					accessModule2.getUid(), intermediateModule1.getUid()))) {
				return Map.of(upperBoundModule1.getUid(), GeneratedFrom.fromModule("ubLinkHash1", "ubContentHash1", "ubDependencyHash1"),
						lowerBoundModule1.getUid(), GeneratedFrom.fromModule("lbLinkHash1", "lbContentHash1", "lbDependencyHash1"),
						lowerBoundModule2.getUid(), GeneratedFrom.fromModule("lbLinkHash2", "lbContentHash2", "lbDependencyHash2"),
						accessModule1.getUid(), GeneratedFrom.fromModule("amLinkHash1", "amContentHash1", "amDependencyHash1"),
						accessModule2.getUid(), GeneratedFrom.fromModule("amLinkHash2", "amContentHash2", "amDependencyHash2"),
						intermediateModule1.getUid(), GeneratedFrom.fromModule("imLinkHash1", "imContentHash1", "imDependencyHash1"));
			} else {
				return Collections.emptyMap();
			}
		});
		when(moduleService.findModules(any())).then(invocation -> {
			final BuildingConsumer<ModuleService.ModuleInquiryBuilder> builderConsumer = invocation.getArgument(0);
			final ModuleService.ModuleInquiryBuilder builder = Mockito.mock(ModuleService.ModuleInquiryBuilder.class);

			final ArgumentCaptor<List<String>> linkHashCaptor = ArgumentCaptor.forClass(List.class);

			when(builder.ofProject(any())).thenReturn(builder);
			when(builder.withLinkHashes(any())).thenReturn(builder);

			builderConsumer.prepare(builder);
			Mockito.verify(builder, atLeastOnce()).withLinkHashes(linkHashCaptor.capture());

			final List<ModulePojo> returnValue;
			final List<String> linkHashes = List.of("ubLinkHash1", "lbLinkHash1", "lbLinkHash2", "amLinkHash1", "amLinkHash2", "imLinkHash1");
			if (linkHashCaptor.getValue().size() == linkHashes.size() && linkHashCaptor.getValue().containsAll(linkHashes)) {
				returnValue = withDeletedModules ? List.of(ub1, am1, am2, lb1, lb2) : List.of(ub1, im1, am1, am2, lb1, lb2);
			} else {
				returnValue = List.of();
			}
			return new ArrayList<>(returnValue);
		});

		final List<FunctionalBlockPojo> functionalBlocks = Arrays.asList(
				upperBoundModule1,
				lowerBoundModule1,
				lowerBoundModule2,
				accessModule1,
				accessModule2,
				intermediateModule1
		);
		when(functionalBlockService.get(any())).thenReturn(functionalBlocks);

        when(moduleService.findRelationship(any())).then(invocation -> {
			final BuildingConsumer<ModuleService.ModuleRelationshipInquiryBuilder> builderConsumer = invocation.getArgument(0);
			final ModuleService.ModuleRelationshipInquiryBuilder builder = Mockito.mock(ModuleService.ModuleRelationshipInquiryBuilder.class);

			final ArgumentCaptor<Collection<UUID>> moduleCaptor = ArgumentCaptor.forClass(Collection.class);

			when(builder.ofProject(any())).thenReturn(builder);
			when(builder.byIds(any())).thenReturn(builder);
			when(builderConsumer.prepare(builder)).thenReturn(builder);

			builderConsumer.prepare(builder);
			Mockito.verify(builder, atLeastOnce()).byIds(moduleCaptor.capture());

			final List<ModuleRelationshipPojo> returnValue;
			if (moduleCaptor.getValue().containsAll(List.of(refUb1Im1.getId(), refIm1Am1.getId(), refAm1Lb1.getId(), refUb1Am2.getId(), refAm2Lb2.getId()))) {
				returnValue = List.of(refUb1Im1, refUb1Am2, refIm1Am1, refAm1Lb1, refAm2Lb2);
			} else {
				returnValue = List.of();
			}
			return new ArrayList<>(returnValue);
		});

        final FunctionalBlockToDependencyGraphService service = new FunctionalBlockToDependencyGraphService(filterObjectServiceMock, functionalBlockService, filterObjectCoercionServiceMock, moduleService);
        final DependencyGraph dependencyGraph = service.toFunctionalBlockGraph(callChainBlock, null);
        assertEquals(6, dependencyGraph.getModules().size());
		if( ! withDeletedModules) {
			assertTrue(dependencyGraph.getModules().stream().map(ModulePojo::identity).collect(Collectors.toList()).containsAll(
					List.of(ub1.identity(), im1.identity(), am1.identity(), am2.identity(), lb1.identity(), lb2.identity())));
		} else {
			dependencyGraph.getModules().stream().filter(m -> m.getName().equals("intermediateModule1")).forEach(m -> {
				assertEquals(0L, m.identity().getNid()); // To validate if it's a dummy module created by us
			});
		}
        assertEquals(5, dependencyGraph.getReferences().size());
		assertEquals(Set.of(RelationshipType.CALLS, RelationshipType.ACCESSES),
				dependencyGraph.getReferences().stream().map(ModuleRelationshipBasePojo::getRelationship).collect(Collectors.toSet()));
		assertEquals(1, dependencyGraph.getRootModuleIds().size());
		assertTrue(dependencyGraph.getRootModuleIds().contains(ub1.getId()));
    }

	/*
	 *                        mr
	 *    +-------------------------------------------------+
	 *    |                            +------R------> lb1  |
	 *    | rb1 -----------> ub1 ------+                    |
	 *    |                            +----1R/2W----> lb2  |
	 *    | rb2 -----------> ub2 ------+                    |
	 *    |                            +----2W/3R----> lb3  |
	 *    +-------------------------------------------------+
	 */
    @Test
    void testCreateDependencyGraphForMergeFunctionalBlock() {
		final ModulePojo ub1 = ModulePojoDummy.build(
				new ModulePojoPrototype().setUid(UUID.randomUUID()).setNid(3L).setLinkHash("ubLinkHash1").setTechnology(Technology.JCL)
						.setType(Type.JOB));
		final ModulePojo ub2 = ModulePojoDummy.build(
				new ModulePojoPrototype().setUid(UUID.randomUUID()).setNid(4L).setLinkHash("ubLinkHash2").setTechnology(Technology.JCL)
						.setType(Type.JOB));
		final ModulePojo lb1 = ModulePojoDummy.build(
				new ModulePojoPrototype().setUid(UUID.randomUUID()).setNid(5L).setLinkHash("lbLinkHash1").setTechnology(Technology.RESOURCE)
						.setType(Type.FILE));
		final ModulePojo lb2 = ModulePojoDummy.build(
				new ModulePojoPrototype().setUid(UUID.randomUUID()).setNid(6L).setLinkHash("lbLinkHash2").setTechnology(Technology.SQL)
						.setType(Type.TABLE));
		final ModulePojo lb3 = ModulePojoDummy.build(
				new ModulePojoPrototype().setUid(UUID.randomUUID()).setNid(7L).setLinkHash("lbLinkHash3").setTechnology(Technology.SQL)
						.setType(Type.TABLE));

		final var refUb1Lb1 = new ModuleRelationshipPojo(UUID.randomUUID(), ub1.getUid(), null, lb1.getUid(), null,
				RelationshipType.ACCESSES, null, null, null, null, Collections.emptyList(),
				null, null, null);
		final var refUb1Lb2 = new ModuleRelationshipPojo(UUID.randomUUID(), ub1.getUid(), null, lb2.getUid(), null,
				RelationshipType.ACCESSES, null, null, null, null, Collections.emptyList(),
				null, null, null);
		final var refUb2Lb2 = new ModuleRelationshipPojo(UUID.randomUUID(), ub2.getUid(), null, lb2.getUid(), null,
				RelationshipType.ACCESSES, null, null, null, null, Collections.emptyList(),
				null, null, null);
		final var refUb2Lb3 = new ModuleRelationshipPojo(UUID.randomUUID(), ub2.getUid(), null, lb3.getUid(), null,
				RelationshipType.ACCESSES, null, null, null, null, Collections.emptyList(),
				null, null, null);

		final var lowerBoundBlock3 = createFunctionalBlock(UUID.randomUUID(), "lowerBoundBlock3",
				List.of(), List.of(lowerBoundModule3.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_LOWER_BOUND.name())));

		final var callChainBlock1 = createFunctionalBlock(UUID.randomUUID(), "Call Chain 1",
				List.of(), List.of(upperBoundModule1.getUid(), lowerBoundModule1.getUid(), lowerBoundModule2.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.CALL_CHAIN.name())));
		final var callChainBlock2 = createFunctionalBlock(UUID.randomUUID(), "Call Chain 2",
				List.of(), List.of(upperBoundModule2.getUid(), lowerBoundModule2.getUid(), lowerBoundModule3.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.CALL_CHAIN.name())));

		final var reachabilityBlock1 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock1",
				List.of(), List.of(upperBoundBlock1.getUid(), lowerBoundBlock1.getUid(), lowerBoundBlock2.getUid(), callChainBlock1.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));
		final var reachabilityBlock2 = createFunctionalBlock(UUID.randomUUID(), "upperBoundBlock2",
				List.of(), List.of(upperBoundBlock2.getUid(), lowerBoundBlock2.getUid(), lowerBoundBlock3.getUid(), callChainBlock2.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));

		final var mergeBlock = createFunctionalBlock(UUID.randomUUID(), "Merge Block",
				List.of(), List.of(reachabilityBlock1.getUid(), reachabilityBlock2.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.MERGE_PARENT.name(),
						FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));

		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final ModuleService moduleService = mock(ModuleService.class);

		when(functionalBlockService.findChildrenDeep(eq(mergeBlock.getUid()), eq(2), any())).then(invocation -> {
			final BuildingConsumer<FunctionalBlockService.FunctionalBlockInquiryBuilder> builderConsumer = invocation.getArgument(2);
			final FunctionalBlockService.FunctionalBlockInquiryBuilder builder = Mockito.mock(FunctionalBlockService.FunctionalBlockInquiryBuilder.class);

			final ArgumentCaptor<FunctionalBlockType> typeCaptor = ArgumentCaptor.forClass(FunctionalBlockType.class);

			when(builder.ofProject(any())).thenReturn(builder);
			when(builder.withType(any())).thenReturn(builder);
			when(builder.byUids(any())).thenReturn(builder);
			when(builder.withParent(any())).thenReturn(builder);
			when(builderConsumer.prepare(builder)).thenReturn(builder);

			builderConsumer.prepare(builder);
			Mockito.verify(builder, atLeastOnce()).withType(typeCaptor.capture());

			final List<FunctionalBlockPojo> returnValue;
			if (typeCaptor.getValue().equals(FunctionalBlockType.CALL_CHAIN)) {
				returnValue = List.of(callChainBlock1, callChainBlock2);
			} else {
				returnValue = List.of();
			}
			return new ArrayList<>(returnValue);
		});
		when(functionalBlockService.getLinks(anyCollection())).then(invocation -> {
			final Collection<UUID> argument = invocation.getArgument(0);
			if (argument.containsAll(List.of(callChainBlock1.getUid(), callChainBlock2.getUid()))) {
				return Map.of(callChainBlock1.getUid(), List.of(
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock1.getUid(), upperBoundModule1.getUid(), lowerBoundModule1.getUid(), null,
								Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN.name(), FunctionalBlockLinkType.DIRECTED.name()),
										FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(refUb1Lb1.getRelationship())), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock1.getUid(), upperBoundModule1.getUid(), lowerBoundModule2.getUid(), null,
								Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN.name(), FunctionalBlockLinkType.DIRECTED.name()),
										FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(refUb1Lb2.getRelationship())), null)),
				callChainBlock2.getUid(), List.of(
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock2.getUid(), upperBoundModule2.getUid(), lowerBoundModule2.getUid(), null,
								Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN.name(), FunctionalBlockLinkType.DIRECTED.name()),
										FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(refUb2Lb2.getRelationship())), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock2.getUid(), upperBoundModule2.getUid(), lowerBoundModule3.getUid(), null,
								Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN.name(), FunctionalBlockLinkType.DIRECTED.name()),
										FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(refUb2Lb3.getRelationship())), null)));
			} else {
				return Map.of();
			}
		});
		when(functionalBlockService.getGeneratedFrom(anyCollection())).then(invocation -> {
			final Collection<UUID> argument = invocation.getArgument(0);
			if (argument.containsAll(List.of(upperBoundModule1.getUid(), upperBoundModule2.getUid(),
					lowerBoundModule1.getUid(), lowerBoundModule2.getUid(), lowerBoundModule3.getUid()))){
				return Map.of(upperBoundModule1.getUid(), GeneratedFrom.fromModule("ubLinkHash1", "ubContentHash1", "ubDependencyHash1"),
						upperBoundModule2.getUid(), GeneratedFrom.fromModule("ubLinkHash2", "ubContentHash2", "ubDependencyHash2"),
						lowerBoundModule1.getUid(), GeneratedFrom.fromModule("lbLinkHash1", "lbContentHash1", "lbDependencyHash1"),
						lowerBoundModule2.getUid(), GeneratedFrom.fromModule("lbLinkHash2", "lbContentHash2", "lbDependencyHash2"),
						lowerBoundModule3.getUid(), GeneratedFrom.fromModule("lbLinkHash3", "lbContentHash3", "lbDependencyHash3"));
			} else {
				return Collections.emptyMap();
			}
		});
		when(moduleService.findModules(any())).then(invocation -> {
			final BuildingConsumer<ModuleService.ModuleInquiryBuilder> builderConsumer = invocation.getArgument(0);
			final ModuleService.ModuleInquiryBuilder builder = Mockito.mock(ModuleService.ModuleInquiryBuilder.class);

			final ArgumentCaptor<List<String>> linkHashCaptor = ArgumentCaptor.forClass(List.class);

			when(builder.ofProject(any())).thenReturn(builder);
			when(builder.withLinkHashes(any())).thenReturn(builder);

			builderConsumer.prepare(builder);
			Mockito.verify(builder, atLeastOnce()).withLinkHashes(linkHashCaptor.capture());

			final List<ModulePojo> returnValue;
			final List<String> linkHashes = List.of("ubLinkHash1", "ubLinkHash2", "lbLinkHash1", "lbLinkHash2", "lbLinkHash3");
			if (linkHashCaptor.getValue().size() == linkHashes.size() && linkHashCaptor.getValue().containsAll(linkHashes)) {
				returnValue = List.of(ub1, ub2, lb1, lb2, lb3);
			} else {
				returnValue = List.of();
			}
			return new ArrayList<>(returnValue);
		});

		final List<FunctionalBlockPojo> functionalBlocks = Arrays.asList(
				upperBoundModule1,
				upperBoundModule2,
				lowerBoundModule1,
				lowerBoundModule2,
				lowerBoundModule3
		);
		when(functionalBlockService.get(any())).thenReturn(functionalBlocks);

		when(moduleService.findRelationship(any())).then(invocation -> {
			final BuildingConsumer<ModuleService.ModuleRelationshipInquiryBuilder> builderConsumer = invocation.getArgument(0);
			final ModuleService.ModuleRelationshipInquiryBuilder builder = Mockito.mock(ModuleService.ModuleRelationshipInquiryBuilder.class);

			final ArgumentCaptor<Collection<UUID>> moduleCaptor = ArgumentCaptor.forClass(Collection.class);

			when(builder.ofProject(any())).thenReturn(builder);
			when(builder.byIds(any())).thenReturn(builder);
			when(builderConsumer.prepare(builder)).thenReturn(builder);

			builderConsumer.prepare(builder);
			Mockito.verify(builder, atLeastOnce()).byIds(moduleCaptor.capture());

			final List<ModuleRelationshipPojo> returnValue;
			if (moduleCaptor.getValue().containsAll(List.of(refUb1Lb1.getId(), refUb1Lb2.getId(), refUb2Lb2.getId(), refUb2Lb3.getId()))) {
				returnValue = List.of(refUb1Lb1, refUb1Lb2, refUb2Lb2, refUb2Lb3);
			} else {
				returnValue = List.of();
			}
			return new ArrayList<>(returnValue);
		});

		final FunctionalBlockToDependencyGraphService service = new FunctionalBlockToDependencyGraphService(filterObjectServiceMock, functionalBlockService, filterObjectCoercionServiceMock, moduleService);
		final DependencyGraph dependencyGraph = service.toFunctionalBlockGraph(mergeBlock, null);
		assertEquals(5, dependencyGraph.getModules().size());
		final var moduleIds = dependencyGraph.getModules().stream().map(ModulePojo::identity).collect(Collectors.toList());
		assertTrue(moduleIds.containsAll(List.of(ub1.identity(), ub2.identity(), lb1.identity(), lb2.identity(), lb3.identity())));
		assertEquals(4, dependencyGraph.getReferences().size());
		assertEquals(Set.of(RelationshipType.ACCESSES), dependencyGraph.getReferences()
				.stream().map(ModuleRelationshipBasePojo::getRelationship).collect(Collectors.toSet()));
		assertEquals(2, dependencyGraph.getRootModuleIds().size());
		assertTrue(dependencyGraph.getRootModuleIds().containsAll(List.of(ub1.getId(), ub2.getId())));
    }

	@Test
	void testCreateDependencyGraphForUnsupportedTypes() {
		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final ModuleService moduleService = mock(ModuleService.class);

		final FunctionalBlockToDependencyGraphService service = new FunctionalBlockToDependencyGraphService(filterObjectServiceMock, functionalBlockService, filterObjectCoercionServiceMock, moduleService);
		final DependencyGraph dependencyGraph = service.toFunctionalBlockGraph(new FunctionalBlockPojo(UUID.randomUUID(), null, EntityId.of(PROJECT_ID),
				List.of(), null, List.of(), "top down ", null, Map.of(TYPE.name(), FunctionalBlockType.MODULE), null), null);

		verify(functionalBlockService, never()).findChildrenDeep(any(UUID.class), anyInt(), any());
		verify(functionalBlockService, never()).getGeneratedFrom(anySet());
		verify(moduleService, never()).findModules(any());
		verify(moduleService, never()).findRelationship(any());

		assertEquals(0, dependencyGraph.getModules().size());
		assertEquals(0, dependencyGraph.getReferences().size());
		assertEquals(0, dependencyGraph.getRootModuleIds().size());
	}

	/*			       calls           calls            References
	 *        Fb1(m1) --------->Fb2(m2)---------->Fb3(m3) ---------> Fb4(m4)
	 *		   		    		|
	 * 							|Access
	 * 				    		|
	 * 							Fb5(m5)---------->Fb6(m6) ---------> Fb7(m7)
	 * 						    		Includes		    Includes
	 *
	 */
	@Test
	void testFilterDependencyGraphOnlyByFunctionalBlockUid() {
		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final ModuleService moduleService = mock(ModuleService.class);

		dependencyGraphFunctionalBlockPojo = createDataForDependencyGraphFiltering(functionalBlockService);

		mockSetupForDependencyGraph(moduleService, functionalBlockService);
		when(moduleService.findModuleUids(any(BuildingConsumer.class))).thenReturn(Collections.emptyList());

		final UUID fbUid1 = UUID.randomUUID();
		when(functionalBlockService.getResolvedModuleParts(Set.of(fbUid1)))
				.thenReturn(Map.of(fbUid1, List.of(new ResolvedModulePart(EntityId.of(modulePojo1.getUid())),
						new ResolvedModulePart(EntityId.of(modulePojo3.getUid())), new ResolvedModulePart(EntityId.of(modulePojo4.getUid())),
						new ResolvedModulePart(EntityId.of(modulePojo5.getUid())), new ResolvedModulePart(EntityId.of(modulePojo6.getUid())),
						new ResolvedModulePart(EntityId.of(modulePojo7.getUid())))));

		final FunctionalBlockToDependencyGraphService service = new FunctionalBlockToDependencyGraphService(filterObjectServiceMock, functionalBlockService,
				filterObjectCoercionServiceMock, moduleService);
		final ReachabilityBlockGraphFilterRequest filterRequest = new ReachabilityBlockGraphFilterRequest(null,
				Set.of(EntityId.of(fbUid1)), null, null);
		final DependencyGraph dependencyGraph = service.toFunctionalBlockGraph(dependencyGraphFunctionalBlockPojo, filterRequest);

		assertEquals(6, dependencyGraph.getModules().size());
		assertEquals(1, dependencyGraph.getRootModuleIds().size());
		assertEquals(5, dependencyGraph.getReferences().size());
		assertEquals(3, dependencyGraph.getRelationshipTypes().size());
		assertEquals(3, dependencyGraph.getModuleTypes().size());

		final Map<UUID, Set<ModuleRelationshipPojo>> outgoingReferences = dependencyGraph.getReferences().stream()
				.collect(Collectors.groupingBy(ModuleRelationshipPojo::getSrcModule, Collectors.toSet()));
		assertEquals(2, outgoingReferences.get(modulePojo1.getUid()).size());
		assertEquals(Set.of(modulePojo3.getUid(), modulePojo5.getUid()),
				outgoingReferences.get(modulePojo1.getUid()).stream().map(ModuleRelationshipPojo::getDstModule).collect(Collectors.toSet()));
		assertEquals(Set.of(RelationshipType.ARTIFICIAL),
				outgoingReferences.get(modulePojo1.getUid()).stream().map(ModuleRelationshipPojo::getRelationship).collect(Collectors.toSet()));
		assertFalse(outgoingReferences.containsKey(modulePojo2.getUid()));
		assertEquals(1, outgoingReferences.get(modulePojo3.getUid()).size());
		assertEquals(1, outgoingReferences.get(modulePojo5.getUid()).size());
		assertEquals(1, outgoingReferences.get(modulePojo6.getUid()).size());
	}

	@Test
	void testFilterDependencyGraphOnlyByRelationshipTypes() {
		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final ModuleService moduleService = mock(ModuleService.class);

		dependencyGraphFunctionalBlockPojo = createDataForDependencyGraphFiltering(functionalBlockService);
		mockSetupForDependencyGraph(moduleService, functionalBlockService);
		when(moduleService.findModuleUids(any(BuildingConsumer.class))).thenReturn(Collections.emptyList());

		final FunctionalBlockToDependencyGraphService service = new FunctionalBlockToDependencyGraphService(filterObjectServiceMock, functionalBlockService, filterObjectCoercionServiceMock, moduleService);
		final ReachabilityBlockGraphFilterRequest filterRequest = new ReachabilityBlockGraphFilterRequest(null, null, null,
				Set.of(RelationshipType.INCLUDES, RelationshipType.REFERENCES, RelationshipType.ACCESSES));
		final DependencyGraph dependencyGraph = service.toFunctionalBlockGraph(dependencyGraphFunctionalBlockPojo, filterRequest);

		assertEquals(5, dependencyGraph.getModules().size());
		assertEquals(1, dependencyGraph.getRootModuleIds().size());
		assertEquals(4, dependencyGraph.getReferences().size());
		assertEquals(2, dependencyGraph.getRelationshipTypes().size());
		assertEquals(3, dependencyGraph.getModuleTypes().size());

		final Map<UUID, Set<ModuleRelationshipPojo>> outgoingReferences = dependencyGraph.getReferences().stream()
				.collect(Collectors.groupingBy(ModuleRelationshipPojo::getSrcModule, Collectors.toSet()));
		assertEquals(2, outgoingReferences.get(modulePojo1.getUid()).size());
		assertEquals(Set.of(modulePojo4.getUid(), modulePojo5.getUid()),
				outgoingReferences.get(modulePojo1.getUid()).stream().map(ModuleRelationshipPojo::getDstModule).collect(Collectors.toSet()));
		assertEquals(Set.of(RelationshipType.ARTIFICIAL),
				outgoingReferences.get(modulePojo1.getUid()).stream().map(ModuleRelationshipPojo::getRelationship).collect(Collectors.toSet()));
		assertFalse(outgoingReferences.containsKey(modulePojo2.getUid()));
		assertFalse(outgoingReferences.containsKey(modulePojo3.getUid()));
		assertEquals(1, outgoingReferences.get(modulePojo5.getUid()).size());
		assertEquals(1, outgoingReferences.get(modulePojo6.getUid()).size());
	}

	@Test
	void testFilterDependencyGraphOnlyByTechnologyType() {
		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final ModuleService moduleService = mock(ModuleService.class);

		dependencyGraphFunctionalBlockPojo = createDataForDependencyGraphFiltering(functionalBlockService);

		mockSetupForDependencyGraph(moduleService, functionalBlockService);
		when(moduleService.findModuleUids(any())).then(invocation -> {
			final BuildingConsumer<ModuleService.ModuleInquiryBuilder> builderConsumer = invocation.getArgument(0);
			final ModuleService.ModuleInquiryBuilder builder = Mockito.mock(ModuleService.ModuleInquiryBuilder.class);

			final ArgumentCaptor<Collection<Tuple2<Technology, Type>>> technologyTypeCaptor = ArgumentCaptor.forClass(Collection.class);

			when(builder.ofProject(any())).thenReturn(builder);
			when(builder.withLinkHashes(any())).thenReturn(builder);
			when(builder.withTechnologiesAndTypes(anyCollection())).thenReturn(builder);

			builderConsumer.prepare(builder);
			Mockito.verify(builder, atLeastOnce()).notWithTechnologiesAndTypes(technologyTypeCaptor.capture());

			final List<UUID> returnValue;
			final List<TechnologyType> technologyTypes =
					List.of(new TechnologyType(Technology.JCL, Type.JOB),
							new TechnologyType(Technology.RESOURCE, Type.FILE));
			if (technologyTypeCaptor.getValue().size() == technologyTypes.size()) {
				returnValue = List.of(modulePojo2.getUid(), modulePojo3.getUid(), modulePojo5.getUid(), modulePojo6.getUid());
			} else {
				returnValue = List.of();
			}
			return new ArrayList<>(returnValue);
		});

		final FunctionalBlockToDependencyGraphService service = new FunctionalBlockToDependencyGraphService(filterObjectServiceMock, functionalBlockService, filterObjectCoercionServiceMock, moduleService);
		final ReachabilityBlockGraphFilterRequest filterRequest = new ReachabilityBlockGraphFilterRequest(null, null,
				Set.of(new TechnologyType(Technology.JCL, Type.JOB),
						new TechnologyType(Technology.RESOURCE, Type.FILE)), null);
		final DependencyGraph dependencyGraph = service.toFunctionalBlockGraph(dependencyGraphFunctionalBlockPojo, filterRequest);

		assertEquals(3, dependencyGraph.getModules().size());
		assertEquals(1, dependencyGraph.getRootModuleIds().size());
		assertEquals(2, dependencyGraph.getReferences().size());
		assertEquals(1, dependencyGraph.getRelationshipTypes().size());
		assertEquals(2, dependencyGraph.getModuleTypes().size());

		final Map<UUID, Set<ModuleRelationshipPojo>> outgoingReferences = dependencyGraph.getReferences().stream()
				.collect(Collectors.groupingBy(ModuleRelationshipPojo::getSrcModule, Collectors.toSet()));
		assertEquals(2, outgoingReferences.get(modulePojo1.getUid()).size());
		assertEquals(Set.of(modulePojo4.getUid(), modulePojo7.getUid()),
				outgoingReferences.get(modulePojo1.getUid()).stream().map(ModuleRelationshipPojo::getDstModule).collect(Collectors.toSet()));
		assertEquals(Set.of(RelationshipType.ARTIFICIAL),
				outgoingReferences.get(modulePojo1.getUid()).stream().map(ModuleRelationshipPojo::getRelationship).collect(Collectors.toSet()));
	}

	@Test
	void testFilterDependencyGraphOnlyByModuleTaxonomies() {
		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final ModuleService moduleService = mock(ModuleService.class);

		dependencyGraphFunctionalBlockPojo = createDataForDependencyGraphFiltering(functionalBlockService);

		mockSetupForDependencyGraph(moduleService, functionalBlockService);
		final EntityId taxonomyId = EntityId.of(UUID.randomUUID());
		when(moduleService.findModuleUids(any())).then(invocation -> {
			final BuildingConsumer<ModuleService.ModuleInquiryBuilder> builderConsumer = invocation.getArgument(0);
			final ModuleService.ModuleInquiryBuilder builder = Mockito.mock(ModuleService.ModuleInquiryBuilder.class);

			final ArgumentCaptor<Set<EntityId>> taxonomiesCaptor = ArgumentCaptor.forClass(Set.class);

			when(builder.ofProject(any())).thenReturn(builder);
			when(builder.withLinkHashes(any())).thenReturn(builder);
			when(builder.notWithTaxonomies(any())).thenReturn(builder);

			builderConsumer.prepare(builder);
			Mockito.verify(builder, atLeastOnce()).notWithTaxonomies(taxonomiesCaptor.capture());

			final List<UUID> returnValue;
			final Set<EntityId> taxonomiesList = Set.of(taxonomyId);
			if (taxonomiesCaptor.getValue().size() == taxonomiesList.size() && taxonomiesCaptor.getValue().containsAll(taxonomiesList)) {
				returnValue = List.of(modulePojo4.getUid());
			} else {
				returnValue = List.of();
			}
			return new ArrayList<>(returnValue);
		});


		final FunctionalBlockToDependencyGraphService service = new FunctionalBlockToDependencyGraphService(filterObjectServiceMock, functionalBlockService, filterObjectCoercionServiceMock, moduleService);
		final ReachabilityBlockGraphFilterRequest filterRequest =
				new ReachabilityBlockGraphFilterRequest(Collections.singleton(taxonomyId), null, null, null);
		final DependencyGraph dependencyGraph = service.toFunctionalBlockGraph(dependencyGraphFunctionalBlockPojo, filterRequest);

		assertEquals(6, dependencyGraph.getModules().size());
		assertEquals(1, dependencyGraph.getRootModuleIds().size());
		assertEquals(5, dependencyGraph.getReferences().size());
		assertEquals(3, dependencyGraph.getRelationshipTypes().size());
		assertEquals(3, dependencyGraph.getModuleTypes().size());
	}

	@Test
	void testFilterDependencyGraphByAllFilters() {
		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final ModuleService moduleService = mock(ModuleService.class);

		dependencyGraphFunctionalBlockPojo = createDataForDependencyGraphFiltering(functionalBlockService);

		mockSetupForDependencyGraph(moduleService, functionalBlockService);
		when(moduleService.findModuleUids(any())).then(invocation -> {
			final BuildingConsumer<ModuleService.ModuleInquiryBuilder> builderConsumer = invocation.getArgument(0);
			final ModuleService.ModuleInquiryBuilder builder = Mockito.mock(ModuleService.ModuleInquiryBuilder.class);

			final ArgumentCaptor<Collection<String>> linkHashCaptor = ArgumentCaptor.forClass(Collection.class);

			when(builder.ofProject(any())).thenReturn(builder);
			when(builder.withLinkHashes(any())).thenReturn(builder);
			when(builder.notWithTechnologiesAndTypes(anyCollection())).thenReturn(builder);

			builderConsumer.prepare(builder);
			Mockito.verify(builder, atLeastOnce()).withLinkHashes(linkHashCaptor.capture());

			final List<UUID> returnValue;
			final List<String> linkHashes = List.of("upLinkHash1", "upLinkHash2", "upLinkHash3", "upLinkHash4", "upLinkHash5", "upLinkHash6", "upLinkHash7");
			if (linkHashCaptor.getValue() != null && linkHashCaptor.getValue().size() == linkHashes.size()) {
				returnValue = List.of(modulePojo2.getUid(), modulePojo3.getUid(), modulePojo5.getUid(), modulePojo6.getUid());
			} else {
				returnValue = List.of();
			}
			return new ArrayList<>(returnValue);
		});

		final UUID fbUid1 = UUID.randomUUID();
		when(functionalBlockService.getResolvedModuleParts(Set.of(fbUid1)))
				.thenReturn(Map.of(fbUid1, List.of(new ResolvedModulePart(EntityId.of(modulePojo1.getUid())),
						new ResolvedModulePart(EntityId.of(modulePojo2.getUid())), new ResolvedModulePart(EntityId.of(modulePojo3.getUid())),
						new ResolvedModulePart(EntityId.of(modulePojo4.getUid())), new ResolvedModulePart(EntityId.of(modulePojo5.getUid())),
						new ResolvedModulePart(EntityId.of(modulePojo6.getUid())), new ResolvedModulePart(EntityId.of(modulePojo7.getUid())))));

		final FunctionalBlockToDependencyGraphService service = new FunctionalBlockToDependencyGraphService(filterObjectServiceMock, functionalBlockService,
				filterObjectCoercionServiceMock, moduleService);
		final ReachabilityBlockGraphFilterRequest filterRequest = new ReachabilityBlockGraphFilterRequest(Collections.singleton(EntityId.of(UUID.randomUUID())),
				Set.of(EntityId.of(fbUid1)), Set.of(new TechnologyType(Technology.JCL, Type.JOB),
				new TechnologyType(Technology.RESOURCE, Type.FILE)),
				Set.of(RelationshipType.CALLS, RelationshipType.INCLUDES, RelationshipType.REFERENCES, RelationshipType.ACCESSES));
		final DependencyGraph dependencyGraph = service.toFunctionalBlockGraph(dependencyGraphFunctionalBlockPojo, filterRequest);

		assertEquals(3, dependencyGraph.getModules().size());
		assertEquals(1, dependencyGraph.getRootModuleIds().size());
		assertEquals(2, dependencyGraph.getReferences().size());
		assertEquals(1, dependencyGraph.getRelationshipTypes().size());
		assertEquals(2, dependencyGraph.getModuleTypes().size());

		final Map<UUID, Set<ModuleRelationshipPojo>> outgoingReferences = dependencyGraph.getReferences().stream()
				.collect(Collectors.groupingBy(ModuleRelationshipPojo::getSrcModule, Collectors.toSet()));
		assertEquals(2, outgoingReferences.get(modulePojo1.getUid()).size());
		assertEquals(Set.of(modulePojo4.getUid(), modulePojo7.getUid()),
				outgoingReferences.get(modulePojo1.getUid()).stream().map(ModuleRelationshipPojo::getDstModule).collect(Collectors.toSet()));
		assertEquals(Set.of(RelationshipType.ARTIFICIAL),
				outgoingReferences.get(modulePojo1.getUid()).stream().map(ModuleRelationshipPojo::getRelationship).collect(Collectors.toSet()));
	}

	/*
	 * 	Dependency Graph should remain the same as filter should not be applied on the root modules
	 */
	@Test
	void testFilterDependencyGraphWhenRootModulesGetFiltered() {
		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final ModuleService moduleService = mock(ModuleService.class);

		dependencyGraphFunctionalBlockPojo = createDataForDependencyGraphFiltering(functionalBlockService);

		when(moduleService.findModuleUids(any(BuildingConsumer.class))).thenReturn(Collections.emptyList());
		mockSetupForDependencyGraph(moduleService, functionalBlockService);

		final UUID fbUid1 = UUID.randomUUID();
		when(functionalBlockService.getResolvedModuleParts(Set.of(fbUid1)))
				.thenReturn(Map.of(fbUid1, List.of(new ResolvedModulePart(EntityId.of(modulePojo2.getUid())),
						new ResolvedModulePart(EntityId.of(modulePojo3.getUid())), new ResolvedModulePart(EntityId.of(modulePojo4.getUid())),
						new ResolvedModulePart(EntityId.of(modulePojo5.getUid())), new ResolvedModulePart(EntityId.of(modulePojo6.getUid())),
						new ResolvedModulePart(EntityId.of(modulePojo7.getUid())))));

		final FunctionalBlockToDependencyGraphService service = new FunctionalBlockToDependencyGraphService(filterObjectServiceMock, functionalBlockService,
		 		filterObjectCoercionServiceMock, moduleService);
		final ReachabilityBlockGraphFilterRequest filterRequest = new ReachabilityBlockGraphFilterRequest(null, Set.of(EntityId.of(fbUid1)), null, null);
		final DependencyGraph dependencyGraph = service.toFunctionalBlockGraph(dependencyGraphFunctionalBlockPojo, filterRequest);

		assertEquals(7, dependencyGraph.getModules().size());
		assertEquals(1, dependencyGraph.getRootModuleIds().size());
		assertEquals(6, dependencyGraph.getReferences().size());
		assertEquals(4, dependencyGraph.getRelationshipTypes().size());
		assertEquals(3, dependencyGraph.getModuleTypes().size());
	}

	/*
	 * 	Dependency Graph should remain the same as all filters are empty or null
	 */
	@Test
	void testFilterDependencyGraphWhenAllFiltersAreEmptyOrNull() {
		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final ModuleService moduleService = mock(ModuleService.class);

		dependencyGraphFunctionalBlockPojo = createDataForDependencyGraphFiltering(functionalBlockService);

		mockSetupForDependencyGraph(moduleService, functionalBlockService);
		when(moduleService.findModuleUids(any(BuildingConsumer.class))).thenReturn(Collections.emptyList());

		final FunctionalBlockToDependencyGraphService service = new FunctionalBlockToDependencyGraphService(filterObjectServiceMock, functionalBlockService, filterObjectCoercionServiceMock, moduleService);
		final ReachabilityBlockGraphFilterRequest filterRequest = new ReachabilityBlockGraphFilterRequest(Collections.emptySet(), Collections.emptySet(),
				Collections.emptySet(), Collections.emptySet());
		final DependencyGraph dependencyGraph1 = service.toFunctionalBlockGraph(dependencyGraphFunctionalBlockPojo, filterRequest);

		assertEquals(7, dependencyGraph1.getModules().size());
		assertEquals(1, dependencyGraph1.getRootModuleIds().size());
		assertEquals(6, dependencyGraph1.getReferences().size());
		assertEquals(4, dependencyGraph1.getRelationshipTypes().size());
		assertEquals(3, dependencyGraph1.getModuleTypes().size());

		final DependencyGraph dependencyGraph2 = service.toFunctionalBlockGraph(dependencyGraphFunctionalBlockPojo,
				new ReachabilityBlockGraphFilterRequest(null, null, null, null));

		assertEquals(7, dependencyGraph2.getModules().size());
		assertEquals(1, dependencyGraph2.getRootModuleIds().size());
		assertEquals(6, dependencyGraph2.getReferences().size());
		assertEquals(4, dependencyGraph2.getRelationshipTypes().size());
		assertEquals(3, dependencyGraph2.getModuleTypes().size());
	}

	/*		Calls       Calls					 Ref		     Ref
	 *   |-------> mod2-------|                |---------> mod5------->mod7
	 *   |					  |			       |
	 * 	mod1                  ------>mod3------|
	 *   |					  |				   |
	 *   |-------->mod4-------|                |---------->mod6------->mod8
	 *		Calls       calls					 Ref			 Ref
	 */
	@Test
	void testDependencyGraphWhenSharedModuleIsFiltered() {
		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final ModuleService moduleService = mock(ModuleService.class);

		final FunctionalBlockPojo functionalBlockPojo = createDataForDependencyGraphFiltering(functionalBlockService);
		functionalBlockUid8 = UUID.randomUUID();

		modulePojo8 = ModulePojoDummy.build(
				new ModulePojoPrototype().setUid(UUID.randomUUID()).setNid(8L).setLinkHash("upLinkHash8").setTechnology(Technology.RESOURCE)
						.setType(Type.FILE));

		generatedFrom8 = GeneratedFrom.fromModule("upLinkHash8", "upContentHash8", "upDependencyHash8");

		final ModuleRelationshipPojo moduleRelationshipPojo1 = new ModuleRelationshipPojo(UUID.randomUUID(), modulePojo1.getUid(), null,
				modulePojo2.getUid(), null, RelationshipType.CALLS, null, null, null,
				null, Collections.emptyList(), null, null, null);
		final ModuleRelationshipPojo moduleRelationshipPojo2 = new ModuleRelationshipPojo(UUID.randomUUID(), modulePojo1.getUid(), null,
				modulePojo4.getUid(), null, RelationshipType.CALLS, null, null,
				null, null, Collections.emptyList(), null, null, null);
		final ModuleRelationshipPojo moduleRelationshipPojo3 = new ModuleRelationshipPojo(UUID.randomUUID(), modulePojo2.getUid(), null,
				modulePojo3.getUid(), null, RelationshipType.CALLS, null, null,null,
				null, Collections.emptyList(), null, null, null);
		final ModuleRelationshipPojo moduleRelationshipPojo4 = new ModuleRelationshipPojo(UUID.randomUUID(), modulePojo4.getUid(), null,
				modulePojo3.getUid(), null, RelationshipType.CALLS, null, null, null,
				null, Collections.emptyList(), null, null, null);
		final ModuleRelationshipPojo moduleRelationshipPojo5 = new ModuleRelationshipPojo(UUID.randomUUID(), modulePojo3.getUid(), null,
				modulePojo5.getUid(), null, RelationshipType.REFERENCES, null, null, null, null,
				Collections.emptyList(), null, null, null);
		final ModuleRelationshipPojo moduleRelationshipPojo6 = new ModuleRelationshipPojo(UUID.randomUUID(), modulePojo3.getUid(), null,
				modulePojo6.getUid(), null, RelationshipType.REFERENCES, null, null, null, null,
				Collections.emptyList(), null, null, null);
		final ModuleRelationshipPojo moduleRelationshipPojo7 = new ModuleRelationshipPojo(UUID.randomUUID(), modulePojo5.getUid(), null,
				modulePojo7.getUid(), null, RelationshipType.REFERENCES, null, null, null, null,
				Collections.emptyList(), null, null, null);
		final ModuleRelationshipPojo moduleRelationshipPojo8 = new ModuleRelationshipPojo(UUID.randomUUID(), modulePojo6.getUid(), null,
				modulePojo8.getUid(), null, RelationshipType.REFERENCES, null, null, null, null,
				Collections.emptyList(), null, null, null);

		when(functionalBlockService.getLinks(functionalBlockPojo.getUid())).thenReturn(List.of(
				new FunctionalBlockLink(UUID.randomUUID(), functionalBlockPojo.getUid(), functionalBlockUid1, functionalBlockUid2, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo1.getRelationship())),null),
				new FunctionalBlockLink(UUID.randomUUID(), functionalBlockPojo.getUid(), functionalBlockUid1, functionalBlockUid4, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo2.getRelationship())), null),
				new FunctionalBlockLink(UUID.randomUUID(), functionalBlockPojo.getUid(), functionalBlockUid2, functionalBlockUid3, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo3.getRelationship())), null),
				new FunctionalBlockLink(UUID.randomUUID(), functionalBlockPojo.getUid(), functionalBlockUid4, functionalBlockUid3, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo4.getRelationship())) , null),
				new FunctionalBlockLink(UUID.randomUUID(), functionalBlockPojo.getUid(), functionalBlockUid3, functionalBlockUid5, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo5.getRelationship())), null),
				new FunctionalBlockLink(UUID.randomUUID(), functionalBlockPojo.getUid(), functionalBlockUid3, functionalBlockUid6, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo6.getRelationship())), null),
				new FunctionalBlockLink(UUID.randomUUID(), functionalBlockPojo.getUid(), functionalBlockUid5, functionalBlockUid7, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo7.getRelationship())), null),
				new FunctionalBlockLink(UUID.randomUUID(), functionalBlockPojo.getUid(), functionalBlockUid6, functionalBlockUid8, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo8.getRelationship())), null)));

		mockSetupForDependencyGraph(moduleService, functionalBlockService);
		when(moduleService.findModuleUids(any(BuildingConsumer.class))).thenReturn(Collections.emptyList());

		final UUID fbUid1 = UUID.randomUUID();
		when(functionalBlockService.getResolvedModuleParts(Set.of(fbUid1)))
				.thenReturn(Map.of(fbUid1, List.of(new ResolvedModulePart(EntityId.of(modulePojo1.getUid())),
						new ResolvedModulePart(EntityId.of(modulePojo2.getUid())), new ResolvedModulePart(EntityId.of(modulePojo4.getUid())),
						new ResolvedModulePart(EntityId.of(modulePojo5.getUid())), new ResolvedModulePart(EntityId.of(modulePojo6.getUid())),
						new ResolvedModulePart(EntityId.of(modulePojo7.getUid())), new ResolvedModulePart(EntityId.of(modulePojo8.getUid())))));

		final FunctionalBlockToDependencyGraphService service = new FunctionalBlockToDependencyGraphService(filterObjectServiceMock, functionalBlockService,
				filterObjectCoercionServiceMock, moduleService);
		final ReachabilityBlockGraphFilterRequest filterRequest = new ReachabilityBlockGraphFilterRequest(null, Set.of(EntityId.of(fbUid1)), null, null);

		final DependencyGraph dependencyGraph = service.toFunctionalBlockGraph(functionalBlockPojo, filterRequest);

		assertEquals(7, dependencyGraph.getModules().size());
		assertEquals(1, dependencyGraph.getRootModuleIds().size());
		assertEquals(8, dependencyGraph.getReferences().size());
		assertEquals(3, dependencyGraph.getRelationshipTypes().size());
		assertEquals(3, dependencyGraph.getModuleTypes().size());

		final Map<UUID, Set<ModuleRelationshipPojo>> outgoingReferences = dependencyGraph.getReferences().stream()
				.collect(Collectors.groupingBy(ModuleRelationshipPojo::getSrcModule, Collectors.toSet()));
		assertEquals(2, outgoingReferences.get(modulePojo2.getUid()).size());
		assertEquals(Set.of(modulePojo5.getUid(), modulePojo6.getUid()),
				outgoingReferences.get(modulePojo2.getUid()).stream().map(ModuleRelationshipPojo::getDstModule).collect(Collectors.toSet()));
		assertEquals(Set.of(RelationshipType.ARTIFICIAL),
				outgoingReferences.get(modulePojo2.getUid()).stream().map(ModuleRelationshipPojo::getRelationship).collect(Collectors.toSet()));

		assertEquals(2, outgoingReferences.get(modulePojo4.getUid()).size());
		assertEquals(Set.of(modulePojo5.getUid(), modulePojo6.getUid()),
				outgoingReferences.get(modulePojo2.getUid()).stream().map(ModuleRelationshipPojo::getDstModule).collect(Collectors.toSet()));
		assertEquals(Set.of(RelationshipType.ARTIFICIAL),
				outgoingReferences.get(modulePojo4.getUid()).stream().map(ModuleRelationshipPojo::getRelationship).collect(Collectors.toSet()));
	}


	/*
	 *								Accesses
	 *            |-------------------------------------------------------------->m6<----+
	 * 			  |                        calls          Accesses                /\     |
	 *     calls  |    calls            |---------->m4--------------------+-------+      |Accesses
	 * 	m1 -----> m2------------------->m3                                | Accesses     |
	 *            |                     |----------->m5-------------------|-------+------|
	 *            |							calls   		Accesses	  |  	  \/
	 *            |-------------------------------------------------------+------>m7
	 *									Accesses
	 */
	@Test
	void testDependencyGraphWhenFilteredAreAppliedOnComplexGraph() {
		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final ModuleService moduleService = mock(ModuleService.class);

		final FunctionalBlockPojo functionalBlockPojo = createDataForDependencyGraphFiltering(functionalBlockService);

		final ModuleRelationshipPojo moduleRelationshipPojo1 = new ModuleRelationshipPojo(UUID.randomUUID(), modulePojo1.getUid(), null,
				modulePojo2.getUid(), null, RelationshipType.CALLS, null, null, null,
				null, Collections.emptyList(), null, null, null);
		final ModuleRelationshipPojo moduleRelationshipPojo2 = new ModuleRelationshipPojo(UUID.randomUUID(), modulePojo2.getUid(), null,
				modulePojo6.getUid(), null, RelationshipType.ACCESSES, null, null,
				null, null, Collections.emptyList(), null, null, null);
		final ModuleRelationshipPojo moduleRelationshipPojo3 = new ModuleRelationshipPojo(UUID.randomUUID(), modulePojo2.getUid(), null,
				modulePojo7.getUid(), null, RelationshipType.ACCESSES, null, null,null,
				null, Collections.emptyList(), null, null, null);
		final ModuleRelationshipPojo moduleRelationshipPojo4 = new ModuleRelationshipPojo(UUID.randomUUID(), modulePojo2.getUid(), null,
				modulePojo3.getUid(), null, RelationshipType.CALLS, null, null, null,
				null, Collections.emptyList(), null, null, null);
		final ModuleRelationshipPojo moduleRelationshipPojo5 = new ModuleRelationshipPojo(UUID.randomUUID(), modulePojo3.getUid(), null,
				modulePojo4.getUid(), null, RelationshipType.CALLS, null, null, null, null,
				Collections.emptyList(), null, null, null);
		final ModuleRelationshipPojo moduleRelationshipPojo6 = new ModuleRelationshipPojo(UUID.randomUUID(), modulePojo3.getUid(), null,
				modulePojo5.getUid(), null, RelationshipType.CALLS, null, null, null, null,
				Collections.emptyList(), null, null, null);
		final ModuleRelationshipPojo moduleRelationshipPojo7 = new ModuleRelationshipPojo(UUID.randomUUID(), modulePojo4.getUid(), null,
				modulePojo6.getUid(), null, RelationshipType.ACCESSES, null, null, null, null,
				Collections.emptyList(), null, null, null);
		final ModuleRelationshipPojo moduleRelationshipPojo8 = new ModuleRelationshipPojo(UUID.randomUUID(), modulePojo4.getUid(), null,
				modulePojo7.getUid(), null, RelationshipType.ACCESSES, null, null, null, null,
				Collections.emptyList(), null, null, null);
		final ModuleRelationshipPojo moduleRelationshipPojo9 = new ModuleRelationshipPojo(UUID.randomUUID(), modulePojo5.getUid(), null,
				modulePojo6.getUid(), null, RelationshipType.ACCESSES, null, null, null, null,
				Collections.emptyList(), null, null, null);
		final ModuleRelationshipPojo moduleRelationshipPojo10 = new ModuleRelationshipPojo(UUID.randomUUID(), modulePojo5.getUid(), null,
				modulePojo7.getUid(), null, RelationshipType.ACCESSES, null, null, null, null,
				Collections.emptyList(), null, null, null);

		when(functionalBlockService.getLinks(functionalBlockPojo.getUid())).thenReturn(List.of(
				new FunctionalBlockLink(UUID.randomUUID(), functionalBlockPojo.getUid(), functionalBlockUid1, functionalBlockUid2, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo1.getRelationship())),null),
				new FunctionalBlockLink(UUID.randomUUID(), functionalBlockPojo.getUid(), functionalBlockUid2, functionalBlockUid6, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo2.getRelationship())), null),
				new FunctionalBlockLink(UUID.randomUUID(), functionalBlockPojo.getUid(), functionalBlockUid2, functionalBlockUid7, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo3.getRelationship())), null),
				new FunctionalBlockLink(UUID.randomUUID(), functionalBlockPojo.getUid(), functionalBlockUid2, functionalBlockUid3, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo4.getRelationship())), null),
				new FunctionalBlockLink(UUID.randomUUID(), functionalBlockPojo.getUid(), functionalBlockUid3, functionalBlockUid4, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo5.getRelationship())), null),
				new FunctionalBlockLink(UUID.randomUUID(), functionalBlockPojo.getUid(), functionalBlockUid3, functionalBlockUid5, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo6.getRelationship())), null),
				new FunctionalBlockLink(UUID.randomUUID(), functionalBlockPojo.getUid(), functionalBlockUid4, functionalBlockUid6, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo7.getRelationship())), null),
				new FunctionalBlockLink(UUID.randomUUID(), functionalBlockPojo.getUid(), functionalBlockUid4, functionalBlockUid7, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo8.getRelationship())), null),
				new FunctionalBlockLink(UUID.randomUUID(), functionalBlockPojo.getUid(), functionalBlockUid5, functionalBlockUid6, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo9.getRelationship())), null),
				new FunctionalBlockLink(UUID.randomUUID(), functionalBlockPojo.getUid(), functionalBlockUid5, functionalBlockUid7, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo10.getRelationship())), null)
		));
		mockSetupForDependencyGraph(moduleService, functionalBlockService);
		final EntityId taxonomyId = EntityId.of(UUID.randomUUID());
		when(moduleService.findModuleUids(any())).then(invocation -> {
			final BuildingConsumer<ModuleService.ModuleInquiryBuilder> builderConsumer = invocation.getArgument(0);
			final ModuleService.ModuleInquiryBuilder builder = Mockito.mock(ModuleService.ModuleInquiryBuilder.class);

			final ArgumentCaptor<Set<EntityId>> taxonomiesCaptor = ArgumentCaptor.forClass(Set.class);

			when(builder.ofProject(any())).thenReturn(builder);
			when(builder.withLinkHashes(any())).thenReturn(builder);
			when(builder.notWithTaxonomies(any())).thenReturn(builder);

			builderConsumer.prepare(builder);
			Mockito.verify(builder, atLeastOnce()).notWithTaxonomies(taxonomiesCaptor.capture());

			final List<UUID> returnValue;
			final Set<EntityId> taxonomiesList = Set.of(taxonomyId);
			if (taxonomiesCaptor.getValue().size() == taxonomiesList.size() && taxonomiesCaptor.getValue().containsAll(taxonomiesList)) {
				returnValue = List.of(modulePojo2.getUid(), modulePojo3.getUid(), modulePojo4.getUid(), modulePojo5.getUid());
			} else {
				returnValue = List.of();
			}
			return new ArrayList<>(returnValue);
		});

		final FunctionalBlockToDependencyGraphService service = new FunctionalBlockToDependencyGraphService(filterObjectServiceMock, functionalBlockService, filterObjectCoercionServiceMock, moduleService);
		final ReachabilityBlockGraphFilterRequest filterRequest = new ReachabilityBlockGraphFilterRequest(Collections.singleton(taxonomyId),
				null, null, null);

		final DependencyGraph dependencyGraph = service.toFunctionalBlockGraph(functionalBlockPojo, filterRequest);

		assertEquals(3, dependencyGraph.getModules().size());
		assertEquals(1, dependencyGraph.getRootModuleIds().size());
		assertEquals(2, dependencyGraph.getReferences().size());
		assertEquals(1, dependencyGraph.getRelationshipTypes().size());
		assertEquals(3, dependencyGraph.getModuleTypes().size());

		final Map<UUID, Set<ModuleRelationshipPojo>> outgoingReferences = dependencyGraph.getReferences().stream()
				.collect(Collectors.groupingBy(ModuleRelationshipPojo::getSrcModule, Collectors.toSet()));
		assertEquals(2, outgoingReferences.get(modulePojo1.getUid()).size());
		assertEquals(Set.of(modulePojo6.getUid(), modulePojo7.getUid()),
				outgoingReferences.get(modulePojo1.getUid()).stream().map(ModuleRelationshipPojo::getDstModule).collect(Collectors.toSet()));
		assertEquals(Set.of(RelationshipType.ARTIFICIAL),
				outgoingReferences.get(modulePojo1.getUid()).stream().map(ModuleRelationshipPojo::getRelationship).collect(Collectors.toSet()));
	}

	/*			       calls           calls            References
	 *        Fb1(m1) --------->Fb2(m2)---------->Fb3(m3) ---------> Fb4(m4)
	 *		   		    		|                  ^
	 * 							|Access            |
	 * 				    		|                  |Includes
	 * 						    \/                 |
	 * 							Fb5(m5)---------->Fb6(m6) ---------> Fb7(m7)
	 * 						    		Includes		    Includes
	 *
	 */
	@Test
	void testFilterDependencyGraphHavingMultipleIncomingEdges() {
		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final ModuleService moduleService = mock(ModuleService.class);

		dependencyGraphFunctionalBlockPojo = createDataForDependencyGraphFiltering(functionalBlockService);
		final ModuleRelationshipPojo moduleRelationshipPojo7 = new ModuleRelationshipPojo(UUID.randomUUID(), modulePojo6.getUid(), null,
				modulePojo3.getUid(), null, RelationshipType.INCLUDES, null, null, null, null,
				Collections.emptyList(), null, null, null);

		when(functionalBlockService.getLinks(dependencyGraphFunctionalBlockPojo.getUid())).thenReturn(List.of(
				new FunctionalBlockLink(UUID.randomUUID(), dependencyGraphFunctionalBlockPojo.getUid(), functionalBlockUid1, functionalBlockUid2, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo1.getRelationship())),null),
				new FunctionalBlockLink(UUID.randomUUID(), dependencyGraphFunctionalBlockPojo.getUid(), functionalBlockUid2, functionalBlockUid3, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo2.getRelationship())), null),
				new FunctionalBlockLink(UUID.randomUUID(), dependencyGraphFunctionalBlockPojo.getUid(), functionalBlockUid3, functionalBlockUid4, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo3.getRelationship())), null),
				new FunctionalBlockLink(UUID.randomUUID(), dependencyGraphFunctionalBlockPojo.getUid(), functionalBlockUid2, functionalBlockUid5, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo4.getRelationship())) , null),
				new FunctionalBlockLink(UUID.randomUUID(), dependencyGraphFunctionalBlockPojo.getUid(), functionalBlockUid5, functionalBlockUid6, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo5.getRelationship())), null),
				new FunctionalBlockLink(UUID.randomUUID(), dependencyGraphFunctionalBlockPojo.getUid(), functionalBlockUid6, functionalBlockUid7, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo6.getRelationship())), null),
				new FunctionalBlockLink(UUID.randomUUID(), dependencyGraphFunctionalBlockPojo.getUid(), functionalBlockUid6, functionalBlockUid3, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo7.getRelationship())), null)));
		when(moduleService.findModuleUids(any(BuildingConsumer.class))).thenReturn(Collections.emptyList());
		mockSetupForDependencyGraph(moduleService, functionalBlockService);

		final FunctionalBlockToDependencyGraphService service = new FunctionalBlockToDependencyGraphService(filterObjectServiceMock, functionalBlockService, filterObjectCoercionServiceMock, moduleService);
		final ReachabilityBlockGraphFilterRequest filterRequest = new ReachabilityBlockGraphFilterRequest(null, null, null,
				Set.of(RelationshipType.INCLUDES, RelationshipType.REFERENCES, RelationshipType.ACCESSES));
		final DependencyGraph dependencyGraph = service.toFunctionalBlockGraph(dependencyGraphFunctionalBlockPojo, filterRequest);

		assertEquals(6, dependencyGraph.getModules().size());
		assertEquals(1, dependencyGraph.getRootModuleIds().size());
		assertEquals(5, dependencyGraph.getReferences().size());
		assertEquals(3, dependencyGraph.getRelationshipTypes().size());
		assertEquals(3, dependencyGraph.getModuleTypes().size());

		final Map<UUID, Set<ModuleRelationshipPojo>> outgoingReferences = dependencyGraph.getReferences().stream()
				.collect(Collectors.groupingBy(ModuleRelationshipPojo::getSrcModule, Collectors.toSet()));
		assertEquals(1, outgoingReferences.get(modulePojo1.getUid()).size());
		assertEquals(Set.of(modulePojo5.getUid()),
				outgoingReferences.get(modulePojo1.getUid()).stream().map(ModuleRelationshipPojo::getDstModule).collect(Collectors.toSet()));
		assertEquals(Set.of(RelationshipType.ARTIFICIAL),
				outgoingReferences.get(modulePojo1.getUid()).stream().map(ModuleRelationshipPojo::getRelationship).collect(Collectors.toSet()));
	}

	private static FunctionalBlockPojo createFunctionalBlock(final UUID uid, final String name, final List<UUID> parents, final List<UUID> children,
			final Map<String, Object> flags) {
		return new FunctionalBlockPojo(uid, null, EntityId.of(UUID.randomUUID(), PROJECT_ID), Collections.emptyList(), parents, children,
				name, "", flags, Instant.now());
	}

	private FunctionalBlockPojo createDataForDependencyGraphFiltering(final FunctionalBlockService functionalBlockService) {
		functionalBlockUid1 = UUID.randomUUID();
		functionalBlockUid2 = UUID.randomUUID();
		functionalBlockUid3 = UUID.randomUUID();
		functionalBlockUid4 = UUID.randomUUID();
		functionalBlockUid5 = UUID.randomUUID();
		functionalBlockUid6 = UUID.randomUUID();
		functionalBlockUid7 = UUID.randomUUID();

		generatedFrom1 = GeneratedFrom.fromModule("upLinkHash1", "upContentHash1", "dependencyHash1");
		generatedFrom2 = GeneratedFrom.fromModule("upLinkHash2", "upContentHash2", "dependencyHash2");
		generatedFrom3 = GeneratedFrom.fromModule("upLinkHash3", "upContentHash3", "dependencyHash3");
		generatedFrom4 = GeneratedFrom.fromModule("upLinkHash4", "upContentHash4", "dependencyHash4");
		generatedFrom5 = GeneratedFrom.fromModule("upLinkHash5", "upContentHash5", "dependencyHash5");
		generatedFrom6 = GeneratedFrom.fromModule("upLinkHash6", "upContentHash6", "dependencyHash6");
		generatedFrom7 = GeneratedFrom.fromModule("upLinkHash7", "upContentHash7", "dependencyHash7");

		modulePojo1 = ModulePojoDummy.build(
				new ModulePojoPrototype().setUid(UUID.randomUUID()).setNid(5L).setLinkHash("upLinkHash1").setTechnology(Technology.JCL).setType(Type.JOB));
		modulePojo2 = ModulePojoDummy.build(
				new ModulePojoPrototype().setUid(UUID.randomUUID()).setNid(6L).setLinkHash("upLinkHash2").setTechnology(Technology.JCL).setType(Type.EXEC_PGM));
		modulePojo3 = ModulePojoDummy.build(
				new ModulePojoPrototype().setUid(UUID.randomUUID()).setNid(7L).setLinkHash("upLinkHash3").setTechnology(Technology.JCL).setType(Type.EXEC_PGM));
		modulePojo4 = ModulePojoDummy.build(
				new ModulePojoPrototype().setUid(UUID.randomUUID()).setNid(8L).setLinkHash("upLinkHash4")
						.setTechnology(Technology.RESOURCE).setType(Type.FILE));
		modulePojo5 = ModulePojoDummy.build(
				new ModulePojoPrototype().setUid(UUID.randomUUID()).setNid(9L).setLinkHash("upLinkHash5").setTechnology(Technology.JCL).setType(Type.EXEC_PGM));
		modulePojo6 = ModulePojoDummy.build(
				new ModulePojoPrototype().setUid(UUID.randomUUID()).setNid(10L).setLinkHash("upLinkHash6").setTechnology(Technology.JCL)
						.setType(Type.EXEC_PGM));
		modulePojo7 = ModulePojoDummy.build(
				new ModulePojoPrototype().setUid(UUID.randomUUID()).setNid(11L).setLinkHash("upLinkHash7").setTechnology(Technology.RESOURCE)
						.setType(Type.FILE));

		moduleRelationshipPojo1 = new ModuleRelationshipPojo(UUID.randomUUID(), modulePojo1.getUid(), null,
				modulePojo2.getUid(), null, RelationshipType.CALLS, null, null, null,
				null, Collections.emptyList(), null, null, null);
		moduleRelationshipPojo2 = new ModuleRelationshipPojo(UUID.randomUUID(), modulePojo2.getUid(), null,
				modulePojo3.getUid(), null, RelationshipType.CALLS, null, null,
				null, null, Collections.emptyList(), null, null, null);
		moduleRelationshipPojo3 = new ModuleRelationshipPojo(UUID.randomUUID(), modulePojo3.getUid(), null,
				modulePojo4.getUid(), null, RelationshipType.REFERENCES, null, null,null,
				null, Collections.emptyList(), null, null, null);
		moduleRelationshipPojo4 = new ModuleRelationshipPojo(UUID.randomUUID(), modulePojo2.getUid(), null,
				modulePojo5.getUid(), null, RelationshipType.ACCESSES, null, null, null,
				null, Collections.emptyList(), null, null, null);
		moduleRelationshipPojo5 = new ModuleRelationshipPojo(UUID.randomUUID(), modulePojo5.getUid(), null,
				modulePojo6.getUid(), null, RelationshipType.INCLUDES, null, null, null, null,
				Collections.emptyList(), null, null, null);
		moduleRelationshipPojo6 = new ModuleRelationshipPojo(UUID.randomUUID(), modulePojo6.getUid(), null,
				modulePojo7.getUid(), null, RelationshipType.INCLUDES, null, null, null, null,
				Collections.emptyList(), null, null, null);

		final FunctionalBlockPojo functionalBlockPojo = new FunctionalBlockPojo(UUID.randomUUID(), null, EntityId.of(PROJECT_ID),
				List.of(), null, List.of(), "Dependency Graph", null, Map.of(TYPE.name(), List.of(FunctionalBlockType.CALL_CHAIN.name())), null);
		when(functionalBlockService.getLinks(functionalBlockPojo.getUid())).thenReturn(List.of(
				new FunctionalBlockLink(UUID.randomUUID(), functionalBlockPojo.getUid(), functionalBlockUid1, functionalBlockUid2, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo1.getRelationship())),null),
				new FunctionalBlockLink(UUID.randomUUID(), functionalBlockPojo.getUid(), functionalBlockUid2, functionalBlockUid3, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo2.getRelationship())), null),
				new FunctionalBlockLink(UUID.randomUUID(), functionalBlockPojo.getUid(), functionalBlockUid3, functionalBlockUid4, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo3.getRelationship())), null),
				new FunctionalBlockLink(UUID.randomUUID(), functionalBlockPojo.getUid(), functionalBlockUid2, functionalBlockUid5, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo4.getRelationship())) , null),
				new FunctionalBlockLink(UUID.randomUUID(), functionalBlockPojo.getUid(), functionalBlockUid5, functionalBlockUid6, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo5.getRelationship())), null),
				new FunctionalBlockLink(UUID.randomUUID(), functionalBlockPojo.getUid(), functionalBlockUid6, functionalBlockUid7, null,
						Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(moduleRelationshipPojo6.getRelationship())), null)
		));

		return functionalBlockPojo;
	}

	private void mockSetupForDependencyGraph(final ModuleService moduleService, final FunctionalBlockService functionalBlockService) {
		when(moduleService.findModules(any())).then(invocation -> {
			final BuildingConsumer<ModuleService.ModuleInquiryBuilder> builderConsumer = invocation.getArgument(0);
			final ModuleService.ModuleInquiryBuilder builder = Mockito.mock(ModuleService.ModuleInquiryBuilder.class);

			final ArgumentCaptor<List<String>> linkHashCaptor = ArgumentCaptor.forClass(List.class);

			when(builder.ofProject(any())).thenReturn(builder);
			when(builder.withLinkHashes(any())).thenReturn(builder);

			builderConsumer.prepare(builder);
			Mockito.verify(builder, atLeastOnce()).withLinkHashes(linkHashCaptor.capture());

			final List<ModulePojo> returnValue;
			final List<String> linkHashes1 = List.of("upLinkHash1", "upLinkHash2", "upLinkHash3", "upLinkHash4", "upLinkHash5", "upLinkHash6", "upLinkHash7");
			final List<String> linkHashes2 = List.of("upLinkHash1", "upLinkHash2", "upLinkHash3",
					"upLinkHash4", "upLinkHash5", "upLinkHash6", "upLinkHash7", "upLinkHash8");
			if (linkHashCaptor.getValue().size() == linkHashes1.size() && linkHashCaptor.getValue().containsAll(linkHashes1)) {
				returnValue = List.of(modulePojo1, modulePojo2, modulePojo3, modulePojo4, modulePojo5, modulePojo6, modulePojo7);
			} else if (linkHashCaptor.getValue().size() == linkHashes2.size() && linkHashCaptor.getValue().containsAll(linkHashes2)) {
				returnValue = List.of(modulePojo1, modulePojo2, modulePojo3, modulePojo4, modulePojo5, modulePojo6, modulePojo7, modulePojo8);
			} else {
				returnValue = List.of();
			}
			return new ArrayList<>(returnValue);
		});
		when(functionalBlockService.getGeneratedFrom(anyCollection())).then(invocation -> {
			final Collection<UUID> argument = invocation.getArgument(0);
			if (argument.size() == 7 && argument.containsAll(List.of(functionalBlockUid1, functionalBlockUid2, functionalBlockUid3, functionalBlockUid4,
					functionalBlockUid5, functionalBlockUid6, functionalBlockUid7))) {
				return Map.of(functionalBlockUid1, generatedFrom1, functionalBlockUid2, generatedFrom2, functionalBlockUid3, generatedFrom3,
						functionalBlockUid4, generatedFrom4, functionalBlockUid5, generatedFrom5, functionalBlockUid6, generatedFrom6,
						functionalBlockUid7, generatedFrom7);
			} else if(argument.size() == 8 && argument.containsAll(List.of(functionalBlockUid1, functionalBlockUid2, functionalBlockUid3, functionalBlockUid4,
					functionalBlockUid5, functionalBlockUid6, functionalBlockUid7, functionalBlockUid8))) {
				return Map.of(functionalBlockUid1, generatedFrom1, functionalBlockUid2, generatedFrom2, functionalBlockUid3, generatedFrom3,
						functionalBlockUid4, generatedFrom4, functionalBlockUid5, generatedFrom5, functionalBlockUid6, generatedFrom6,
						functionalBlockUid7, generatedFrom7, functionalBlockUid8, generatedFrom8);
			}
			else {
				return Collections.emptyMap();
			}
		});

		final List<UUID> functionalBlockUids = Arrays.asList(
				functionalBlockUid1,
				functionalBlockUid2,
				functionalBlockUid3,
				functionalBlockUid4,
				functionalBlockUid5,
				functionalBlockUid6,
				functionalBlockUid7,
				functionalBlockUid8
		);
		final List<FunctionalBlockPojo> functionalBlocks = new ArrayList<>();
		functionalBlockUids.forEach(fbUid -> {
			if (fbUid != null) {
				functionalBlocks.add(createFunctionalBlock(fbUid, "Fb" + fbUid.toString(), List.of(), List.of(),
						Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.MODULE.name()))));
			}
		});
		when(functionalBlockService.get(any())).thenReturn(functionalBlocks);
	}
}

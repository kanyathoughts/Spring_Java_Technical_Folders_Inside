/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.generation.ReachabilityDataGeneration;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import innowake.mining.extensions.export.callchain.CallChainService;
import innowake.mining.extensions.export.callchain.model.CallChain.CallChainDirection;
import innowake.mining.extensions.export.callchain.model.CallChainGraph;
import innowake.mining.extensions.export.callchain.model.CallChainGraph.CallChainEdge;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationContext;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationResult;
import innowake.mining.server.functionalblocks.generation.reachabilityanalysis.ReachabilityBottomUpGeneration;
import innowake.mining.server.functionalblocks.service.FunctionalBlockGenerationService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.testing.ModuleLightweightPojoDummy;
import innowake.mining.shared.entities.testing.ModulePojoDummy;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Mocked Test for ReachabilityBottomUpGeneration
 */
@Tag("mocked")
class ReachabilityBottomUpGenerationTest {

	private static final Long PROJECT_ID = 1L;
	private static FunctionalBlockPojo startModuleFunctionalBlock;
	private static FunctionalBlockPojo functionalBlock1;
	private static FunctionalBlockPojo functionalBlock2;
	private static FunctionalBlockPojo functionalBlock3;
	private static FunctionalBlockService functionalBlockService;
	private static FunctionalBlockGenerationService functionalBlockGenerationService;
	private static CallChainService callChainService;
	private static ReachabilityBottomUpGeneration reachabilityBottomUpGeneration;
	private static ModulePojo startModule;
	private static ModulePojo accessModule1;
	private static ModulePojo accessModule2;
	private static ModulePojo accessModule3;
	private static CallChainGraph callChainGraphWithUpperModule;
	private static CallChainGraph callChainGraphWithNoUpperModule;

	@BeforeAll
	static void init() {
		final UUID functionalblockUID = UUID.randomUUID();
		final UUID functionalblockUID1 = UUID.randomUUID();
		final UUID functionalblockUID2 = UUID.randomUUID();
		final UUID functionalblockUID3 = UUID.randomUUID();
		final UUID projectUUid = UUID.randomUUID();

		startModule = ModulePojoDummy.build(new ModulePojoPrototype().setLinkHash("LinkHashStartModule1").setNid(10L));
		accessModule1 = ModulePojoDummy.build(new ModulePojoPrototype().setLinkHash("LinkHashaccessModule1").setNid(11L));
		accessModule2 = ModulePojoDummy.build(new ModulePojoPrototype().setLinkHash("LinkHashaccessModule2").setNid(12L));
		accessModule3 = ModulePojoDummy.build(new ModulePojoPrototype().setLinkHash("LinkHashaccessModule3").setNid(13L));
		final ModuleLightweightPojo startModuleLightweight = ModuleLightweightPojoDummy.build(new ModulePojoPrototype()
				.setNid(1291L)
				.setName("startModuleLightWeight")
				.setTechnology(Technology.RESOURCE)
				.setType(Type.FILE)
				.setLinkHash(startModule.getLinkHash()));
		final ModuleLightweightPojo moduleLightweight1 = ModuleLightweightPojoDummy.build(new ModulePojoPrototype()
				.setNid(817L)
				.setName("moduleLightWeight1")
				.setTechnology(Technology.RESOURCE)
				.setType(Type.FILE)
				.setLinkHash(accessModule1.getLinkHash()));
		final ModuleLightweightPojo moduleLightweight2 = ModuleLightweightPojoDummy.build(new ModulePojoPrototype()
				.setNid(753L)
				.setName("moduleLightWeight2")
				.setTechnology(Technology.RESOURCE)
				.setType(Type.FILE)
				.setLinkHash(accessModule2.getLinkHash()));
		final ModuleLightweightPojo moduleLightweight3 = ModuleLightweightPojoDummy.build(new ModulePojoPrototype()
				.setNid(634L)
				.setName("moduleLightWeight3")
				.setTechnology(Technology.JCL)
				.setType(Type.JOB)
				.setLinkHash(accessModule3.getLinkHash()));

		startModuleFunctionalBlock = new FunctionalBlockPojo(functionalblockUID, null, EntityId.of(projectUUid, PROJECT_ID), Collections.emptyList(), null,
				Collections.emptyList(), "LowerFunctionalBlock", null, null, null);
		functionalBlock1 = new FunctionalBlockPojo(functionalblockUID1, null, EntityId.of(projectUUid, PROJECT_ID), Collections.emptyList(), null,
				Collections.emptyList(), "ModuleBlock1", null, null, null);
		functionalBlock2 = new FunctionalBlockPojo(functionalblockUID2, null, EntityId.of(projectUUid, PROJECT_ID), Collections.emptyList(), null,
				Collections.emptyList(), "ModuleBlock2", null, null, null);
		functionalBlock3 = new FunctionalBlockPojo(functionalblockUID3, null, EntityId.of(projectUUid, PROJECT_ID), Collections.emptyList(), null,
				Collections.emptyList(), "ModuleBlock3", null, null, null);

		final CallChainEdge callChainEdge1 = new CallChainEdge("callChainEdge1", moduleLightweight2, RelationshipType.CALLS, Collections.emptyMap());
		final CallChainEdge callChainEdge2 = new CallChainEdge("callChainEdge2", moduleLightweight3, RelationshipType.ACCESSES,
				Map.of(RelationshipType.ACCESSES.toString(), "READS"));

		callChainGraphWithUpperModule = new CallChainGraph(CallChainDirection.IN, startModuleLightweight);
		callChainGraphWithUpperModule.add(moduleLightweight1, callChainEdge2);
		callChainGraphWithUpperModule.add(moduleLightweight2, callChainEdge2);
		callChainGraphWithUpperModule.add(startModuleLightweight, callChainEdge2);
		callChainGraphWithUpperModule.add(startModuleLightweight, callChainEdge1);

		callChainGraphWithNoUpperModule = new CallChainGraph(CallChainDirection.IN, startModuleLightweight);
		callChainGraphWithNoUpperModule.add(moduleLightweight1, callChainEdge2);
		callChainGraphWithNoUpperModule.add(moduleLightweight2, callChainEdge2);
		callChainGraphWithNoUpperModule.add(moduleLightweight3, callChainEdge1);
		callChainGraphWithNoUpperModule.add(startModuleLightweight, callChainEdge2);
		callChainGraphWithNoUpperModule.add(startModuleLightweight, callChainEdge1);
	}

	@Test
	void testWhenStartModuleIdIsNull() {
		functionalBlockService = mock(FunctionalBlockService.class);
		callChainService = mock(CallChainService.class);
		reachabilityBottomUpGeneration = new ReachabilityBottomUpGeneration(functionalBlockService, callChainService);
		assertTrue(reachabilityBottomUpGeneration.generate(new FunctionalBlockGenerationContext(EntityId.of(PROJECT_ID)), null).isEmpty());
		/* Verify that the createCallChainGraphs method was never called with any instance of callChainService */
		verify(callChainService, never()).createCallChainGraphs(any(), any());
	}

	@Test
	void testWhenStartModuleIdIsNotNullAndWhenCallChainGraphIsEmpty() {
		functionalBlockService = mock(FunctionalBlockService.class);
		callChainService = mock(CallChainService.class);
		reachabilityBottomUpGeneration = new ReachabilityBottomUpGeneration(functionalBlockService, callChainService);
		assertTrue(reachabilityBottomUpGeneration.generate(
				new FunctionalBlockGenerationContext(EntityId.of(PROJECT_ID)), EntityId.of(startModule.getId())).isEmpty());

		/* Verify that the createCallChainGraphs method was once called with any instance of callChainService */
		verify(callChainService, times(1)).createCallChainGraphs(any(), any());
	}

	/*
	 * Illustration for Reachability Blocks Containing UpperBound
	 *
	 * accessModule3(Upper Bound)-------------------------------------------------------------------+
	 *   (functionalBlock3)		|																	|
	 *							|								 									|---------->BottomModule(Lower Bound)
	 *							|																	|			(StartModuleFunctionalBlock)
	 *							+------------------------->accessModule2(Access Bound)--------------+			(StartModule for Bottom Up Approach)
	 *														(functionalBlock2)
	 *
	 */
	@Test
	void testReachabilityBlockHavingUpperBound() {
		functionalBlockService = mock(FunctionalBlockService.class);
		callChainService = mock(CallChainService.class);
		functionalBlockGenerationService = mock(FunctionalBlockGenerationService.class);
		reachabilityBottomUpGeneration = new ReachabilityBottomUpGeneration(functionalBlockService, callChainService);
		reachabilityBottomUpGeneration.setFunctionalBlockGenerationService(functionalBlockGenerationService);
		when(callChainService.createCallChainGraphs(any(), any())).thenReturn(Optional.of(Collections.singletonList(callChainGraphWithUpperModule)));
		when(functionalBlockService.findGeneratedFromModules(any(), any())).thenReturn(Map.of(startModule.getLinkHash(), startModuleFunctionalBlock.getUid(),
				accessModule1.getLinkHash(), functionalBlock1.getUid(), accessModule2.getLinkHash(), functionalBlock2.getUid(), accessModule3.getLinkHash(),
				functionalBlock3.getUid()));
		final List<FunctionalBlockPojoPrototype> functionalBlockPojoPrototypeList =
				reachabilityBottomUpGeneration.generate(new FunctionalBlockGenerationContext(EntityId.of(PROJECT_ID)), EntityId.of(startModule.getId()))
				.stream().map(FunctionalBlockGenerationResult::getFunctionalBlock).collect(Collectors.toList());
		final Map<List<String>, Set<UUID>> functionalBlockMap = extractFunctionalBlockMap(functionalBlockPojoPrototypeList);
		assertEquals(1, functionalBlockMap.get(Arrays.asList(FunctionalBlockType.RA_LOWER_BOUND.toString())).size());
		assertEquals(Set.of(startModuleFunctionalBlock.getUid()), functionalBlockMap.get(Arrays.asList(FunctionalBlockType.RA_LOWER_BOUND.toString())));
		assertEquals(1, functionalBlockMap.get(Arrays.asList(FunctionalBlockType.RA_UPPER_BOUND.toString())).size());
		assertEquals(Set.of(functionalBlock3.getUid()), functionalBlockMap.get(Arrays.asList(FunctionalBlockType.RA_UPPER_BOUND.toString())));
		assertEquals(2, functionalBlockMap.get(Arrays.asList(FunctionalBlockType.RA_ACCESS_MODULE.toString())).size());
		assertEquals(Set.of(functionalBlock2.getUid(), functionalBlock3.getUid()),
				functionalBlockMap.get(Arrays.asList(FunctionalBlockType.RA_ACCESS_MODULE.toString())));
	}

	/*
	 * Illustration for Reachability Blocks Containing No Upper Bound
	 *
	 * accessModule2(Access Bound)-------------------------------------------------------------------+
	 *   (functionalBlock2)																			|
	 *															 									|---------->BottomModule(Lower Bound)
	 *																								|			(StartModuleFunctionalBlock)
	 *	accessModule3(Access Bound)-----------------------------------------------------------------+			(StartModule for Bottom Up Approach)
	 *	(functionalBlock3)
	 */
	@Test
	void testReachabilityBlockHavingNoUpperBound() {
		functionalBlockService = mock(FunctionalBlockService.class);
		callChainService = mock(CallChainService.class);
		functionalBlockGenerationService = mock(FunctionalBlockGenerationService.class);
		reachabilityBottomUpGeneration = new ReachabilityBottomUpGeneration(functionalBlockService, callChainService);
		reachabilityBottomUpGeneration.setFunctionalBlockGenerationService(functionalBlockGenerationService);
		when(callChainService.createCallChainGraphs(any(), any())).thenReturn(Optional.of(Collections.singletonList(callChainGraphWithNoUpperModule)));
		when(functionalBlockService.findGeneratedFromModules(any(), any())).thenReturn(Map.of(startModule.getLinkHash(), startModuleFunctionalBlock.getUid(),
				accessModule1.getLinkHash(), functionalBlock1.getUid(), accessModule2.getLinkHash(), functionalBlock2.getUid(), accessModule3.getLinkHash(),
				functionalBlock3.getUid()));
		final List<FunctionalBlockPojoPrototype> functionalBlockPojoPrototypeList =
				reachabilityBottomUpGeneration.generate(new FunctionalBlockGenerationContext(EntityId.of(PROJECT_ID)), EntityId.of(startModule.getId()))
				.stream().map(FunctionalBlockGenerationResult::getFunctionalBlock).collect(Collectors.toList());
		final Map<List<String>, Set<UUID>> functionalBlockMap = extractFunctionalBlockMap(functionalBlockPojoPrototypeList);
		assertEquals(1, functionalBlockMap.get(Arrays.asList(FunctionalBlockType.RA_LOWER_BOUND.toString())).size());
		assertEquals(Set.of(startModuleFunctionalBlock.getUid()), functionalBlockMap.get(Arrays.asList(FunctionalBlockType.RA_LOWER_BOUND.toString())));
		assertFalse(functionalBlockMap.containsKey(Arrays.asList(FunctionalBlockType.RA_UPPER_BOUND.toString())));
		assertEquals(2, functionalBlockMap.get(Arrays.asList(FunctionalBlockType.RA_ACCESS_MODULE.toString())).size());
		assertEquals(Set.of(functionalBlock2.getUid(), functionalBlock3.getUid()),
				functionalBlockMap.get(Arrays.asList(FunctionalBlockType.RA_ACCESS_MODULE.toString())));
	}
	
	private Map<List<String>, Set<UUID>> extractFunctionalBlockMap(final List<FunctionalBlockPojoPrototype> functionalBlockPojoPrototypeList) {
		final Map<List<String>, Set<UUID>> functionalBlockMap = new HashMap<>();
		for(final FunctionalBlockPojoPrototype blockPojoPrototype : functionalBlockPojoPrototypeList) {
			final String object = blockPojoPrototype.flags.getNonNull().get(FunctionalBlockFlag.TYPE.toString()).toString();
			final List<String> flagList = Arrays.asList(object.replaceAll("\\[|\\]", "").split(", "));
			if (functionalBlockMap.containsKey(flagList)) {
				final Set<UUID> set = new HashSet<>();
				set.addAll(blockPojoPrototype.children.getNonNull());
				set.addAll(functionalBlockMap.get(flagList));
				functionalBlockMap.put(flagList, set);
			} else {
				functionalBlockMap.put(flagList, blockPojoPrototype.children.getNonNull().stream().collect(Collectors.toSet()));
			}
		}
		return functionalBlockMap;
	}

}

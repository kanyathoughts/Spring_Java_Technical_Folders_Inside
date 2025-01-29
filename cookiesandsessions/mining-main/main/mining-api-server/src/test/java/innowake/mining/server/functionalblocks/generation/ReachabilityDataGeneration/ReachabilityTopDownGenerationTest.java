/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.generation.ReachabilityDataGeneration;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.data.model.discovery.attribute.ModelAttributeValue;
import innowake.mining.extensions.export.callchain.CallChainService;
import innowake.mining.extensions.export.callchain.model.CallChain.CallChainDirection;
import innowake.mining.extensions.export.callchain.model.CallChainGraph;
import innowake.mining.extensions.export.callchain.model.CallChainGraph.CallChainEdge;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationContext;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationResult;
import innowake.mining.server.functionalblocks.generation.reachabilityanalysis.ReachabilityTopDownGeneration;
import innowake.mining.server.functionalblocks.service.FunctionalBlockGenerationService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.SourceMetricsPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLink;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.testing.ModuleLightweightPojoDummy;
import innowake.mining.shared.entities.testing.ModulePojoDummy;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.DatabaseAccessType;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Mocked Test for ReachabilityTopDownGeneration
 */
@Tag("mocked")
class ReachabilityTopDownGenerationTest {
	
	private static final EntityId PROJECT_ID = EntityId.of(UUID.randomUUID(), 1L);
	private static FunctionalBlockPojo topModuleFunctionalBlock;
	private static FunctionalBlockPojo functionalBlock1;
	private static FunctionalBlockPojo functionalBlock2;
	private static FunctionalBlockPojo lowerBoundFunctionalBlock;
	private static FunctionalBlockService functionalBlockService;
	private static FunctionalBlockGenerationService functionalBlockGenerationService;
	private static CallChainService callChainService;
	private static ReachabilityTopDownGeneration reachabilityTopDownGeneration;
	private static ModulePojo topModule;
	private static ModulePojo accessModule1;
	private static ModulePojo accessModule2;
	private static ModulePojo lowerModule;
	private static CallChainGraph mockedCallChainGraph;
	private static CallChainGraph mockedCallChainGraphWithNoEndModules;
	
	@BeforeAll
	static void init() {
		final UUID functionalBlockUID1 = UUID.randomUUID();
		final UUID functionalBlockUID2 = UUID.randomUUID();
		final UUID functionalBlockUID3 = UUID.randomUUID();
		final UUID functionalBlockUID4 = UUID.randomUUID();
		
		topModule = newModule("topModule", "src/topModule.cbl", PROJECT_ID, Technology.JCL, Type.JOB, Storage.FILE, Identification.IDENTIFIED,
				Origin.CUSTOM, Creator.API, new HashMap<>(), "test description for topModule", null, "test content for Start module",
				"LinkHashTopModule1",
				10L);
		
		accessModule1 = newModule("accessModule1", "src/accessModule", PROJECT_ID, Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED,
				Origin.CUSTOM, Creator.API, new HashMap<>(), "test description for accessModule1", null, "test content for access Module 1",
				"LinkHashAccessModule1",
				11L);
		
		accessModule2 = newModule("accessModule2", "src/accessModule2", PROJECT_ID, Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED,
				Origin.CUSTOM, Creator.API, new HashMap<>(), "test description for accessModule2", null, "test content for access Module2",
				"LinkHashAccessModule2",
				12L);
		
		lowerModule = newModule("lowerModule", "src/lowerModule", PROJECT_ID, Technology.RESOURCE, Type.FILE, Storage.FILE, Identification.IDENTIFIED,
				Origin.CUSTOM, Creator.API, new HashMap<>(), "test description for lowerModule", null, "test content for lower Module",
				"LinkHashLowerModule",
				13L);
		
		final ModuleLightweightPojo topModuleLightweight =
				newModuleLightweight(1291L, topModule.getName(),topModule.getPath(), topModule.getTechnology(), topModule.getType(), topModule.getLinkHash(),
						topModule.getIdentification(), null, null);
		final ModuleLightweightPojo moduleLightweight1 =
				newModuleLightweight(817L, accessModule1.getName(), accessModule1.getPath(), accessModule1.getTechnology(), accessModule1.getType(),
						accessModule1.getLinkHash(), accessModule1.getIdentification(), null, null);
		final ModuleLightweightPojo moduleLightweight2 =
				newModuleLightweight(753L, accessModule2.getName(), accessModule2.getPath(), accessModule2.getTechnology(), accessModule2.getType(),
						accessModule2.getLinkHash(), accessModule2.getIdentification(), null, null);
		final ModuleLightweightPojo loweModuleLightweight =
				newModuleLightweight(634L, lowerModule.getName(), lowerModule.getPath(), lowerModule.getTechnology(), lowerModule.getType(),
						lowerModule.getLinkHash(), lowerModule.getIdentification(), null, null);
		
		topModuleFunctionalBlock = new FunctionalBlockPojo(functionalBlockUID1, null, PROJECT_ID, Collections.emptyList(), null,
				Collections.emptyList(), "topModuleFunctionalBlock", null, null,  null);
		functionalBlock1 = new FunctionalBlockPojo(functionalBlockUID2, null, PROJECT_ID, Collections.emptyList(), null,
				Collections.emptyList(), "ModuleBlock1", null, null,  null);
		functionalBlock2 = new FunctionalBlockPojo(functionalBlockUID3, null, PROJECT_ID, Collections.emptyList(), null,
				Collections.emptyList(), "ModuleBlock2", null, null, null);
		lowerBoundFunctionalBlock = new FunctionalBlockPojo(functionalBlockUID4, null, PROJECT_ID, Collections.emptyList(), null,
				Collections.emptyList(), "lowerBoundFunctionalBlock", null, null,  null);

		final CallChainEdge callModule1 = new CallChainEdge("callModule1", moduleLightweight1, RelationshipType.CALLS, Collections.emptyMap());
		final CallChainEdge callModule2 = new CallChainEdge("callModule2", moduleLightweight2, RelationshipType.CALLS, Collections.emptyMap());
		final CallChainEdge readLower = new CallChainEdge("readLower", loweModuleLightweight, RelationshipType.ACCESSES,
				Map.of(ModelAttributeKey.FILE_ACCESS_TYPE.toString(), ModelAttributeValue.FileAccess.READ.toString()));
		final CallChainEdge writeLower = new CallChainEdge("writeLower", loweModuleLightweight, RelationshipType.ACCESSES,
				Map.of(ModelAttributeKey.DB_ACCESS_TYPE.toString(), DatabaseAccessType.STORE.toString()));
		
		mockedCallChainGraph = new CallChainGraph(CallChainDirection.OUT, topModuleLightweight);
		mockedCallChainGraph.add(topModuleLightweight, callModule1);
		mockedCallChainGraph.add(topModuleLightweight, callModule2);
		mockedCallChainGraph.add(moduleLightweight1, readLower);
		mockedCallChainGraph.add(moduleLightweight2, writeLower);

		mockedCallChainGraphWithNoEndModules = new CallChainGraph(CallChainDirection.OUT, topModuleLightweight);
		mockedCallChainGraphWithNoEndModules.add(topModuleLightweight, callModule1);
		mockedCallChainGraphWithNoEndModules.add(topModuleLightweight, callModule2);
		mockedCallChainGraphWithNoEndModules.add(moduleLightweight1, callModule2);
		mockedCallChainGraphWithNoEndModules.add(moduleLightweight2, callModule1);
	}
	
	@Test
	void testWhenTopModuleIdIsNull() {
		functionalBlockService = mock(FunctionalBlockService.class);
		callChainService = mock(CallChainService.class);
		reachabilityTopDownGeneration = new ReachabilityTopDownGeneration(functionalBlockService, callChainService);
		assertTrue(reachabilityTopDownGeneration.generate(new FunctionalBlockGenerationContext(PROJECT_ID), null).isEmpty());
		
		/* Verify that the createCallChainGraphs method was never called with any instance of callChainService */
		verify(callChainService, never()).createCallChainGraphs(any(), any());
	}
	
	@Test
	void testWhenTopModuleIdIsNotNullAndWhenCallChainGraphIsEmpty() {
		functionalBlockService = mock(FunctionalBlockService.class);
		callChainService = mock(CallChainService.class);
		reachabilityTopDownGeneration = new ReachabilityTopDownGeneration(functionalBlockService, callChainService);
		
		assertTrue(reachabilityTopDownGeneration.generate(
				new FunctionalBlockGenerationContext(PROJECT_ID),EntityId.of(topModule.getId())).isEmpty());
		
		/* Verify that the createCallChainGraphs method was once called with any instance of callChainService */
		verify(callChainService, times(1)).createCallChainGraphs(any(), any());
	}

	/*
	 * Illustration for ReachabilityBlock Having no Lower Bound
	 *
	 * 								 +--------------------->accessModule2(Access Bound)<-----------------+
	 *								 |						(functionalBlock2)							 |
	 *								 |																	 |
	 * TopModule(Upper Bound)--------|                                                                   |
	 * (topModuleFunctionalBlock)	 |							 										 |
	 * 								 |																	 |
	 * 								 +--------------------->accessModule1(Access Bound)<-----------------+
	 *														(functionalBlock1)
	 */
	@Test
	void testReachabilityBlockHavingNoLowerBound() {
		functionalBlockService = mock(FunctionalBlockService.class);
		callChainService = mock(CallChainService.class);
		functionalBlockGenerationService = mock(FunctionalBlockGenerationService.class);
		reachabilityTopDownGeneration = new ReachabilityTopDownGeneration(functionalBlockService, callChainService);
		reachabilityTopDownGeneration.setFunctionalBlockGenerationService(functionalBlockGenerationService);
		when(callChainService.createCallChainGraphs(any(), any())).thenReturn(Optional.of(Collections.singletonList(mockedCallChainGraphWithNoEndModules)));
		when(functionalBlockService.findGeneratedFromModules(any(), any())).thenReturn(Map.of(topModule.getLinkHash(), topModuleFunctionalBlock.getUid(),
				accessModule1.getLinkHash(), functionalBlock1.getUid(), accessModule2.getLinkHash(), functionalBlock2.getUid()));
		final var result =
				reachabilityTopDownGeneration.generate(new FunctionalBlockGenerationContext(PROJECT_ID), EntityId.of(topModule.getId()));
		final var createBlocks = result.parallelStream().filter(f -> f.getOperation() == FunctionalBlockGenerationResult.Operation.CREATE)
				.map(FunctionalBlockGenerationResult::getFunctionalBlock).collect(Collectors.toList());
		final var updateBlocks = result.parallelStream().filter(f -> f.getOperation() == FunctionalBlockGenerationResult.Operation.UPDATE)
				.collect(Collectors.toList());
		final var upperBoundBlock = createBlocks.parallelStream()
				.filter(f -> isFunctionalBlockType(f, FunctionalBlockType.RA_UPPER_BOUND))
				.findAny().orElseThrow();
		final var reachabilityBlock = createBlocks.parallelStream()
				.filter(f -> isFunctionalBlockType(f, FunctionalBlockType.REACHABILITY))
				.findAny().orElseThrow();
		final var callChainBlock = createBlocks.parallelStream()
				.filter(f -> isFunctionalBlockType(f, FunctionalBlockType.CALL_CHAIN))
				.findAny().orElseThrow();
		assertEquals(3, createBlocks.size());
		assertEquals(List.of(topModuleFunctionalBlock.getUid()), upperBoundBlock.children.getNonNull());
		assertEquals(List.of(upperBoundBlock.uid.getNonNull(), callChainBlock.uid.getNonNull()), reachabilityBlock.children.getNonNull());
		assertEquals(Set.of(topModuleFunctionalBlock.getUid(), functionalBlock1.getUid(), functionalBlock2.getUid()),
				new HashSet<>(callChainBlock.children.getNonNull()));
		final var callChainLinks = result.parallelStream().filter(f -> f.getOperation() == FunctionalBlockGenerationResult.Operation.CREATE
				&& isFunctionalBlockType(f.getFunctionalBlock(), FunctionalBlockType.CALL_CHAIN)).findAny().orElseThrow().getAdditionalData().orElseThrow();
		assertEquals(Set.of(Pair.of(topModuleFunctionalBlock.getUid(), functionalBlock1.getUid()), Pair.of(topModuleFunctionalBlock.getUid(),
						functionalBlock2.getUid()), Pair.of(functionalBlock1.getUid(), functionalBlock2.getUid()), Pair.of(functionalBlock2.getUid(),
						functionalBlock1.getUid())),
				callChainLinks.parallelStream().map(f -> Pair.of(f.getChildA(), f.getChildB())).collect(Collectors.toSet()));
		assertEquals(1, updateBlocks.size());
		assertEquals(reachabilityBlock.uid.getNonNull(), updateBlocks.get(0).getFunctionalBlock().uid.getNonNull());
		final var reachabilityLinks = updateBlocks.get(0).getAdditionalData().orElseThrow().parallelStream().collect(Collectors.toMap(
				FunctionalBlockLink::getChildA, FunctionalBlockLink::getFlags));
		assertEquals(0, reachabilityLinks.size());
	}
	

	/*
	 * Illustration for ReachabilityBlock Having Lower Bound
	 * 
	 * 								 +--------------------->accessModule2(Access Bound)----------------->+
	 *								 |						(functionalBlock2)							 |
	 *								 |																	 |
	 * TopModule(Upper Bound)--------|                                                                   |->lowerBound(Lower Bound)
	 * (topModuleFunctionalBlock)	 |							 										 |(lowerBoundFunctionalBlock)
	 * 								 |																	 |
	 * 								 +--------------------->accessModule1(Access Bound)----------------->+
	 *														(functionalBlock1)
	 */
	@Test
	void testReachabilityBlockHavingLowerBound() {
		functionalBlockService = mock(FunctionalBlockService.class);
		callChainService = mock(CallChainService.class);
		functionalBlockGenerationService = mock(FunctionalBlockGenerationService.class);
		reachabilityTopDownGeneration = new ReachabilityTopDownGeneration(functionalBlockService, callChainService);
		reachabilityTopDownGeneration.setFunctionalBlockGenerationService(functionalBlockGenerationService);

		when(callChainService.createCallChainGraphs(any(), any())).thenReturn(Optional.of(Collections.singletonList(mockedCallChainGraph)));
		when(functionalBlockService.findGeneratedFromModules(any(), any())).thenReturn(Map.of(topModule.getLinkHash(), topModuleFunctionalBlock.getUid(),
				lowerModule.getLinkHash(), lowerBoundFunctionalBlock.getUid(), accessModule1.getLinkHash(), functionalBlock1.getUid(), accessModule2.getLinkHash(),
				functionalBlock2.getUid()));

		final var result = reachabilityTopDownGeneration.generate(new FunctionalBlockGenerationContext(PROJECT_ID),
				EntityId.of(topModule.getId()));

		final var createBlocks = result.parallelStream().filter(f -> f.getOperation() == FunctionalBlockGenerationResult.Operation.CREATE)
				.map(FunctionalBlockGenerationResult::getFunctionalBlock).collect(Collectors.toList());
		final var updateBlocks = result.parallelStream().filter(f -> f.getOperation() == FunctionalBlockGenerationResult.Operation.UPDATE)
				.collect(Collectors.toList());

		final var upperBoundBlock = createBlocks.parallelStream()
				.filter(f -> isFunctionalBlockType(f, FunctionalBlockType.RA_UPPER_BOUND))
				.findAny().orElseThrow();
		final var lowerBoundBlock = createBlocks.parallelStream()
				.filter(f -> isFunctionalBlockType(f, FunctionalBlockType.RA_LOWER_BOUND))
				.findAny().orElseThrow();
		final var accessModuleBlocks = createBlocks.parallelStream()
				.filter(f -> isFunctionalBlockType(f, FunctionalBlockType.RA_ACCESS_MODULE))
				.collect(Collectors.toList());
		final var reachabilityBlock = createBlocks.parallelStream()
				.filter(f -> isFunctionalBlockType(f, FunctionalBlockType.REACHABILITY))
				.findAny().orElseThrow();
		final var callChainBlock = createBlocks.parallelStream()
				.filter(f -> isFunctionalBlockType(f, FunctionalBlockType.CALL_CHAIN))
				.findAny().orElseThrow();
		assertEquals(6, createBlocks.size());
		assertEquals(2, accessModuleBlocks.size());
		assertEquals(List.of(topModuleFunctionalBlock.getUid()), upperBoundBlock.children.getNonNull());
		assertEquals(List.of(lowerBoundFunctionalBlock.getUid()), lowerBoundBlock.children.getNonNull());
		assertEquals(Set.of(functionalBlock1.getUid(), functionalBlock2.getUid()),
				accessModuleBlocks.parallelStream().map(f -> f.children.getNonNull()).flatMap(List::stream).collect(Collectors.toSet()));
		assertEquals(5, reachabilityBlock.children.getNonNull().size());
		assertTrue(reachabilityBlock.children.getNonNull().containsAll(
				List.of(upperBoundBlock.uid.getNonNull(), lowerBoundBlock.uid.getNonNull(), callChainBlock.uid.getNonNull())));
		assertTrue(reachabilityBlock.children.getNonNull().containsAll(
				accessModuleBlocks.parallelStream().map(f -> f.uid.getNonNull()).collect(Collectors.toList())));
		assertEquals(Set.of(functionalBlock1.getUid(), functionalBlock2.getUid(), lowerBoundFunctionalBlock.getUid(), topModuleFunctionalBlock.getUid()),
				new HashSet<>(callChainBlock.children.getNonNull()));
		final var callChainLinks = result.parallelStream().filter(f -> f.getOperation() == FunctionalBlockGenerationResult.Operation.CREATE
				&& isFunctionalBlockType(f.getFunctionalBlock(), FunctionalBlockType.CALL_CHAIN)).findAny().orElseThrow().getAdditionalData().orElseThrow();
		assertEquals(Set.of(Pair.of(topModuleFunctionalBlock.getUid(), functionalBlock1.getUid()), Pair.of(topModuleFunctionalBlock.getUid(), functionalBlock2.getUid()),
				Pair.of(functionalBlock1.getUid(), lowerBoundFunctionalBlock.getUid()), Pair.of(functionalBlock2.getUid(), lowerBoundFunctionalBlock.getUid())),
				callChainLinks.parallelStream().map(f -> Pair.of(f.getChildA(), f.getChildB())).collect(Collectors.toSet()));

		assertEquals(1, updateBlocks.size());
		assertEquals(reachabilityBlock.uid.getNonNull(), updateBlocks.get(0).getFunctionalBlock().uid.getNonNull());
		final var reachabilityLinks = updateBlocks.get(0).getAdditionalData().orElseThrow().parallelStream().collect(Collectors.toMap(
				FunctionalBlockLink::getChildA, FunctionalBlockLink::getFlags));
		assertEquals(2, reachabilityLinks.size());
		assertEquals(Set.of("READ"), new HashSet<>((Collection<?>) reachabilityLinks.get(accessModuleBlocks.parallelStream()
				.filter(b -> b.children.getNonNull().contains(functionalBlock1.getUid())).findAny().orElseThrow().uid.getNonNull())
				.get(FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name())));
		assertEquals(Set.of("WRITE"), new HashSet<>((Collection<?>) reachabilityLinks.get(accessModuleBlocks.parallelStream()
						.filter(b -> b.children.getNonNull().contains(functionalBlock2.getUid())).findAny().orElseThrow().uid.getNonNull())
				.get(FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name())));
	}

	private  boolean isFunctionalBlockType(final FunctionalBlockPojoPrototype block, final FunctionalBlockType type) {
		final Collection<?> flags = (Collection<?>) block.flags.getNonNull().get(FunctionalBlockFlag.TYPE.name());
		return flags.contains(type.name()) || flags.contains(type);
	}

	private static ModulePojo newModule(final String name, @Nullable final String path, final EntityId projectId, final Technology technology, final Type type, final Storage storage,
			final Identification identification, final Origin origin, @Nullable final Creator creator, final Map<String, Object> info,
			@Nullable final String description, @Nullable final SourceMetricsPojoPrototype sourceMetrics, @Nullable final String content, final String linkHash, final Long id) {
		final ModulePojoPrototype module = new ModulePojoPrototype()
			.setName(name)
			.setPath(path)
			.setProject(projectId)
			.setTechnology(technology)
			.setType(type)
			.setStorage(storage)
			.setIdentification(identification)
			.setOrigin(origin)
			.setInfo(info)
			.setSourceMetrics(sourceMetrics)
			.setLinkHash(linkHash)
			.setNid(id);

		if (creator != null) {
			module.setCreator(creator);
		}
		if (description != null) {
			module.setDescription(description);
		}
		if (content != null) {
			module.setContent(content);
		}

		return ModulePojoDummy.build(module);
	}
	
	private static ModuleLightweightPojo newModuleLightweight(final Long id, final String name, final Optional<String> path, final Technology technology, final Type type,
			final String linkHash, @Nullable final Identification identification, @Nullable final String containsPath,
			@Nullable final EntityId containingModuleId) {
		final ModulePojoPrototype module = new ModulePojoPrototype()
				.setName(name)
				.setPath(path.orElse(null))
				.setTechnology(technology)
				.setType(type)
				.setLinkHash(linkHash)
				.setNid(id);

		if (identification != null) {
			module.setIdentification(identification);
		}
		if (containingModuleId != null) {
			module.setParent(containingModuleId);
		}
		if (containsPath != null) {
			module.setParentPath(containsPath);
		}

		return ModuleLightweightPojoDummy.build(module);
	}
}

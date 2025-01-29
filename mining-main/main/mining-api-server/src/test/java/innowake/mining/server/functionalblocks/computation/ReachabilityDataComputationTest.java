/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.computation;

import innowake.lib.core.lang.Assert;
import innowake.lib.job.api.NullProgressMonitor;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.MockInquiryBuilder;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ModuleService.ModuleInquiryBuilder;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLink;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkType;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.GeneratedFrom;
import innowake.mining.shared.entities.functionalblocks.ReachabilityDataPojoPrototype;
import innowake.mining.shared.entities.testing.ModulePojoDummy;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.opentest4j.AssertionFailedError;

import java.time.Instant;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static java.util.Map.entry;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyCollection;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * test ReachAbility Data computation.
 */
@Tag("mocked")
@TestInstance(Lifecycle.PER_CLASS)
class ReachabilityDataComputationTest {

	private static final Long PROJECT_ID = 1L;
	private static final UUID PROJECT_UUID = UUID.randomUUID();

	private FunctionalBlockPojo reachabilityBlock;
	private FunctionalBlockPojo reachabilityBlock2;
	private FunctionalBlockPojo upperBoundBlock;
	private FunctionalBlockPojo upperBoundBlock2;
	private FunctionalBlockPojo lowerBoundBlock;
	private FunctionalBlockPojo lowerBoundBlock2;
	private FunctionalBlockPojo accessModuleBlock;
	private FunctionalBlockPojo accessModuleBlock2;
	private FunctionalBlockPojo callChainBlock;
	private FunctionalBlockPojo callChainBlock2;
	private FunctionalBlockPojo upperBoundModule1;
	private FunctionalBlockPojo lowerboundModule1;
	private FunctionalBlockPojo accessModule1;
	private FunctionalBlockPojo upperBoundModule2;
	private FunctionalBlockPojo lowerBoundModule2;
	private FunctionalBlockPojo accessModule2;
	private ModulePojo module1;
	private ModulePojo module2;
	private ModulePojo module3;
	private ModulePojo module4;
	private ModulePojo module5;
	private ModulePojo module6;
	private Map<String, EntityId> moduleIdsByLinkHashMap;

	@BeforeEach
	void init() {
		final UUID upperBoundModuleUid = UUID.randomUUID();
		final UUID upperBoundModuleUid2 = UUID.randomUUID();
		final UUID lowerBoundModuleUid = UUID.randomUUID();
		final UUID lowerBoundModuleUid2 = UUID.randomUUID();
		final UUID accessModuleModuleUid = UUID.randomUUID();
		final UUID accessModuleModuleUid2 = UUID.randomUUID();
		final UUID upperBoundUid = UUID.randomUUID();
		final UUID upperBoundUid2 = UUID.randomUUID();
		final UUID lowerBoundUid = UUID.randomUUID();
		final UUID lowerBoundUid2 = UUID.randomUUID();
		final UUID accessModuleUid = UUID.randomUUID();
		final UUID accessModuleUid2 = UUID.randomUUID();
		final UUID reachabilityUid = UUID.randomUUID();
		final UUID reachabilityUid2 = UUID.randomUUID();

		module1 = createModule(1L, "Module1", "src/upperBound.cbl", "LinkHash1");
		module2 = createModule(2L, "Module2", "src/lowerBound.cbl", "LinkHash2");
		module3 = createModule(3L, "Module3", "src/access.cbl", "LinkHash3");
		module4 = createModule(4L, "Module4", "src/upperBound2.cbl", "LinkHash4");
		module5 = createModule(5L, "Module5", "src/lowerBound2.cbl", "LinkHash5");
		module6 = createModule(6L, "Module6", "src/access2.cbl", "LinkHash6");

		moduleIdsByLinkHashMap = new HashMap<>(Map.of(
				"LinkHash1", module1.identity(),
				"LinkHash2", module2.identity(),
				"LinkHash3", module3.identity(),
				"LinkHash4", module4.identity(),
				"LinkHash5", module5.identity(),
				"LinkHash6", module6.identity()
		));

		upperBoundModule1 = createFunctionalBlock(upperBoundModuleUid, "Upper Bound Module",
				Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.MODULE));
		lowerboundModule1 = createFunctionalBlock(lowerBoundModuleUid, "Lower Bound Module",
				Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.MODULE));
		accessModule1 = createFunctionalBlock(accessModuleModuleUid, "Access Module Module",
				Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.MODULE));
		upperBoundModule2 = createFunctionalBlock(upperBoundModuleUid2, "Upper Bound Module 2",
				Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.MODULE));
		lowerBoundModule2 = createFunctionalBlock(lowerBoundModuleUid2, "Lower Bound Module 2",
				Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.MODULE));
		accessModule2 = createFunctionalBlock(accessModuleModuleUid2, "Access Module Module 2",
				Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.MODULE));

		upperBoundBlock = createFunctionalBlock(upperBoundUid, "Upper Bound",
				List.of(upperBoundModuleUid),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_UPPER_BOUND));
		lowerBoundBlock = createFunctionalBlock(lowerBoundUid, "Lower Bound",
				List.of(lowerBoundModuleUid),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_LOWER_BOUND, FunctionalBlockFlag.RA_ACCESS_TYPE.toString(),"READ"));
		accessModuleBlock = createFunctionalBlock(accessModuleUid, "Access Module",
				List.of(accessModuleModuleUid),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_ACCESS_MODULE));
		callChainBlock = createFunctionalBlock(UUID.randomUUID(), "Call Chain Block",
				List.of(upperBoundModuleUid, lowerBoundModuleUid, accessModuleModuleUid),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.CALL_CHAIN));
		reachabilityBlock = createFunctionalBlock(reachabilityUid, "Reachability Block",
				List.of(upperBoundUid, lowerBoundUid, accessModuleUid),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_TOP_DOWN));

		upperBoundBlock2 = createFunctionalBlock(upperBoundUid2, "Upper Bound 2",
				List.of(upperBoundModuleUid2),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_UPPER_BOUND));
		lowerBoundBlock2 = createFunctionalBlock(lowerBoundUid2, "Lower Bound 2",
				List.of(lowerBoundModuleUid2),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_LOWER_BOUND, FunctionalBlockFlag.RA_ACCESS_TYPE.toString(),"READ"));
		accessModuleBlock2 = createFunctionalBlock(accessModuleUid2, "Access Module 2",
				List.of(accessModuleModuleUid2),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_ACCESS_MODULE));
		callChainBlock2 = createFunctionalBlock(UUID.randomUUID(), "Call Chain Block",
				List.of(upperBoundModuleUid, lowerBoundModuleUid, accessModuleModuleUid),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.CALL_CHAIN));
		reachabilityBlock2 = createFunctionalBlock(reachabilityUid2, "Reachability Block 2",
				List.of(upperBoundUid2, lowerBoundUid2, accessModuleUid2),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_TOP_DOWN));
	}

	private ModulePojo createModule(final Long id, final String name, final String path, final String linkHash) {
		return ModulePojoDummy.build(new ModulePojoPrototype()
				.setName(name)
				.setPath(path)
				.setProject(EntityId.of(PROJECT_ID))
				.setTechnology(Technology.COBOL)
				.setType(Type.PROGRAM)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setCreator(Creator.API)
				.setCustomProperties(new HashMap<>())
				.setDescription("test description for module")
				.setContent("test content for Upper module")
				.setLinkHash(linkHash)
				.setNid(id)
		);
	}

	private FunctionalBlockPojo createFunctionalBlock(final UUID uid, final String name, final List<UUID> children, final Map<String, Object> flags) {
		return new FunctionalBlockPojo(uid,
				CustomPropertiesMap.empty(),
				EntityId.of(PROJECT_UUID, PROJECT_ID),
				Collections.emptyList(),
				Collections.emptyList(),
				children,
				name,
				"",
				flags,
				Instant.now());
	}

	private ModuleService mockModuleService() {
		final ModuleService moduleService = mock(ModuleService.class);

		when(moduleService.findModuleIdsByLinkHash(any())).then(invocation -> {
			final BuildingConsumer<ModuleInquiryBuilder> consumer = invocation.getArgument(0);

			final MockInquiryBuilder<ModuleInquiryBuilder> mockBuilder = new MockInquiryBuilder<>(ModuleInquiryBuilder.class);

			consumer.prepare(mockBuilder.mockBuilder());
			final Collection<String> linkHashes = mockBuilder.getFirstArgument("withLinkHashes");

			if (linkHashes == null) {
				return moduleIdsByLinkHashMap;
			} else {
				final Map<String, EntityId> ret = new HashMap<>();
				for (final String linkHash : linkHashes) {
					ret.put(linkHash, moduleIdsByLinkHashMap.get(linkHash));
				}
				return ret;
			}
		});

		return moduleService;
	}

	private void mockGeneratedFrom(final FunctionalBlockService functionalBlockService, final Map<UUID, GeneratedFrom> generatedFromMap) {
		when(functionalBlockService.getGeneratedFrom(anyCollection())).then(invocation -> {
			Collection<UUID> uuids = invocation.getArgument(0);

			final Map<UUID, GeneratedFrom> ret = new HashMap<>();
			for (final UUID uuid : uuids) {
				if ( ! generatedFromMap.containsKey(uuid)) {
					throw new AssertionFailedError("test is missing a 'generatedFrom' mapping for " + uuid);
				}
				ret.put(uuid, generatedFromMap.get(uuid));
			}

			return ret;
		});
	}

	@Test
	void testReachabilityDataComputationHavingNoMergeParentFlag() {
		/* test ReachabilityDataComputation for a single, not merged Reachability Block */
		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final ModuleService moduleService = mockModuleService();

		when(Assert.assertNotNull(functionalBlockService).find(any(BuildingConsumer.class)))
				.thenReturn(List.of(upperBoundBlock, lowerBoundBlock, accessModuleBlock, callChainBlock));


		mockGeneratedFrom(functionalBlockService, Map.of(
				upperBoundModule1.getUid(), GeneratedFrom.fromModule("LinkHash1", "contentHash", "DependencyHash1"),
				lowerboundModule1.getUid(), GeneratedFrom.fromModule("LinkHash2", "contentHash", "DependencyHash2"),
				accessModule1.getUid(), GeneratedFrom.fromModule("LinkHash3", "contentHash", "DependencyHash3")
		));

		/* return link from accessModuleBlock TO lowerBoundBlock */
		when(functionalBlockService.getLinks(any(UUID.class)))
				.thenReturn(List.of(new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), lowerboundModule1.getUid(), accessModule1.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), FunctionalBlockLinkType.RA_ACCESS), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), upperBoundModule1.getUid(), lowerboundModule1.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), FunctionalBlockLinkType.RA_ACCESS), null)))
				.thenReturn(List.of(new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock.getUid(), accessModuleBlock.getUid(), lowerBoundBlock.getUid(),
						null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), FunctionalBlockLinkType.RA_ACCESS), null)));

		final ReachabilityDataComputation reachabilityDataComputation = new ReachabilityDataComputation(functionalBlockService, moduleService);

		final var computationResult = reachabilityDataComputation.computeBatched(List.of(reachabilityBlock), new NullProgressMonitor());
		assertEquals(Set.of(reachabilityBlock), computationResult.keySet());
		assertEquals(1, computationResult.get(reachabilityBlock).size());

		final ReachabilityDataPojoPrototype reachabilityData = computationResult.get(reachabilityBlock).stream().findFirst().orElseThrow();
		assertEquals(module1.identity(), reachabilityData.upperBoundModuleId.getNonNull());
		assertEquals(module2.identity(), reachabilityData.lowerBoundModuleId.getNonNull());
		assertEquals(List.of("READ"),  reachabilityData.accessTypes.getNonNull());
		assertEquals(EntityId.of(module3.getId()), reachabilityData.accessModuleId.getNonNull());
	}

	@Test
	void testReachabilityDataComputationHavingMergeParentFlag() {
		/* test ReachabilityDataComputation for a merged block containing "reachabilityBlock" and "reachabilityBlock2" */
		final UUID mergedFunctionalBlockUid = UUID.randomUUID();
		final FunctionalBlockPojo mergedFunctionalBlock = createFunctionalBlock(mergedFunctionalBlockUid, "Merged Block",
				List.of(reachabilityBlock.getUid(), reachabilityBlock2.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.MERGE_PARENT.name())));

		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final ModuleService moduleService = mockModuleService();

		when(Assert.assertNotNull(functionalBlockService).find(any(BuildingConsumer.class)))
				.thenReturn(List.of(reachabilityBlock, reachabilityBlock2))
				.thenReturn(List.of(upperBoundBlock, lowerBoundBlock, accessModuleBlock, callChainBlock))
				.thenReturn(List.of(upperBoundBlock2, lowerBoundBlock2, accessModuleBlock2, callChainBlock2));

		mockGeneratedFrom(functionalBlockService, Map.of(
				upperBoundModule1.getUid(), GeneratedFrom.fromModule("LinkHash1", "contentHash", "DependencyHash1"),
				lowerboundModule1.getUid(), GeneratedFrom.fromModule("LinkHash2", "contentHash", "DependencyHash2"),
				accessModule1.getUid(), GeneratedFrom.fromModule("LinkHash3", "contentHash", "DependencyHash3"),
				upperBoundModule2.getUid(), GeneratedFrom.fromModule("LinkHash4", "contentHash", "DependencyHash4"),
				lowerBoundModule2.getUid(), GeneratedFrom.fromModule("LinkHash5", "contentHash", "DependencyHash5"),
				accessModule2.getUid(), GeneratedFrom.fromModule("LinkHash6", "contentHash", "DependencyHash6")
		));

		/* return link from accessModuleBlock TO lowerBoundBlock */
		when(functionalBlockService.getLinks(callChainBlock.getUid()))
				.thenReturn(List.of(
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), accessModule1.getUid(), lowerboundModule1.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), FunctionalBlockLinkType.RA_ACCESS), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), upperBoundModule1.getUid(), accessModule1.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), FunctionalBlockLinkType.RA_ACCESS), null))
				);
		when(functionalBlockService.getLinks(reachabilityBlock.getUid())).thenReturn(List.of(
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock.getUid(), accessModuleBlock.getUid(), lowerBoundBlock.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), FunctionalBlockLinkType.RA_ACCESS), null)
				));
		when(functionalBlockService.getLinks(callChainBlock2.getUid())).thenReturn(List.of(
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock2.getUid(), accessModule2.getUid(), lowerBoundModule2.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), FunctionalBlockLinkType.RA_ACCESS), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock2.getUid(), upperBoundModule2.getUid(), accessModule2.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), FunctionalBlockLinkType.RA_ACCESS), null))
				);
		when(functionalBlockService.getLinks(reachabilityBlock2.getUid())).thenReturn(List.of(
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock2.getUid(), accessModuleBlock2.getUid(), lowerBoundBlock2.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), FunctionalBlockLinkType.RA_ACCESS), null)
				));

		final ReachabilityDataComputation reachabilityDataComputation = new ReachabilityDataComputation(functionalBlockService, moduleService);
		final var computationResult = reachabilityDataComputation.computeBatched(List.of(mergedFunctionalBlock), new NullProgressMonitor());
		assertEquals(Set.of(mergedFunctionalBlock), computationResult.keySet());
		assertEquals(2, computationResult.get(mergedFunctionalBlock).size());

		for (final ReachabilityDataPojoPrototype reachabilityData : computationResult.get(mergedFunctionalBlock)) {
			if (reachabilityData.upperBoundModuleId.getNonNull().equals(module1.identity())) {
				/* expected ReachabilityData for "reachabilityBlock" */
				assertEquals(module1.identity(), reachabilityData.upperBoundModuleId.getNonNull());
				assertEquals(module2.identity(), reachabilityData.lowerBoundModuleId.getNonNull());
				assertEquals(List.of("READ"),  reachabilityData.accessTypes.getNonNull());
				assertEquals(module3.identity(), reachabilityData.accessModuleId.getNonNull());
			} else if (reachabilityData.upperBoundModuleId.getNonNull().equals(module4.identity())) {
				/* expected ReachabilityData for "reachabilityBlock2" */
				assertEquals(module4.identity(), reachabilityData.upperBoundModuleId.getNonNull());
				assertEquals(module5.identity(), reachabilityData.lowerBoundModuleId.getNonNull());
				assertEquals(List.of("READ"),  reachabilityData.accessTypes.get());
				assertEquals(module6.identity(), reachabilityData.accessModuleId.getNonNull());
			} else {
				fail("Unexpected upper bound module in result: " + reachabilityData.upperBoundModuleId.getNonNull());
			}
		}
	}

	/*
	 *             +---------am1---------+
	 *      ub1 -> |---------am2---------|-> lb1
	 *             +---------am3---------+
	 */
	@Test
	void testReachabilityDataAccessTypesFromReachabilityBlockLinks() {
		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);

		final var upperBoundModule = createModule(1L, "UpperBoundModule", "src/upperBound.cbl", "LinkHashUpperBound");
		final var lowerBoundModule = createModule(2L, "LowerBoundModule", "src/lowerBound.cbl", "LinkHashLowerBound");
		final var accessModule1 = createModule(3L, "AccessModule1", "src/access1.cbl", "LinkHashAccess1");
		final var accessModule2 = createModule(4L, "AccessModule2", "src/access2.cbl", "LinkHashAccess2");
		final var accessModule3 = createModule(5L, "AccessModule3", "src/access3.cbl", "LinkHashAccess3");

		final var upperBoundModuleFb = createFunctionalBlock(UUID.randomUUID(), "UpperBoundModule",
				Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var lowerBoundModuleFb = createFunctionalBlock(UUID.randomUUID(), "LowerBoundModule",
				Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var accessModule1Fb = createFunctionalBlock(UUID.randomUUID(), "AccessModule1",
				Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var accessModule2Fb = createFunctionalBlock(UUID.randomUUID(), "AccessModule2",
				Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var accessModule3Fb = createFunctionalBlock(UUID.randomUUID(), "AccessModule3",
				Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));

		final var callChainBlock = createFunctionalBlock(UUID.randomUUID(), "CallChainBlock",
				List.of(upperBoundModuleFb.getUid(), lowerBoundModuleFb.getUid(), accessModule1Fb.getUid(), accessModule2Fb.getUid(), accessModule3Fb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.CALL_CHAIN));
		final var upperBoundFb = createFunctionalBlock(UUID.randomUUID(), "UpperBound",
				List.of(upperBoundModuleFb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_UPPER_BOUND));
		final var lowerBoundFb = createFunctionalBlock(UUID.randomUUID(), "LowerBound",
				List.of(lowerBoundModuleFb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_LOWER_BOUND,
						FunctionalBlockFlag.RA_ACCESS_TYPE.name(), Set.of("READ", "WRITE")));
		final var accessBlock1Fb = createFunctionalBlock(UUID.randomUUID(), "AccessModule1",
				List.of(accessModule1Fb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_ACCESS_MODULE));
		final var accessBlock2Fb = createFunctionalBlock(UUID.randomUUID(), "AccessModule2",
				List.of(accessModule2Fb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_ACCESS_MODULE));
		final var accessBlock3Fb = createFunctionalBlock(UUID.randomUUID(), "AccessModule3",
				List.of(accessModule3Fb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_ACCESS_MODULE));
		final var reachabilityBlock = createFunctionalBlock(UUID.randomUUID(), "ReachabilityBlock",
				List.of(upperBoundFb.getUid(), lowerBoundFb.getUid(), accessBlock1Fb.getUid(), accessBlock2Fb.getUid(), accessBlock3Fb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.RA_TOP_DOWN, FunctionalBlockType.REACHABILITY)));

		when(Assert.assertNotNull(functionalBlockService).find(any(BuildingConsumer.class)))
				.thenReturn(List.of(upperBoundFb, lowerBoundFb, accessBlock1Fb, accessBlock2Fb, accessBlock3Fb, callChainBlock));

		mockGeneratedFrom(functionalBlockService, Map.of(
				upperBoundModuleFb.getUid(), GeneratedFrom.fromModule(upperBoundModule.getLinkHash(),
						upperBoundModule.getContentHash().toString(), upperBoundModule.getDependencyHash().orElse(null)),
				lowerBoundModuleFb.getUid(), GeneratedFrom.fromModule(lowerBoundModule.getLinkHash(),
						lowerBoundModule.getContentHash().toString(), lowerBoundModule.getDependencyHash().orElse(null)),
				accessModule1Fb.getUid(), GeneratedFrom.fromModule(accessModule1.getLinkHash(),
						accessModule1.getContentHash().toString(), accessModule1.getDependencyHash().orElse(null)),
				accessModule2Fb.getUid(), GeneratedFrom.fromModule(accessModule2.getLinkHash(),
						accessModule2.getContentHash().toString(), accessModule2.getDependencyHash().orElse(null)),
				accessModule3Fb.getUid(), GeneratedFrom.fromModule(accessModule3.getLinkHash(),
						accessModule3.getContentHash().toString(), accessModule3.getDependencyHash().orElse(null))
		));

		final ModuleService moduleService = mockModuleService();

		moduleIdsByLinkHashMap = Map.of(
				upperBoundModule.getLinkHash(), upperBoundModule.identity(),
				lowerBoundModule.getLinkHash(), lowerBoundModule.identity(),
				accessModule1.getLinkHash(), accessModule1.identity(),
				accessModule2.getLinkHash(), accessModule2.identity(),
				accessModule3.getLinkHash(), accessModule3.identity()
		);

		when(functionalBlockService.getLinks(callChainBlock.getUid()))
				.thenReturn(List.of(
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), accessModule1Fb.getUid(), lowerBoundModuleFb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), accessModule2Fb.getUid(), lowerBoundModuleFb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), accessModule3Fb.getUid(), lowerBoundModuleFb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), upperBoundModuleFb.getUid(), accessModule1Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), upperBoundModuleFb.getUid(), accessModule2Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), upperBoundModuleFb.getUid(), accessModule3Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null)
				));
		when(functionalBlockService.getLinks(reachabilityBlock.getUid()))
				.thenReturn(List.of(
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock.getUid(), accessBlock1Fb.getUid(), lowerBoundFb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), FunctionalBlockLinkType.RA_ACCESS,
								FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name(), Set.of("READ")), null),
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock.getUid(), accessBlock2Fb.getUid(), lowerBoundFb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), FunctionalBlockLinkType.RA_ACCESS,
								FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name(), Set.of("WRITE")), null),
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock.getUid(), accessBlock3Fb.getUid(), lowerBoundFb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), FunctionalBlockLinkType.RA_ACCESS,
								FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name(), Set.of("READ", "WRITE")), null)));

		final ReachabilityDataComputation reachabilityDataComputation = new ReachabilityDataComputation(functionalBlockService, moduleService);
		final var computationResult = reachabilityDataComputation.computeBatched(List.of(reachabilityBlock), new NullProgressMonitor());
		assertEquals(Set.of(reachabilityBlock), computationResult.keySet());
		assertEquals(3, computationResult.get(reachabilityBlock).size());
		assertTrue(computationResult.get(reachabilityBlock).stream().map(p -> p.accessModuleId.getNonNull()).toList().containsAll(
				List.of(EntityId.of(accessModule1.getId()), EntityId.of(accessModule2.getId()), EntityId.of(accessModule3.getId()))));

		for (final ReachabilityDataPojoPrototype reachabilityData : computationResult.get(reachabilityBlock)) {
			assertFalse(reachabilityData.intermediateModules.isDefined() && ! reachabilityData.intermediateModules.getNonNull().isEmpty());
			if (reachabilityData.accessModuleId.getNonNull().equals(accessModule1.identity())) {
				assertEquals(1, reachabilityData.accessTypes.getNonNull().size());
				assertTrue(Set.of("READ").containsAll(reachabilityData.accessTypes.getNonNull()));
			} else if (reachabilityData.accessModuleId.getNonNull().equals(EntityId.of(accessModule2.getId()))) {
				assertEquals(1, reachabilityData.accessTypes.getNonNull().size());
				assertTrue(Set.of("WRITE").containsAll(reachabilityData.accessTypes.getNonNull()));
			} else if (reachabilityData.accessModuleId.getNonNull().equals(accessModule3.identity())) {
				assertEquals(2, reachabilityData.accessTypes.getNonNull().size());
				assertTrue(Set.of("READ", "WRITE").containsAll(reachabilityData.accessTypes.getNonNull()));
			} else {
				fail("Unexpected access module in result: " + reachabilityData.accessModuleId.getNonNull());
			}
		}
	}

	/*
	 *             +---------am1---------+
	 *      ub1 -> |---------am2---------|-> lb1
	 *             +---------am3---------+
	 */
	@Test
	void testReachabilityDataAccessTypesFromReachabilityBlockLinksHavingNoAccessType() {
		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final ModuleService moduleService = mockModuleService();

		final var upperBoundModule = createModule(1L, "UpperBoundModule", "src/upperBound.cbl", "LinkHashUpperBound");
		final var lowerBoundModule = createModule(2L, "LowerBoundModule", "src/lowerBound.cbl", "LinkHashLowerBound");
		final var accessModule1 = createModule(3L, "AccessModule1", "src/access1.cbl", "LinkHashAccess1");
		final var accessModule2 = createModule(4L, "AccessModule2", "src/access2.cbl", "LinkHashAccess2");
		final var accessModule3 = createModule(5L, "AccessModule3", "src/access3.cbl", "LinkHashAccess3");

		moduleIdsByLinkHashMap.putAll(Map.of(
				upperBoundModule.getLinkHash(), upperBoundModule.identity(),
				lowerBoundModule.getLinkHash(), lowerBoundModule.identity(),
				accessModule1.getLinkHash(), accessModule1.identity(),
				accessModule2.getLinkHash(), accessModule2.identity(),
				accessModule3.getLinkHash(), accessModule3.identity()
		));

		final var upperBoundModuleFb = createFunctionalBlock(UUID.randomUUID(), "UpperBoundModule", Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var lowerBoundModuleFb = createFunctionalBlock(UUID.randomUUID(), "LowerBoundModule", Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var accessModule1Fb = createFunctionalBlock(UUID.randomUUID(), "AccessModule1", Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var accessModule2Fb = createFunctionalBlock(UUID.randomUUID(), "AccessModule2", Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var accessModule3Fb = createFunctionalBlock(UUID.randomUUID(), "AccessModule3", Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));

		final var callChainBlock = createFunctionalBlock(UUID.randomUUID(), "CallChainBlock",
				List.of(upperBoundModuleFb.getUid(), lowerBoundModuleFb.getUid(), accessModule1Fb.getUid(), accessModule2Fb.getUid(), accessModule3Fb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.CALL_CHAIN));
		final var upperBoundFb = createFunctionalBlock(UUID.randomUUID(), "UpperBound", List.of(upperBoundModuleFb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_UPPER_BOUND));
		final var lowerBoundFb = createFunctionalBlock(UUID.randomUUID(), "LowerBound", List.of(lowerBoundModuleFb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_LOWER_BOUND,
						FunctionalBlockFlag.RA_ACCESS_TYPE.name(), Set.of("READ", "WRITE")));
		final var accessBlock1Fb = createFunctionalBlock(UUID.randomUUID(), "AccessModule1", List.of(accessModule1Fb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_ACCESS_MODULE));
		final var accessBlock2Fb = createFunctionalBlock(UUID.randomUUID(), "AccessModule2", List.of(accessModule2Fb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_ACCESS_MODULE));
		final var accessBlock3Fb = createFunctionalBlock(UUID.randomUUID(), "AccessModule3", List.of(accessModule3Fb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_ACCESS_MODULE));
		final var reachabilityBlock = createFunctionalBlock(UUID.randomUUID(), "ReachabilityBlock",
				List.of(upperBoundFb.getUid(), lowerBoundFb.getUid(), accessBlock1Fb.getUid(), accessBlock2Fb.getUid(), accessBlock3Fb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));

		when(Assert.assertNotNull(functionalBlockService).find(any(BuildingConsumer.class))).thenReturn(
				List.of(upperBoundFb, lowerBoundFb, accessBlock1Fb, accessBlock2Fb, accessBlock3Fb, callChainBlock));

		mockGeneratedFrom(functionalBlockService, Map.of(
				upperBoundModuleFb.getUid(), GeneratedFrom.fromModule(upperBoundModule.getLinkHash(),
						upperBoundModule.getContentHash().toString(), upperBoundModule.getDependencyHash().orElse(null)),
				lowerBoundModuleFb.getUid(), GeneratedFrom.fromModule(lowerBoundModule.getLinkHash(),
						lowerBoundModule.getContentHash().toString(), lowerBoundModule.getDependencyHash().orElse(null)),
				accessModule1Fb.getUid(), GeneratedFrom.fromModule(accessModule1.getLinkHash(),
						accessModule1.getContentHash().toString(), accessModule1.getDependencyHash().orElse(null)),
				accessModule2Fb.getUid(), GeneratedFrom.fromModule(accessModule2.getLinkHash(),
						accessModule2.getContentHash().toString(), accessModule2.getDependencyHash().orElse(null)),
				accessModule3Fb.getUid(), GeneratedFrom.fromModule(accessModule3.getLinkHash(),
						accessModule3.getContentHash().toString(), accessModule3.getDependencyHash().orElse(null))
		));


		when(functionalBlockService.getLinks(callChainBlock.getUid())).thenReturn(
				List.of(
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), accessModule1Fb.getUid(), lowerBoundModuleFb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), accessModule2Fb.getUid(), lowerBoundModuleFb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), accessModule3Fb.getUid(), lowerBoundModuleFb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), upperBoundModuleFb.getUid(), accessModule1Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), upperBoundModuleFb.getUid(), accessModule2Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), upperBoundModuleFb.getUid(), accessModule3Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null)
				));
		when(functionalBlockService.getLinks(reachabilityBlock.getUid())).thenReturn(
				List.of(
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock.getUid(), accessBlock1Fb.getUid(), lowerBoundFb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), FunctionalBlockLinkType.RA_ACCESS), null),
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock.getUid(), accessBlock2Fb.getUid(), lowerBoundFb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), FunctionalBlockLinkType.RA_ACCESS), null),
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock.getUid(), accessBlock3Fb.getUid(), lowerBoundFb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), FunctionalBlockLinkType.RA_ACCESS), null)));

		final ReachabilityDataComputation reachabilityDataComputation = new ReachabilityDataComputation(functionalBlockService, moduleService);
		final var computationResult = reachabilityDataComputation.computeBatched(List.of(reachabilityBlock), new NullProgressMonitor());
		assertEquals(Set.of(reachabilityBlock), computationResult.keySet());
		assertEquals(3, computationResult.get(reachabilityBlock).size());

		for (final ReachabilityDataPojoPrototype reachabilityData : computationResult.get(reachabilityBlock)) {
			assertEquals(2, reachabilityData.accessTypes.getNonNull().size());
			assertTrue(Set.of("READ", "WRITE").containsAll(reachabilityData.accessTypes.getNonNull()));
			assertFalse(reachabilityData.intermediateModules.isDefined() && ! reachabilityData.intermediateModules.getNonNull().isEmpty());
		}
	}

	/*
	 *             +-------->im1-------------->am1----------> lb1
	 *             |                            ^
	 *             |-------->im21-----+         |
	 * 		       |                  |------->am2----------> lb2
	 *      ub1 -> |-------->im2------+
	 * 		       |          ^
	 *             |         im321
	 *             |          |
	 *             +-------->im3------------->am3----------> lb3
	 */
	@Test
	void testReachabilityDataIntermediateModules() {
		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final ModuleService moduleService = mockModuleService();

		final var upperBoundModule = createModule(1L, "UpperBoundModule", "src/upperBound.cbl", "LinkHashUpperBound");
		final var lowerBoundModule1 = createModule(2L, "LowerBoundModule1", "src/lowerBound1.cbl", "LinkHashLowerBound1");
		final var lowerBoundModule2 = createModule(3L, "LowerBoundModule2", "src/lowerBound2.cbl", "LinkHashLowerBound2");
		final var lowerBoundModule3 = createModule(4L, "LowerBoundModule3", "src/lowerBound3.cbl", "LinkHashLowerBound3");
		final var accessModule1 = createModule(5L, "AccessModule1", "src/access1.cbl", "LinkHashAccess1");
		final var accessModule2 = createModule(6L, "AccessModule2", "src/access2.cbl", "LinkHashAccess2");
		final var accessModule3 = createModule(7L, "AccessModule3", "src/access3.cbl", "LinkHashAccess3");
		final var intermediateModule1 = createModule(8L, "IntermediateModule1", "src/intermediate1.cbl", "LinkHashIntermediate1");
		final var intermediateModule2 = createModule(9L, "IntermediateModule2", "src/intermediate2.cbl", "LinkHashIntermediate2");
		final var intermediateModule3 = createModule(10L, "IntermediateModule3", "src/intermediate3.cbl", "LinkHashIntermediate3");
		final var intermediateModule21 = createModule(11L, "IntermediateModule21", "src/intermediate21.cbl", "LinkHashIntermediate21");
		final var intermediateModule321 = createModule(12L, "IntermediateModule321", "src/intermediate321.cbl", "LinkHashIntermediate321");

		final var upperBoundModuleFb = createFunctionalBlock(UUID.randomUUID(), "UpperBoundModule", Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var lowerBoundModule1Fb = createFunctionalBlock(UUID.randomUUID(), "LowerBoundModule1", Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var lowerBoundModule2Fb = createFunctionalBlock(UUID.randomUUID(), "LowerBoundModule2", Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var lowerBoundModule3Fb = createFunctionalBlock(UUID.randomUUID(), "LowerBoundModule3", Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var accessModule1Fb = createFunctionalBlock(UUID.randomUUID(), "AccessModule1", Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var accessModule2Fb = createFunctionalBlock(UUID.randomUUID(), "AccessModule2", Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var accessModule3Fb = createFunctionalBlock(UUID.randomUUID(), "AccessModule3", Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var intermediateModule1Fb = createFunctionalBlock(UUID.randomUUID(), "IntermediateModule1", Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var intermediateModule2Fb = createFunctionalBlock(UUID.randomUUID(), "IntermediateModule2", Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var intermediateModule3Fb = createFunctionalBlock(UUID.randomUUID(), "IntermediateModule3", Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var intermediateModule21Fb = createFunctionalBlock(UUID.randomUUID(), "IntermediateModule21", Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var intermediateModule321Fb = createFunctionalBlock(UUID.randomUUID(), "IntermediateModule321", Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));

		final var callChainBlock = createFunctionalBlock(UUID.randomUUID(), "CallChainBlock",
				List.of(upperBoundModuleFb.getUid(), lowerBoundModule1Fb.getUid(), lowerBoundModule2Fb.getUid(), lowerBoundModule3Fb.getUid(),
						accessModule1Fb.getUid(), accessModule2Fb.getUid(), accessModule3Fb.getUid(), intermediateModule1Fb.getUid(),
						intermediateModule2Fb.getUid(), intermediateModule3Fb.getUid(), intermediateModule21Fb.getUid(), intermediateModule321Fb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.CALL_CHAIN));
		final var upperBoundFb = createFunctionalBlock(UUID.randomUUID(), "UpperBound", List.of(upperBoundModuleFb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_UPPER_BOUND));
		final var lowerBoundFb1 = createFunctionalBlock(UUID.randomUUID(), "LowerBound1", List.of(lowerBoundModule1Fb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_LOWER_BOUND,
						FunctionalBlockFlag.RA_ACCESS_TYPE.name(), Set.of("READ", "WRITE")));
		final var lowerBoundFb2 = createFunctionalBlock(UUID.randomUUID(), "LowerBound2", List.of(lowerBoundModule2Fb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_LOWER_BOUND,
						FunctionalBlockFlag.RA_ACCESS_TYPE.name(), Set.of("READ")));
		final var lowerBoundFb3 = createFunctionalBlock(UUID.randomUUID(), "LowerBound3", List.of(lowerBoundModule3Fb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_LOWER_BOUND,
						FunctionalBlockFlag.RA_ACCESS_TYPE.name(), Set.of("WRITE")));
		final var accessBlock1Fb = createFunctionalBlock(UUID.randomUUID(), "AccessModule1", List.of(accessModule1Fb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_ACCESS_MODULE));
		final var accessBlock2Fb = createFunctionalBlock(UUID.randomUUID(), "AccessModule2", List.of(accessModule2Fb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_ACCESS_MODULE));
		final var accessBlock3Fb = createFunctionalBlock(UUID.randomUUID(), "AccessModule3", List.of(accessModule3Fb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_ACCESS_MODULE));
		final var reachabilityBlock = createFunctionalBlock(UUID.randomUUID(), "ReachabilityBlock",
				List.of(upperBoundFb.getUid(), lowerBoundFb1.getUid(), lowerBoundFb2.getUid(), lowerBoundFb3.getUid(),
						accessBlock1Fb.getUid(), accessBlock2Fb.getUid(), accessBlock3Fb.getUid(), intermediateModule1Fb.getUid(),
						intermediateModule2Fb.getUid(), intermediateModule3Fb.getUid(), intermediateModule21Fb.getUid(), intermediateModule321Fb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));

		when(Assert.assertNotNull(functionalBlockService).find(any(BuildingConsumer.class))).thenReturn(
				List.of(upperBoundFb, lowerBoundFb1, lowerBoundFb2, lowerBoundFb3, accessBlock1Fb, accessBlock2Fb, accessBlock3Fb,
						intermediateModule1Fb, intermediateModule2Fb, intermediateModule3Fb, intermediateModule21Fb, intermediateModule321Fb, callChainBlock));

		mockGeneratedFrom(functionalBlockService, Map.ofEntries(
				entry(upperBoundModuleFb.getUid(), GeneratedFrom.fromModule(upperBoundModule.getLinkHash(),
						upperBoundModule.getContentHash().toString(), upperBoundModule.getDependencyHash().orElse(null))),
				entry(lowerBoundModule1Fb.getUid(), GeneratedFrom.fromModule(lowerBoundModule1.getLinkHash(),
						lowerBoundModule1.getContentHash().toString(), lowerBoundModule1.getDependencyHash().orElse(null))),
				entry(lowerBoundModule2Fb.getUid(), GeneratedFrom.fromModule(lowerBoundModule2.getLinkHash(),
						lowerBoundModule2.getContentHash().toString(), lowerBoundModule2.getDependencyHash().orElse(null))),
				entry(lowerBoundModule3Fb.getUid(), GeneratedFrom.fromModule(lowerBoundModule3.getLinkHash(),
						lowerBoundModule3.getContentHash().toString(), lowerBoundModule3.getDependencyHash().orElse(null))),
				entry(accessModule1Fb.getUid(), GeneratedFrom.fromModule(accessModule1.getLinkHash(),
						accessModule1.getContentHash().toString(), accessModule1.getDependencyHash().orElse(null))),
				entry(accessModule2Fb.getUid(), GeneratedFrom.fromModule(accessModule2.getLinkHash(),
						accessModule2.getContentHash().toString(), accessModule2.getDependencyHash().orElse(null))),
				entry(accessModule3Fb.getUid(), GeneratedFrom.fromModule(accessModule3.getLinkHash(),
						accessModule3.getContentHash().toString(), accessModule3.getDependencyHash().orElse(null))),
				entry(intermediateModule1Fb.getUid(), GeneratedFrom.fromModule(intermediateModule1.getLinkHash(),
						intermediateModule1.getContentHash().toString(), intermediateModule1.getDependencyHash().orElse(null))),
				entry(intermediateModule2Fb.getUid(), GeneratedFrom.fromModule(intermediateModule2.getLinkHash(),
						intermediateModule2.getContentHash().toString(), intermediateModule2.getDependencyHash().orElse(null))),
				entry(intermediateModule3Fb.getUid(), GeneratedFrom.fromModule(intermediateModule3.getLinkHash(),
						intermediateModule3.getContentHash().toString(), intermediateModule3.getDependencyHash().orElse(null))),
				entry(intermediateModule21Fb.getUid(), GeneratedFrom.fromModule(intermediateModule21.getLinkHash(),
						intermediateModule21.getContentHash().toString(), intermediateModule21.getDependencyHash().orElse(null))),
				entry(intermediateModule321Fb.getUid(), GeneratedFrom.fromModule(intermediateModule321.getLinkHash(),
						intermediateModule321.getContentHash().toString(), intermediateModule321.getDependencyHash().orElse(null)))
		));

		moduleIdsByLinkHashMap = Map.ofEntries(
				entry(upperBoundModule.getLinkHash(), upperBoundModule.identity()),
				entry(lowerBoundModule1.getLinkHash(), lowerBoundModule1.identity()),
				entry(lowerBoundModule2.getLinkHash(), lowerBoundModule2.identity()),
				entry(lowerBoundModule3.getLinkHash(), lowerBoundModule3.identity()),
				entry(accessModule1.getLinkHash(), accessModule1.identity()),
				entry(accessModule2.getLinkHash(), accessModule2.identity()),
				entry(accessModule3.getLinkHash(), accessModule3.identity()),
				entry(intermediateModule1.getLinkHash(), intermediateModule1.identity()),
				entry(intermediateModule2.getLinkHash(), intermediateModule2.identity()),
				entry(intermediateModule3.getLinkHash(), intermediateModule3.identity()),
				entry(intermediateModule21.getLinkHash(), intermediateModule21.identity()),
				entry(intermediateModule321.getLinkHash(), intermediateModule321.identity())
		);

		when(functionalBlockService.getLinks(callChainBlock.getUid())).thenReturn(
				List.of(
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), accessModule1Fb.getUid(), lowerBoundModule1Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), accessModule2Fb.getUid(), lowerBoundModule2Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), accessModule3Fb.getUid(), lowerBoundModule3Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), accessModule2Fb.getUid(), accessModule1Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), intermediateModule1Fb.getUid(), accessModule1Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), intermediateModule2Fb.getUid(), accessModule2Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), intermediateModule21Fb.getUid(), accessModule2Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), intermediateModule3Fb.getUid(), accessModule3Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), intermediateModule3Fb.getUid(), intermediateModule321Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), intermediateModule321Fb.getUid(), intermediateModule2Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), upperBoundModuleFb.getUid(), intermediateModule1Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), upperBoundModuleFb.getUid(), intermediateModule2Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), upperBoundModuleFb.getUid(), intermediateModule21Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), upperBoundModuleFb.getUid(), intermediateModule3Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null)
				));
		when(functionalBlockService.getLinks(reachabilityBlock.getUid())).thenReturn(
				List.of(
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock.getUid(), accessBlock1Fb.getUid(), lowerBoundFb1.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), FunctionalBlockLinkType.RA_ACCESS,
								FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name(), Set.of("READ", "WRITE")), null),
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock.getUid(), accessBlock2Fb.getUid(), lowerBoundFb2.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), FunctionalBlockLinkType.RA_ACCESS,
								FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name(), Set.of("READ")), null),
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock.getUid(), accessBlock3Fb.getUid(), lowerBoundFb3.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), FunctionalBlockLinkType.RA_ACCESS,
								FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name(), Set.of("WRITE")), null)));

		final ReachabilityDataComputation reachabilityDataComputation = new ReachabilityDataComputation(functionalBlockService, moduleService);
		final var computationResult = reachabilityDataComputation.computeBatched(List.of(reachabilityBlock), new NullProgressMonitor());
		assertEquals(Set.of(reachabilityBlock), computationResult.keySet());
		assertEquals(3, computationResult.get(reachabilityBlock).size());
		assertTrue(computationResult.get(reachabilityBlock).stream().map(p -> p.lowerBoundModuleId.getNonNull()).toList().containsAll(
				List.of(EntityId.of(lowerBoundModule1.getId()), EntityId.of(lowerBoundModule2.getId()), EntityId.of(lowerBoundModule3.getId()))));

		for (final ReachabilityDataPojoPrototype reachabilityData : computationResult.get(reachabilityBlock)) {
			if (reachabilityData.lowerBoundModuleId.getNonNull().equals(lowerBoundModule1.identity())) {
				/* WMIN-14071: added "accessModule2" as expected "intermediate module" for "lower bound 1", as I think this is correct
				 * "access module 2" is on the call chain, but not the access module for "lower bound 1" */
				assertThat(reachabilityData.intermediateModules.getNonNull(), containsInAnyOrder(
						intermediateModule1.identity(), intermediateModule2.identity(), intermediateModule21.identity(),
						intermediateModule321.identity(), intermediateModule3.identity(), accessModule2.identity()
				));
			} else if (reachabilityData.lowerBoundModuleId.getNonNull().equals(lowerBoundModule2.identity())) {
				assertThat(reachabilityData.intermediateModules.getNonNull(), containsInAnyOrder(
						intermediateModule2.identity(),intermediateModule21.identity(), intermediateModule321.identity(), intermediateModule3.identity()
				));
			} else if (reachabilityData.lowerBoundModuleId.getNonNull().equals(lowerBoundModule3.identity())) {
				assertThat(reachabilityData.intermediateModules.getNonNull(), containsInAnyOrder(
						intermediateModule3.identity()
				));
			} else {
				fail("Unexpected access module in result: " + reachabilityData.accessModuleId.getNonNull());
			}
		}
	}

	/*
	 *             +-------->im1-------> im2 ------->am1----------> lb1
	 *      ub1 -> |                     | ^
	 * 	           |      +--------------+ |
	 *             +------+->im3-------> im4 ------>am2----------> lb2
	 */
	@Test
	void testReachabilityDataIntermediateModulesWithCycleInCallChain() {
		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final ModuleService moduleService = mockModuleService();

		final var upperBoundModule = createModule(1L, "UpperBoundModule", "src/upperBound.cbl", "LinkHashUpperBound");
		final var lowerBoundModule1 = createModule(2L, "LowerBoundModule1", "src/lowerBound1.cbl", "LinkHashLowerBound1");
		final var lowerBoundModule2 = createModule(3L, "LowerBoundModule2", "src/lowerBound2.cbl", "LinkHashLowerBound2");
		final var accessModule1 = createModule(5L, "AccessModule1", "src/access1.cbl", "LinkHashAccess1");
		final var accessModule2 = createModule(6L, "AccessModule2", "src/access2.cbl", "LinkHashAccess2");
		final var intermediateModule1 = createModule(8L, "IntermediateModule1", "src/intermediate1.cbl", "LinkHashIntermediate1");
		final var intermediateModule2 = createModule(9L, "IntermediateModule2", "src/intermediate2.cbl", "LinkHashIntermediate2");
		final var intermediateModule3 = createModule(8L, "IntermediateModule3", "src/intermediate3.cbl", "LinkHashIntermediate3");
		final var intermediateModule4 = createModule(9L, "IntermediateModule4", "src/intermediate4.cbl", "LinkHashIntermediate4");

		final var upperBoundModuleFb = createFunctionalBlock(UUID.nameUUIDFromBytes("UpperBoundModule".getBytes()), "UpperBoundModule", Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var lowerBoundModule1Fb = createFunctionalBlock(UUID.nameUUIDFromBytes("LowerBoundModule1".getBytes()), "LowerBoundModule1", Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var lowerBoundModule2Fb = createFunctionalBlock(UUID.nameUUIDFromBytes("LowerBoundModule2".getBytes()), "LowerBoundModule2", Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var accessModule1Fb = createFunctionalBlock(UUID.nameUUIDFromBytes("AccessModule1".getBytes()), "AccessModule1", Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var accessModule2Fb = createFunctionalBlock(UUID.nameUUIDFromBytes("AccessModule2".getBytes()), "AccessModule2", Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var intermediateModule1Fb = createFunctionalBlock(UUID.nameUUIDFromBytes("IntermediateModule1".getBytes()), "IntermediateModule1",
				Collections.emptyList(), Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var intermediateModule2Fb = createFunctionalBlock(UUID.nameUUIDFromBytes("IntermediateModule2".getBytes()), "IntermediateModule2",
				Collections.emptyList(), Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var intermediateModule3Fb = createFunctionalBlock(UUID.nameUUIDFromBytes("IntermediateModule3".getBytes()), "IntermediateModule3",
				Collections.emptyList(), Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var intermediateModule4Fb = createFunctionalBlock(UUID.nameUUIDFromBytes("IntermediateModule4".getBytes()), "IntermediateModule4",
				Collections.emptyList(), Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));

		final var callChainBlock = createFunctionalBlock(UUID.randomUUID(), "CallChainBlock",
				List.of(upperBoundModuleFb.getUid(), lowerBoundModule1Fb.getUid(), lowerBoundModule2Fb.getUid(), accessModule1Fb.getUid(),
						accessModule2Fb.getUid(), intermediateModule1Fb.getUid(), intermediateModule2Fb.getUid(), intermediateModule3Fb.getUid(),
						intermediateModule4Fb.getUid()), Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.CALL_CHAIN));
		final var upperBoundFb = createFunctionalBlock(UUID.randomUUID(), "UpperBound", List.of(upperBoundModuleFb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_UPPER_BOUND));
		final var lowerBoundFb1 = createFunctionalBlock(UUID.randomUUID(), "LowerBound1", List.of(lowerBoundModule1Fb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_LOWER_BOUND,
						FunctionalBlockFlag.RA_ACCESS_TYPE.name(), Set.of("READ", "WRITE")));
		final var lowerBoundFb2 = createFunctionalBlock(UUID.randomUUID(), "LowerBound2", List.of(lowerBoundModule2Fb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_LOWER_BOUND,
						FunctionalBlockFlag.RA_ACCESS_TYPE.name(), Set.of("READ")));
		final var accessBlock1Fb = createFunctionalBlock(UUID.randomUUID(), "AccessModule1", List.of(accessModule1Fb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_ACCESS_MODULE));
		final var accessBlock2Fb = createFunctionalBlock(UUID.randomUUID(), "AccessModule2", List.of(accessModule2Fb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_ACCESS_MODULE));
		final var reachabilityBlock = createFunctionalBlock(UUID.randomUUID(), "ReachabilityBlock",
				List.of(upperBoundFb.getUid(), lowerBoundFb1.getUid(), lowerBoundFb2.getUid(), accessBlock1Fb.getUid(), accessBlock2Fb.getUid(),
						callChainBlock.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));

		when(Assert.assertNotNull(functionalBlockService).find(any(BuildingConsumer.class))).thenReturn(
				List.of(upperBoundFb, lowerBoundFb1, lowerBoundFb2, accessBlock1Fb, accessBlock2Fb, callChainBlock)
		);

		mockGeneratedFrom(functionalBlockService, Map.ofEntries(
				entry(upperBoundModuleFb.getUid(), GeneratedFrom.fromModule(upperBoundModule.getLinkHash(),
						upperBoundModule.getContentHash().toString(), upperBoundModule.getDependencyHash().orElse(null))),
				entry(lowerBoundModule1Fb.getUid(), GeneratedFrom.fromModule(lowerBoundModule1.getLinkHash(),
						lowerBoundModule1.getContentHash().toString(), lowerBoundModule1.getDependencyHash().orElse(null))),
				entry(lowerBoundModule2Fb.getUid(), GeneratedFrom.fromModule(lowerBoundModule2.getLinkHash(),
						lowerBoundModule2.getContentHash().toString(), lowerBoundModule2.getDependencyHash().orElse(null))),
				entry(accessModule1Fb.getUid(), GeneratedFrom.fromModule(accessModule1.getLinkHash(),
						accessModule1.getContentHash().toString(), accessModule1.getDependencyHash().orElse(null))),
				entry(accessModule2Fb.getUid(), GeneratedFrom.fromModule(accessModule2.getLinkHash(),
						accessModule2.getContentHash().toString(), accessModule2.getDependencyHash().orElse(null))),
				entry(intermediateModule1Fb.getUid(), GeneratedFrom.fromModule(intermediateModule1.getLinkHash(),
						intermediateModule1.getContentHash().toString(), intermediateModule1.getDependencyHash().orElse(null))),
				entry(intermediateModule2Fb.getUid(), GeneratedFrom.fromModule(intermediateModule2.getLinkHash(),
						intermediateModule2.getContentHash().toString(), intermediateModule2.getDependencyHash().orElse(null))),
				entry(intermediateModule3Fb.getUid(), GeneratedFrom.fromModule(intermediateModule3.getLinkHash(),
						intermediateModule3.getContentHash().toString(), intermediateModule3.getDependencyHash().orElse(null))),
				entry(intermediateModule4Fb.getUid(), GeneratedFrom.fromModule(intermediateModule4.getLinkHash(),
						intermediateModule4.getContentHash().toString(), intermediateModule4.getDependencyHash().orElse(null)))
		));

		moduleIdsByLinkHashMap = Map.ofEntries(
				entry(upperBoundModule.getLinkHash(), upperBoundModule.identity()),
				entry(lowerBoundModule1.getLinkHash(), lowerBoundModule1.identity()),
				entry(lowerBoundModule2.getLinkHash(), lowerBoundModule2.identity()),
				entry(accessModule1.getLinkHash(), accessModule1.identity()),
				entry(accessModule2.getLinkHash(), accessModule2.identity()),
				entry(intermediateModule1.getLinkHash(), intermediateModule1.identity()),
				entry(intermediateModule2.getLinkHash(), intermediateModule2.identity()),
				entry(intermediateModule3.getLinkHash(), intermediateModule3.identity()),
				entry(intermediateModule4.getLinkHash(), intermediateModule4.identity())
		);

		when(functionalBlockService.getLinks(callChainBlock.getUid())).thenReturn(
				List.of(
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), accessModule1Fb.getUid(), lowerBoundModule1Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), accessModule2Fb.getUid(), lowerBoundModule2Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), intermediateModule1Fb.getUid(), intermediateModule2Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), intermediateModule3Fb.getUid(), intermediateModule4Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), intermediateModule4Fb.getUid(), intermediateModule2Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), intermediateModule4Fb.getUid(), accessModule2Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), intermediateModule2Fb.getUid(), intermediateModule3Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), intermediateModule2Fb.getUid(), accessModule1Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), upperBoundModuleFb.getUid(), intermediateModule1Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), upperBoundModuleFb.getUid(), intermediateModule3Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null)
				));
		when(functionalBlockService.getLinks(reachabilityBlock.getUid())).thenReturn(
				List.of(
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock.getUid(), accessBlock1Fb.getUid(), lowerBoundFb1.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), FunctionalBlockLinkType.RA_ACCESS,
								FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name(), Set.of("READ", "WRITE")), null),
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock.getUid(), accessBlock2Fb.getUid(), lowerBoundFb2.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), FunctionalBlockLinkType.RA_ACCESS,
								FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name(), Set.of("READ")), null)));

		final ReachabilityDataComputation reachabilityDataComputation = new ReachabilityDataComputation(functionalBlockService, moduleService);
		final var computationResult = reachabilityDataComputation.computeBatched(List.of(reachabilityBlock), new NullProgressMonitor());
		assertEquals(Set.of(reachabilityBlock), computationResult.keySet());
		assertEquals(2, computationResult.get(reachabilityBlock).size());
		assertTrue(computationResult.get(reachabilityBlock).stream().map(p -> p.lowerBoundModuleId.getNonNull()).toList().containsAll(
				List.of(EntityId.of(lowerBoundModule1.getId()), EntityId.of(lowerBoundModule2.getId()))));

		for (final ReachabilityDataPojoPrototype reachabilityData : computationResult.get(reachabilityBlock)) {
			if (reachabilityData.lowerBoundModuleId.getNonNull().equals(lowerBoundModule1.identity())) {
				assertEquals(4, reachabilityData.intermediateModules.getNonNull().size());
				assertTrue(reachabilityData.intermediateModules.getNonNull().containsAll(Set.of(intermediateModule1.identity(),
						intermediateModule2.identity(), intermediateModule3.identity(), intermediateModule4.identity())));
			} else if (reachabilityData.lowerBoundModuleId.getNonNull().equals(lowerBoundModule2.identity())) {
				assertEquals(4, reachabilityData.intermediateModules.getNonNull().size());
				assertTrue(reachabilityData.intermediateModules.getNonNull().containsAll(Set.of(intermediateModule2.identity(),
						intermediateModule1.identity(), intermediateModule3.identity(), intermediateModule4.identity())));
			} else {
				fail("Unexpected access module in result: " + reachabilityData.accessModuleId.getNonNull());
			}
		}
	}

	/*
	 *      ub1 -> -------->im1-------> im2 ------->am1----------> lb1
	 *                       ^                       |
	 *                       +-----------------------+
	 */
	@Test
	void testReachabilityDataIntermediateModulesWithCycleInSinglePathCallChain() {
		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final ModuleService moduleService = mockModuleService();

		final var upperBoundModule = createModule(1L, "UpperBoundModule", "src/upperBound.cbl", "LinkHashUpperBound");
		final var lowerBoundModule1 = createModule(2L, "LowerBoundModule1", "src/lowerBound1.cbl", "LinkHashLowerBound1");
		final var accessModule1 = createModule(5L, "AccessModule1", "src/access1.cbl", "LinkHashAccess1");
		final var intermediateModule1 = createModule(8L, "IntermediateModule1", "src/intermediate1.cbl", "LinkHashIntermediate1");
		final var intermediateModule2 = createModule(9L, "IntermediateModule2", "src/intermediate2.cbl", "LinkHashIntermediate2");

		final var upperBoundModuleFb = createFunctionalBlock(UUID.nameUUIDFromBytes("UpperBoundModule".getBytes()), "UpperBoundModule", Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var lowerBoundModule1Fb = createFunctionalBlock(UUID.nameUUIDFromBytes("LowerBoundModule1".getBytes()), "LowerBoundModule1", Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var accessModule1Fb = createFunctionalBlock(UUID.nameUUIDFromBytes("AccessModule1".getBytes()), "AccessModule1", Collections.emptyList(),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var intermediateModule1Fb = createFunctionalBlock(UUID.nameUUIDFromBytes("IntermediateModule1".getBytes()), "IntermediateModule1",
				Collections.emptyList(), Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));
		final var intermediateModule2Fb = createFunctionalBlock(UUID.nameUUIDFromBytes("IntermediateModule2".getBytes()), "IntermediateModule2",
				Collections.emptyList(), Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.MODULE)));

		final var callChainBlock = createFunctionalBlock(UUID.randomUUID(), "CallChainBlock",
				List.of(upperBoundModuleFb.getUid(), lowerBoundModule1Fb.getUid(), accessModule1Fb.getUid(), intermediateModule1Fb.getUid(),
						intermediateModule2Fb.getUid()), Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.CALL_CHAIN));
		final var upperBoundFb = createFunctionalBlock(UUID.randomUUID(), "UpperBound", List.of(upperBoundModuleFb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_UPPER_BOUND));
		final var lowerBoundFb1 = createFunctionalBlock(UUID.randomUUID(), "LowerBound1", List.of(lowerBoundModule1Fb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_LOWER_BOUND,
						FunctionalBlockFlag.RA_ACCESS_TYPE.name(), Set.of("READ", "WRITE")));
		final var accessBlock1Fb = createFunctionalBlock(UUID.randomUUID(), "AccessModule1", List.of(accessModule1Fb.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), FunctionalBlockType.RA_ACCESS_MODULE));
		final var reachabilityBlock = createFunctionalBlock(UUID.randomUUID(), "ReachabilityBlock",
				List.of(upperBoundFb.getUid(), lowerBoundFb1.getUid(), accessBlock1Fb.getUid(), callChainBlock.getUid()),
				Map.of(FunctionalBlockFlag.TYPE.toString(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name())));

		when(Assert.assertNotNull(functionalBlockService).find(any(BuildingConsumer.class))).then(i -> {
			final BuildingConsumer<FunctionalBlockService.FunctionalBlockInquiryBuilder> builderConsumer = i.getArgument(0);
			final FunctionalBlockService.FunctionalBlockInquiryBuilder builder = Mockito.mock(FunctionalBlockService.FunctionalBlockInquiryBuilder.class);
			final ArgumentCaptor<Collection<UUID>> uidCaptor = ArgumentCaptor.forClass(Collection.class);

			builderConsumer.prepare(builder);
			Mockito.verify(builder).byUids(uidCaptor.capture());
			final Collection<UUID> uids = uidCaptor.getValue();

			if (uids.equals(callChainBlock.getChildren())) {
				return List.of(upperBoundModuleFb, lowerBoundModule1Fb, accessModule1Fb, intermediateModule1Fb, intermediateModule2Fb);
			} else if (uids.equals(reachabilityBlock.getChildren())) {
				return List.of(upperBoundFb, lowerBoundFb1, accessBlock1Fb, callChainBlock);
			} else {
				return Collections.emptyList();
			}
		});

		mockGeneratedFrom(functionalBlockService, Map.of(
				upperBoundModuleFb.getUid(), GeneratedFrom.fromModule(upperBoundModule.getLinkHash(),
						upperBoundModule.getContentHash().toString(), upperBoundModule.getDependencyHash().orElse(null)),
				lowerBoundModule1Fb.getUid(), GeneratedFrom.fromModule(lowerBoundModule1.getLinkHash(),
						lowerBoundModule1.getContentHash().toString(), lowerBoundModule1.getDependencyHash().orElse(null)),
				accessModule1Fb.getUid(), GeneratedFrom.fromModule(accessModule1.getLinkHash(),
						accessModule1.getContentHash().toString(), accessModule1.getDependencyHash().orElse(null)),
				intermediateModule1Fb.getUid(), GeneratedFrom.fromModule(intermediateModule1.getLinkHash(),
						intermediateModule1.getContentHash().toString(), intermediateModule1.getDependencyHash().orElse(null)),
				intermediateModule2Fb.getUid(), GeneratedFrom.fromModule(intermediateModule2.getLinkHash(),
						intermediateModule2.getContentHash().toString(), intermediateModule2.getDependencyHash().orElse(null))
		));

		moduleIdsByLinkHashMap = Map.of(
				upperBoundModule.getLinkHash(), upperBoundModule.identity(),
				lowerBoundModule1.getLinkHash(), lowerBoundModule1.identity(),
				accessModule1.getLinkHash(), accessModule1.identity(),
				intermediateModule1.getLinkHash(), intermediateModule1.identity(),
				intermediateModule2.getLinkHash(), intermediateModule2.identity()
		);

		when(functionalBlockService.getLinks(callChainBlock.getUid())).thenReturn(
				List.of(
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), accessModule1Fb.getUid(), lowerBoundModule1Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), accessModule1Fb.getUid(), intermediateModule1Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), intermediateModule1Fb.getUid(), intermediateModule2Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), intermediateModule2Fb.getUid(), accessModule1Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null),
						new FunctionalBlockLink(UUID.randomUUID(), callChainBlock.getUid(), upperBoundModuleFb.getUid(), intermediateModule1Fb.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN,
								FunctionalBlockLinkType.DIRECTED)), null)
				));

		when(functionalBlockService.getLinks(reachabilityBlock.getUid())).thenReturn(
				List.of(
						new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock.getUid(), accessBlock1Fb.getUid(), lowerBoundFb1.getUid(),
								null, Map.of(FunctionalBlockLinkFlag.TYPE.name(), FunctionalBlockLinkType.RA_ACCESS,
								FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name(), Set.of("READ", "WRITE")), null)));

		final ReachabilityDataComputation reachabilityDataComputation = new ReachabilityDataComputation(functionalBlockService, moduleService);
		final var computationResult = reachabilityDataComputation.computeBatched(List.of(reachabilityBlock), new NullProgressMonitor());
		assertEquals(Set.of(reachabilityBlock), computationResult.keySet());
		assertEquals(1, computationResult.get(reachabilityBlock).size());
		final ReachabilityDataPojoPrototype prototype = computationResult.get(reachabilityBlock).iterator().next();
		assertEquals(lowerBoundModule1.identity(), prototype.lowerBoundModuleId.getNonNull());
		assertEquals(accessModule1.identity(), prototype.accessModuleId.getNonNull());
		assertEquals(Set.of(intermediateModule1.identity(), intermediateModule2.identity()), prototype.intermediateModules.getNonNull());
	}
}

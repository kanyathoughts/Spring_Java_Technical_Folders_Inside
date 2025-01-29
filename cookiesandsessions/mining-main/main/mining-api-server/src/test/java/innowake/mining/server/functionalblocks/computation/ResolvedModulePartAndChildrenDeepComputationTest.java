/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.functionalblocks.computation;

import innowake.lib.core.lang.Assert;
import innowake.lib.job.api.NullProgressMonitor;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.ModulePart;
import innowake.mining.shared.entities.functionalblocks.ResolvedModulePart;
import innowake.mining.shared.model.ModuleLocation;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * test Resolving Module part computation.
 */
class ResolvedModulePartAndChildrenDeepComputationTest {
	
	private static final Long PROJECT_ID = 1l;
	private static final EntityId PROJECT = EntityId.of(UUID.randomUUID(), PROJECT_ID);
	
	private static FunctionalBlockPojo functionalBlock ;
	private static FunctionalBlockPojo childBlock ;

	@BeforeAll
	static void init() {
		final List<ModulePart> moduleParts = new ArrayList<>();
		moduleParts.add(new ModulePart("linkHash", null));
		moduleParts.add(new ModulePart("linkHash", new ModuleLocation(10, 15)));
		moduleParts.add(new ModulePart("linkHash2", new ModuleLocation(20, 5)));
		moduleParts.add(new ModulePart("linkHash2", new ModuleLocation(11, 10)));
		moduleParts.add(new ModulePart("linkHash3", new ModuleLocation(11, 5)));
		moduleParts.add(new ModulePart("linkHash3", new ModuleLocation(15, 5)));
		moduleParts.add(new ModulePart("linkHash3", new ModuleLocation(10, 5)));
		moduleParts.add(new ModulePart("linkHash3", new ModuleLocation(30, 5)));
		
		final List<ModulePart> modulePartchild = new ArrayList<>();
		modulePartchild.add(new ModulePart("linkHash4", null));
		modulePartchild.add(new ModulePart("linkHash4", new ModuleLocation(10, 15)));
		modulePartchild.add(new ModulePart("linkHash5", new ModuleLocation(20, 5)));
		modulePartchild.add(new ModulePart("linkHash5", new ModuleLocation(11, 10)));
		
		final UUID functionalblockUID = UUID.randomUUID();
		final UUID childUID = UUID.randomUUID();

		final List<UUID> parentUIDs = new ArrayList<>();
		parentUIDs.add(functionalblockUID);

		final List<UUID> childUIDs = new ArrayList<>();
		childUIDs.add(childUID);


		functionalBlock = new FunctionalBlockPojo(functionalblockUID, null, PROJECT, moduleParts,
				null, childUIDs, "block1", null, Collections.emptyMap(), null);
		childBlock = new FunctionalBlockPojo(childUID, null, PROJECT, modulePartchild,
				parentUIDs, new ArrayList<>(), "blockChild", null, Collections.emptyMap(), null);
	}
	
	@Test
	void testComputeResolvedModulePart() {
		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final var moduleService = Mockito.mock(ModuleService.class);

		when(moduleService.findModuleIdsByLinkHash(any())).thenReturn(Map.of(
				"linkHash", EntityId.of(1L),
				"linkHash2", EntityId.of(2L),
				"linkHash3", EntityId.of(3L),
				"linkHash4", EntityId.of(4L),
				"linkHash5", EntityId.of(5L)
		));

		final Map<UUID, List<UUID>> childMap = Map.of(functionalBlock.getUid(), List.of(childBlock.getUid()));
		when(Assert.assertNotNull(functionalBlockService).findChildrenIdsDeep(anyList(), anyInt())).thenReturn(childMap);

		final Map<UUID, List<ModulePart>> modulePartMap = Map.of(childBlock.getUid(), childBlock.getModuleParts());
		when(Assert.assertNotNull(functionalBlockService).getModuleParts(anyList())).thenReturn(modulePartMap);

		final ResolvedModulePartAndChildrenDeepComputation resolvedModulePartComputation =
				new ResolvedModulePartAndChildrenDeepComputation(functionalBlockService, moduleService);

		final Map<FunctionalBlockPojo, Pair<List<UUID>, List<ResolvedModulePart>>> result =
				resolvedModulePartComputation.computeBatched(List.of(functionalBlock), new NullProgressMonitor());
		assertEquals(Set.of(functionalBlock), result.keySet());
		final List<UUID> children = result.get(functionalBlock).getLeft();
		assertEquals(List.of(childBlock.getUid()), children);

		final List<ResolvedModulePart> resolvedModuleParts = result.get(functionalBlock).getRight();
		assertEquals(6, resolvedModuleParts.size(), "the number of ResolvedModuleParts should be correct");
		final List<ResolvedModulePart> sortedResolvedModuleParts = resolvedModuleParts.stream()
				.sorted(Comparator.<ResolvedModulePart, Long>comparing(rmp -> rmp.getModuleId().getNid())
						.thenComparing(part -> part.getLocation().map(ModuleLocation::getOffset).orElse(-1)))
				.collect(Collectors.toList());
		final List<ResolvedModulePart> expectedLocations = new ArrayList<>();
		expectedLocations.add(new ResolvedModulePart(EntityId.of(1L), null));
		expectedLocations.add(new ResolvedModulePart(EntityId.of(2L), new ModuleLocation(11, 14)));
		expectedLocations.add(new ResolvedModulePart(EntityId.of(3L), new ModuleLocation(10, 10)));
		expectedLocations.add(new ResolvedModulePart(EntityId.of(3L), new ModuleLocation(30, 5)));
		expectedLocations.add(new ResolvedModulePart(EntityId.of(4L), null));
		expectedLocations.add(new ResolvedModulePart(EntityId.of(5L), new ModuleLocation(11, 14)));
		
		for(int i = 0 ; i < sortedResolvedModuleParts.size(); i++) {
			assertEquals(expectedLocations.get(i) , sortedResolvedModuleParts.get(i), "expected ResolvedModulePart must be equal to actual");
		}
	}

	@Test
	void testPersist() {
		final FunctionalBlockService functionalBlockService = mock(FunctionalBlockService.class);
		final ModuleService moduleService = mock(ModuleService.class);
		final ResolvedModulePartAndChildrenDeepComputation resolvedModulePartComputation = new ResolvedModulePartAndChildrenDeepComputation(functionalBlockService, moduleService);

		final UUID uuid = UUID.randomUUID();
		final List<UUID> children = List.of(UUID.randomUUID());
		final List<ResolvedModulePart> moduleParts = List.of(new ResolvedModulePart(EntityId.of(42L), new ModuleLocation(123, 456)));
		resolvedModulePartComputation.persist(uuid, Pair.of(children, moduleParts));

		/* verify that both, childrenDeep and ResolvedModuleParts is persisted */
		verify(functionalBlockService).setChildrenDeep(uuid, children);
		verify(functionalBlockService).setResolvedModuleParts(uuid, moduleParts);
	}
}

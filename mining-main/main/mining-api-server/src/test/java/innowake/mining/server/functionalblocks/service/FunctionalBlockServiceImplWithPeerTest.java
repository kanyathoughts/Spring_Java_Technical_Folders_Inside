/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.service;

import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService.FunctionalBlockInquiryBuilder;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.ResolvedModulePart;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for {@link FunctionalBlockInquiryBuilder#withPeer(BuildingConsumer)} and {@link FunctionalBlockInquiryBuilder#notWithPeer(BuildingConsumer)}.
 */
class FunctionalBlockServiceImplWithPeerTest extends DatabaseRelatedTest {

	@Autowired
	private FunctionalBlockServiceImpl blockServiceImpl;

	@Autowired
	private ModuleService moduleService;

	private ProjectPojo project;

	/**
	 * Tests that blocks that reference the same module(s) are peers, but blocks that do not reference the same module(s) are not peers.
	 */
	@Test
	void testFilterWithPeer() {
		/* Module must exist or else we can not setResolvedModulePart(), so we must actually insert a Module */
		final EntityId moduleId = createModule(project.identity());
		final EntityId otherModuleId = createModule(project.identity());

		final UUID block1Uid = blockServiceImpl.create(new FunctionalBlockPojoPrototype()
				.setProject(project.identity())
				.setName("Block 1")
				.setDescription("Block 1"));

		final UUID block2Uid = blockServiceImpl.create(new FunctionalBlockPojoPrototype()
				.setProject(project.identity())
				.setName("Block 2")
				.setDescription("Block 2"));

		/* adding this block just to check that it is NOT returned from the query */
		final UUID unrelatedBlockUid = blockServiceImpl.create(new FunctionalBlockPojoPrototype()
				.setProject(project.identity())
				.setName("Unrelated Block")
				.setDescription("This block is not a peer"));

		blockServiceImpl.setResolvedModuleParts(block1Uid, Collections.singleton(new ResolvedModulePart(moduleId)));
		blockServiceImpl.setResolvedModuleParts(block2Uid, Collections.singleton(new ResolvedModulePart(moduleId)));
		blockServiceImpl.setResolvedModuleParts(unrelatedBlockUid, Collections.singleton(new ResolvedModulePart(otherModuleId)));

		final List<UUID> peersOfBlock1 = blockServiceImpl.find(q -> q.withPeer(peer -> peer.byUid(block1Uid))).stream()
				.map(FunctionalBlockPojo::getUid).collect(Collectors.toList());
		assertEquals(Collections.singletonList(block2Uid), peersOfBlock1, "expected to find block2 as peer of block1");

		final List<UUID> peersOfBlock2 = blockServiceImpl.find(q -> q.withPeer(peer -> peer.byUid(block2Uid))).stream()
				.map(FunctionalBlockPojo::getUid)
				.collect(Collectors.toList());
		assertEquals(Collections.singletonList(block1Uid), peersOfBlock2, "expected to find block1 as peer of block2");

		final Set<UUID> notPeersOfBlock1 = blockServiceImpl.find(q -> q.notWithPeer(peer -> peer.byUid(block1Uid))).stream()
				.map(FunctionalBlockPojo::getUid).collect(Collectors.toSet());
		assertTrue(notPeersOfBlock1.contains(unrelatedBlockUid), "expected to find unrelated block as NOT peer of block1");
		assertFalse(notPeersOfBlock1.contains(block2Uid), "expected NOT to find block2 as NOT peer of block1");

		final Set<UUID> notPeersOfBlock2 = blockServiceImpl.find(q -> q.notWithPeer(peer -> peer.byUid(block2Uid))).stream()
				.map(FunctionalBlockPojo::getUid).collect(Collectors.toSet());
		assertTrue(notPeersOfBlock2.contains(unrelatedBlockUid), "expected to find unrelated block as NOT peer of block2");
		assertFalse(notPeersOfBlock2.contains(block1Uid), "expected NOT to find block1 as NOT peer of block2");
	}

	/**
	 * Tests that blocks that are parent or child of each other are not peers, even when referencing the same module(s).
	 */
	@Test
	void testFilterWithPeerParent() {
		/* Module must exist or else we can not setResolvedModulePart(), so we must actually insert a Module */
		final EntityId moduleId = createModule(project.identity());

		final UUID child1Uid = blockServiceImpl.create(new FunctionalBlockPojoPrototype()
				.setProject(project.identity())
				.setName("Child of Block 1")
				.setDescription("This block is child of Block 1 and therefore not a peer"));

		final UUID block1Uid = blockServiceImpl.create(new FunctionalBlockPojoPrototype()
				.setProject(project.identity())
				.setName("Block 1")
				.setDescription("Block 1")
				.setChildren(List.of(child1Uid)));

		final UUID block2Uid = blockServiceImpl.create(new FunctionalBlockPojoPrototype()
				.setProject(project.identity())
				.setName("Block 2")
				.setDescription("Block 2"));

		blockServiceImpl.setResolvedModuleParts(block1Uid, Collections.singleton(new ResolvedModulePart(moduleId)));
		blockServiceImpl.setResolvedModuleParts(block2Uid, Collections.singleton(new ResolvedModulePart(moduleId)));
		blockServiceImpl.setResolvedModuleParts(child1Uid, Collections.singleton(new ResolvedModulePart(moduleId)));
		blockServiceImpl.setChildrenDeep(block1Uid, List.of(child1Uid));

		final Set<UUID> peersOfBlock1 = blockServiceImpl.find(q -> q.withPeer(peer -> peer.byUid(block1Uid))).stream()
				.map(FunctionalBlockPojo::getUid)
				.collect(Collectors.toSet());
		assertEquals(Collections.singleton(block2Uid), peersOfBlock1, "expected to find only block2 as peer of block1");

		final Set<UUID> peersOfChild1 = blockServiceImpl.find(q -> q.withPeer(peer -> peer.byUid(child1Uid))).stream()
				.map(FunctionalBlockPojo::getUid)
				.collect(Collectors.toSet());
		assertEquals(Collections.singleton(block2Uid), peersOfChild1, "expected to find only block2 as peer of child1");

		final Set<UUID> peersOfBlock2 = blockServiceImpl.find(q -> q.withPeer(peer -> peer.byUid(block2Uid))).stream()
				.map(FunctionalBlockPojo::getUid)
				.collect(Collectors.toSet());
		assertEquals(Set.of(block1Uid, child1Uid), peersOfBlock2, "expected to find block1 and child1 as peer of block2");
	}

	/**
	 * Tests that, when location is specified, blocks are peers only when referencing the same or overlapping location(s) within the same module.
	 */
	@Test
	void testFilterWithPeerOverlap() {
		/* Module must exist or else we can not setResolvedModulePart(), so we must actually insert a Module */
		final EntityId moduleId = createModule(project.identity());

		final UUID blockUid = blockServiceImpl.create(new FunctionalBlockPojoPrototype()
				.setProject(project.identity())
				.setName("Block")
				.setDescription("Block"));

		final UUID overlappingBlockUid = blockServiceImpl.create(new FunctionalBlockPojoPrototype()
				.setProject(project.identity())
				.setName("Overlapping Block")
				.setDescription("Overlapping Block"));

		final UUID notOverlappingBlockUid = blockServiceImpl.create(new FunctionalBlockPojoPrototype()
				.setProject(project.identity())
				.setName("Not Overlapping Block")
				.setDescription("Not Overlapping Block"));

		blockServiceImpl.setResolvedModuleParts(blockUid, Collections.singleton(new ResolvedModulePart(moduleId, new ModuleLocation(100, 100))));
		blockServiceImpl.setResolvedModuleParts(overlappingBlockUid, Collections.singleton(new ResolvedModulePart(moduleId, new ModuleLocation(150, 100))));
		blockServiceImpl.setResolvedModuleParts(notOverlappingBlockUid, Collections.singleton(new ResolvedModulePart(moduleId, new ModuleLocation(300, 100))));

		final Set<UUID> peersOfBlock = blockServiceImpl.find(q -> q.withPeer(peer -> peer.byUid(blockUid))).stream()
				.map(FunctionalBlockPojo::getUid)
				.collect(Collectors.toSet());
		assertEquals(Collections.singleton(overlappingBlockUid), peersOfBlock, "expected to find only overlappingBlock as peer of block");

		final Set<UUID> peersOfOverlappingBlock = blockServiceImpl.find(q -> q.withPeer(peer -> peer.byUid(overlappingBlockUid))).stream()
				.map(FunctionalBlockPojo::getUid)
				.collect(Collectors.toSet());
		assertEquals(Collections.singleton(blockUid), peersOfOverlappingBlock, "expected to find only block as peer of overlappingBlock");

		final Set<UUID> peersOfNotOverlappingBlock = blockServiceImpl.find(q -> q.withPeer(peer -> peer.byUid(notOverlappingBlockUid))).stream()
				.map(FunctionalBlockPojo::getUid)
				.collect(Collectors.toSet());
		assertEquals(Collections.emptySet(), peersOfNotOverlappingBlock, "expected to find no blocks as peers of notOverlappingBlock");
	}

	@BeforeAll
	void setUp() {
		project = createProject(1L);
	}

	private ProjectPojo createProject(final Long clientId) {
		return projectService.create(new ProjectPojoPrototype().setName("Test Project" + clientId).setClient(EntityId.of(clientId)).setNatures(Collections.emptySet()));
	}

	private EntityId createModule(final EntityId projectId) {
		return moduleService.create(new ModulePojoPrototype()
				.setProject(projectId)
				.setName("Test Module")
				.setTechnology(Technology.COBOL)
				.setType(Type.PROGRAM)
				.setOrigin(Origin.CUSTOM)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setCreator(Creator.DISCOVERY));
	}
}

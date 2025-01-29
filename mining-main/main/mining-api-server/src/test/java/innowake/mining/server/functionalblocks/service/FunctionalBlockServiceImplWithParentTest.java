/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.service;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService.FunctionalBlockInquiryBuilder;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * Tests for {@link FunctionalBlockInquiryBuilder#withParent(BuildingConsumer)} and {@link FunctionalBlockInquiryBuilder#notWithParent(BuildingConsumer)}.
 */
class FunctionalBlockServiceImplWithParentTest extends DatabaseRelatedTest {

	@Autowired
	private FunctionalBlockServiceImpl blockServiceImpl;

	private ProjectPojo project;

	@Test
	void testFilterWithParent() {
		final UUID child1Uid = blockServiceImpl.create(new FunctionalBlockPojoPrototype()
				.setProject(project.identity())
				.setName("Child 1 Block")
				.setDescription("Child 1 Block"));
		final UUID child2Uid = blockServiceImpl.create(new FunctionalBlockPojoPrototype()
				.setProject(project.identity())
				.setName("Child 2 Block")
				.setDescription("Child 2 Block"));

		final UUID parentUid = blockServiceImpl.create(new FunctionalBlockPojoPrototype()
				.setProject(project.identity())
				.setName("Parent Block")
				.setDescription("Parent Block")
						.setChildren(List.of(child1Uid, child2Uid))
				.setFlags(Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP))));

		final UUID parentOfParentUid = blockServiceImpl.create(new FunctionalBlockPojoPrototype()
				.setProject(project.identity())
				.setName("Parent of parent block")
				.setDescription("Parent of parent Block")
				.setChildren(List.of(parentUid))
				.setFlags(Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP))));

		final UUID otherUid = blockServiceImpl.create(new FunctionalBlockPojoPrototype()
				.setProject(project.identity())
				.setName("Other Block")
				.setDescription("Other Block"));

		/* search for blocks with parent of type "FUNCTIONAL_GROUP" -> expect to find child1 and child2 */
		Set<UUID> foundIds = blockServiceImpl.find(q -> q.withParent(parent -> parent.withType(FunctionalBlockType.FUNCTIONAL_GROUP)))
				.stream()
				.map(FunctionalBlockPojo::getUid)
				.collect(Collectors.toSet());
		assertEquals(3, foundIds.size(), "expected to find the two children of functionalBlockPojoUUID because it has type FUNCTIONAL_GROUP and Parent Block");
		assertTrue(foundIds.contains(child1Uid), "expected to find first child");
		assertTrue(foundIds.contains(child2Uid), "expected to find second child");
		assertTrue(foundIds.contains(parentUid), "expected to find  Parent Block");

		/* doing the same but "NOT with parent" -> expect to find all other blocks, but not child 1 and child 2 */
		foundIds = blockServiceImpl.find(q -> q.notWithParent(parent -> parent.withType(FunctionalBlockType.FUNCTIONAL_GROUP)))
				.stream()
				.map(FunctionalBlockPojo::getUid)
				.collect(Collectors.toSet());


		assertEquals(2, foundIds.size(), "expected to find the parent block and other block");
		assertTrue(foundIds.contains(otherUid), "expected to find other block");

		List<FunctionalBlockPojo> functionalBlockPojos = blockServiceImpl.find(q -> {
				 q.withParent(parent -> parent.withParent(parent2 ->
						parent2.withType(FunctionalBlockType.FUNCTIONAL_GROUP)));

		});
		assertEquals(2, functionalBlockPojos.size(), "expected to find the child blocks");
		final List<UUID> functionalBlockIds = functionalBlockPojos.stream().map(FunctionalBlockPojo::getUid).collect(Collectors.toList());
		assertThat("expected to find first and second child", functionalBlockIds, containsInAnyOrder(child1Uid, child2Uid));
	}

	@BeforeAll
	void setUp() {
		project = createProject(1L);
	}

	private ProjectPojo createProject(final Long clientId) {
		return projectService.create(new ProjectPojoPrototype().setName("Test Project" + clientId)
				.setClient(EntityId.of(clientId)).setNatures(Collections.emptySet()));
	}

}

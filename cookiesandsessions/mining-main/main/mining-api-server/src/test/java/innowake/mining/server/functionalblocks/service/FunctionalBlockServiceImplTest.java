/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.functionalblocks.service;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.Definable.ValueNotDefinedException;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.datapoints.definition.filters.FilterOperators;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.TaxonomyCategoryPojoPrototype;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFieldName;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLink;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkCondition;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockStatus;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.GeneratedFrom;
import innowake.mining.shared.entities.functionalblocks.ModulePart;
import innowake.mining.shared.entities.functionalblocks.ReachabilityDataPojo;
import innowake.mining.shared.entities.functionalblocks.ReachabilityDataPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.ResolvedModulePart;
import innowake.mining.shared.entities.testing.AnnotationPojoDummy;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.ProjectNature;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.aggregations.AggregationOperator;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import junit.framework.AssertionFailedError;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.dao.DuplicateKeyException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

@TestMethodOrder(OrderAnnotation.class)
class FunctionalBlockServiceImplTest extends DatabaseRelatedTest {

	private static final Long ONE = Long.valueOf(1);
	private static final Long TWO = Long.valueOf(2);
	private static final Long THREE = Long.valueOf(3);
	
	private ProjectPojo project;
	private ProjectPojo project3;
	private ModulePojo module1;
	private ModulePojo module2;
	private UUID childFunctionalBlockPojoUid1;
	private UUID childFunctionalBlockPojoUid2;
	private UUID childFunctionalBlockPojoUid3;
	private UUID childFunctionalBlockPojoUid4;
	private UUID childFunctionalBlockPojoUid5;
	private UUID childFunctionalBlockPojoUid6;
	private FunctionalBlockPojoPrototype childFunctionalBlockPojoPrototype1;
	private FunctionalBlockPojoPrototype childFunctionalBlockPojoPrototype2;
	private FunctionalBlockPojoPrototype childFunctionalBlockPojoPrototype5;
	private FunctionalBlockPojoPrototype childFunctionalBlockPojoPrototype6;
	private FunctionalBlockPojoPrototype functionalBlockPojoPrototype;
	private FunctionalBlockPojoPrototype functionalBlockPojoPrototype2;
	private UUID functionalBlockPojoUUID;
	private FunctionalBlockLinkCondition functionalBlockLinkCondition1;
	private FunctionalBlockLinkCondition functionalBlockLinkCondition2;

	@Autowired
	private FunctionalBlockServiceImpl blockServiceImpl;

	@Autowired
	private ModuleService moduleService;
	@Autowired
	private TaxonomyService taxonomyService;
	
	@BeforeAll
	void setUp() {
		project = createProject(ONE);
		project3 = createProject1(THREE);
		module1 = createTestModule("TEST_MODULE", "src/cobol/programs/Test.cbl", project.identity());
		module2 = createTestModule("TEST_MODULE2", "src/cobol/programs/Test2.cbl", project.identity());
		createTestModule("TEST_MODULE3", "src/cobol/programs/Test3.cbl", project.identity());

		/* create couple of child functional block Nodes */
		childFunctionalBlockPojoPrototype1 = createFunctionalBlockPojoPrototype("1st Child_Functional_block", "1st Child_Functional_block_description", 
				new ModuleLocation(100, 20), null, null, project);
		childFunctionalBlockPojoUid1 = blockServiceImpl.create(childFunctionalBlockPojoPrototype1);

		childFunctionalBlockPojoPrototype2 = createFunctionalBlockPojoPrototype("2nd Child_Functional_block", "2nd Child_Functional_block_description", 
				new ModuleLocation(200, 30), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)),  project);
		childFunctionalBlockPojoUid2 = blockServiceImpl.create(childFunctionalBlockPojoPrototype2);
		
		childFunctionalBlockPojoPrototype5 = createFunctionalBlockPojoPrototype("5th Child_Functional_block", "5th Child_Functional_block_description", 
				new ModuleLocation(150, 30), null, null,  project);
		childFunctionalBlockPojoUid5 = blockServiceImpl.create(childFunctionalBlockPojoPrototype5);
		
		childFunctionalBlockPojoPrototype6 = createFunctionalBlockPojoPrototype("6th Child_Functional_block", "6th Child_Functional_block_description", 
				new ModuleLocation(160, 10), null, null,  project);
		childFunctionalBlockPojoUid6 = blockServiceImpl.create(childFunctionalBlockPojoPrototype6);

		/* create functional block Node*/
		final List<UUID> childUUIDs = new ArrayList<>();
		childUUIDs.add(childFunctionalBlockPojoUid1);
		childUUIDs.add(childFunctionalBlockPojoUid2);
		functionalBlockLinkCondition1 = new FunctionalBlockLinkCondition(UUID.randomUUID(),"label1");


		functionalBlockPojoPrototype = createFunctionalBlockPojoPrototype("Functional_block", "Functional_block_description", 
				new ModuleLocation(100, 250), childUUIDs, null,  project);
		functionalBlockPojoPrototype2 = createFunctionalBlockPojoPrototype("Functional_block", "Functional_block_description", 
				new ModuleLocation(100, 250), null, null, project);
	}

	@Test
	@Order(1)
	void shouldCreateFunctionalBlockSuccessfully() {
		functionalBlockPojoUUID = blockServiceImpl.create(functionalBlockPojoPrototype);
		final Optional<FunctionalBlockPojo> createdFunctionalBlock = blockServiceImpl.find(functionalBlockPojoUUID);
		assertTrue(createdFunctionalBlock.isPresent(), "FunctionalBlock should be found as it was created.");
        createdFunctionalBlock.ifPresent(functionalBlockPojo -> assertFunctionalBlock(functionalBlockPojo, functionalBlockPojoPrototype));
	}
	
	@Test
	@Order(2)
	void shouldThrowErrorForCreateEmptyFunctionalBlock() {
		final FunctionalBlockPojoPrototype functionalBlockPojoPrototype= new FunctionalBlockPojoPrototype();
		assertThrows(IllegalArgumentException.class, () -> {
			blockServiceImpl.create(functionalBlockPojoPrototype);
		});
	}

	@Test
	@Order(3)
	void shouldFindFunctionalBlockSuccessfully() {
		final Optional<FunctionalBlockPojo> searchedChildfunctionalBlockPojo1 = blockServiceImpl.find(childFunctionalBlockPojoUid1);
		assertTrue(searchedChildfunctionalBlockPojo1.isPresent(), "Child FunctionalBlock 1 should be found as it was created.");
		if (searchedChildfunctionalBlockPojo1.isPresent()) {
			assertFunctionalBlock(searchedChildfunctionalBlockPojo1.get(), childFunctionalBlockPojoPrototype1);
		}
		
		final Optional<FunctionalBlockPojo> searchedChildfunctionalBlockPojo2 = blockServiceImpl.find(childFunctionalBlockPojoUid2);
		assertTrue(searchedChildfunctionalBlockPojo2.isPresent(), "Child FunctionalBlock 2 should be found as it was created.");
		if (searchedChildfunctionalBlockPojo2.isPresent()) {
			assertFunctionalBlock(searchedChildfunctionalBlockPojo2.get(), childFunctionalBlockPojoPrototype2);
		}
		
		final Optional<FunctionalBlockPojo> searchedfunctionalBlockPojo = blockServiceImpl.find(functionalBlockPojoUUID);
		assertTrue(searchedfunctionalBlockPojo.isPresent(), "FunctionalBlock should be found as it was created.");
		if (searchedfunctionalBlockPojo.isPresent()) {
			assertFunctionalBlock(searchedfunctionalBlockPojo.get(), functionalBlockPojoPrototype);
		}
	}

	@Test
	@Order(4)
	void shouldSetResolvedModulePartsForFunctionalBlock() {
		final List<ResolvedModulePart> resolvedModuleParts = new ArrayList<>();
		final ResolvedModulePart resolvedModulePart = new ResolvedModulePart(module1.identity(), new ModuleLocation(100, 250));
		resolvedModuleParts.add(resolvedModulePart);
		blockServiceImpl.setResolvedModuleParts(functionalBlockPojoUUID, resolvedModuleParts);
		final List<ResolvedModulePart> actualResolvedParts = blockServiceImpl.getResolvedModuleParts(functionalBlockPojoUUID);
		assertEquals(resolvedModuleParts.size(), actualResolvedParts.size(), "Number of functional block resolved module part differs");
		
		for (final ResolvedModulePart modulePart : resolvedModuleParts) {
			assertTrue(actualResolvedParts.contains(modulePart), "Expected functional block resolved module part not found in actual list");
		}
	}

	@Test
	@Order(5)
	void shouldUpdateFunctionalBlockSuccessfully() {
		final FunctionalBlockPojoPrototype childFunctionalBlock3 = createFunctionalBlockPojoPrototype("3rd Child_Functional_block", 
				"3rd Child_Functional_block_description", new ModuleLocation(50, 10), null, null, project);
		childFunctionalBlockPojoUid3 = blockServiceImpl.create(childFunctionalBlock3);
		
		final List<UUID> childUUIDs = functionalBlockPojoPrototype.children.get();
		if (childUUIDs != null) {
			childUUIDs.add(childFunctionalBlockPojoUid3);
			functionalBlockPojoPrototype.setChildren(childUUIDs);
		}
		functionalBlockPojoPrototype.setUid(functionalBlockPojoUUID);
		functionalBlockPojoPrototype.setDescription("Updated_Functional_block_description");
		final List<ModulePart> functionalModuleParts = new ArrayList<>();
		final ModulePart functionalBlockModulePart = new ModulePart(module1.getLinkHash(), new ModuleLocation(100, 350));
		functionalModuleParts.add(functionalBlockModulePart);
		functionalBlockPojoPrototype.setModuleParts(functionalModuleParts);
		blockServiceImpl.update(functionalBlockPojoPrototype);
		
		final Optional<FunctionalBlockPojo> updatedfunctionalBlockPojo = blockServiceImpl.find(functionalBlockPojoUUID);
		assertTrue(updatedfunctionalBlockPojo.isPresent(), "FunctionalBlock should be found as it was created.");
		if (updatedfunctionalBlockPojo.isPresent()) {
			assertFunctionalBlock(updatedfunctionalBlockPojo.get(), functionalBlockPojoPrototype);
		}
	}
	
	@Test
	@Order(6)
	void shouldUpdateChildsOnlyInFunctionalBlock() {
		
		/* create a new child FunctionalBlockPojoPrototype */
		final FunctionalBlockPojoPrototype childFunctionalBlock4 = createFunctionalBlockPojoPrototype("4th Child_Functional_block", 
				"4th Child_Functional_block_description", new ModuleLocation(150, 60), null, null, project);
		childFunctionalBlockPojoUid4 = blockServiceImpl.create(childFunctionalBlock4);
		
		final List<UUID> childUUIDs = functionalBlockPojoPrototype.children.get();
		if (childUUIDs != null) {
			childUUIDs.add(childFunctionalBlockPojoUid4);
			/* Adding a child to the global functional block to assert the entire block after updating only children with empty attributes. */
			functionalBlockPojoPrototype.setChildren(childUUIDs);
		}
		/* create a FunctionalBlockPojoPrototype with UID and children only and other attributes are empty*/
		final FunctionalBlockPojoPrototype childOnlyfunctionalBlockPojoPrototype = new FunctionalBlockPojoPrototype();
		childOnlyfunctionalBlockPojoPrototype.setChildren(childUUIDs);
		childOnlyfunctionalBlockPojoPrototype.setUid(functionalBlockPojoUUID);
		childOnlyfunctionalBlockPojoPrototype.setProject(project.identity());
		blockServiceImpl.update(childOnlyfunctionalBlockPojoPrototype);
		
		final Optional<FunctionalBlockPojo> updatedfunctionalBlockPojo = blockServiceImpl.find(functionalBlockPojoUUID);
		assertTrue(updatedfunctionalBlockPojo.isPresent(), "FunctionalBlock should be found as it was created.");
		if (updatedfunctionalBlockPojo.isPresent()) {
			assertFunctionalBlock(updatedfunctionalBlockPojo.get(), functionalBlockPojoPrototype);
		}
	}
	
	@Test
	@Order(7)
	void shouldUpdateModulePartOnlyInFunctionalBlock() {
		
		final List<ModulePart> moduleParts = new ArrayList<>();
		final ModulePart functionalBlockModulePart = new ModulePart(module1.getLinkHash(), new ModuleLocation(300, 70));
		moduleParts.add(functionalBlockModulePart);
		
		/* Adding module part to the global functional block to assert the entire block after updating only module part with empty attributes. */
		functionalBlockPojoPrototype.setModuleParts(moduleParts);
		
		/* create a FunctionalBlockPojoPrototype with UID and module part only and other attributes are empty*/
		final FunctionalBlockPojoPrototype childOnlyfunctionalBlockPojoPrototype = new FunctionalBlockPojoPrototype();
		childOnlyfunctionalBlockPojoPrototype.setModuleParts(moduleParts);
		childOnlyfunctionalBlockPojoPrototype.setUid(functionalBlockPojoUUID);
		blockServiceImpl.update(childOnlyfunctionalBlockPojoPrototype);
		
		final Optional<FunctionalBlockPojo> updatedfunctionalBlockPojo = blockServiceImpl.find(functionalBlockPojoUUID);
		assertTrue(updatedfunctionalBlockPojo.isPresent(), "FunctionalBlock should be found as it was created.");
		if (updatedfunctionalBlockPojo.isPresent()) {
			assertFunctionalBlock(updatedfunctionalBlockPojo.get(), functionalBlockPojoPrototype);
		}
	}
	
	@Test
	@Order(8)
	void createFunctionalBlockWithLinksAndConditions() {
		final List<UUID> childUuidsOnCreation = new ArrayList<>();
		childUuidsOnCreation.add(childFunctionalBlockPojoUid5);
		childUuidsOnCreation.add(childFunctionalBlockPojoUid6);
		final FunctionalBlockPojoPrototype functionalBlock = new FunctionalBlockPojoPrototype();
		functionalBlock.setProject(project.identity());
		functionalBlock.setName("TestBlock Name");
		functionalBlock.setDescription("Test Block Description");
		functionalBlock.setChildren(childUuidsOnCreation);
		final UUID conditionUid = UUID.randomUUID();
		final UUID conditionUid1 = UUID.randomUUID();
		final UUID uid = blockServiceImpl.create(functionalBlock);
		blockServiceImpl.setLinks(uid, Collections.singletonList(new FunctionalBlockLink(UUID.randomUUID(), null, childFunctionalBlockPojoUid5,
				childFunctionalBlockPojoUid6, "label",
				Collections.singletonMap(FunctionalBlockFlag.TYPE.name(), FunctionalBlockType.REACHABILITY), new FunctionalBlockLinkCondition(conditionUid,"Label1"))));
		blockServiceImpl.setLinks(uid, Collections.singletonList(new FunctionalBlockLink(UUID.randomUUID(), null, childFunctionalBlockPojoUid6,
				childFunctionalBlockPojoUid5, "label1",
				Collections.singletonMap(FunctionalBlockFlag.TYPE.name(), FunctionalBlockType.RA_TOP_DOWN), new FunctionalBlockLinkCondition(conditionUid1,"Label2"))));
		final Optional<FunctionalBlockPojo> reloadedFunctionalBlockAfterCreate = blockServiceImpl.find(uid);
		final List<FunctionalBlockLink> links =blockServiceImpl.getLinks(uid);
		assertTrue(reloadedFunctionalBlockAfterCreate.isPresent(), "FunctionalBlock should be found as it was created.");
		assertEquals(childFunctionalBlockPojoUid6, links.get(0).getChildA());
		assertEquals(childFunctionalBlockPojoUid5, links.get(0).getChildB());
		assertEquals(conditionUid1, Objects.requireNonNull(links.get(0).getCondition()).getUid());
		assertEquals("Label2", Objects.requireNonNull(links.get(0).getCondition()).getLabel());
	}

	@Test
	@Order(9)
	void createMultipleLinksWithDifferentConditions() {
		final List<UUID> childUuidsOnCreation = new ArrayList<>();
		childUuidsOnCreation.add(childFunctionalBlockPojoUid5);
		childUuidsOnCreation.add(childFunctionalBlockPojoUid6);
		final FunctionalBlockPojoPrototype functionalBlock = new FunctionalBlockPojoPrototype();
		functionalBlock.setProject(project.identity());
		functionalBlock.setName("TestBlock Name");
		functionalBlock.setDescription("Test Block Description");
		functionalBlock.setChildren(childUuidsOnCreation);
		final UUID conditionUid = UUID.randomUUID();
		final UUID conditionUid1 = UUID.randomUUID();
		final UUID uid = blockServiceImpl.create(functionalBlock);
		blockServiceImpl.setLinks(uid,
				List.of(new FunctionalBlockLink(UUID.randomUUID(), null, childFunctionalBlockPojoUid5, childFunctionalBlockPojoUid6, "label",
								Collections.singletonMap(FunctionalBlockFlag.TYPE.name(), FunctionalBlockType.REACHABILITY),
								new FunctionalBlockLinkCondition(conditionUid, "Label1")),
						new FunctionalBlockLink(UUID.randomUUID(), null, childFunctionalBlockPojoUid5, childFunctionalBlockPojoUid6, "label",
								Collections.singletonMap(FunctionalBlockFlag.TYPE.name(), FunctionalBlockType.REACHABILITY),
								new FunctionalBlockLinkCondition(conditionUid1, "Label2"))));


		final Optional<FunctionalBlockPojo> reloadedFunctionalBlockAfterCreate = blockServiceImpl.find(uid);
		final List<FunctionalBlockLink> links = blockServiceImpl.getLinks(uid);
		assertTrue(reloadedFunctionalBlockAfterCreate.isPresent(), "FunctionalBlock should be found as it was created.");
		assertEquals(2 , links.size());
	}


	@Test
	@Order(10)
	void updateLinksAndConditionsForExistingBlock() {
		final List<UUID> childUuidsOnCreation = new ArrayList<>();
		childUuidsOnCreation.add(childFunctionalBlockPojoUid1);
		childUuidsOnCreation.add(childFunctionalBlockPojoUid2);
		childUuidsOnCreation.add(childFunctionalBlockPojoUid5);
		childUuidsOnCreation.add(childFunctionalBlockPojoUid6);
		final FunctionalBlockPojoPrototype functionalBlock = new FunctionalBlockPojoPrototype();
		functionalBlock.setProject(project.identity());
		functionalBlock.setName("TestBlock Name");
		functionalBlock.setDescription("Test Block Description");
		functionalBlock.setChildren(childUuidsOnCreation);
		final UUID uid = blockServiceImpl.create(functionalBlock);
		functionalBlock.setUid(uid);
		final Optional<FunctionalBlockPojo> reloadedFunctionalBlock = blockServiceImpl.find(uid);
		assertTrue(reloadedFunctionalBlock.isPresent(), "FunctionalBlock should be found as it was created.");
		
		/* updating the values along with links and conditions */
		functionalBlock.setProject(project.identity());
		functionalBlock.setName("TestBlock Name");
		functionalBlock.setDescription("Test Block Description");
		functionalBlock.setChildren(childUuidsOnCreation);
		final Map<UUID, FunctionalBlockLinkCondition> conditions = new HashMap<>();
		final UUID conditionUid = UUID.randomUUID();
		functionalBlock.setUid(uid);
		conditions.put(conditionUid, new FunctionalBlockLinkCondition(UUID.randomUUID(), "Example lable1"));
		blockServiceImpl.setLinks(functionalBlock.uid.getNonNull(),Collections.singletonList(new FunctionalBlockLink(UUID.randomUUID(), uid, childFunctionalBlockPojoUid5,
				childFunctionalBlockPojoUid6, "Example lable1", null,  new FunctionalBlockLinkCondition(conditionUid,"Label1"))));
		blockServiceImpl.update(functionalBlock);
		final List<FunctionalBlockLink> links = blockServiceImpl.getLinks(uid);
		final Optional<FunctionalBlockPojo> reloadedFunctionalBlockAfterUpdate = blockServiceImpl.find(uid);
		assertTrue(reloadedFunctionalBlockAfterUpdate.isPresent(), "FunctionalBlock should be found as it was updated.");
		assertEquals(childFunctionalBlockPojoUid5, links.get(0).getChildA());
		assertEquals(childFunctionalBlockPojoUid6, links.get(0).getChildB());
		assertEquals(conditionUid, Objects.requireNonNull(links.get(0).getCondition()).getUid());
		assertEquals("Label1", Objects.requireNonNull(links.get(0).getCondition()).getLabel());


	}
	
	@Test
	@Order(11)
	void retrieveFunctionalBlockWithLinksAndConditions() {
		final FunctionalBlockPojoPrototype functionalBlock1 = new FunctionalBlockPojoPrototype();
		functionalBlock1.setProject(project.identity());
		functionalBlock1.setName("Functional Block1");
		functionalBlock1.setDescription("Test Block Description");
		functionalBlock1.setChildren(Arrays.asList(childFunctionalBlockPojoUid5, childFunctionalBlockPojoUid6));
		final UUID conditionUid = UUID.randomUUID();
		final UUID uid = blockServiceImpl.create(functionalBlock1);
		blockServiceImpl.setLinks(uid,Collections.singletonList(new FunctionalBlockLink(UUID.randomUUID(), null, childFunctionalBlockPojoUid5,
				childFunctionalBlockPojoUid6,"label",
				Collections.singletonMap(FunctionalBlockFlag.TYPE.name(), FunctionalBlockType.REACHABILITY),  new FunctionalBlockLinkCondition(conditionUid,"Label1"))));

		final FunctionalBlockPojoPrototype functionalBlock2 = new FunctionalBlockPojoPrototype();
		functionalBlock2.setProject(project.identity());
		functionalBlock2.setName("Functional Block2");
		functionalBlock2.setDescription("Test Block Description");
		functionalBlock2.setChildren(Arrays.asList(childFunctionalBlockPojoUid1, childFunctionalBlockPojoUid2));
		final UUID conditionUid2 = UUID.randomUUID();
		final UUID uid2 = blockServiceImpl.create(functionalBlock2);
		blockServiceImpl.setLinks(uid2,Collections.singletonList(new FunctionalBlockLink(UUID.randomUUID(), null, childFunctionalBlockPojoUid1,
				childFunctionalBlockPojoUid2, "Example lable2",
				Collections.singletonMap(FunctionalBlockFlag.TYPE.name(), FunctionalBlockType.REACHABILITY),  new FunctionalBlockLinkCondition(conditionUid2,"Label2"))));

		/* when we query the blocks, making sure that each only show the 1 link that was created for the block */
		final Optional<FunctionalBlockPojo> retrievedFunctionalBlock1 = blockServiceImpl.find(uid);
		final List<FunctionalBlockLink> links = blockServiceImpl.getLinks(uid);
		assertTrue(retrievedFunctionalBlock1.isPresent(), "FunctionalBlock should be found as it was created.");
		assertEquals(1,links.size());
		assertEquals(childFunctionalBlockPojoUid5, links.get(0).getChildA());
		assertEquals(childFunctionalBlockPojoUid6, links.get(0).getChildB());
		assertEquals(conditionUid, Objects.requireNonNull(links.get(0).getCondition()).getUid());
		assertEquals("Label1", Objects.requireNonNull(links.get(0).getCondition()).getLabel());

		final Optional<FunctionalBlockPojo> retrievedFunctionalBlock2 = blockServiceImpl.find(uid2);
		final List<FunctionalBlockLink> links2 = blockServiceImpl.getLinks(uid2);
		assertTrue(retrievedFunctionalBlock2.isPresent(), "FunctionalBlock should be found as it was created.");
		assertEquals(1, links2.size());
		assertEquals(childFunctionalBlockPojoUid1, links2.get(0).getChildA());
		assertEquals(childFunctionalBlockPojoUid2, links2.get(0).getChildB());
		assertEquals(conditionUid2, Objects.requireNonNull(links2.get(0).getCondition()).getUid());
		assertEquals("Label2", Objects.requireNonNull(links2.get(0).getCondition()).getLabel());

	}
	
	@Test
	@Order(12)
	void shouldUpdateEmptyFunctionalBlockSuccessfully() {
		/* create a FunctionalBlockPojoPrototype with UID and other attributes are empty*/
		final FunctionalBlockPojoPrototype childOnlyfunctionalBlockPojoPrototype = new FunctionalBlockPojoPrototype();
		childOnlyfunctionalBlockPojoPrototype.setUid(functionalBlockPojoUUID);
		blockServiceImpl.update(childOnlyfunctionalBlockPojoPrototype);
		
		final Optional<FunctionalBlockPojo> updatedfunctionalBlockPojo = blockServiceImpl.find(functionalBlockPojoUUID);
		assertTrue(updatedfunctionalBlockPojo.isPresent(), "FunctionalBlock should be found as it was created.");
		if (updatedfunctionalBlockPojo.isPresent()) {
			assertFunctionalBlock(updatedfunctionalBlockPojo.get(), functionalBlockPojoPrototype);
		}
	}

	@Test
	@Order(13)
	void shouldUpdateFunctionalBlock() {
		final FunctionalBlockPojoPrototype functionalBlock = createFunctionalBlockPojoPrototype("Functional_block", "Functional_block_description",
				new ModuleLocation(100, 250), null, null, project);
		final UUID createdFunctionalBlockUUID = blockServiceImpl.create(functionalBlock);
		functionalBlock.setName("updatedName");
		assertThrows(ValueNotDefinedException.class, () -> {
			blockServiceImpl.update(functionalBlock);
		});
		functionalBlock.setUid(createdFunctionalBlockUUID);
		blockServiceImpl.update(functionalBlock);
		final Optional<FunctionalBlockPojo> searchedfunctionalBlockPojo = blockServiceImpl.find(createdFunctionalBlockUUID);
		assertFalse(searchedfunctionalBlockPojo.isEmpty(), "The functional block with this Uid should exists");
		assertEquals("updatedName", searchedfunctionalBlockPojo.get().getName(), "After update name should be updated");

		functionalBlock.setName("updatedNameForSecondTime");
		assertThrows(DuplicateKeyException.class, () -> blockServiceImpl.create(functionalBlock));
		final Optional<FunctionalBlockPojo> searchedfunctionalBlockPojo2 = blockServiceImpl.find(createdFunctionalBlockUUID);
		assertEquals("updatedName", searchedfunctionalBlockPojo2.get().getName(), "create() should not have updated the existing block");

	}
	
	@Test
	@Order(14)
	void shouldDeleteFunctionalBlockService() {
		blockServiceImpl.delete(functionalBlockPojoUUID);
		final Optional<FunctionalBlockPojo> searchedfunctionalBlockPojo = blockServiceImpl.find(functionalBlockPojoUUID);
		assertTrue(searchedfunctionalBlockPojo.isEmpty(), "FunctionalBlock Should be deleted");
		blockServiceImpl.find(functionalBlockPojoUUID);
	}

	@Test
	@Order(15)
	void shouldThrowExceptionWhenLinksPresentAndChildrenNotSet() {
		final UUID uuid = UUID.randomUUID();
		functionalBlockPojoPrototype2.setUid(uuid);
		blockServiceImpl.create(functionalBlockPojoPrototype2);

		final IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> blockServiceImpl.setLinks(uuid, Collections.singletonList(new FunctionalBlockLink(UUID.randomUUID(), null, childFunctionalBlockPojoUid1,
				childFunctionalBlockPojoUid2, "Example lable2",
				Collections.singletonMap(FunctionalBlockFlag.TYPE.name(), FunctionalBlockType.REACHABILITY),  new FunctionalBlockLinkCondition(UUID.randomUUID(),"Label2")))));
		assertThat(exception.getMessage(), containsString("Block has no children, so no links can be created"));
	}
	
	@Test
	@Order(16)
	void testFindBlockOfType() {
		final List<FunctionalBlockPojo> results = blockServiceImpl.find(q -> q.withType(FunctionalBlockType.FUNCTIONAL_UNIT));
		assertEquals(1, results.size(), "Expected to find only 1 block with type FUNCTIONAL_UNIT");
		assertEquals("2nd Child_Functional_block", results.get(0).getName(), "Expected to find '2nd Child_Functional_block' block");
	}
	
	@Test
	@Order(17)
	void shouldThrowExceptionWhenChildAOrChildBNotPresentInChildren() {
		final UUID uuid = UUID.randomUUID();

		final FunctionalBlockPojoPrototype functionalBlockPojoPrototype3 = createFunctionalBlockPojoPrototype("Functional_block",
				"Functional_block_description", new ModuleLocation(100, 250), Collections.singletonList(childFunctionalBlockPojoUid1), null, project);
		functionalBlockPojoPrototype3.setUid(uuid);
		blockServiceImpl.create(functionalBlockPojoPrototype3);
		final IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> blockServiceImpl.setLinks(uuid, Collections.singletonList(new FunctionalBlockLink(UUID.randomUUID(), null, childFunctionalBlockPojoUid3,
				childFunctionalBlockPojoUid2, "Example lable2",
				Collections.singletonMap(FunctionalBlockFlag.TYPE.name(), FunctionalBlockType.REACHABILITY),  new FunctionalBlockLinkCondition(UUID.randomUUID(),"Label2"))) ));
		assertThat(exception.getMessage(), containsString(childFunctionalBlockPojoUid3 + " is not a child of the block"));
	}
	

	@Test
	@Order(18)
	void createFunctionalBlockWithLinksNoConditions() {
		final List<UUID> childUuidsOnCreation = new ArrayList<>();
		childUuidsOnCreation.add(childFunctionalBlockPojoUid5);
		childUuidsOnCreation.add(childFunctionalBlockPojoUid6);
		final FunctionalBlockPojoPrototype functionalBlock = new FunctionalBlockPojoPrototype();
		functionalBlock.setProject(project.identity());
		functionalBlock.setName("TestBlock Name");
		functionalBlock.setDescription("Test Block Description");
		functionalBlock.setChildren(childUuidsOnCreation);
		final UUID uid = blockServiceImpl.create(functionalBlock);
		final Optional<FunctionalBlockPojo> reloadedFunctionalBlockAfterCreate = blockServiceImpl.find(uid);
		blockServiceImpl.setLinks(uid, Collections.singletonList(new FunctionalBlockLink(UUID.randomUUID(), uid, childFunctionalBlockPojoUid5,
				childFunctionalBlockPojoUid6,null, null, null)));
		final List<FunctionalBlockLink> links = blockServiceImpl.getLinks(uid);
		assertTrue(reloadedFunctionalBlockAfterCreate.isPresent(), "FunctionalBlock should be found as it was created.");
		assertEquals(childFunctionalBlockPojoUid5, links.get(0).getChildA());
		assertEquals(childFunctionalBlockPojoUid6, links.get(0).getChildB());
		assertNull(links.get(0).getCondition());
	}
	
	@Test
	@Order(19)
	void testFindFunctionalGroupsByAnnotationId() {
		final FunctionalBlockPojoPrototype childBlock = createFunctionalBlockPojoPrototype("Child block_FU", "Child_Functional_block_description",
				new ModuleLocation(160, 10), null, Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)), project);
		final UUID childBlockUid = blockServiceImpl.create(childBlock);

		blockServiceImpl.setGeneratedFrom(childBlockUid, GeneratedFrom.fromAnnotation(EntityId.of(2L)));

		final var functionalBlock1 = blockServiceImpl.create(createFunctionalBlockPojoPrototype("FG1", "FG1", new ModuleLocation(100, 250),
				Arrays.asList(childBlockUid), Map.of(FunctionalBlockFlag.TYPE.name(),
						List.of(FunctionalBlockType.FUNCTIONAL_GROUP))));
		final Optional<FunctionalBlockPojo> fg1 = blockServiceImpl.find(functionalBlock1);
		assertTrue(fg1.isPresent(), "functionalBlock1 should be found as it was created.");

		final var functionalBlock2 = blockServiceImpl.create(createFunctionalBlockPojoPrototype("FG2",
				"FG2", new ModuleLocation(100, 250), Arrays.asList(childBlockUid), Map.of(FunctionalBlockFlag.TYPE.name(),
						List.of(FunctionalBlockType.FUNCTIONAL_GROUP))));
		final Optional<FunctionalBlockPojo> fg2 = blockServiceImpl.find(functionalBlock2);
		assertTrue(fg2.isPresent(), "functionalBlock2 should be found as it was created.");
		
		final Set<String> groupNames = blockServiceImpl.findFunctionalBlockNamesByAnnotationId(2L);
		assertThat(groupNames, Matchers.containsInAnyOrder(fg1.get().getName(), fg2.get().getName()));
	}

	@Test
	@Order(20)
	void testDeleteFunctionalBlockByAnnotationId() {
		final var functionalBlock = blockServiceImpl.create(createFunctionalBlockPojoPrototype("FB", "FB", new ModuleLocation(100, 250),
				null, Map.of(FunctionalBlockFlag.TYPE.name(),
						List.of(FunctionalBlockType.FUNCTIONAL_UNIT))));
		final Optional<FunctionalBlockPojo> fb = blockServiceImpl.find(functionalBlock);
		assertTrue(fb.isPresent(), "functionalBlock should be found as it was created.");

		blockServiceImpl.setGeneratedFrom(functionalBlock, GeneratedFrom.fromAnnotation(EntityId.of(3L)));

		blockServiceImpl.deleteGeneratedFromAnnotation(3L);
		final Optional<FunctionalBlockPojo> searchedFunctionalBlockPojo = blockServiceImpl.find(functionalBlock);
		assertTrue(searchedFunctionalBlockPojo.isEmpty(), "FunctionalBlock Should be deleted");
	}

	@Test
	@Order(21)
	void testUpdateFunctionalBlockByAnnotationId() {
		final var functionalBlock = blockServiceImpl.create(createFunctionalBlockPojoPrototype(
			    "FB",
			    "FB",
			    new ModuleLocation(100, 250),
			    Collections.emptyList(), // Pass an empty list instead of null
			    Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT))));
		blockServiceImpl.setGeneratedFrom(functionalBlock, GeneratedFrom.fromAnnotation(EntityId.of(3L)));
		
		final AnnotationPojo fb = AnnotationPojoDummy.build(new AnnotationPojoPrototype()
																	.setName("Sample Name"));
		blockServiceImpl.updateFunctionBlockDescriptionAndNameOnAnnotationUpdate(3L, fb);
		final Optional<FunctionalBlockPojo> searchedFunctionalBlockPojo = blockServiceImpl.find(functionalBlock);
		assertTrue(searchedFunctionalBlockPojo.isPresent(), "FunctionalBlock Should be updated");
		assertEquals("Sample Name", searchedFunctionalBlockPojo.get().getName());
		assertEquals("Sample Name", searchedFunctionalBlockPojo.get().getDescription());
	}

	@Test
	void testFunctionalBlockChildrenOrderAfterCreateAndUpdate() {
		final List<UUID> childUuidsOnCreation = new ArrayList<>();
		childUuidsOnCreation.add(childFunctionalBlockPojoUid1);
		childUuidsOnCreation.add(childFunctionalBlockPojoUid2);
		childUuidsOnCreation.add(childFunctionalBlockPojoUid5);
		childUuidsOnCreation.add(childFunctionalBlockPojoUid6);
		final FunctionalBlockPojoPrototype functionalBlockPojoPrototypeWithChildren = new FunctionalBlockPojoPrototype();
		functionalBlockPojoPrototypeWithChildren.setProject(project.identity());
		functionalBlockPojoPrototypeWithChildren.setName("TestBlock Name");
		functionalBlockPojoPrototypeWithChildren.setDescription("Test Block Description");
		functionalBlockPojoPrototypeWithChildren.setChildren(childUuidsOnCreation);
		final UUID uid = blockServiceImpl.create(functionalBlockPojoPrototypeWithChildren);
		functionalBlockPojoPrototypeWithChildren.setUid(uid);
		final Optional<FunctionalBlockPojo> reloadedFunctionalBlock = blockServiceImpl.find(uid);
		assertTrue(reloadedFunctionalBlock.isPresent(), "FunctionalBlock should be found as it was created.");

		if (reloadedFunctionalBlock.isPresent()) {
			assertEquals(childUuidsOnCreation, reloadedFunctionalBlock.get().getChildren(), "List of children does not match");
		}

		final List<UUID> updateExpectedChildUUIDs = new ArrayList<>();
		updateExpectedChildUUIDs.add(childFunctionalBlockPojoUid6);
		updateExpectedChildUUIDs.add(childFunctionalBlockPojoUid2);
		updateExpectedChildUUIDs.add(childFunctionalBlockPojoUid5);
		updateExpectedChildUUIDs.add(childFunctionalBlockPojoUid1);
		functionalBlockPojoPrototypeWithChildren.setChildren(updateExpectedChildUUIDs);
		blockServiceImpl.update(functionalBlockPojoPrototypeWithChildren);
		final Optional<FunctionalBlockPojo> reloadedFunctionalBlockAfterUpdate = blockServiceImpl.find(uid);
		assertTrue(reloadedFunctionalBlockAfterUpdate.isPresent(), "FunctionalBlock should be found as it was updated.");

		if (reloadedFunctionalBlockAfterUpdate.isPresent()) {
			assertEquals(reloadedFunctionalBlockAfterUpdate.get().getChildren(), updateExpectedChildUUIDs, "List of children does not match");
		}
	}

	@Test
	void testTransactionalUpdate() {
		/* it should roll back the update when referenced child block does not exist */
		final FunctionalBlockPojoPrototype prototype = new FunctionalBlockPojoPrototype();
		prototype.setProject(project.identity());
		prototype.setName("Test Block before Update");
		prototype.setDescription("Test Block before Update");
		final UUID uid = blockServiceImpl.create(prototype);

		/* perform update, referencing non-existent child-block */
		prototype.setUid(uid);
		prototype.setName("Test Block AFTER Update");
		prototype.setDescription("Test Block AFTER Update");
		prototype.setChildren(List.of(UUID.randomUUID()));
		assertThrows(DataIntegrityViolationException.class, () -> blockServiceImpl.update(prototype));

		/* assert name and description were not updated (update was rolled back) */
		final FunctionalBlockPojo fb = blockServiceImpl.find(uid).orElseThrow(() -> new AssertionFailedError("created functional block does not exist"));
		assertEquals("Test Block before Update", fb.getName(), "Expected name NOT to be changed");
		assertEquals("Test Block before Update", fb.getDescription(), "Expected description NOT to be changed");
	}

	@Test
	void testFindBlockGeneratedFromAnnotation() {
		/* for now, we do not check if the Annotation actually exists, so we can just pass an arbitrary Annotation id here */
		blockServiceImpl.setGeneratedFrom(childFunctionalBlockPojoUid1, GeneratedFrom.fromAnnotation(EntityId.of(42L)));
		final List<FunctionalBlockPojo> results = blockServiceImpl.find(q -> q.generatedFromAnnotation(EntityId.of(42L)));
		assertEquals(1, results.size(), "Expected to find 1 block generated from the annotation");
		assertEquals("1st Child_Functional_block", results.get(0).getName(), "Expected to find '1st Child_Functional_block' block");
	}

	@Test
	void testFindBlockReferencingModule() {
		/* references only offset 100-119 */
		blockServiceImpl.setResolvedModuleParts(childFunctionalBlockPojoUid1,
				List.of(new ResolvedModulePart(module1.identity(), new ModuleLocation(100, 20))));
		/* references the whole module */
		blockServiceImpl.setResolvedModuleParts(childFunctionalBlockPojoUid2,
				List.of(new ResolvedModulePart(module1.identity())));

		List<FunctionalBlockPojo> results;

		/* test find without location */
		results = blockServiceImpl.find(q -> q.withResolvedModulePart(module1.identity()));
		assertEquals(2, results.size(), "Expected to find 2 blocks referencing the module");
		assertTrue(results.stream().anyMatch(b -> "1st Child_Functional_block".equals(b.getName())),"Expected to find '1st Child_Functional_block' block");
		assertTrue(results.stream().anyMatch(b -> "2nd Child_Functional_block".equals(b.getName())),"Expected to find '2nd Child_Functional_block' block");

		/* test find with matching offset */
		results = blockServiceImpl.find(q -> q.withResolvedModulePartAtOffset(module1.identity(), 110));
		assertEquals(2, results.size(), "Expected to find 2 blocks referencing the module");
		assertTrue(results.stream().anyMatch(b -> "1st Child_Functional_block".equals(b.getName())),"Expected to find '1st Child_Functional_block' block");
		assertTrue(results.stream().anyMatch(b -> "2nd Child_Functional_block".equals(b.getName())),"Expected to find '2nd Child_Functional_block' block");

		/* test find with non-matching offset - should still find the block that referenced the entire module */
		results = blockServiceImpl.find(q -> q.withResolvedModulePartAtOffset(module1.identity(), 120));
		assertEquals(1, results.size(), "Expected to find 1 block referencing the module");
		assertTrue(results.stream().anyMatch(b -> "2nd Child_Functional_block".equals(b.getName())),"Expected to find '2nd Child_Functional_block' block");
	}
	
	@Test
	void testResolvedModulePartHavingTaxonomy() {
		final UUID functionalBlockPojoUUID1 = blockServiceImpl.create(functionalBlockPojoPrototype);
		final List<ResolvedModulePart> resolvedModuleParts1 = new ArrayList<>();
		final ResolvedModulePart resolvedModulePart1 = new ResolvedModulePart(module1.identity(), new ModuleLocation(100, 250));
		resolvedModuleParts1.add(resolvedModulePart1);
		blockServiceImpl.setResolvedModuleParts(functionalBlockPojoUUID1, resolvedModuleParts1);
		
		final FunctionalBlockPojoPrototype functionalBlockPojoPrototype2 = createFunctionalBlockPojoPrototype("Functional_block 2",
				"Functional_block_description 1", new ModuleLocation(100, 20), null, null, project);
		final UUID functionalBlockPojoUUID2 = blockServiceImpl.create(functionalBlockPojoPrototype2);
		final List<ResolvedModulePart> resolvedModuleParts2 = new ArrayList<>();
		final ResolvedModulePart resolvedModulePart2 = new ResolvedModulePart(module2.identity(), new ModuleLocation(100, 250));
		resolvedModuleParts2.add(resolvedModulePart2);
		blockServiceImpl.setResolvedModuleParts(functionalBlockPojoUUID2, resolvedModuleParts2);
		
		final var taxonomyCategory = taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype()
																		.setName("Business Taxonomies")
																		.setProject(project.identity()));

		
		final var type = taxonomyService.createType(new TaxonomyTypePojoPrototype()
														.setName("DataDomain")
														.setProject(project.identity())
														.setCategoryId(taxonomyCategory));

		final TaxonomyPojoPrototype taxonomy1 = new TaxonomyPojoPrototype()
														.setName("Taxonomy A")
														.setType(type)
														.setProject(project.identity());
		final EntityId taxonomy1Id = taxonomyService.create(taxonomy1);
		taxonomyService.createModuleLink(module1.getUid(), taxonomy1Id);

		final TaxonomyPojoPrototype taxonomy2 = new TaxonomyPojoPrototype()
				.setName("Taxonomy B")
				.setType(type)
				.setProject(project.identity());
		final EntityId taxonomy2Id = taxonomyService.create(taxonomy2);
		taxonomyService.createModuleLink(module2.getUid(), taxonomy2Id);
		
		
		List<FunctionalBlockPojo> results;

		/* test fetch Functional Block from Module having Taxonomy1 */
		results = blockServiceImpl.find(q -> q.ofProject(project.identity()).withResolvedModulePartHavingTaxonomy(taxonomy1Id));
		assertEquals(1, results.size(), "Expected to find 1 blocks referencing the module having taxonomy");
		assertEquals("Functional_block", results.get(0).getName(), "Expected to find 'Functional_block' block");
		
		/* test fetch Functional Block from Module having Taxonomy2 */
		results = blockServiceImpl.find(q -> q.ofProject(project.identity()).withResolvedModulePartHavingTaxonomy(taxonomy2Id));
		assertEquals(1, results.size(), "Expected to find 1 blocks referencing the module having taxonomy");
		assertEquals("Functional_block 2", results.get(0).getName(), "Expected to find 'Functional_block 2' block");
		
		/* test fetch Functional Block from Module having Taxonomy1 */
		results = blockServiceImpl.find(q -> q.ofProject(project.identity()).withResolvedModulePartHavingTaxonomies(
				Arrays.asList(taxonomy1Id)));
		assertEquals(1, results.size(), "Expected to find 1 blocks referencing the module having taxonomy");
		assertEquals("Functional_block", results.get(0).getName(), "Expected to find 'Functional_block' block");
		
		/* test ResolvedModulePart having multiple taxonomy*/
		results = blockServiceImpl.find(q -> q.ofProject(project.identity()).withResolvedModulePartHavingTaxonomies(
				Arrays.asList(taxonomy1Id, taxonomy2Id)));
		assertEquals(2, results.size(), "Expected to find 2 blocks referencing the module having taxonomy");
		assertEquals(Set.of("Functional_block", "Functional_block 2"), results.stream().map(FunctionalBlockPojo::getName).collect(Collectors.toSet()));
		
		/* deleting both the functional Block After Tests */
		blockServiceImpl.delete(functionalBlockPojoUUID1);
		blockServiceImpl.delete(functionalBlockPojoUUID2);
	}

	@Test
	void testFindResolvedModuleParts() {
		final List<ResolvedModulePart> resolvedModuleParts1 =
				Collections.singletonList(new ResolvedModulePart(module1.identity()));
		final List<ResolvedModulePart> resolvedModuleParts2 =
				Collections.singletonList(new ResolvedModulePart(module2.identity()));
		blockServiceImpl.setResolvedModuleParts(childFunctionalBlockPojoUid1, resolvedModuleParts1);
		blockServiceImpl.setResolvedModuleParts(childFunctionalBlockPojoUid2, resolvedModuleParts2);

		final Map<UUID, List<ResolvedModulePart>> resolvedModuleParts =
				blockServiceImpl.getResolvedModuleParts(List.of(childFunctionalBlockPojoUid1, childFunctionalBlockPojoUid2));

		assertEquals(2, resolvedModuleParts.size(), "should have found resolved module parts for both blocks");
		assertEquals(resolvedModuleParts1, resolvedModuleParts.get(childFunctionalBlockPojoUid1));
		assertEquals(resolvedModuleParts2, resolvedModuleParts.get(childFunctionalBlockPojoUid2));
	}

	@Test
	void testFindGeneratedFrom() {
		final GeneratedFrom generatedFrom1 = GeneratedFrom.fromModule(module1.getLinkHash(),
				module1.getContentHash().orElseThrow(() -> new IllegalStateException("Content hash must be present in module 1")).toString(),
				module1.getDependencyHash().orElse(null));
		final GeneratedFrom generatedFrom2 = GeneratedFrom.fromModule(module2.getLinkHash(),
				module2.getContentHash().orElseThrow(() -> new IllegalStateException("Content hash must be present in module 2")).toString(),
				module2.getDependencyHash().orElse(null));
		blockServiceImpl.setGeneratedFrom(childFunctionalBlockPojoUid1, generatedFrom1);
		blockServiceImpl.setGeneratedFrom(childFunctionalBlockPojoUid2, generatedFrom2);

		final Map<UUID, GeneratedFrom> generatedFrom = blockServiceImpl.getGeneratedFrom(List.of(childFunctionalBlockPojoUid1, childFunctionalBlockPojoUid2));

		assertEquals(2, generatedFrom.size(), "should have found 'generated from' for both blocks");
		assertEquals(generatedFrom1, generatedFrom.get(childFunctionalBlockPojoUid1));
		assertEquals(generatedFrom2, generatedFrom.get(childFunctionalBlockPojoUid2));
	}

	@Test
	void testFunctionBlockDeepChildrenIdRetrieval() {
		/*
		 *                                   Base Test Block
		 *                                          |
		 *               -----------------------------------------------------------
		 *               |                          |							   |
		 *         Child Block 1 			Child Block 2                     Test Block
		 *                                                                         |
		 *                                                            ----------------------------
		 *                                                            |                          |
		 *                                                       Child Block 3            Child Block 5
		 * 										                      |
		 * 										                 Child Block 6
		 */
		childFunctionalBlockPojoUid3 = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Child Block 3", "Child Block 3 Description",
				new ModuleLocation(100, 250), List.of(childFunctionalBlockPojoUid6),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)),  project));
		final FunctionalBlockPojoPrototype childFunctionalBlockWithChildren = createFunctionalBlockPojoPrototype("TestBlock Name",
				"Test Block Description", new ModuleLocation(100, 250),
				List.of(childFunctionalBlockPojoUid3, childFunctionalBlockPojoUid5), null,  project);
		final UUID testBlockUid = blockServiceImpl.create(childFunctionalBlockWithChildren);
		final FunctionalBlockPojoPrototype functionalBlockWithChildren = createFunctionalBlockPojoPrototype("Base TestBlock Name",
				"Base Test Block Description", new ModuleLocation(100, 250),
				List.of(testBlockUid, childFunctionalBlockPojoUid1, childFunctionalBlockPojoUid2), null,  project);
		final UUID baseBlockUid = blockServiceImpl.create(functionalBlockWithChildren);

		final Map<UUID, List<UUID>> childMap = blockServiceImpl.findChildrenIdsDeep(List.of(baseBlockUid), -1);
		assertEquals(Set.of(baseBlockUid), childMap.keySet());
		final List<UUID> children = childMap.get(baseBlockUid);

		assertEquals(6, children.size(), "Expected to find 6 children");
		assertTrue(children.containsAll(List.of(
				childFunctionalBlockPojoUid1,
				childFunctionalBlockPojoUid2,
				childFunctionalBlockPojoUid3,
				childFunctionalBlockPojoUid5,
				childFunctionalBlockPojoUid6,
				testBlockUid
		)), "Expected to find all children");

		blockServiceImpl.delete(baseBlockUid);
		blockServiceImpl.delete(testBlockUid);
		blockServiceImpl.delete(childFunctionalBlockPojoUid3);
		childFunctionalBlockPojoUid3 = null;
	}

	@Test
	void testFunctionBlockDeepChildrenRetrieval() {
		/*
		 *                                   Base Test Block
		 *                                          |
		 *               -----------------------------------------------------------
		 *               |                          |							   |
		 *         Child Block 1 			Child Block 2                     Test Block
		 *                                                                         |
		 *                                                            ----------------------------
		 *                                                            |                          |
		 *                                                       Child Block 3            Child Block 5
		 * 										                      |
		 * 										                 Child Block 6
		 */
		childFunctionalBlockPojoUid3 = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Child Block 3", "Child Block 3 Description",
				new ModuleLocation(100, 250), List.of(childFunctionalBlockPojoUid6),
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)),  project));
		final FunctionalBlockPojoPrototype childFunctionalBlockWithChildren = createFunctionalBlockPojoPrototype("TestBlock Name",
				"Test Block Description", new ModuleLocation(100, 250),
				List.of(childFunctionalBlockPojoUid3, childFunctionalBlockPojoUid5), null,  project);
		final UUID testBlockUid = blockServiceImpl.create(childFunctionalBlockWithChildren);
		final FunctionalBlockPojoPrototype functionalBlockWithChildren = createFunctionalBlockPojoPrototype("Base TestBlock Name",
				"Base Test Block Description", new ModuleLocation(100, 250),
				List.of(testBlockUid, childFunctionalBlockPojoUid1, childFunctionalBlockPojoUid2), null,  project);
		final UUID baseBlockUid = blockServiceImpl.create(functionalBlockWithChildren);

		final List<FunctionalBlockPojo> allChildren = blockServiceImpl.findChildrenDeep(baseBlockUid, -1, q -> {});
		assertEquals(6, allChildren.size(), "Expected to find 6 children");
		assertTrue(allChildren.stream().map(FunctionalBlockPojo :: getUid).collect(Collectors.toList()).containsAll(List.of(
				childFunctionalBlockPojoUid1,
				childFunctionalBlockPojoUid2,
				childFunctionalBlockPojoUid3,
				childFunctionalBlockPojoUid5,
				childFunctionalBlockPojoUid6,
				testBlockUid
		)), "Expected to find all children");

		final List<FunctionalBlockPojo> allChildrenWithFilter = blockServiceImpl.findChildrenDeep(baseBlockUid, -1,
				q -> q.withType(FunctionalBlockType.FUNCTIONAL_UNIT));
		assertEquals(2, allChildrenWithFilter.size(), "Expected to find 2 children");
		assertTrue(allChildrenWithFilter.stream().map(FunctionalBlockPojo :: getUid).collect(Collectors.toList()).containsAll(List.of(
				childFunctionalBlockPojoUid2,
				childFunctionalBlockPojoUid3
		)), "Expected to find children 2 and 3");

		blockServiceImpl.delete(baseBlockUid);
		blockServiceImpl.delete(testBlockUid);
		blockServiceImpl.delete(childFunctionalBlockPojoUid3);
		childFunctionalBlockPojoUid3 = null;
	}

	@Test
	void testFindReachabilityDataOfFunctionalBlock() {
		final ModulePojo lowerModule1 = createTestModule("lowerModule1", "src/cobol/programs/lowerModule1.cbl", project.identity());
		final ModulePojo lowerModule2 = createTestModule("lowerModule2", "src/cobol/programs/lowerModule2.cbl", project.identity());
		final ModulePojo upperModule1 = createTestModule("upperModule1", "src/cobol/programs/upperModule1.cbl", project.identity());
		final ModulePojo upperModule2 = createTestModule("upperModule2", "src/cobol/programs/upperModule2.cbl", project.identity());
		final ModulePojo accessModule1 = createTestModule("accessModule1", "src/cobol/programs/accessModule1.cbl", project.identity());
		final ModulePojo accessModule2 = createTestModule("accessModule2", "src/cobol/programs/accessModule2.cbl", project.identity());
		final ModulePojo accessModule3 = createTestModule("accessModule3", "src/cobol/programs/accessModule3.cbl", project.identity());
		final ModulePojo accessModule4 = createTestModule("accessModule4", "src/cobol/programs/accessModule4.cbl", project.identity());
		
		final UUID functionalBlockPojoUUID1 = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Functional_block 1",
				"Functional_block_description 1", new ModuleLocation(100, 20), null, null, project));

		final ReachabilityDataPojoPrototype ReachabilityDataPojoPrototype1 = new ReachabilityDataPojoPrototype().setUid(UUID.randomUUID())
				.setUpperBoundModuleId(upperModule1.identity())
				.setLowerBoundModuleId(lowerModule1.identity()).setAccessModuleId(accessModule1.identity())
				.setAccessTypes(List.of("AccessType1", "AccessType2"));
		final ReachabilityDataPojoPrototype ReachabilityDataPojoPrototype2 = new ReachabilityDataPojoPrototype().setUid(UUID.randomUUID())
				.setUpperBoundModuleId(upperModule1.identity())
				.setLowerBoundModuleId(lowerModule1.identity()).setAccessModuleId(accessModule2.identity())
				.setAccessTypes(List.of("AccessType1", "AccessType2"));
		blockServiceImpl.setReachabilityData(functionalBlockPojoUUID1, List.of(ReachabilityDataPojoPrototype1, ReachabilityDataPojoPrototype2));

		final UUID functionalBlockPojoUUID2 = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Functional_block 2",
				"Functional_block_description 2", new ModuleLocation(100, 20), null, null, project));
		final ReachabilityDataPojoPrototype ReachabilityDataPojoPrototype3 = new ReachabilityDataPojoPrototype().setUid(UUID.randomUUID())
				.setUpperBoundModuleId(upperModule2.identity())
				.setLowerBoundModuleId(lowerModule2.identity()).setAccessTypes(List.of("AccessType3", "AccessType4"));
		blockServiceImpl.setReachabilityData(functionalBlockPojoUUID2, List.of(ReachabilityDataPojoPrototype3));

		final UUID functionalBlockPojoUUID3 = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Functional_block 3",
				"Functional_block_description 3", new ModuleLocation(100, 20), null, null, project));
		final ReachabilityDataPojoPrototype ReachabilityDataPojoPrototype4 = new ReachabilityDataPojoPrototype().setUid(UUID.randomUUID())
				.setUpperBoundModuleId(upperModule1.identity())
				.setLowerBoundModuleId(lowerModule1.identity()).setAccessModuleId(accessModule3.identity());
		final ReachabilityDataPojoPrototype ReachabilityDataPojoPrototype5 = new ReachabilityDataPojoPrototype().setUid(UUID.randomUUID())
				.setUpperBoundModuleId(upperModule1.identity())
				.setLowerBoundModuleId(lowerModule1.identity()).setAccessModuleId(accessModule4.identity());

		blockServiceImpl.setReachabilityData(functionalBlockPojoUUID3, List.of(ReachabilityDataPojoPrototype4, ReachabilityDataPojoPrototype5));

		final UUID functionalBlockPojoUUID4 = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Functional_block 4",
				"Functional_block_description 4", new ModuleLocation(100, 20), null, null, project));

		final ReachabilityDataPojoPrototype ReachabilityDataPojoPrototype6 = new ReachabilityDataPojoPrototype().setUid(UUID.randomUUID())
				.setUpperBoundModuleId(upperModule2.identity());
		blockServiceImpl.setReachabilityData(functionalBlockPojoUUID4, List.of(ReachabilityDataPojoPrototype6));

		final ReachabilityDataPojo reachabilityDataPojo1 = new ReachabilityDataPojo(UUID.randomUUID(), functionalBlockPojoUUID1,
				upperModule1.identity(), Optional.of(lowerModule1.identity()), List.of(accessModule1.identity(),
				accessModule2.identity()), List.of("AccessType1", "AccessType2"), null);
		final ReachabilityDataPojo reachabilityDataPojo2 = new ReachabilityDataPojo(UUID.randomUUID(), functionalBlockPojoUUID2,
				upperModule2.identity(), Optional.of(lowerModule2.identity()), Collections.emptyList(), List.of("AccessType3", "AccessType4"), null);
		final ReachabilityDataPojo reachabilityDataPojo3 = new ReachabilityDataPojo(UUID.randomUUID(), functionalBlockPojoUUID3,
				upperModule1.identity(), Optional.of(lowerModule1.identity()), List.of(accessModule3.identity(),
				accessModule4.identity()), Collections.emptyList(), null);
		final ReachabilityDataPojo reachabilityDataPojo4 = new ReachabilityDataPojo(UUID.randomUUID(), functionalBlockPojoUUID4,
				upperModule2.identity(), Optional.empty(), Collections.emptyList(), Collections.emptyList(), null);

		final List<ReachabilityDataPojo> reachabilityDataList1 = blockServiceImpl.findReachabilityData(q -> q.ofFunctionalBlock(functionalBlockPojoUUID1));
		assertEquals(1, reachabilityDataList1.size());
		assertReachabilityData(reachabilityDataList1.get(0), reachabilityDataPojo1);

		final List<ReachabilityDataPojo> reachabilityDataList2 = blockServiceImpl.findReachabilityData(q -> q.ofFunctionalBlock(functionalBlockPojoUUID2));
		assertEquals(1, reachabilityDataList2.size());
		assertReachabilityData(reachabilityDataList2.get(0), reachabilityDataPojo2);

		final List<ReachabilityDataPojo> reachabilityDataList3 = blockServiceImpl.findReachabilityData(q -> q.ofFunctionalBlocks(List.of(functionalBlockPojoUUID3)));
		assertEquals(1, reachabilityDataList3.size());
		assertReachabilityData(reachabilityDataList3.get(0), reachabilityDataPojo3);

		final List<ReachabilityDataPojo> reachabilityDataList4 = blockServiceImpl.findReachabilityData(q -> q.ofFunctionalBlocks(List.of(functionalBlockPojoUUID3,
				functionalBlockPojoUUID4)));
		assertEquals(2, reachabilityDataList4.size());
		assertReachabilityData(reachabilityDataList4.stream()
				.filter(rd -> rd.getUpperBoundModuleId().getNid().equals(reachabilityDataPojo3.getUpperBoundModuleId().getNid()))
				.collect(Collectors.toList()).get(0), reachabilityDataPojo3);
		assertReachabilityData(reachabilityDataList4.stream()
				.filter(rd -> rd.getUpperBoundModuleId().getNid().equals(reachabilityDataPojo4.getUpperBoundModuleId().getNid()))
				.collect(Collectors.toList()).get(0), reachabilityDataPojo4);

		/* deleting both the functional Block After Tests */
		blockServiceImpl.delete(functionalBlockPojoUUID1);
		blockServiceImpl.delete(functionalBlockPojoUUID2);
		blockServiceImpl.delete(functionalBlockPojoUUID3);
		blockServiceImpl.delete(functionalBlockPojoUUID4);
	}

	@Test
	void testValidMergeAndUnMergeFunctionalBlocks() {
		final UUID preMergedBlock = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Pre_Merged_block", "Pre_Merged_block_description",
				new ModuleLocation(100, 250), null, null));
		final UUID mergeParent = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Merge_block", "Merge_block_description", new ModuleLocation(100, 250),
				List.of(preMergedBlock), null));
		final UUID commonParent = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Parent_block", "Parent_block_description", new ModuleLocation(100, 250),
				List.of(mergeParent, childFunctionalBlockPojoUid5, childFunctionalBlockPojoUid6), null));


		final UUID conditionUid1 = UUID.randomUUID();
		final UUID conditionUid2 = UUID.randomUUID();
		final UUID conditionUid3 = UUID.randomUUID();
		blockServiceImpl.setLinks(commonParent,List.of(new FunctionalBlockLink(UUID.randomUUID(), null, mergeParent, childFunctionalBlockPojoUid6, null,
						Map.of(FunctionalBlockLinkFlag.MERGE_CHILD_A.name(), preMergedBlock), new FunctionalBlockLinkCondition(conditionUid1,"Label1")),
				new FunctionalBlockLink(UUID.randomUUID(), null, childFunctionalBlockPojoUid5, mergeParent,  null,
						Map.of(FunctionalBlockLinkFlag.MERGE_CHILD_B.name(), preMergedBlock),  new FunctionalBlockLinkCondition(conditionUid2,"Label2")),
				new FunctionalBlockLink(UUID.randomUUID(), null, childFunctionalBlockPojoUid5, childFunctionalBlockPojoUid6, null,
						null,  new FunctionalBlockLinkCondition(conditionUid3,"Label3"))));
		blockServiceImpl.merge(commonParent, mergeParent, List.of(childFunctionalBlockPojoUid6));
		var commonParentBlock = blockServiceImpl.find(commonParent).orElseThrow();
		var mergeParentBlock = blockServiceImpl.find(mergeParent).orElseThrow();
		var commonParentBlockLinks = blockServiceImpl.getLinks(commonParent);
		var mergeParentBlockLinks = blockServiceImpl.getLinks(mergeParent);
		assertEquals(2, commonParentBlock.getChildren().size());
		assertEquals(2, mergeParentBlock.getChildren().size());
		assertEquals(2, commonParentBlockLinks.size());
		assertEquals(1, mergeParentBlockLinks.size());
		assertEquals(Set.of(childFunctionalBlockPojoUid5), commonParentBlockLinks.stream().map(FunctionalBlockLink::getChildA).collect(Collectors.toSet()));
		assertEquals(Set.of(mergeParent), commonParentBlockLinks.stream().map(FunctionalBlockLink::getChildB).collect(Collectors.toSet()));
		assertEquals(Set.of(FunctionalBlockLinkFlag.MERGE_CHILD_B.name()),
				commonParentBlockLinks.stream().map(FunctionalBlockLink::getFlags).map(Map::keySet).flatMap(Set::stream).collect(Collectors.toSet()));
		assertEquals(Set.of(preMergedBlock, childFunctionalBlockPojoUid6), commonParentBlockLinks.stream()
				.map(FunctionalBlockLink::getFlags)
				.map(Map::values)
				.flatMap(Collection::stream)
				.map(String::valueOf)
				.map(UUID::fromString)
				.collect(Collectors.toSet()));
		assertEquals(preMergedBlock, mergeParentBlockLinks.get(0).getChildA());
		assertEquals(childFunctionalBlockPojoUid6, mergeParentBlockLinks.get(0).getChildB());
		assertTrue(mergeParentBlockLinks.get(0).getFlags().isEmpty());

		blockServiceImpl.unmerge(commonParent, mergeParent, Collections.singleton(preMergedBlock));
		commonParentBlockLinks =blockServiceImpl.getLinks(commonParent);
		mergeParentBlockLinks =blockServiceImpl.getLinks(mergeParent);
		commonParentBlock = blockServiceImpl.find(commonParent).orElseThrow();
		mergeParentBlock = blockServiceImpl.find(mergeParent).orElseThrow();

		assertEquals(3, commonParentBlock.getChildren().size());
		assertEquals(1, mergeParentBlock.getChildren().size());
		assertEquals(3, commonParentBlockLinks.size());
		assertEquals(0, mergeParentBlockLinks.size());
		assertEquals(Set.of(preMergedBlock, childFunctionalBlockPojoUid5), commonParentBlockLinks.stream()
				.map(FunctionalBlockLink::getChildA).collect(Collectors.toSet()));
		assertEquals(Set.of(preMergedBlock, mergeParent), commonParentBlockLinks.stream().map(FunctionalBlockLink::getChildB).collect(Collectors.toSet()));
		assertEquals(Set.of(FunctionalBlockLinkFlag.MERGE_CHILD_B.name()),
				commonParentBlockLinks.stream().map(FunctionalBlockLink::getFlags).map(Map::keySet).flatMap(Set::stream).collect(Collectors.toSet()));
		assertEquals(Set.of(childFunctionalBlockPojoUid6), commonParentBlockLinks.stream()
				.map(FunctionalBlockLink::getFlags)
				.map(Map::values)
				.flatMap(Collection::stream)
				.map(String::valueOf)
				.map(UUID::fromString)
				.collect(Collectors.toSet()));

		blockServiceImpl.unmerge(commonParent, mergeParent, Collections.singleton(childFunctionalBlockPojoUid6));
		commonParentBlock = blockServiceImpl.find(commonParent).orElseThrow();
		mergeParentBlock = blockServiceImpl.find(mergeParent).orElseThrow();
		commonParentBlockLinks =blockServiceImpl.getLinks(commonParent);
		mergeParentBlockLinks =blockServiceImpl.getLinks(mergeParent);
		assertEquals(4, commonParentBlock.getChildren().size());
		assertEquals(0, mergeParentBlock.getChildren().size());
		assertEquals(3,commonParentBlockLinks.size());
		assertEquals(0, mergeParentBlockLinks.size());
		assertEquals(Set.of(preMergedBlock, childFunctionalBlockPojoUid5), commonParentBlockLinks.stream()
				.map(FunctionalBlockLink::getChildA).collect(Collectors.toSet()));
		assertEquals(Set.of(preMergedBlock, childFunctionalBlockPojoUid6), commonParentBlockLinks.stream()
				.map(FunctionalBlockLink::getChildB).collect(Collectors.toSet()));
		assertEquals(0, mergeParentBlockLinks.stream().map(FunctionalBlockLink::getFlags).map(Map::keySet).mapToLong(Set::size).sum());

		blockServiceImpl.delete(commonParent);
		blockServiceImpl.delete(mergeParent);
		blockServiceImpl.delete(preMergedBlock);
	}

	@Test
	void testValidMergeAndUnMergeForMultipleFunctionalBlocks() {
		final UUID mergeParent = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Merge_block", "Merge_block_description", new ModuleLocation(100, 250),
				null, null));
		final UUID commonParent = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Parent_block", "Parent_block_description", new ModuleLocation(100, 250),
				List.of(mergeParent, childFunctionalBlockPojoUid5, childFunctionalBlockPojoUid6, childFunctionalBlockPojoUid1), null));
		final UUID conditionId = UUID.randomUUID();
		final UUID conditionId1 = UUID.randomUUID();
		functionalBlockLinkCondition1 = new FunctionalBlockLinkCondition(conditionId , "Label1");
		functionalBlockLinkCondition2 = new FunctionalBlockLinkCondition(conditionId1 , "Label2");
		blockServiceImpl.setLinks(commonParent,List.of(new FunctionalBlockLink(UUID.randomUUID(), null, childFunctionalBlockPojoUid1, childFunctionalBlockPojoUid6, null, null,
						functionalBlockLinkCondition1),
				new FunctionalBlockLink(UUID.randomUUID(), null, childFunctionalBlockPojoUid5, childFunctionalBlockPojoUid1, null, null,
						functionalBlockLinkCondition2)));

		blockServiceImpl.merge(commonParent, mergeParent, List.of(childFunctionalBlockPojoUid5, childFunctionalBlockPojoUid6));
		var commonParentBlock = blockServiceImpl.find(commonParent).orElseThrow();
		var mergeParentBlock = blockServiceImpl.find(mergeParent).orElseThrow();
		final List<FunctionalBlockLink> commonParentBlockLink = blockServiceImpl.getLinks(commonParentBlock.getUid());
		final List<FunctionalBlockLink> mergeParentBlockLink = blockServiceImpl.getLinks(mergeParentBlock.getUid());
		assertEquals(2, commonParentBlock.getChildren().size());
		assertEquals(2, mergeParentBlock.getChildren().size());
		assertEquals(2, commonParentBlockLink.size());
		assertEquals(0,mergeParentBlockLink.size());

		var link1 = commonParentBlockLink.stream().filter(l -> l.getChildA().equals(childFunctionalBlockPojoUid1)).findFirst().orElseThrow();
		var link2 = commonParentBlockLink.stream().filter(l -> l.getChildB().equals(childFunctionalBlockPojoUid1)).findFirst().orElseThrow();
		assertEquals(mergeParent, link1.getChildB());
		assertEquals(mergeParent, link2.getChildA());
		assertEquals(Set.of(FunctionalBlockLinkFlag.MERGE_CHILD_B.name()), link1.getFlags().keySet());
		assertEquals(Set.of(FunctionalBlockLinkFlag.MERGE_CHILD_A.name()), link2.getFlags().keySet());
		assertEquals(childFunctionalBlockPojoUid6.toString(), link1.getFlags().get(FunctionalBlockLinkFlag.MERGE_CHILD_B.name()));
		assertEquals(childFunctionalBlockPojoUid5.toString(), link2.getFlags().get(FunctionalBlockLinkFlag.MERGE_CHILD_A.name()));

		blockServiceImpl.unmerge(commonParent, mergeParent, List.of(childFunctionalBlockPojoUid5, childFunctionalBlockPojoUid6));
		commonParentBlock = blockServiceImpl.find(commonParent).orElseThrow();
		mergeParentBlock = blockServiceImpl.find(mergeParent).orElseThrow();
		final List<FunctionalBlockLink> commonParentLinks = blockServiceImpl.getLinks(commonParentBlock.getUid());
		final List<FunctionalBlockLink> mergeParentLinks =blockServiceImpl.getLinks(mergeParentBlock.getUid());

		assertEquals(4, commonParentBlock.getChildren().size());
		assertEquals(0, mergeParentBlock.getChildren().size());
		assertEquals(2, commonParentLinks.size());
		assertEquals(0,mergeParentLinks.size());

		link1 = commonParentLinks.stream().filter(l -> l.getChildA().equals(childFunctionalBlockPojoUid1)).findFirst().orElseThrow();
		link2 = commonParentLinks.stream().filter(l -> l.getChildB().equals(childFunctionalBlockPojoUid1)).findFirst().orElseThrow();

		assertEquals(childFunctionalBlockPojoUid6, link1.getChildB());
		assertEquals(childFunctionalBlockPojoUid5, link2.getChildA());
		assertTrue(link1.getFlags().isEmpty());
		assertTrue(link2.getFlags().isEmpty());

		blockServiceImpl.delete(commonParent);
		blockServiceImpl.delete(mergeParent);
	}

	@Test
	void testMergingBlocksWithInValidRelationshipThrowsErrors() {
		/* Merge Parent is not a child of common parent */
		final UUID commonParent = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Pre_Merged_block", "Pre_Merged_block_description",
				new ModuleLocation(100, 250), List.of(childFunctionalBlockPojoUid6), null));
		final UUID mergeParent = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Merge_block", "Merge_block_description", new ModuleLocation(100, 250),
				null, null));
		final IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
				() -> blockServiceImpl.merge(commonParent, mergeParent, List.of(childFunctionalBlockPojoUid6)));
		assertTrue(exception.getMessage().startsWith("mergeParent"));
		blockServiceImpl.delete(commonParent);
		blockServiceImpl.delete(mergeParent);

		/* Merge Block is not a child of common parent */
		final UUID mergeParent1 = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Merge_block", "Merge_block_description", new ModuleLocation(100, 250),
				null, null));
		final UUID commonParent1 = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Pre_Merged_block", "Pre_Merged_block_description",
				new ModuleLocation(100, 250), List.of(mergeParent1), null));
		final IllegalArgumentException exception1 = assertThrows(IllegalArgumentException.class, () -> blockServiceImpl.merge(commonParent1, mergeParent1,
				List.of(childFunctionalBlockPojoUid6)));
		assertTrue(exception1.getMessage().startsWith("mergeParent"));
		blockServiceImpl.delete(commonParent1);
		blockServiceImpl.delete(mergeParent1);

		/* Merge Block is a parent of Merge Child */
		final UUID mergeParent2 = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Merge_block", "Merge_block_description", new ModuleLocation(100, 250),
				List.of(childFunctionalBlockPojoUid6), null));
		final UUID commonParent2 = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Pre_Merged_block", "Pre_Merged_block_description",
				new ModuleLocation(100, 250), List.of(mergeParent2, childFunctionalBlockPojoUid6), null));
		final IllegalArgumentException exception2 = assertThrows(IllegalArgumentException.class, () -> blockServiceImpl.merge(commonParent2, mergeParent2,
				List.of(childFunctionalBlockPojoUid6)));
		assertTrue(exception2.getMessage().startsWith("mergeParent"));
		blockServiceImpl.delete(commonParent2);
		blockServiceImpl.delete(mergeParent2);
	}

	@Test
	void testUnMergingBlocksWithInValidRelationshipThrowsErrors() {
		/* Merge Parent is not a child of common parent */
		final UUID commonParent = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Pre_Merged_block", "Pre_Merged_block_description",
				new ModuleLocation(100, 250), null, null));
		final UUID mergeParent = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Merge_block", "Merge_block_description", new ModuleLocation(100, 250),
				List.of(childFunctionalBlockPojoUid6), null));
		final IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> blockServiceImpl.unmerge(commonParent, mergeParent,
				List.of(childFunctionalBlockPojoUid6)));
		assertTrue(exception.getMessage().startsWith("mergeChildren"));
		blockServiceImpl.delete(commonParent);
		blockServiceImpl.delete(mergeParent);

		/* Merge Block is not a child of merge parent */
		final UUID mergeParent1 = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Merge_block", "Merge_block_description", new ModuleLocation(100, 250),
				null, null));
		final UUID commonParent1 = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Pre_Merged_block", "Pre_Merged_block_description",
				new ModuleLocation(100, 250), List.of(mergeParent1), null));
		final IllegalArgumentException exception1 = assertThrows(IllegalArgumentException.class, () -> blockServiceImpl.unmerge(commonParent1, mergeParent1,
				List.of(childFunctionalBlockPojoUid6)));
		assertTrue(exception1.getMessage().startsWith("mergeChildren"));
		blockServiceImpl.delete(commonParent1);
		blockServiceImpl.delete(mergeParent1);
	}
	
	@SuppressWarnings("unchecked")
	@Test
	void testFunctionalBlockAggregation() {
		final ProjectPojo project2 = createProject(TWO);
		final UUID functionalBlockPojoUUID1 = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Functional_block1",
				"Functional_block_description1", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)),  project2));
		final UUID functionalBlockPojoUUID2 = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Functional_block2",
				"Functional_block_description2", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)), project2));
		final UUID functionalBlockPojoUUID3 = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Functional_block3",
				"Functional_block_description3", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)),  project2));
		final UUID functionalBlockPojoUUID4 = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Functional_block4",
				"Functional_block_description4", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY)),  project2));
		final UUID functionalBlockPojoUUID5 = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Functional_block5",
				"Functional_block_description5", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY)),  project2));
		final UUID functionalBlockPojoUUID6 = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Functional_block6",
				"Functional_block_description6", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.MODULE)), project2));
		
		/* testing List of UUID aggregation for functionalBlock. */
		final AggregationRequest<FunctionalBlockFieldName> request1 = new AggregationRequest<>();
		request1.getGroupBy().add(FunctionalBlockFieldName.TYPE);
		request1.getFields().put(FunctionalBlockFieldName.UID, AggregationOperator.LIST);
		request1.getFilterObject().put(FunctionalBlockFieldName.PROJECT_ID, Map.of(FilterOperators.OPERATOR_EQ, project2.identity()));

		final Map<String, Set<UUID>> aggregationUUIDList = blockServiceImpl.getAggregations(request1).stream().collect(Collectors.toMap(
						agg -> ((List<String>) agg.getGroup().get(FunctionalBlockFieldName.TYPE)).get(0),
						agg -> ((List<UUID>) agg.getFields().get(FunctionalBlockFieldName.UID)).stream().collect(Collectors.toSet())));
		
		assertEquals(3, aggregationUUIDList.size());
		assertEquals(1, aggregationUUIDList.get("MODULE").size());
		assertEquals(Set.of(functionalBlockPojoUUID6), aggregationUUIDList.get("MODULE"));
		assertEquals(3, aggregationUUIDList.get("FUNCTIONAL_UNIT").size());
		assertEquals(Set.of(functionalBlockPojoUUID1, functionalBlockPojoUUID2, functionalBlockPojoUUID3), aggregationUUIDList.get("FUNCTIONAL_UNIT"));
		assertEquals(2, aggregationUUIDList.get("REACHABILITY").size());
		assertEquals(Set.of(functionalBlockPojoUUID4, functionalBlockPojoUUID5), aggregationUUIDList.get("REACHABILITY"));
		
		/* testing Count aggregation for functionalBlock. */
		final AggregationRequest<FunctionalBlockFieldName> request2 = new AggregationRequest<>();
		request2.getGroupBy().add(FunctionalBlockFieldName.TYPE);
		request2.getFields().put(FunctionalBlockFieldName.UID, AggregationOperator.COUNT);
		request2.getFilterObject().put(FunctionalBlockFieldName.PROJECT_ID, Map.of(FilterOperators.OPERATOR_EQ, project2.identity()));
		final Map<String, Long> aggregationCount = blockServiceImpl.getAggregations(request2).stream().collect(Collectors.toMap(
						agg -> ((List<String>) agg.getGroup().get(FunctionalBlockFieldName.TYPE)).get(0),
						agg -> ((Long) agg.getFields().get(FunctionalBlockFieldName.UID))));
		
		assertEquals(1, aggregationCount.get("MODULE"));
		assertEquals(3, aggregationCount.get("FUNCTIONAL_UNIT"));
		assertEquals(2, aggregationCount.get("REACHABILITY"));
		blockServiceImpl.delete(functionalBlockPojoUUID1);
		blockServiceImpl.delete(functionalBlockPojoUUID2);
		blockServiceImpl.delete(functionalBlockPojoUUID3);
		blockServiceImpl.delete(functionalBlockPojoUUID4);
		blockServiceImpl.delete(functionalBlockPojoUUID5);
		blockServiceImpl.delete(functionalBlockPojoUUID6);
	}

	private void assertReachabilityData(final ReachabilityDataPojo actualReachabilityData, final ReachabilityDataPojo expectedReachabilityData) {
		assertEquals(expectedReachabilityData.getFunctionalBlock(), actualReachabilityData.getFunctionalBlock());
		assertEquals(expectedReachabilityData.getUpperBoundModuleId(), actualReachabilityData.getUpperBoundModuleId());
		assertEquals(expectedReachabilityData.getLowerBoundModuleId(), actualReachabilityData.getLowerBoundModuleId());
		assertEquals(expectedReachabilityData.getAccessTypes().size(), actualReachabilityData.getAccessTypes().size());
		assertEquals(expectedReachabilityData.getAccessModuleIds().size(), actualReachabilityData.getAccessModuleIds().size());
		assertEquals(expectedReachabilityData.getAccessTypes().stream().collect(Collectors.toSet()),
				actualReachabilityData.getAccessTypes().stream().collect(Collectors.toSet()));
		assertEquals(expectedReachabilityData.getAccessModuleIds().stream()
						.map(id -> moduleService.findAnyModuleLightweight(q -> q.ofProject(project.identity()).byId(id))
												.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with id: " + id + " in project: " + project)))
						.map(ModuleLightweightPojo::getName).collect(Collectors.toSet()), 
					actualReachabilityData.getAccessModuleIds().stream()
						.map(id -> moduleService.findAnyModuleLightweight(q -> q.ofProject(project.identity()).byId(id))
												.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with id: " + id + " in project: " + project)))
						.map(ModuleLightweightPojo::getName).collect(Collectors.toSet()));
	}

	@Test
	void testChildrenDeepMultipleRoot() {
		/*
		 * RootBlock1                RootBlock2
		 *     |                         |
		 *     |                          |
		 *   Child1                    Child2
		 *     |                          |
		 *     |                          |
		 *     |                          |
		 *     |----------Child3----------|
		 */

		final UUID child3 = blockServiceImpl.create(new FunctionalBlockPojoPrototype().setProject(project.identity()).setName("Child3")
				.setDescription(""));
		final UUID child2 = blockServiceImpl.create(new FunctionalBlockPojoPrototype().setProject(project.identity()).setName("Child2")
				.setDescription("").setChildren(List.of(child3)));
		final UUID child1 = blockServiceImpl.create(new FunctionalBlockPojoPrototype().setProject(project.identity()).setName("Child1")
				.setDescription("").setChildren(List.of(child3)));
		final UUID root2 = blockServiceImpl.create(new FunctionalBlockPojoPrototype().setProject(project.identity()).setName("RootBlock2")
				.setDescription("").setChildren(List.of(child2)));
		final UUID root1 = blockServiceImpl.create(new FunctionalBlockPojoPrototype().setProject(project.identity()).setName("RootBlock1")
				.setDescription("").setChildren(List.of(child1)));

		final Map<UUID, List<FunctionalBlockPojo>> childrenDeep = blockServiceImpl.findChildrenDeep(List.of(root1, root2), -1, q -> {});

		assertEquals(2, childrenDeep.size(), "expected to find children for 2 root blocks");
		final List<FunctionalBlockPojo> childrenForRoot1 = childrenDeep.get(root1);
		final List<FunctionalBlockPojo> childrenForRoot2 = childrenDeep.get(root2);
		assertNotNull(childrenForRoot1, "expected to find children for RootBlock1");
		assertNotNull(childrenForRoot2, "expected to find children for RootBlock2");

		assertEquals(Set.of(child1, child3), childrenForRoot1.stream().map(FunctionalBlockPojo::getUid).collect(Collectors.toSet()),
				"expected to find Child1, Child3 for RootBlock1");
		assertEquals(Set.of(child2, child3), childrenForRoot2.stream().map(FunctionalBlockPojo::getUid).collect(Collectors.toSet()),
				"expected to find Child2, Child3 for RootBlock2");
	}

	@Test
	void testChildrenDeepLoop() {
		/*
		 * RootBlock1--|
		 *     |       |
		 *     |       |
		 *   Child1    |
		 *     |       |
		 *     |       |
		 *   Child2----|
		 */

		final UUID child2 = blockServiceImpl.create(new FunctionalBlockPojoPrototype().setProject(project.identity()).setName("Child2").setDescription(""));
		final UUID child1 = blockServiceImpl.create(new FunctionalBlockPojoPrototype().setProject(project.identity()).setName("Child1").setDescription(""));
		final UUID root1 = blockServiceImpl.create(new FunctionalBlockPojoPrototype().setProject(project.identity()).setName("RootBlock1").setDescription(""));

		blockServiceImpl.update(new FunctionalBlockPojoPrototype().setUid(child2).setChildren(List.of(root1)));
		blockServiceImpl.update(new FunctionalBlockPojoPrototype().setUid(child1).setChildren(List.of(child2)));
		blockServiceImpl.update(new FunctionalBlockPojoPrototype().setUid(root1).setChildren(List.of(child1)));

		final List<FunctionalBlockPojo> childrenDeep = blockServiceImpl.findChildrenDeep(root1, -1, q -> {});
		assertEquals(3, childrenDeep.size(), "expected to find 3 children of RootBlock1 (because it finds RootBlock1 itself to indicate loop)");
		assertEquals(List.of(child1, child2, root1), childrenDeep.stream().map(FunctionalBlockPojo::getUid).collect(Collectors.toList()),
				"expected to find Child1, Child2, RootBlock1");
	}

	@Test
	void testChildrenDeepMaxDepth() {
		/*
		 * RootBlock1 (recursion depth)
		 *     |
		 *     |
		 *   Child1 (0)
		 *     |
		 *     |
		 *   Child2 (1)
		 *     |
		 *     |
		 *   Child3 (2)
		 *     |
		 *     |
		 *   Child4 (3)
		 */

		final UUID child4 = blockServiceImpl.create(new FunctionalBlockPojoPrototype().setProject(project.identity()).setName("Child4")
				.setDescription(""));
		final UUID child3 = blockServiceImpl.create(new FunctionalBlockPojoPrototype().setProject(project.identity()).setName("Child3")
				.setDescription("").setChildren(List.of(child4)));
		final UUID child2 = blockServiceImpl.create(new FunctionalBlockPojoPrototype().setProject(project.identity()).setName("Child2")
				.setDescription("").setChildren(List.of(child3)));
		final UUID child1 = blockServiceImpl.create(new FunctionalBlockPojoPrototype().setProject(project.identity()).setName("Child1")
				.setDescription("").setChildren(List.of(child2)));
		final UUID root1 = blockServiceImpl.create(new FunctionalBlockPojoPrototype().setProject(project.identity()).setName("RootBlock1")
				.setDescription("").setChildren(List.of(child1)));

		final List<FunctionalBlockPojo> unlimited = blockServiceImpl.findChildrenDeep(root1, -1, q -> {});
		assertEquals(List.of(child1, child2, child3, child4), unlimited.stream().map(FunctionalBlockPojo::getUid).collect(Collectors.toList()),
				"With unlimited depth, expected to find Child1, Child2, Child3, Child4");
		final List<FunctionalBlockPojo> depth0 = blockServiceImpl.findChildrenDeep(root1, 0, q -> {});
		assertEquals(List.of(child1), depth0.stream().map(FunctionalBlockPojo::getUid).collect(Collectors.toList()),
				"With max depth 0, expected to find Child1 only");
		final List<FunctionalBlockPojo> depth1 = blockServiceImpl.findChildrenDeep(root1, 1, q -> {});
		assertEquals(List.of(child1, child2), depth1.stream().map(FunctionalBlockPojo::getUid).collect(Collectors.toList()),
				"With max depth 1, expected to find Child1, Child2");
		final List<FunctionalBlockPojo> depth2 = blockServiceImpl.findChildrenDeep(root1, 2, q -> {});
		assertEquals(List.of(child1, child2, child3), depth2.stream().map(FunctionalBlockPojo::getUid).collect(Collectors.toList()),
				"With max depth 2, expected to find Child1, Child2, Child3");
		final List<FunctionalBlockPojo> depth3 = blockServiceImpl.findChildrenDeep(root1, 3, q -> {});
		assertEquals(List.of(child1, child2, child3, child4), depth3.stream().map(FunctionalBlockPojo::getUid).collect(Collectors.toList()),
				"With max depth 3, expected to find Child1, Child2, Child3, Child4");
		/* depth 4+ should find the same as depth 3 (and unlimited) */
		final List<FunctionalBlockPojo> depth4 = blockServiceImpl.findChildrenDeep(root1, 4, q -> {});
		assertEquals(List.of(child1, child2, child3, child4), depth4.stream().map(FunctionalBlockPojo::getUid).collect(Collectors.toList()),
				"With max depth 4, expected to find Child1, Child2, Child3, Child4");
	}

	@Test
	void testSetChildrenDeep() {
		final FunctionalBlockPojoPrototype child2 = new FunctionalBlockPojoPrototype();
		child2.setProject(project.identity());
		child2.setUid(UUID.randomUUID());
		child2.setName("Child 2");
		child2.setDescription("");

		final FunctionalBlockPojoPrototype child1 = new FunctionalBlockPojoPrototype();
		child1.setProject(project.identity());
		child1.setUid(UUID.randomUUID());
		child1.setName("Child 1");
		child1.setDescription("");
		child1.setChildren(List.of(child2.uid.getNonNull()));


		final FunctionalBlockPojoPrototype parent = new FunctionalBlockPojoPrototype();
		parent.setProject(project.identity());
		parent.setUid(UUID.randomUUID());
		parent.setName("Parent");
		parent.setDescription("");
		parent.setChildren(List.of(child1.uid.getNonNull()));

		blockServiceImpl.create(child2);
		blockServiceImpl.create(child1);
		blockServiceImpl.create(parent);

		final List<UUID> initialChildrenDeep = blockServiceImpl.getChildrenDeep(parent.uid.getNonNull());
		assertEquals(Collections.emptyList(), initialChildrenDeep, "expected to get empty list when childrenDeep was not set");

		blockServiceImpl.setChildrenDeep(parent.uid.getNonNull(), List.of(child1.uid.getNonNull(), child2.uid.getNonNull()));
		/* the order of the children deep is not persisted (but they don't really have any order anyway) */
		final Set<UUID> afterFirstSet = new HashSet<>(blockServiceImpl.getChildrenDeep(parent.uid.getNonNull()));
		assertEquals(Set.of(child1.uid.getNonNull(), child2.uid.getNonNull()), afterFirstSet, "expected it to persist the list of children");

		blockServiceImpl.setChildrenDeep(parent.uid.getNonNull(), List.of(child1.uid.getNonNull()));
		final List<UUID> afterSecondSet = blockServiceImpl.getChildrenDeep(parent.uid.getNonNull());
		assertEquals(List.of(child1.uid.getNonNull()), afterSecondSet, "expected it to replace the list of children");
	}

	@Test
	void testUpdateFlags() {
		final var testBlock = blockServiceImpl.create(createFunctionalBlockPojoPrototype("TestBlock", "TestBlock", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT))));
		final var testBlockWithExistingStatus = blockServiceImpl.create(createFunctionalBlockPojoPrototype("TestBlockWithExistingStatus",
				"TestBlockWithExistingStatus", new ModuleLocation(100, 250), null, Map.of(FunctionalBlockFlag.TYPE.name(),
						List.of(FunctionalBlockType.FUNCTIONAL_UNIT), FunctionalBlockFlag.STATUS.name(), FunctionalBlockStatus.INACTIVE)));

		blockServiceImpl.updateBlocksStatus(List.of(testBlock), FunctionalBlockStatus.INACTIVE);
		blockServiceImpl.updateBlocksStatus(List.of(testBlockWithExistingStatus), FunctionalBlockStatus.ACTIVE);

		final var updatedTestBlock = blockServiceImpl.find(testBlock).orElseThrow();
		final var updatedTestBlockWithExistingStatus = blockServiceImpl.find(testBlockWithExistingStatus).orElseThrow();

		assertTrue(updatedTestBlock.getFlags().containsKey(FunctionalBlockFlag.STATUS.name()));
		assertEquals(FunctionalBlockStatus.INACTIVE.name(), updatedTestBlock.getFlags().get(FunctionalBlockFlag.STATUS.name()));
		assertFalse(updatedTestBlockWithExistingStatus.getFlags().containsKey(FunctionalBlockFlag.STATUS.name()));
	}

	@Test
	void testFindGeneratedFromModules() {
		final ModulePojo cobolModule1 = createTestModule("TEST_MODULE1", "src/cobol/Test1.cbl", project.identity());
		final ModulePojo cobolModule2 = createTestModule("TEST_MODULE2", "src/cobol/Test2.cbl", project.identity());
		final ModulePojo cobolModule3 = createTestModule("TEST_MODULE3", "src/cobol/Test3.cbl", project3.identity());
		ModulePojo cobolModule4 = createTestModule("TEST_MODULE4", "src/cobol/Test4.cbl", project3.identity());

		final String linkHash = cobolModule1.getLinkHash();
		cobolModule4 = moduleService.getModule(moduleService.update(new ModulePojoPrototype()
																			.withId(cobolModule4.identity())
																			.setLinkHash(linkHash)));

		final UUID functionalBlockPojoUUID1 = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Functional_block1",
				"Functional_block_description1", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)), project));
		blockServiceImpl.setGeneratedFrom(functionalBlockPojoUUID1,
				new GeneratedFrom(cobolModule1.getLinkHash(), null, null, null, null, null, null));

		final UUID functionalBlockPojoUUID2 = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Functional_block2",
				"Functional_block_description2", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)), project));
		blockServiceImpl.setGeneratedFrom(functionalBlockPojoUUID2,
				new GeneratedFrom(cobolModule2.getLinkHash(), null, null, null, null, null, null));

		final UUID functionalBlockPojoUUID3 = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Functional_block3",
				"Functional_block_description3", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)), project3));
		blockServiceImpl.setGeneratedFrom(functionalBlockPojoUUID3,
				new GeneratedFrom(cobolModule3.getLinkHash(), null, null, null, null, null, null));

		final UUID functionalBlockPojoUUID4 = blockServiceImpl.create(createFunctionalBlockPojoPrototype("Functional_block4",
				"Functional_block_description4", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)), project3));
		blockServiceImpl.setGeneratedFrom(functionalBlockPojoUUID4,
				new GeneratedFrom(cobolModule4.getLinkHash(), null, null, null, null, null, null));

		final Map<String, UUID> generatedFromModules1 = blockServiceImpl.findGeneratedFromModules(List.of(cobolModule1.getLinkHash(),
				cobolModule2.getLinkHash(), cobolModule3.getLinkHash()), project.identity());
		assertEquals(2, generatedFromModules1.size());
		assertEquals(functionalBlockPojoUUID1, generatedFromModules1.get(cobolModule1.getLinkHash()));
		assertEquals(functionalBlockPojoUUID2, generatedFromModules1.get(cobolModule2.getLinkHash()));

		final Map<String, UUID> generatedFromModules2 = blockServiceImpl.findGeneratedFromModules(List.of(cobolModule1.getLinkHash(),
				cobolModule2.getLinkHash(), cobolModule3.getLinkHash()), project3.identity());
		assertEquals(2, generatedFromModules2.size());
		assertEquals(functionalBlockPojoUUID4, generatedFromModules2.get(cobolModule1.getLinkHash()));
		assertEquals(functionalBlockPojoUUID3, generatedFromModules2.get(cobolModule3.getLinkHash()));

		blockServiceImpl.delete(functionalBlockPojoUUID1);
		blockServiceImpl.delete(functionalBlockPojoUUID2);
		blockServiceImpl.delete(functionalBlockPojoUUID3);
		blockServiceImpl.delete(functionalBlockPojoUUID4);
	}

	@Test
	void testFindIntermediateModuleForReachabilityData() {
		final ModulePojo lowerModule = createTestModule("lowerModule", "path/lowerModule.cbl", project.identity());
		final ModulePojo upperModule = createTestModule("upperModule", "path/upperModule.cbl", project.identity());
		final ModulePojo accessModule1 = createTestModule("accessModule1", "path/accessModule1", project.identity());
		final ModulePojo accessModule2 = createTestModule("accessModule2", "path/accessModule2", project.identity());

		final ModulePojo intermediateModule1 = createTestModule("intermediateModule1", "path/intermediateModule1.cbl", project.identity());
		final ModulePojo intermediateModule2 = createTestModule("intermediateModule2", "path/intermediateModule2.cbl", project.identity());
		final ModulePojo intermediateModule3 = createTestModule("intermediateModule3", "path/intermediateModule3.cbl", project.identity());
		final ModulePojo intermediateModule4 = createTestModule("intermediateModule4", "path/intermediateModule4.cbl", project.identity());

		final FunctionalBlockPojoPrototype functionalBlockPojoPrototype = createFunctionalBlockPojoPrototype("Functional_block", "Functional_block_description",
				new ModuleLocation(100, 20), null, null, project);
		final UUID functionalBlockPojoUUID = blockServiceImpl.create(functionalBlockPojoPrototype);
		final UUID reachabilityDataUUID1 = UUID.randomUUID();
		final UUID reachabilityDataUUID2 = UUID.randomUUID();
		blockServiceImpl.setReachabilityData(functionalBlockPojoUUID, List.of(
				new ReachabilityDataPojoPrototype().setUid(reachabilityDataUUID1)
						.setUpperBoundModuleId(upperModule.identity())
						.setLowerBoundModuleId(lowerModule.identity())
						.setAccessModuleId(accessModule1.identity())
						.setAccessTypes(List.of("AccessType1", "AccessType2"))
						.setIntermediateModules(List.of(intermediateModule1.identity(), intermediateModule2.identity())),
				new ReachabilityDataPojoPrototype().setUid(reachabilityDataUUID2)
						.setUpperBoundModuleId(upperModule.identity())
						.setLowerBoundModuleId(lowerModule.identity())
						.setAccessModuleId(accessModule2.identity())
						.setAccessTypes(List.of("AccessType2", "AccessType3"))
						.setIntermediateModules(List.of(intermediateModule3.identity(), intermediateModule4.identity()))
		));

		final List<ReachabilityDataPojo> reachabilityData = blockServiceImpl.findReachabilityData(q ->
				q.ofFunctionalBlock(functionalBlockPojoUUID).aggregateAccessModulesPerLowerBound(false));
		assertEquals(2, reachabilityData.size());
		final ReachabilityDataPojo reachabilityData1 = reachabilityData.stream().filter(reachabilityDataPojo ->
				reachabilityDataPojo.getAccessModuleIds().get(0).getNid() .equals(accessModule1.getId())).findAny().orElseThrow();
		assertEquals(2, reachabilityData1.getIntermediateModules().size());
		assertEquals(Set.of(intermediateModule1.getId(), intermediateModule2.getId()), reachabilityData1.getIntermediateModules().stream()
				.map(EntityId::getNid).collect(Collectors.toSet()));
		assertEquals(accessModule1.identity(), reachabilityData1.getAccessModuleIds().get(0));
		assertEquals(Set.of("AccessType1", "AccessType2"), new HashSet<>(reachabilityData1.getAccessTypes()));

		final ReachabilityDataPojo reachabilityData2 = reachabilityData.stream().filter(reachabilityDataPojo ->
				reachabilityDataPojo.getAccessModuleIds().get(0).getNid().equals(accessModule2.getId())).findFirst().orElseThrow();
		assertEquals(2, reachabilityData2.getIntermediateModules().size());
		assertEquals(Set.of(intermediateModule3.getId(), intermediateModule4.getId()), reachabilityData2.getIntermediateModules().stream()
				.map(EntityId::getNid).collect(Collectors.toSet()));
		assertEquals(accessModule2.identity(), reachabilityData2.getAccessModuleIds().get(0));
		assertEquals(Set.of("AccessType2", "AccessType3"), new HashSet<>(reachabilityData2.getAccessTypes()));

		final List<ReachabilityDataPojo> reachabilityDataWithAccessModuleAggregation = blockServiceImpl.findReachabilityData(q ->
				q.ofFunctionalBlock(functionalBlockPojoUUID).aggregateAccessModulesPerLowerBound(true));
		assertEquals(1, reachabilityDataWithAccessModuleAggregation.size());
		assertEquals(2, reachabilityDataWithAccessModuleAggregation.get(0).getAccessModuleIds().size());
		assertEquals(4, reachabilityDataWithAccessModuleAggregation.get(0).getIntermediateModules().size());
		assertEquals(Set.of(intermediateModule1.getId(), intermediateModule2.getId(), intermediateModule3.getId(), intermediateModule4.getId()),
				reachabilityDataWithAccessModuleAggregation.get(0).getIntermediateModules().stream().map(EntityId::getNid).collect(Collectors.toSet()));
		assertEquals(Set.of(accessModule1.identity(), accessModule2.identity()),
				new HashSet<>(reachabilityDataWithAccessModuleAggregation.get(0).getAccessModuleIds()));
		assertEquals(Set.of("AccessType1", "AccessType2", "AccessType3"),
				new HashSet<>(reachabilityDataWithAccessModuleAggregation.get(0).getAccessTypes()));
		/* deleting the functional Block After Tests */
		blockServiceImpl.delete(functionalBlockPojoUUID);
	}

	@Test
	void testAggregateFunctionalBlockPeers() {
		final ModulePojo module = createTestModule("Peers_Module", "Peers_Test_Module", project.identity());
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

		blockServiceImpl.setResolvedModuleParts(block1Uid, Collections.singleton(new ResolvedModulePart(module.identity())));
		blockServiceImpl.setResolvedModuleParts(block2Uid, Collections.singleton(new ResolvedModulePart(module.identity())));
		blockServiceImpl.setResolvedModuleParts(child1Uid, Collections.singleton(new ResolvedModulePart(module.identity())));
		blockServiceImpl.setChildrenDeep(block1Uid, List.of(child1Uid));

		final Set<UUID> peersOfBlock1 = blockServiceImpl.find(q -> q.withPeer(peer -> peer.byUid(block1Uid))).stream()
				.map(FunctionalBlockPojo::getUid)
				.collect(Collectors.toSet());
		assertEquals(1, peersOfBlock1.size());
		assertTrue(peersOfBlock1.contains(block2Uid));
		final var table1 = blockServiceImpl.getAggregations(q -> q.withPeer(p -> p.byUid(block1Uid))
				.aggregate(FunctionalBlockFieldName.UID, AggregationOperator.COUNT).distinct());
		assertEquals(peersOfBlock1.size(), Integer.parseInt(table1.map(data -> data.isEmpty() ? null : data.get(0))
				.map(row -> row.get(FunctionalBlockFieldName.UID.name().toLowerCase()))
				.orElse(0).toString()));

		final Set<UUID> peersOfBlock2 = blockServiceImpl.find(q -> q.withPeer(peer -> peer.byUid(block2Uid))).stream()
				.map(FunctionalBlockPojo::getUid)
				.collect(Collectors.toSet());
		assertEquals(2, peersOfBlock2.size());
		assertTrue(peersOfBlock2.containsAll(List.of(block1Uid, child1Uid)));
		final var table2 = blockServiceImpl.getAggregations(q -> q.withPeer(p -> p.byUid(block2Uid))
				.aggregate(FunctionalBlockFieldName.UID, AggregationOperator.COUNT).distinct());
		assertEquals(peersOfBlock2.size(), Integer.parseInt(table2.map(data -> data.isEmpty() ? null : data.get(0))
				.map(row -> row.get(FunctionalBlockFieldName.UID.name().toLowerCase()))
				.orElse(0).toString()));
	}

	private void assertFunctionalBlock(final FunctionalBlockPojo functionalBlockPojo, final FunctionalBlockPojoPrototype functionalBlockPojoPrototype) {
		assertEquals(functionalBlockPojoPrototype.name.get(), functionalBlockPojo.getName(), "Functional block name assertion failed");
		assertEquals(functionalBlockPojoPrototype.description.get(), functionalBlockPojo.getDescription(), "Functional block description assertion failed");
		final List<UUID> expectedChilds = functionalBlockPojoPrototype.children.isDefined() ?
				functionalBlockPojoPrototype.children.get(): new ArrayList<>();
		assertNotNull(expectedChilds);
		final List<UUID> actualChilds = functionalBlockPojo.getChildren();
		assertNotNull(actualChilds, "Actual functional block child UUIDs are null");
		assertEquals(expectedChilds.size(), actualChilds.size(), "Number of functional block child UUIDs differs");
		assertTrue(expectedChilds.containsAll(actualChilds),
				"Expected and actual functional block child lists do not match: " + expectedChilds.toString() + " vs " + actualChilds.toString());
		final List<ModulePart> expectedModuleParts = functionalBlockPojoPrototype.moduleParts.isDefined() ?
				functionalBlockPojoPrototype.moduleParts.get(): new ArrayList<>();
		assertNotNull(expectedModuleParts);
		final List<ModulePart> actualModuleParts = functionalBlockPojo.getModuleParts();
		assertNotNull(actualModuleParts, "Actual functional block module parts are null");
		assertEquals(expectedModuleParts.size(), actualModuleParts.size(), "Number of functional block module parts differs");
		assertTrue(expectedModuleParts.containsAll(actualModuleParts),
				"Expected and actual functional block module part do not match: " + expectedModuleParts.toString() + " vs " + actualModuleParts.toString());
	}

	private FunctionalBlockPojoPrototype createFunctionalBlockPojoPrototype(final String name, final String desc, final ModuleLocation moduleLocation,
			final List<UUID> childs, final Map<String, Object> flags) {
		return createFunctionalBlockPojoPrototype(name, desc, moduleLocation, childs, flags, project);
	}

	private FunctionalBlockPojoPrototype createFunctionalBlockPojoPrototype(final String name, final String desc, final ModuleLocation moduleLocation,
			final List<UUID> childs, final Map<String, Object> flags, final ProjectPojo project) {
		final FunctionalBlockPojoPrototype functionalBlockPojoPrototype= new FunctionalBlockPojoPrototype();
		functionalBlockPojoPrototype.setProject(project.identity());
		functionalBlockPojoPrototype.setName(name);
		functionalBlockPojoPrototype.setDescription(desc);
		final List<ModulePart> moduleParts = new ArrayList<>();
		final ModulePart functionalBlockModulePart = new ModulePart(module1.getLinkHash(), moduleLocation);
		moduleParts.add(functionalBlockModulePart);
		functionalBlockPojoPrototype.setModuleParts(moduleParts);
		if(childs != null) {
			functionalBlockPojoPrototype.setChildren(childs);
		}
		if (flags != null) {
			functionalBlockPojoPrototype.setFlags(flags);
		}
		return functionalBlockPojoPrototype;
	}

	private ProjectPojo createProject(final Long clientId) {
		return projectService.create(new ProjectPojoPrototype().setName("Test Project" + clientId).setClient(EntityId.of(clientId)).setNatures(Collections.emptySet()));
	}
	private ProjectPojo createProject1(final Long clientId) {
		return projectService.create(new ProjectPojoPrototype()
				.setName("Test Project"+ clientId)
				.setClient(EntityId.of(2L))
				.setNatures(Collections.singleton(ProjectNature.MINING)));
	}

	private ModulePojo createTestModule(final String name, @Nullable final String path, final EntityId projectId) {
		final ModulePojoPrototype cobolProgram = new ModulePojoPrototype();
		cobolProgram.setProject(projectId);
		cobolProgram.setName(name);
		cobolProgram.setTechnology(Technology.COBOL);
		cobolProgram.setType(Type.PROGRAM);
		cobolProgram.setOrigin(Origin.CUSTOM);
		cobolProgram.setStorage(Storage.FILE);
		cobolProgram.setIdentification(Identification.IDENTIFIED);
		cobolProgram.setCreator(Creator.DISCOVERY);
		cobolProgram.setPath(path);
		cobolProgram.setContent("");//dummy content to enable content hash creation
		return moduleService.getModule(moduleService.create(cobolProgram));
	}

}

package innowake.mining.server.integration.data;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.testcontainers.shaded.org.hamcrest.MatcherAssert.assertThat;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Random;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.testcontainers.shaded.org.hamcrest.Matchers;

import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.dependency.graph.DependencyGraph;

/**
* Tests {@link ModuleService}
*/
class ModuleRelationshipTest extends DatabaseResettingTest {

	protected final EntityId TEST_PROJECT_ID = EntityId.of(4L);
	protected final Long TEST_PROJECT_ID_LONG = TEST_PROJECT_ID.getNid();

	@Autowired
	protected ModuleService moduleService;

	/**
	 * This test creates and deletes a Reference with ModuleLocations.
	 */
	@Test
	void testCreateAndDeleteReferenceWithModuleLocations() {
		final EntityId testModule = insertTestModule();
		final int fromOffset = 0;
		final int fromLength = 42;
		final int toOffset = 666;
		final int toLength = 1337;

		final UUID referenceId = moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.CALLS)
				.setSrcModule(testModule)
				.setDstModule(testModule)
				.setSrcLocation(new ModuleLocation(fromOffset, fromLength))
				.setDstLocation(new ModuleLocation(toOffset, toLength))
				);

		ModuleRelationshipPojo reference = moduleService.getRelationship(referenceId);
		
		/* Check that Reference and ModuleLocations were created */
		assertNotNull(referenceId);
		assertLocation(reference.getSrcLocation(), fromOffset, fromLength);
		assertLocation(reference.getDstLocation(), toOffset, toLength);

		moduleService.deleteRelationship(q -> q.byId(referenceId));

		/* Check that Reference and ModuleLocations were deleted */
		assertEquals(0, moduleService.countRelationships(q -> q.byId(referenceId)), "Reference should no longer exist.");
	}
	
	/**
	 * Test no references found by moduleIds and relationship.
	 */
	@Test
	void testFindNoReferencesFoundByModuleIds() {
		final EntityId testModule1 = insertTestModule();
		final EntityId testModule2 = insertTestModule();

		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.CALLS)
				.setSrcModule(testModule1)
				.setDstModule(testModule2));

		final List<UUID> references = moduleService.findRelationship(q -> q.withType(RelationshipType.CALLS)
				.ofSource(testModule2)
				.ofDestination(testModule1))
				.stream()
				.map(ref -> ref.getId())
				.collect(Collectors.toList());

		/* No reference found */
		assertTrue(references.isEmpty());
	}
	
	/**
	 * Test find all references by moduleIds and relationship.
	 */
	@Test
	void testFindAllReferencesByModuleIds() {
		final EntityId testModule1 = insertTestModule();
		final EntityId testModule2 = insertTestModule();

		final UUID referenceId1 = moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.CALLS)
				.setSrcModule(testModule1)
				.setDstModule(testModule2));
		final UUID referenceId2 = moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.CALLS)
				.setSrcModule(testModule1)
				.setDstModule(testModule2));
		//irrelevant variable, to create reference that won't be found
		@SuppressWarnings("unused")
		final UUID referenceId3 = moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.CALLS)
				.setSrcModule(testModule2)
				.setDstModule(testModule1));
		
		final Set<UUID> expectedReferences = Set.of(referenceId1, referenceId2);
		
		/* Fetch references by moduleIds and relationship */
		final List<UUID> references = moduleService.findRelationship(q -> q.withType(RelationshipType.CALLS)
				.ofSource(testModule1)
				.ofDestination(testModule2))
				.stream()
				.map(ref -> ref.getId())
				.collect(Collectors.toList());
		assertEquals(2, references.size());
		
		for (UUID uuid : references) {
			assertTrue(expectedReferences.contains(uuid));
		}
	}

	@Test
	void testReferenceWithoutLocationReturnsNull() {
		final EntityId testModule1 = insertTestModule();
		final EntityId testModule2 = insertTestModule();

		final UUID referenceId1 = moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.CALLS)
				.setSrcModule(testModule1)
				.setDstModule(testModule2));

		ModuleRelationshipPojo reference1 = moduleService.getRelationship(referenceId1);
		
		assertTrue(reference1.getSrcLocation().isEmpty(), "fromModuleLocation should be null if it was not set");
		assertTrue(reference1.getDstLocation().isEmpty(), "toModuleLocation should be null if it was not set");
	}
	
	protected EntityId insertTestModule() {
		final ModulePojoPrototype testModule = new ModulePojoPrototype();
		final Random random = new Random();
		testModule.setProject(TEST_PROJECT_ID);
		testModule.setName("TESTMODULE " + random.nextInt(99));
		testModule.setTechnology(Technology.COBOL);
		testModule.setType(Type.PROGRAM);
		testModule.setStorage(Storage.FILE);
		testModule.setIdentification(Identification.IDENTIFIED);
		testModule.setOrigin(Origin.CUSTOM);
		testModule.setRepresentation(Representation.PHYSICAL);
		testModule.setCreator(Creator.DISCOVERY);
		return moduleService.create(testModule);
	}

	private static void assertLocation(final Optional<ModuleLocation> location, final int expectedOffset, final int expectedLength) {
		assertTrue(location.isPresent(), "Location must exist");
		assertEquals(expectedOffset, location.get().getOffset().intValue(), "Offset must match");
		assertEquals(expectedLength, location.get().getLength().intValue(), "Length must match");
	}
	
	@Test
	void testCreateReference() {
		final EntityId testFromModule = insertTestModule();
		final EntityId testToModule = insertTestModule();
		final List<EntityId> conditionalDependency = constructConditionalDependency(10);
		final List<UUID> expConditionalDependency = conditionalDependency.stream().map(EntityId::getUid).collect(Collectors.toList());

		final UUID refid = moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
																.setSrcModule(testFromModule)
																.setDstModule(testToModule)
																.setRelationship(RelationshipType.CALLS)
																.setValidIfReachedFrom(conditionalDependency));

		final ModuleRelationshipPojo reference = moduleService.getRelationship(refid);
		assertEquals(expConditionalDependency.size(), reference.getValidIfReachedFrom().size());
		assertTrue(CollectionUtils.containsAll(expConditionalDependency, reference.getValidIfReachedFrom()));
	}
	
	@Test
	void testFindAll() {
		final EntityId testFromModule = insertTestModule();
		final EntityId testToModule = insertTestModule();
		
		final List<EntityId> conditionalDependency1 = constructConditionalDependency(2);
		final List<UUID> expConditionalDependency1 = conditionalDependency1.stream().map(EntityId::getUid).collect(Collectors.toList());
		final UUID refid = moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setSrcModule(testFromModule)
				.setDstModule(testToModule)
				.setRelationship(RelationshipType.CALLS)
				.setValidIfReachedFrom(conditionalDependency1));
		moduleService.getRelationship(refid);
		
		final List<EntityId> conditionalDependency2 = constructConditionalDependency(3);
		final List<UUID> expConditionalDependency2 = conditionalDependency2.stream().map(EntityId::getUid).collect(Collectors.toList());
		final UUID refid2 = moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setSrcModule(testFromModule)
				.setDstModule(testToModule)
				.setRelationship(RelationshipType.CALLS)
				.setValidIfReachedFrom(conditionalDependency2));
		moduleService.getRelationship(refid2);

		List<ModuleRelationshipPojo> fetchedReferences = moduleService.findRelationship(q -> q.ofProject(TEST_PROJECT_ID).withConditionalDependencies(conditionalDependency2));
		
		assertEquals(1, fetchedReferences.size());
		assertTrue(expConditionalDependency2.containsAll(fetchedReferences.get(0).getValidIfReachedFrom()));
		
		fetchedReferences = moduleService.findRelationship(q -> q.ofProject(TEST_PROJECT_ID));
		assertEquals(2, fetchedReferences.size());
		
		fetchedReferences = moduleService.findRelationship(q -> q.ofProject(TEST_PROJECT_ID).withConditionalDependencies(constructConditionalDependency(1)));
		/* filter based on random */
		assertEquals(0, fetchedReferences.size());
		
		final List<EntityId> filterConditionalDependency = conditionalDependency1.subList(0, 1);
		fetchedReferences = moduleService.findRelationship(q -> q.ofProject(TEST_PROJECT_ID).withConditionalDependencies(filterConditionalDependency));

		assertEquals(1, fetchedReferences.size());
		assertEquals(expConditionalDependency1, fetchedReferences.get(0).getValidIfReachedFrom()); 
	}
	
	@Test
	void testDependencyTraversalForModules() {
		final EntityId ajob = insertTestModule("AJOB", Technology.JCL, Type.JOB);
		final EntityId step1 = insertTestModule("AJOB.STEP010.EXEC", Technology.JCL, Type.EXEC);
		final EntityId step2 = insertTestModule("AJOB.STEP020.EXEC", Technology.JCL, Type.EXEC);
		final EntityId aProc = insertTestModule("APROC", Technology.JCL, Type.PROC);
		final EntityId exec = insertTestModule("APROC.ACHSD000.EXEC", Technology.JCL, Type.EXEC);
		final EntityId execPgm = insertTestModule("APROC.ACHSD005.EXEC_PGM", Technology.JCL, Type.EXEC_PGM);
		final EntityId proc = insertTestModule("AFCPCAFC", Technology.JCL, Type.PROC);
		final EntityId xyz = insertTestModule("XYZ", Technology.UNKNOWN, Type.UNKNOWN);
		
		createRelationships(ajob, step1, RelationshipType.CALLS);
		createRelationships(ajob, step2, RelationshipType.CALLS);
		createRelationships(step1, aProc, RelationshipType.CALLS);
		createRelationships(step2, aProc, RelationshipType.CALLS);
		createRelationships(aProc, exec, RelationshipType.CALLS);
		createRelationships(aProc, execPgm, RelationshipType.CALLS);
		createRelationships(exec, proc, RelationshipType.CALLS);
		createRelationships(execPgm, xyz, RelationshipType.CALLS);
		
		/* For Depth = 1 */
		DependencyGraph dependencyGraph = moduleService.traverseDependencies(TEST_PROJECT_ID, aProc, 1l, Optional.of(20),
				Collections.emptyList(), Collections.emptyList(), true, false);
		assertEquals(4, dependencyGraph.getReferences().size());
		assertEquals(5, dependencyGraph.getModules().size());
		assertEquals(4, dependencyGraph.getModuleTypes().size() + dependencyGraph.getRelationshipTypes().size());
		
		/* For Depth = 2 */
		dependencyGraph = moduleService.traverseDependencies(TEST_PROJECT_ID, aProc, 2l, Optional.of(20),
				Collections.emptyList(), Collections.emptyList(), true, false);
		assertEquals(8, dependencyGraph.getReferences().size());
		assertEquals(8, dependencyGraph.getModules().size());
		assertEquals(6, dependencyGraph.getModuleTypes().size() + dependencyGraph.getRelationshipTypes().size());
	}

	@Test
	void testNoSubclassInDependencyGraph() {
		final EntityId module1 = insertTestModule("Module 1", Technology.JAVA, Type.TYPE);
		final EntityId module2 = insertTestModule("Module 2", Technology.JAVA, Type.TYPE);
		final EntityId module3 = insertTestModule("Module 3", Technology.JAVA, Type.TYPE);
		final EntityId module4 = insertTestModule("Module 4", Technology.JAVA, Type.INTERFACE);
		final EntityId module5 = insertTestModule("SubModule", Technology.JAVA, Type.TYPE);

		createRelationships(module2, module1, RelationshipType.REFERENCES);
		createRelationships(module3, module1, RelationshipType.REFERENCES);
		createRelationships(module1, module4, RelationshipType.REFERENCES);
		createRelationships(module1, module5, RelationshipType.CONTAINS);

		final DependencyGraph dependencyGraph = moduleService.traverseDependencies(TEST_PROJECT_ID, module1, 1L, Optional.of(20),
				Collections.emptyList(), Collections.emptyList(), true, false);

		assertEquals(3, dependencyGraph.getReferences().size());
		assertThat ("The Contains relation is not present", Set.of(RelationshipType.REFERENCES), Matchers.containsInAnyOrder(
				dependencyGraph.getReferences().stream().map(ModuleRelationshipPojo::getRelationship).distinct().toArray()));
		assertEquals(4, dependencyGraph.getModules().size());
		final Set<String> moduleNames = dependencyGraph.getModules().stream().map(ModulePojo::getName).collect(Collectors.toSet());
		assertThat("SubModule is not present", Set.of("Module 1", "Module 2", "Module 3", "Module 4"),
				Matchers.containsInAnyOrder(dependencyGraph.getModules().stream().map(ModulePojo::getName).distinct().toArray()));
	}
	
	private List<EntityId> constructConditionalDependency(final int size) {
		final List<EntityId> conditionalModules = new ArrayList<>(size);
		int i = 0;
		while (i++ < size) {
			conditionalModules.add(insertTestModule());
		}
		return conditionalModules;
	}
	
	
	private EntityId insertTestModule(final String name, final Technology technology, final Type type) {
		final ModulePojoPrototype testModule = new ModulePojoPrototype();
		testModule.setProject(TEST_PROJECT_ID);
		testModule.setName(name);
		testModule.setTechnology(technology);
		testModule.setType(type);
		testModule.setStorage(Storage.FILE);
		testModule.setIdentification(Identification.IDENTIFIED);
		testModule.setOrigin(Origin.CUSTOM);
		testModule.setRepresentation(Representation.PHYSICAL);
		testModule.setCreator(Creator.DISCOVERY);
		return moduleService.create(testModule);
	}
	
	private UUID createRelationships(final EntityId testModule1, final EntityId testModule2, final RelationshipType relationship) {
		return moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(relationship)
				.setSrcModule(testModule1)
				.setDstModule(testModule2));
	}
	
}

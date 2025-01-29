/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.shared.model.Technology.COBOL;
import static innowake.mining.shared.model.Type.PROGRAM;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import javax.persistence.EntityNotFoundException;

import org.eclipse.core.internal.resources.Project;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Tests if {@link DatabaseRelatedTest} for CRUD operations on SourcePojo.
 * 
 */
@WithMockUser
class SourceObjectDbTest extends DatabaseResettingTest {

	private final Long ONE = Long.valueOf(1);
	
	@Autowired
	private ModuleService moduleService;
	
	@Autowired
	private SourceCachingService sourceService;

	/**
	 * Test to check auto wiring.
	 */
	@Test
	void autowiredNotNullTest() {
		assertNotNull(projectService);
		assertNotNull(sourceService);
		assertNotNull(moduleService);
	}
	
	/**
	 * Test to check the creation of {@link SourcePojo}.
	 *
	 */
	@Test
	void createSourceObjectsWithIdOneTest() {
		createSourceObjectsWithIdOne();
	}
	
	/**
	 * Test to fetch the {@link SourcePojo} of specified {@link Technology} and {@link Type}.
	 *
	 */
	@Test
	void testFindByTechnologyAndTypeSourceObjects() {
		findByTechnologyAndTypeSourceObjects();
	}
	
	/**
	 * Test to fetch the {@link SourcePojo} of specified {@link Technology}.
	 *
	 */
	@Test
	void testFindByTechnologySourceObjects() {
		findByTechnologySourceObjects();
	}
	
	/**
	 * Test to fetch the {@link SourcePojo} of specified {@link Type}.
	 *
	 */
	@Test
	void testFindByTypeSourceObjects() {
		findByTypeSourceObjects();
	}
	
	/**
	 * Test to fetch the {@link SourcePojo} of specified {@link Type} that have changed
	 */
	@Test
	void testFindChangedIdsByTypeSourceObjects() {
		final ProjectPojo project = createProject();
		final EntityId projectId = project.identity();
		
		final SourcePojo prog1 = createProgram(projectId, "programs", "PROG1", Technology.COBOL, Type.PROGRAM);
		final SourcePojo prog2 = createProgram(projectId, "programs", "PROG2", Technology.COBOL, Type.PROGRAM);	
		final SourcePojo prog3 = createProgram(projectId, "programs", "PROG3", Technology.COBOL, Type.PROGRAM);
		createModule(projectId, prog1);
		createModule(projectId, prog2);
		createModule(projectId, prog3);

		var changedIds = EntityId.allNids(sourceService.findIDs(q -> q.ofProject(projectId)
				.withTechnology(Technology.COBOL).withModuleHashDiffers(true, false)));
		assertTrue("No IDs should be returned since no sources are changed yet", changedIds.isEmpty());
		
		/* Simulate change in source by updating content of prog1 and prog2 */
		sourceService.update(new SourcePojoPrototype().withId(prog1.identity()).setContent(new BinaryString("New Code")));
		sourceService.update(new SourcePojoPrototype().withId(prog2.identity()).setContent(new BinaryString("TEST")));

		final List<Long> expectedChangedIds = Arrays.asList(prog1.getId(), prog2.getId());
		changedIds = EntityId.allNids(sourceService.findIDs(q -> q.ofProject(projectId)
				.withTechnology(Technology.COBOL).withModuleHashDiffers(true, false)));
		assertTrue(changedIds.containsAll(expectedChangedIds));
	}
	
	/**
	 * Test to fetch {@link SourcePojo} with regex on path.
	 */
	@Test
	void testSourceObjectWithPathRegex() {
		final ProjectPojo projectResult = createProject();
		final EntityId projectId = projectResult.identity();
		createProgram(projectId, "programs/DPG1.cbl");
		createProgram(projectId, "programs/DPG2.cbl");
		final List<SourcePojo> sourceObject = sourceService.find(q -> q.ofProject(projectId).withPathRegex("programs/*"));
		assertNotNull(sourceObject);
		assertEquals(2, sourceObject.size());
		assertTrue("No path named 'programs/DPG1.cbl' could be found", sourceObject.stream().anyMatch(so -> so.getPath().equals("programs/DPG1.cbl")));
	}
	
	/**
	 * Test to delete {@link SourcePojo}.
	 */
	@Test
	void testDeleteSourceObjectWithPath() {	
		final ProjectPojo projectResult = createProject();
		final EntityId projectId = projectResult.identity();
		createProgram(projectId, "programs/DPG1.cbl");
		createProgram(projectId, "programs/DPG2.cbl");
		sourceService.removeAll(q -> q.ofProject(projectId).withPath("programs%"));
		Assertions.assertThrows(EntityNotFoundException.class, () -> sourceService.get(q -> q.ofProject(projectId).withPath("programs%")));
	}
	
	/**
	 * Test to update {@link SourcePojo}.
	 */
	@Test
	void testUpdateSourceObject() {	
		final ProjectPojo projectResult = createProject();
		final EntityId projectId = projectResult.identity();
		SourcePojo sourceObject = createProgram(projectId, "programs", "DPG1");
		sourceObject = sourceService.get(sourceService.update(new SourcePojoPrototype().withId(sourceObject.identity()).setType(Type.COPYBOOK)));
		assertEquals(Type.COPYBOOK, sourceObject.getType());
	}

	/**
	 * Test to find {@link SourcePojo}s by name.
	 */
	@Test
	void testFindByName() {	
		final ProjectPojo projectResult = createProject();
		final EntityId projectId = projectResult.identity();

		createProgram(projectId, "programs", "SKO1", Technology.COBOL, Type.PROGRAM);
		createProgram(projectId, "jobs", "SKO1", Technology.JCL, Type.JOB);
		createProgram(projectId, "programs", "SKO2", Technology.BASIC, Type.PROGRAM);	

		assertEquals(2, sourceService.count(q -> q.ofProject(projectId).withName("SKO1")));
		assertEquals(1, sourceService.count(q -> q.ofProject(projectId).withName("SKO2")));
		assertEquals(0, sourceService.count(q -> q.ofProject(projectId).withName("SKO3")));
	}

	/**
	 * Test caching of {@link SourcePojo}s in findByPath.
	 */
	@Test
	@Disabled("Caching is disabled for now: WMIN-14258")
	void testFindByPathCaching() {	
		final ProjectPojo projectResult = createProject();
		final EntityId projectId = projectResult.identity();
		createProgram(projectId, "programs", "SKO1", Technology.COBOL, Type.COPYBOOK);

		final SourcePojo found1 = sourceService.cachingByProjectPath(projectId.getNid(), "programs/SKO1.cbl");
		assertEquals(Type.COPYBOOK, found1.getType());
		final SourcePojo found2 = sourceService.cachingByProjectPath(projectId.getNid(), "programs/SKO1.cbl");
		/* Cached sourceObjects should be copies of each other */
		assertSame(found2, found1);

		/* Test cache invalidation after update */
		sourceService.update(new SourcePojoPrototype().withId(found1.identity()).setType(Type.PROGRAM));
		final SourcePojo found4 = sourceService.cachingByProjectPath(projectId.getNid(), "programs/SKO1.cbl");
		assertEquals(Type.PROGRAM, found4.getType());
		assertNotSame(found1, found4);

		/* Test cache invalidation after delete */
		sourceService.removeAll(q -> q.ofProject(projectId).withPath("programs%"));
		try {
			sourceService.cachingByProjectPath(projectId.getNid(), "programs/SKO1.cbl");
			fail();
		} catch (Exception e) {
			assertEquals(EntityNotFoundException.class, e.getClass());
		}

		/* Test cache invalidation after create */
		createProgram(projectId, "programs", "SKO1", Technology.COBOL, Type.BMS_MAP);
		final SourcePojo found5 = assertNotNull(sourceService.cachingByProjectPath(projectId.getNid(), "programs/SKO1.cbl"));
		assertEquals(Type.BMS_MAP.name(), found5.getType().name());
	}

	/**
	 * Test to upsert {@link SourcePojo}.
	 */
	@Test
	void testUpsertSourceObject() {	
		final ProjectPojo projectResult = createProject();
		final EntityId projectId = projectResult.identity();
		SourcePojo sourceObject = createProgram(projectId, "programs", "DPG1");
		sourceObject = sourceService.get(sourceService.update(new SourcePojoPrototype().withId(sourceObject.identity()).setType(Type.COPYBOOK)));
		assertEquals(Type.COPYBOOK, sourceObject.getType());
		assertNotNull(sourceObject.getContentHash());
	}
	
	/**
	 * Tests that changing the path of a SourcePojo via UPSERT invalidates the path cache.
	 */
	@Test
	void testUpsertInvalidateCache() {	
		final ProjectPojo projectResult = createProject();
		final EntityId projectId = projectResult.identity();
		createProgram(projectId, "programs", "SKO1", Technology.COBOL, Type.COPYBOOK);

		final SourcePojo found1 = sourceService.get(q -> q.ofProject(projectId).withPath("programs/SKO1.cbl"));
		assertEquals("SKO1", found1.getName());

		sourceService.update(new SourcePojoPrototype().withId(found1.identity()).setPath("programs/SKO2.cbl"));
		
		/* SourcePojo can be found under new path but not under previous path */
		assertThrows(EntityNotFoundException.class, () -> sourceService.get(q -> q.ofProject(projectId).withPath("programs/SKO1.cbl")));
		final SourcePojo found2 = sourceService.get(q -> q.ofProject(projectId).withPath("programs/SKO2.cbl"));
		assertEquals("SKO1", found2.getName()); /* name wasn't changed */
		
		/* can re-create program in previous location */
		createProgram(projectId, "programs", "SKO1", Technology.COBOL, Type.COPYBOOK);
	}

	@Test
	void testDeleteSourceObjectByIdInvalidateCache() {
		final ProjectPojo projectResult = createProject();
		final EntityId projectId = projectResult.identity();
		createProgram(projectId, "programs", "SKO1", Technology.COBOL, Type.COPYBOOK);

		final SourcePojo found1 = sourceService.cachingByProjectPath(projectId.getNid(), "programs/SKO1.cbl");
		assertEquals("SKO1", found1.getName());
		sourceService.remove(found1.identity(), projectId);
		final var projectNid = projectId.getNid();
		assertThrows(EntityNotFoundException.class, () -> sourceService.cachingByProjectPath(projectNid, "programs/SKO1.cbl"));
	}

	@Test
	void testDeleteMultipleSourceObjectsByIdInvalidateCache() {
		final ProjectPojo projectResult = createProject();
		final EntityId projectId = projectResult.identity();
		createProgram(projectId, "programs", "SKO1", Technology.COBOL, Type.COPYBOOK);
		createProgram(projectId, "programs", "SKO2", Technology.COBOL, Type.COPYBOOK);

		final SourcePojo found1 = sourceService.cachingByProjectPath(projectId.getNid(), "programs/SKO1.cbl");
		final SourcePojo found2 = sourceService.cachingByProjectPath(projectId.getNid(), "programs/SKO2.cbl");
		assertEquals("SKO1", found1.getName());
		assertEquals("SKO2", found2.getName());
		final var projectNid = projectId.getNid();
		sourceService.remove(List.of(found1.identity(), found2.identity()), projectId);
		assertThrows(EntityNotFoundException.class, () -> sourceService.cachingByProjectPath(projectNid, "programs/SKO1.cbl"));
		assertThrows(EntityNotFoundException.class, () -> sourceService.cachingByProjectPath(projectNid, "programs/SKO2.cbl"));
	}
	
	/**
	 * Test to fetch the {@link SourcePojo} of specified {@link Project} that were undiscovered
	 */
	@Test
	void testFindUndiscoveredIds() {
		final ProjectPojo project = createProject();
		final EntityId projectId = project.identity();
		
		final SourcePojo prog1 = createProgram(projectId, "programs", "PROG1", Technology.COBOL, Type.PROGRAM);
		final SourcePojo prog2 = createProgram(projectId, "programs", "PROG2", Technology.COBOL, Type.PROGRAM);
		assertEquals(2, sourceService.count(q -> q.ofProject(projectId).withModuleExists(false)));
		createModule(projectId, prog1);
		createModule(projectId, prog2);

		List<EntityId> undiscoveredIds = sourceService.findIDs(q -> q.ofProject(projectId).withModuleExists(false));
		assertTrue("No IDs should be returned since no sources are changed yet", undiscoveredIds.isEmpty());
		
		/* Create new undiscovered source */
		final SourcePojo prog3 = createProgram(projectId, "programs", "PROG3", Technology.COBOL, Type.PROGRAM);

		undiscoveredIds = sourceService.findIDs(q -> q.ofProject(projectId).withModuleExists(false));
		assertEquals(1, undiscoveredIds.size());
		assertEquals(prog3.identity(), undiscoveredIds.get(0));
	}
	
	private void findByTypeSourceObjects() {
		final ProjectPojo projectResult = createProject();
		final EntityId projectId = projectResult.identity();
		
		final List<String> programNames = new ArrayList<>();
		Collections.addAll(programNames, "DPG1", "DPG2", "DPG3");
		createProgram(projectId, "programs", programNames.get(0),Technology.COBOL, Type.PROGRAM);
		createProgram(projectId, "programs", programNames.get(1),Technology.BASIC, Type.PROGRAM);	
		createProgram(projectId, "programs", programNames.get(2),Technology.COBOL, Type.COPYBOOK);
		
		List<SourcePojo> collectModule = sourceService.find(q -> q.ofProject(projectId).withType(Type.PROGRAM));
		assertNotNull(collectModule);
		assertEquals(2, collectModule.size());
		final List<SourcePojo> sortedModules = sortBySourceObjectName(collectModule);
		assertEquals("DPG1", sortedModules.get(0).getName());
		assertEquals("DPG2", sortedModules.get(1).getName());
		collectModule = sourceService.find(q -> q.ofProject(projectId).withType(Type.COPYBOOK));
		assertEquals(1, collectModule.size());
		assertEquals("DPG3", collectModule.get(0).getName());
		assertNotNull(collectModule.get(0).getContentHash());
	}

	private void findByTechnologySourceObjects() {
		final ProjectPojo projectResult = createProject();
		final EntityId projectId = projectResult.identity();
		
		final List<String> programNames = new ArrayList<>();
		Collections.addAll(programNames, "DPG1", "DPG2", "DPG3");
		createProgram(projectId, "programs", programNames.get(0),Technology.COBOL, Type.PROGRAM);
		createProgram(projectId, "programs", programNames.get(1), Technology.COBOL, Type.PROGRAM);
		
		createProgram(projectId, "programs", programNames.get(2),Technology.BASIC, Type.PROGRAM);
		
		List<SourcePojo> collectModule = sourceService.find(q -> q.ofProject(projectId).withTechnology(Technology.COBOL));
		assertNotNull(collectModule);
		assertEquals(2, collectModule.size());
		final List<SourcePojo> sortedModules = sortBySourceObjectName(collectModule);
		assertEquals("DPG1", sortedModules.get(0).getName());
		assertEquals("DPG2", sortedModules.get(1).getName());
		
		collectModule = sourceService.find(q -> q.ofProject(projectId).withTechnology(Technology.BASIC));
		assertNotNull(collectModule);
		assertEquals(1, collectModule.size());
		assertEquals("DPG3", collectModule.get(0).getName());
		assertNotNull(collectModule.get(0).getContentHash());
	}

	private void findByTechnologyAndTypeSourceObjects() {
		final ProjectPojo projectResult = createProject();
		final EntityId projectId = projectResult.identity();
		
		final List<String> programNames = new ArrayList<>();
		Collections.addAll(programNames, "DPG1", "DPG2", "DPG3");
		createProgram(projectId, "programs", programNames.get(0),Technology.COBOL, Type.PROGRAM);
		createProgram(projectId, "programs", programNames.get(1), Technology.COBOL, Type.PROGRAM);
		
		createProgram(projectId, "programs", programNames.get(2),Technology.BASIC, Type.PROGRAM);
		
		List<SourcePojo> collectModule = sourceService.find(q -> q.ofProject(projectId).withTechnology(Technology.COBOL).withType(Type.PROGRAM));
		assertNotNull(collectModule);
		assertEquals(2, collectModule.size());
		
		final List<SourcePojo> sortedModules = sortBySourceObjectName(collectModule);
		assertEquals("DPG1", sortedModules.get(0).getName());
		assertEquals("DPG2", sortedModules.get(1).getName());
		assertNotNull(sortedModules.get(0).getContentHash());
		
		collectModule = sourceService.find(q -> q.ofProject(projectId).withTechnology(Technology.BASIC).withType(Type.PROGRAM));
		assertNotNull(collectModule);
		assertEquals(1, collectModule.size());
		assertEquals("DPG3", collectModule.get(0).getName());
		assertNotNull(collectModule.get(0).getContentHash());
	}

	private void createSourceObjectsWithIdOne() {
		final ProjectPojo projectResult = createProject();
		final EntityId projectId = projectResult.identity();
		
		final List<String> programNames = new ArrayList<>();
		Collections.addAll(programNames, "DPG1", "DPG2");
		createProgram(projectId, "programs", programNames.get(0),Technology.COBOL, Type.PROGRAM);
		createProgram(projectId, "programs", programNames.get(1),Technology.BASIC, Type.PROGRAM);	
		
		final List<SourcePojo> collectModule = sourceService.find(q -> q.ofProject(projectId));
		assertNotNull(collectModule);
		assertEquals(2, collectModule.size());
		final List<SourcePojo> sortedModules = sortBySourceObjectName(collectModule);
		assertEquals("DPG1", sortedModules.get(0).getName());
		assertEquals("DPG2", sortedModules.get(1).getName());
		assertNotNull(sortedModules.get(0).getContentHash());
	}

	private List<SourcePojo> sortBySourceObjectName(final List<SourcePojo> collectModule) {
		return collectModule.stream()
				.sorted(Comparator.comparing(SourcePojo::getName, String.CASE_INSENSITIVE_ORDER))
				.collect(Collectors.toList());
	}
	
	private ProjectPojo createProject() {
		return projectService.create(new ProjectPojoPrototype()
				.setName("TEST PROJECT 1")
				.setClient(EntityId.of(ONE))
				.setNatures(Collections.emptySet()));
	}
	
	private SourcePojo createProgram(final EntityId projectId, final String path) {
		sourceService.create(new SourcePojoPrototype()
				.setProject(projectId)
				.setName("")
				.setPath(path)
				.setTechnology(COBOL)
				.setType(PROGRAM) 
				.setContent(new BinaryString("some code")));
		return sourceService.get(q -> q.ofProject(projectId).withPath(path));
	}
	
	private SourcePojo createProgram(final EntityId projectId, final String folder, final String name) {
		final String path = folder + "/" + name + ".cbl";
		sourceService.create(new SourcePojoPrototype()
				.setProject(projectId)
				.setName(name)
				.setPath(path)
				.setTechnology(COBOL)
				.setType(PROGRAM) 
				.setContent(new BinaryString("some code")));
		return sourceService.get(q -> q.ofProject(projectId).withPath(path));
	}
	
	private SourcePojo createProgram(final EntityId projectId, final String folder, final String name, final Technology technology, final Type type) {
		final String path = folder + "/" + name + ".cbl";
		sourceService.create(new SourcePojoPrototype()
				.setProject(projectId)
				.setName(name)
				.setPath(path)
				.setTechnology(technology)
				.setType(type)
				.setContent(new BinaryString("some code")));
		return sourceService.get(q -> q.ofProject(projectId).withPath(path));
	}
	
	private EntityId createModule(final EntityId projectId, final SourcePojo prog) {
		final String description = "Was drawing natural fat respect husband. " + "An as noisy an offer drawn blush place. "
				+ "In mr began music weeks after at begin. " + "Education no dejection so direction pretended household do to.";
		final ModulePojoPrototype module = new ModulePojoPrototype()
				.setProject(projectId)
				.setName(prog.getName())
				.setPath(prog.getPath())
				.setTechnology(prog.getTechnology())
				.setType(prog.getType())
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setCreator(Creator.DISCOVERY)
				.setDescription(description);
		
		return moduleService.create(module);
	}
}

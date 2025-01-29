/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.data;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.*;

import innowake.mining.shared.model.ProjectNature;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.discovery.config.searchorder.SearchOrder;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojo;

/**
 * Test to verify Project service
 */
 class ProjectServiceTest extends DatabaseRelatedTest {

	private static final Long ONE = Long.valueOf(1);
	private static final Long TEN = Long.valueOf(10);
	private static final EntityId TAX_ENTITY = EntityId.of(Long.valueOf(100));
	
	@Autowired
	private TaxonomyService taxonomyService;

	@BeforeAll
	void ensureSetup() {
		final var project = projectService.find(EntityId.of(ONE));
		if (project.isEmpty()) {
			projectService.create(new ProjectPojoPrototype()
					.setNid(ONE)
					.setClient(EntityId.of(1L))
					.setName("ProjectServiceTest Project 1")
					.setNatures(Set.of(ProjectNature.MINING))
			);
		}
	}

	@Test
	void testFindProject() {
		final ProjectPojo project = projectService.get(EntityId.of(ONE));
		assertNotNull(project, "project object should not be null");
	}
	
	@Test
	void testFindAboveId() {
		final List<ProjectPojo> projectList = projectService.find(q -> q.withIdAbove(ONE));
		assertFalse("Project List should not be empty", projectList.isEmpty());
	}
	
	@Test
	void testFindById() {
		assertTrue("Should return Project for given Id", projectService.find(EntityId.of(ONE)).isPresent());
	}
	
	@Test
	void testFindProjectForClientId() {
		final List<ProjectPojo> projectList = projectService.find(q -> q.ofClient(EntityId.of(ONE)));
		assertFalse("Project List should not be empty", projectList.isEmpty());
	}
	
	@Test
	void testCheckCreatedTaxonomies() {
		final ProjectPojo project = createTestDataTaxonomy();
		final List<TaxonomyTypePojo> taxonomyList = taxonomyService.findTypes(q -> q.ofProject(project.identity()).withName("Utility"));
		assertTrue(taxonomyList.size() == 1);
	}
	
	@Test
	void testUpdateProject() {
		final UUID uid = createTestData().getUid();
		
		final ProjectPojoPrototype projectChanged = new ProjectPojoPrototype();
		projectChanged.setUid(uid);
		projectChanged.setName("Test Project new");
		projectService.update(projectChanged);
		final Optional<ProjectPojo> project =  projectService.find(uid);
		assertTrue("Project Should not be empty", project.isPresent());
		assertEquals("Test Project new", project.get().getName());
		assertEquals(10, project.get().getSourceCodeRevision());
	}

	@Test
	void getUids() {
		final List<UUID> uidList = projectService.getUids(q -> q.withIdAbove(ONE));
		assertTrue("Project Uid List Should not be empty", uidList.size() > 0);
	}
	
	@Test
	void getNids() {
		final List<Long> nidList = projectService.getNids(q -> q.withIdAbove(ONE));
		assertTrue("Project Id List Should not be empty", nidList.size() > 0);
	}
	
	private ProjectPojo createTestData() {
		final ProjectPojoPrototype project = new ProjectPojoPrototype();
		project.setClient(EntityId.of(ONE));
		project.setName("Test Project");
		project.setMetricsBaseRevision(ONE);
		project.setSourceCodeRevision(TEN);
		project.setClient(EntityId.of(1L));
		project.setNatures(Collections.emptySet());
		project.searchOrders.set(Arrays.asList(new SearchOrder()));
		return projectService.create(project);
	}
	
	private ProjectPojo createTestDataTaxonomy() {
		final ProjectPojoPrototype project = new ProjectPojoPrototype();
		project.setClient(TAX_ENTITY);
		project.setName("Test Project Taxonomies");
		project.setMetricsBaseRevision(ONE);
		project.setSourceCodeRevision(TEN);
		project.setClient(EntityId.of(1L));
		project.setNatures(Collections.emptySet());
		project.searchOrders.set(Arrays.asList(new SearchOrder()));
		return projectService.create(project);
	}
}

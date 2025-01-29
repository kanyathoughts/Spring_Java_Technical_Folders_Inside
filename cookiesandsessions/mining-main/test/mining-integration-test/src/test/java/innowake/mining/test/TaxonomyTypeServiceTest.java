/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;

import org.junit.jupiter.api.Test;

import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.project.ProjectServiceProvider;
import innowake.mining.client.service.taxonomytype.TaxonomyTypeServiceProvider;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojo;

/**
 * Integration tests for the {@link TaxonomyTypePojo} service.
 */
class TaxonomyTypeServiceTest extends IntegrationTest {

	private static final Long ONE = Long.valueOf(1);
	private static final Long NON_EXISTING_ID = Long.valueOf(Long.MAX_VALUE);
	
	private final TaxonomyTypeServiceProvider taxonomyTypeServiceProvider = MiningApiClient.taxonomyTypeService(getConnectionInfo());
	private final ProjectServiceProvider projectServiceProvider = MiningApiClient.projectService(getConnectionInfo());
	
	@Test
	void findAllTest() throws IOException {
		final Result<TaxonomyTypePojo[]> result = taxonomyTypeServiceProvider.findAllTaxonomyTypes().setProjectId(EntityId.of(ONE)).execute();
		assertEquals(200, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		final TaxonomyTypePojo[] taxonomyTypes = result.getValue().get();
		assertEquals(3, taxonomyTypes.length);
		
		Arrays.sort(taxonomyTypes, (a, b) -> a.getName().compareTo(b.getName()));

		var projectOneId = projectServiceProvider.findProjectById()
				.setProjectId(1L)
				.execute().getValue().get().identity();

		assertEquals("Business Taxonomies", taxonomyTypes[0].getCategory().getName());
		assertEquals("BusinessProcess", taxonomyTypes[0].getName());
		assertEquals(projectOneId, taxonomyTypes[0].getProject());

		assertEquals("Business Taxonomies", taxonomyTypes[1].getCategory().getName());
		assertEquals("BusinessSubsystem", taxonomyTypes[1].getName());
		assertEquals(projectOneId, taxonomyTypes[1].getProject());

		assertEquals("Business Taxonomies", taxonomyTypes[2].getCategory().getName());
		assertEquals("DataDomain", taxonomyTypes[2].getName());
		assertEquals(projectOneId, taxonomyTypes[2].getProject());
	}
	
	@Test
	void findAllTestWithNonExistingProject() throws IOException {
		final Result<TaxonomyTypePojo[]> result = taxonomyTypeServiceProvider.findAllTaxonomyTypes().setProjectId(EntityId.of(NON_EXISTING_ID)).execute();
		assertEquals(404, result.getStatusCode());
		assertFalse(result.getValue().isPresent());
	}
	
	@Test
	void findAllTestForProjectWithoutTaxonomyTypes() throws IOException {
		final Result<ProjectPojo> resultCreateProject = projectServiceProvider.createProject().setProject(new ProjectPojoPrototype()
				.setClient(EntityId.of(ONE))
				.setName("NEW TEST PROJECT WITHOUT TAXONOMY TYPES")
				.setNatures(Collections.emptySet())
			).execute();
		assertEquals(201, resultCreateProject.getStatusCode());
		assertTrue(resultCreateProject.getValue().isPresent());
		final Result<TaxonomyTypePojo[]> resultFindTaxonomyTypes = taxonomyTypeServiceProvider.findAllTaxonomyTypes().setProjectId(resultCreateProject.getValue().get().identity()).execute();
		assertEquals(200, resultFindTaxonomyTypes.getStatusCode());
		assertTrue(resultFindTaxonomyTypes.getValue().isPresent());
		assertEquals(4, resultFindTaxonomyTypes.getValue().get().length);
	}
}

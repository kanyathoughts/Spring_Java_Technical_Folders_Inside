/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.TestPropertySource;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.TaxonomyCategoryPojo;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.TaxonomyTypePojo;
import innowake.mining.shared.model.ProjectNature;

/**
 * Tests to validate default values of taxonomy categories from application.yml
 */
@TestPropertySource(properties = {"mining.taxonomies.technicalTaxonomyCategoryName= UI Technical Taxonomies",
		"mining.taxonomies.defaultTaxonomyCategoryName= UI Business Taxonomies"})
class ProjectTaxonomyCategoryTest extends DatabaseResettingTest {
	
	private static final String BUSINESS_TAXONOMIES = "UI Business Taxonomies";
	private static final String TECHNICAL_TAXONOMIES = "UI Technical Taxonomies";

	@Autowired
	private TaxonomyService taxonomyService;
	
	private static final Long ONE = Long.valueOf(1);
	
	private ProjectPojo createTestProject(boolean withNature) {
		ProjectPojoPrototype newProject = new ProjectPojoPrototype()
				.setName("Test Project")
				.setClient(EntityId.of(ONE))
				.setNatures(Collections.emptySet());
		if (withNature) {
			newProject.setNatures(new HashSet<>(Arrays.asList(ProjectNature.DISCOVERY)));
		}
		return projectService.create(newProject);
	}
	
	private Map<Long, TaxonomyCategoryPojo> getTaxonomyCategories(EntityId projectId) {
		return taxonomyService.findCategories(q -> q.ofProject(projectId)).stream().collect(Collectors.toMap(o -> o.getId(), Function.identity()));
	}

	@Test
	void testToValidateTaxonomyCategoryCreationOnProject() {
		final ProjectPojo createdProject = createTestProject(false);
		Map<Long, TaxonomyCategoryPojo> taxonomyCategories = getTaxonomyCategories(createdProject.identity());
		assertEquals(TECHNICAL_TAXONOMIES, 
				assertNotNull(taxonomyCategories.get(assertNotNull(createdProject.getTechnicalTaxonomyCategoryId()))).getName());
		assertEquals(BUSINESS_TAXONOMIES, 
				assertNotNull(taxonomyCategories.get(assertNotNull(createdProject.getDefaultTaxonomyCategoryId()))).getName());
	}

	@Test
	void testToValidateTaxonomyCategoryCreationOnProjectV2() {
		final ProjectPojo createdProject = createTestProject(true);
		Map<Long, TaxonomyCategoryPojo> taxonomyCategories = getTaxonomyCategories(createdProject.identity());
		final String actualTechnicalTaxonomyCategoryName =
				assertNotNull(taxonomyCategories.get(assertNotNull(createdProject.getTechnicalTaxonomyCategoryId()))).getName();
		final String actualDefaultTaxonomyCategoryName = 
				assertNotNull(taxonomyCategories.get(assertNotNull(createdProject.getDefaultTaxonomyCategoryId()))).getName();
		assertEquals(TECHNICAL_TAXONOMIES, actualTechnicalTaxonomyCategoryName);
		assertEquals(BUSINESS_TAXONOMIES, actualDefaultTaxonomyCategoryName);
	}
	
	@Test
	void testToValidateTechnicalTaxonomyCreationOnProjectV2() {
		final ProjectPojo createdProject = createTestProject(true);
		final EntityId id = assertNotNull(createdProject.identity());
		List<TaxonomyTypePojo> taxonomyTypes = taxonomyService.findTypes(q -> q.ofProject(id));
		List<TaxonomyPojo> taxonomies = taxonomyService.find(q -> q.ofProject(id));
		assertEquals(4, taxonomyTypes.size());
		assertEquals(13, taxonomies.size());
	}

}

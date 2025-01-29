/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */

package innowake.mining.server.integration;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.datapoints.FilterObjectService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Test to validate module search by it's name
 */
class ModuleSearchTest extends DatabaseRelatedTest {

	private static final Long CLIENT_ID = Long.valueOf(1);
	
	/* Modules to be created in database. */
	private static final List<String> MODULE_NAMES = Collections.unmodifiableList(
			Arrays.asList(
					"Test",
					"test1",
					"TEST2",
					"test1.test2.test3",
					"TeSt1",
					"tesot.taest",
					"TEstSample",
					"ttestt"
					));
	
	/* Possible searchText values that needs to be validated. */
	private static final List<String> SEARCH_TEXTS = Collections.unmodifiableList(
			Arrays.asList( 
					"Test",
					"test",
					"TEST",
					"test1.test2.test3",
					"testtesttest",
					"randomtext"
					));
	
	@Autowired
	private ModuleService moduleService;

	@Autowired
	private FilterObjectService filterObjectService;
	
	@Nullable
	private ProjectPojo testProject;
	
	/**
	 * Creates list of module for each MODULE_NAMES
	 *
	 */
	@BeforeAll
	public void insertTestData() {
		testProject = projectService.create(new ProjectPojoPrototype()
			.setName("TEST PROJECT MODULE SEARCH")
			.setClient(EntityId.of(CLIENT_ID))
			.setNatures(Collections.emptySet()));
		createTestModules(testProject.identity());
	}

	/**
	 * Tests each valid string in SEARCH_TEXTS against the actual search records.
	 *
	 */
	@Test
	void testSearchByNameForValidTexts() {
		/* Search for all the texts in SEARCH_TEXTS. */
		SEARCH_TEXTS.forEach(searchText -> validateSearchRecords(assertNotNull(testProject).identity(), searchText));
	}

	private void validateSearchRecords(final EntityId projectId,final String searchText) {
		/* Get actual search result based on searchText.*/
		final var modules = moduleService.findModules(q -> {
			q.ofProject(projectId);
			Map<String, Object> filterObject = Map.of("content_name", Map.of("eq", searchText));
			filterObjectService.applyFilterObject(projectId.getNid(), "modules", filterObject, q);
		});
		final List<String> actualSearchResult = modules.stream()
				.map(ModulePojo::getName)
				.sorted()
				.collect(Collectors.toList());

		/* Get expected search result based on searchText from MODULE_NAMES. */
		final List<String> expectedSearchResult = MODULE_NAMES.stream()
				.filter(moduleName -> moduleName.toLowerCase().startsWith(searchText.toLowerCase()))
				.sorted()
				.collect(Collectors.toList());

		assertEquals(expectedSearchResult, actualSearchResult,
				String.format("Incorrect module search results for search text '%s'", searchText));
	}

	private void createTestModules(final EntityId projectId) {
		for (int index = 0; index < MODULE_NAMES.size(); index++) {
			final ModulePojoPrototype cobolProgram = new ModulePojoPrototype();
			cobolProgram.setProject(projectId);
			cobolProgram.setName(MODULE_NAMES.get(index));
			cobolProgram.setTechnology(Technology.COBOL);
			cobolProgram.setType(Type.PROGRAM);
			cobolProgram.setOrigin(Origin.CUSTOM);
			cobolProgram.setStorage(Storage.FILE);
			cobolProgram.setIdentification(Identification.MISSING);
			cobolProgram.setPath("src/cobol/TESTCOBA" + index + ".cbl");
			cobolProgram.setCreator(Creator.DISCOVERY);
			moduleService.create(cobolProgram);
		}
	}
}

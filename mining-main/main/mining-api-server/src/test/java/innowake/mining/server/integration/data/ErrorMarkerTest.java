/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.data;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.entities.ErrorMarkerPojoPrototype;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
* Tests ErrorMarkers
*/
class ErrorMarkerTest extends DatabaseRelatedTest {
	
	private final EntityId projectId = EntityId.of(1L);

	@Autowired
	private ModuleService moduleService;
	
	@Test
	void testErrorMarkerLinkCreationAndDeletionForModule() {
		final ModulePojo testModule = createTestModule("Module1", Storage.FILE, "content");
		
		moduleService.createErrorMarker(new ErrorMarkerPojoPrototype()
												.setProject(projectId)
												.setModule(testModule.identity())
												.setCause("Error"));
		assertEquals(1, countErrorMarkers(testModule.identity()));
		
		moduleService.deleteErrorMarkers(q -> q.ofProject(projectId).ofModule(testModule.identity()));
		assertEquals(0, countErrorMarkers(testModule.identity()));
	}
	
	@Test
	void testErrorMarkerLinkDeletionForProject() {
		final var testModule1 = createTestModule("Module2", Storage.FILE, null);
		final var testModule2 = createTestModule("Module3", Storage.FILE, null);

		moduleService.createErrorMarker(new ErrorMarkerPojoPrototype()
												.setProject(projectId)
												.setModule(testModule1.identity())
												.setCause("Error1"));

		moduleService.createErrorMarker(new ErrorMarkerPojoPrototype()
												.setProject(projectId)
												.setModule(testModule2.identity())
												.setCause("Error2"));

		assertEquals(1, countErrorMarkers(testModule1.identity()));
		assertEquals(2, countModulesWithMarkers());
		
		moduleService.deleteErrorMarkers(q -> q.ofProject(projectId));
		assertEquals(0, countModulesWithMarkers());
	}
	@Test
	void testSelectModulesOrderedByErrorMarker() {
		final var testModule1 = createTestModule("Module2", Storage.FILE, null);
		final var testModule2 = createTestModule("Module3", Storage.FILE, null);

		moduleService.createErrorMarker(new ErrorMarkerPojoPrototype()
												.setProject(projectId)
												.setModule(testModule1.identity())
												.setCause("Error1"));

		moduleService.createErrorMarker(new ErrorMarkerPojoPrototype()
												.setProject(projectId)
												.setModule(testModule2.identity())
												.setCause("Error2"));

		moduleService.createErrorMarker(new ErrorMarkerPojoPrototype()
												.setProject(projectId)
												.setModule(testModule2.identity())
												.setCause("Error3"));

		assertEquals(1, countErrorMarkers(testModule1.identity()));
		assertEquals(2, countErrorMarkers(testModule2.identity()));
		assertEquals(2, countModulesWithMarkers());

		var modsDesc = moduleService.findModules(q -> q.ofProject(projectId)
				.withNames(Arrays.asList("Module2", "Module3"))
				.sortErrorCount(SortDirection.DESCENDING));
		var modsAsc = moduleService.findModules(q -> q.ofProject(projectId)
				.withNames(Arrays.asList("Module2", "Module3"))
				.sortErrorCount(SortDirection.ASCENDING));

		assertEquals("Module3", modsDesc.get(0).getName());
		assertEquals("Module2", modsAsc.get(0).getName());

		moduleService.deleteErrorMarkers(q -> q.ofProject(projectId));
		assertEquals(0, countModulesWithMarkers());
	}

	private long countModulesWithMarkers() {
		return moduleService.countModules(q -> q.ofProject(projectId).withErrors());
	}
	
	private long countErrorMarkers(final EntityId moduleId) {
		return moduleService.countErrorMarkers(q -> q.ofModule(moduleId));
	}

	private ModulePojo createTestModule(final String name, final Storage storage, @Nullable final String content) {
		final ModulePojoPrototype testModule = new ModulePojoPrototype();
		testModule.setProject(projectId);
		testModule.setName(name);
		testModule.setDescription(name);
		testModule.setTechnology(Technology.COBOL);
		testModule.setType(Type.PROGRAM);
		testModule.setIdentification(Identification.IDENTIFIED);
		testModule.setOrigin(Origin.CUSTOM);
		testModule.setStorage(storage);
		testModule.setRepresentation(Representation.PHYSICAL);
		testModule.setCreator(Creator.DISCOVERY);
		if (content != null) {
			testModule.setContent(content);
			testModule.setPath("some arbitrary path");
		}
		return moduleService.getModule(moduleService.create(testModule));
	}
}

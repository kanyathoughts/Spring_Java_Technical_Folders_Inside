/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.data;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;

import org.apache.commons.collections4.map.HashedMap;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;

import innowake.mining.server.event.CustomPropertiesModifiedEvent;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.CustomPropertyFieldType;
import innowake.mining.shared.model.CustomPropertyMetadata;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Test Module tagging by updating tag values not present in Custom Property record.
 */
class ModuleTagsTest extends DatabaseRelatedTest {
	
	private static final String COLOR_TAGS = "colorTags";
	private static final String MODULE_ENTITY_NAME = "Module";
	
	private EntityId projectId = EntityId.VOID;
	
	@Autowired
	private ModuleService moduleService;
	@Autowired
	private ApplicationEventPublisher eventPublisher;
	
	@Test
	void testModuleTags() {
		projectId = createProject("moduleTagsProject").identity();
		final String className = MODULE_ENTITY_NAME + "CustomProperties" + projectId.getNid();
		final Set<String> colorTags = new HashSet<String>(Arrays.asList("red", "pink"));
		final Map<String, Set<String>> autoCompletionMap = new HashedMap<>();
		
		/* Create new custom property */
		final CustomPropertyMetadata propertyMetaData = new CustomPropertyMetadata();
		propertyMetaData.setName(COLOR_TAGS);
		propertyMetaData.setLabel(COLOR_TAGS);
		propertyMetaData.setDescription("Test Module Custom Property");
		propertyMetaData.setDataType("EMBEDDEDLIST");
		propertyMetaData.setFieldType(CustomPropertyFieldType.TAG);
		propertyMetaData.setCustomViewIndex(1);
		propertyMetaData.setAutoCompletionKey("colorTagsAutoCompletionKey");
		propertyMetaData.setPluginVisible(false);
		
		customPropertiesService.defineProperty(projectId, MODULE_ENTITY_NAME, propertyMetaData.getName(), propertyMetaData);
		eventPublisher.publishEvent(new CustomPropertiesModifiedEvent(Optional.of(projectId)));
		customPropertiesService.putEnumValues(projectId, Collections.singletonMap(propertyMetaData.getAutoCompletionKey(), colorTags));
		
		assertEquals(1L, customPropertiesService.countPropertyDefinitions(q -> q.ofParent(null).withName(className)), className + " should be existing in DB");
		assertEquals(1L, customPropertiesService.countPropertyDefinitions(q -> q.withParent(className).withName(propertyMetaData.getName())),
				propertyMetaData.getName() + " should exist in " + className);
		
		autoCompletionMap.put(propertyMetaData.getAutoCompletionKey(), new TreeSet<>(Arrays.asList("red", "blue", "pink")));
		
		final EntityId moduleId = createModule(projectId, "1", className, COLOR_TAGS);

		/* Assert if the new tag value is updated in Custom Property record. */
		assertProjectTags(projectId, autoCompletionMap);
		
		autoCompletionMap.put(propertyMetaData.getAutoCompletionKey(), new TreeSet<>(Arrays.asList("red", "blue", "green", "pink")));

		ModulePojoPrototype proto = new ModulePojoPrototype()
				.withId(moduleId)
				.setCustomProperties(new HashMap<>(Map.of(className, new HashMap<>(Map.of(COLOR_TAGS, Arrays.asList("red", "green"))))));

		/* Update Module record with tag value not present in Custom Property */
		moduleService.update(proto);

		/* Assert if the new tag value is updated in Custom Property record. */
		assertProjectTags(projectId, autoCompletionMap);
	}
	
	private void assertProjectTags(final EntityId projectId, final Map<String, Set<String>> autoCompletionMap) {
		final Map<String, Set<String>> actAutoCompletionMap = getAutoCompletionMap(projectId);
		assertEquals(autoCompletionMap, actAutoCompletionMap, () -> "autoCompletionMaps of project must be equal.");
	}
	
	private Map<String, Set<String>> getAutoCompletionMap(final EntityId projectId) {
		return customPropertiesService.getEnumsAndValues(projectId);
	}
	
	private ProjectPojo createProject(final String name) {
		return projectService.create(new ProjectPojoPrototype()
				.setName(name)
				.setClient(EntityId.of(1L))
				.setNatures(Collections.emptySet()));
	}
	
	private EntityId createModule(final EntityId projectId, final String suffix, final String className, final String propertyName) {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setProject(projectId);
		module.setName("MODULE" + suffix);
		module.setTechnology(Technology.COBOL);
		module.setType(Type.PROGRAM);
		module.setOrigin(Origin.CUSTOM);
		module.setStorage(Storage.FILE);
		module.setIdentification(Identification.MISSING);
		module.setPath("src/cobol/TESTCOBA" + suffix + ".cbl");
		module.setCreator(Creator.DISCOVERY);
		
		/* Assign new property to DataDictionaryEntry */
		module.setCustomProperties(new HashMap<>(Map.of(className, new HashMap<>(Map.of(propertyName, Arrays.asList("red","blue"))))));
		
		return moduleService.create(module);
	}
	
	@AfterAll
	void cleanUp() {
		resetCustomProperties(MODULE_ENTITY_NAME, projectId);
	}

}

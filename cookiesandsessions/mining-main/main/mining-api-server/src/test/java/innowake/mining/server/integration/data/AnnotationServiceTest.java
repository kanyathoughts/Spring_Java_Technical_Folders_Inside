/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.data;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ActiveProfiles;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.CustomPropertiesService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.lang.NestedMap;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.CustomPropertyFieldType;
import innowake.mining.shared.model.CustomPropertyMetadata;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;

/**
* Tests {@link AnnotationDao}
*/
@ActiveProfiles("no_caching")
class AnnotationServiceTest extends DatabaseResettingTest {
	
	private final Long TEST_PROJECT_ID_LONG = Long.valueOf(4);
	private final EntityId TEST_PROJECT_ID = EntityId.of(TEST_PROJECT_ID_LONG);
	
	@Autowired
	private AnnotationService annotationService;
	
	@Autowired
	private ModuleService moduleService;
	
	@Autowired
	private CustomPropertiesService customPropertiesService;
	
	@Test
	void testFindAnnotationsWithCustomProperty() {
		final EntityId moduleId = createTestModule("Module", Storage.FILE, "Some arbitrary content");

		/* Creating a Tag type custom property and adding this custom property to annotation */
		final String entityName = "Annotation";
		final String customPropertiesClassName = entityName + "CustomProperties" + TEST_PROJECT_ID.getNid();
		final String autoCompKey = "AnnotationAutoCompletionKey";
		final CustomPropertyMetadata propertyMetaData = createCustomPropertyMetadata("TagType",
				"TagType", "Test Annotation Custom Property for graph ql test", "EMBEDDEDLIST",
				CustomPropertyFieldType.TAG, 1, false, "AnnotationAutoCompletionKey", TEST_PROJECT_ID, entityName, customPropertiesClassName);

		final List<String> tagsAvailable = Arrays.asList("orange", "grape", "apple", "banana", "kiwi");
		final Map<String, Set<String>> map = new HashMap<>();
		map.put(autoCompKey, new HashSet<>(tagsAvailable));
		customPropertiesService.putEnumValues(TEST_PROJECT_ID, map);
		assertTrue(customPropertiesService.getEnumValues(TEST_PROJECT_ID, autoCompKey).containsAll(tagsAvailable), "All entries should match for key: " + autoCompKey);

		final List<String> tagsAssigned = Arrays.asList("grape", "kiwi");
		final AnnotationPojo annotation = createAnnotation("Annotation 1", AnnotationType.DATABASE, WorkingState.CANDIDATE,
				"This is if-ELSE source attachment \n content", moduleId, "1",
				new NestedMap().set(customPropertiesClassName, propertyMetaData.getName(), tagsAssigned));

		final Map<Long, Object> idAndDefaultValues = annotationService.getCustomProperties(TEST_PROJECT_ID, "TagType");
		assertEquals(Map.of(annotation.getId(), tagsAssigned), idAndDefaultValues);
	}
	
	private EntityId createTestModule(final String name, final Storage storage, @Nullable final String content) {
		final ModulePojoPrototype testModule = new ModulePojoPrototype();
		testModule.setProject(TEST_PROJECT_ID);
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
		return moduleService.create(testModule);
	}
	
	private AnnotationPojo createAnnotation(final String name, final AnnotationType annotationType, final WorkingState workingState,
			final String sourceAttachment, final EntityId moduleId, final String createdByUserId, final Map<String, Object> customProperties) {

		final ModuleLocation dummyLocation = new ModuleLocation();
		dummyLocation.setOffset(Integer.valueOf(600));
		dummyLocation.setLength(Integer.valueOf(50));

		final AnnotationPojoPrototype annotation = new AnnotationPojoPrototype();
		annotation.setName(name);
		annotation.setType(annotationType);
		annotation.setState(workingState);
		annotation.setSourceAttachment(sourceAttachment);
		annotation.setModule(moduleId);
		annotation.setLocation(dummyLocation);
		annotation.setCreatedByUserId(createdByUserId);
		annotation.setCustomProperties(customProperties);
		
		return annotationService.get(annotationService.create(annotation));
	}
	
	private CustomPropertyMetadata createCustomPropertyMetadata(final String name, final String label, final String description, final String dataType,
			final CustomPropertyFieldType customPropertyFieldType, final int customViewIndex, final boolean pluginVisible, @Nullable final String autoCompKey,
			final EntityId projectId, final String entityName, final String className) {
		final CustomPropertyMetadata customPropertyMetadata = new CustomPropertyMetadata();
		customPropertyMetadata.setName(name);
		customPropertyMetadata.setLabel(label);
		customPropertyMetadata.setDescription(description);
		customPropertyMetadata.setDataType(dataType);
		customPropertyMetadata.setFieldType(customPropertyFieldType);
		customPropertyMetadata.setCustomViewIndex(customViewIndex);
		customPropertyMetadata.setPluginVisible(pluginVisible);
		customPropertyMetadata.setAutoCompletionKey(autoCompKey);
		customPropertiesService.defineProperty(projectId, entityName, customPropertyMetadata.getName(), customPropertyMetadata);
		
		assertEquals(1L, customPropertiesService.countPropertyDefinitions(q -> q.ofParent(null).withName(className)), className + " should be existing in DB");
		assertEquals(1L, customPropertiesService.countPropertyDefinitions(q -> q.withParent(className).withName(customPropertyMetadata.getName())),
				customPropertyMetadata.getName() + " should exist in " + className);
		
		
		return customPropertyMetadata;
	}
}

/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.data;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.event.CustomPropertiesModifiedEvent;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.lang.NestedMap;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.CustomPropertyFieldType;
import innowake.mining.shared.model.CustomPropertyMetadata;
import innowake.mining.shared.model.DataDictionaryVariableAttribute;
import innowake.mining.shared.model.DataDictionaryVariableScope;
import innowake.mining.shared.model.DefinedLocation;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;

/**
 * Test data dictionary tagging by updating tag values not present in Custom Property record.
 */
class DataDictionaryTagsTest extends DatabaseRelatedTest {

	private static final String COLOR_TAGS = "colorTags";
	private static final String DD_ENTITY_NAME = "DataDictionaryEntry";
	
	@Nullable
	private EntityId projectId;
	
	@Autowired
	private DataDictionaryService dataDictionaryService;
	@Autowired
	private ModuleService moduleService;
	@Autowired
	private ApplicationEventPublisher eventPublisher;
	
	@Test
	void testDataDictionaryTags() {
		projectId = createProject("ddTagsProject").identity();
		final EntityId moduleId = createModule(projectId, "1");
		final String className = DD_ENTITY_NAME + "CustomProperties" + assertNotNull(projectId).getNid();
		final Set<String> colorTags = new HashSet<>(Arrays.asList("red", "yellow"));
		final Map<String, Set<String>> autoCompletionMap = new HashMap<>();
		
		/* Create new custom property */
		final CustomPropertyMetadata propertyMetaData = new CustomPropertyMetadata();
		propertyMetaData.setName(COLOR_TAGS);
		propertyMetaData.setLabel(COLOR_TAGS);
		propertyMetaData.setDescription("Test Data dictionary Custom Property");
		propertyMetaData.setDataType("EMBEDDEDLIST");
		propertyMetaData.setFieldType(CustomPropertyFieldType.TAG);
		propertyMetaData.setCustomViewIndex(1);
		propertyMetaData.setAutoCompletionKey("colorTagsAutoCompletionKey");
		propertyMetaData.setPluginVisible(false);
		
		customPropertiesService.defineProperty(assertNotNull(projectId), DD_ENTITY_NAME, propertyMetaData.getName(), propertyMetaData);
		eventPublisher.publishEvent(new CustomPropertiesModifiedEvent(Optional.of(projectId)));
		customPropertiesService.putEnumValues(assertNotNull(projectId), Collections.singletonMap(propertyMetaData.getAutoCompletionKey(), colorTags));
		
		assertEquals(1L, customPropertiesService.countPropertyDefinitions(q -> q.ofParent(null).withName(className)), className + " should be existing in DB");
		assertEquals(1L, customPropertiesService.countPropertyDefinitions(q -> q.withParent(className).withName(propertyMetaData.getName())),
				propertyMetaData.getName() + " should exist in " + className);
		
		autoCompletionMap.put(propertyMetaData.getAutoCompletionKey(), new TreeSet<>(Arrays.asList("red", "yellow", "blue")));
		
		/* Create a Data dictionary with tag value not present in Custom property */
		final DataDictionaryPojo dataDictionary = createFullDataDictionaryEntry("MY-PROGRAM-NAME", moduleId, className, COLOR_TAGS);
		assertNotNull(dataDictionary);
		
		/* Assert if the new tag value is updated in Custom Property record. */
		assertProjectTags(assertNotNull(projectId), autoCompletionMap);
		autoCompletionMap.put(propertyMetaData.getAutoCompletionKey(), new TreeSet<>(Arrays.asList("red", "yellow", "blue", "green")));
		
		/* Update data dictionary record with tag value not present in Custom Property */
		dataDictionaryService.update(new DataDictionaryPojoPrototype()
				.setUpdatedByUserId(dataDictionary.getCreatedByUserId())
				.setModule(dataDictionary.getModule())
				.setUid(dataDictionary.getUid())
				.setCustomProperties(new NestedMap().set(className, new HashMap<>(Map.of(COLOR_TAGS, Arrays.asList("red", "green"))))));
		
		/* Assert if the new tag value is updated in Custom Property record. */
		assertProjectTags(assertNotNull(projectId), autoCompletionMap);
	}
	
	private void assertProjectTags(final EntityId projectId, final Map<String, Set<String>> autoCompletionMap) {
		final Map<String, Set<String>> actAutoCompletionMap = getAutoCompletionMap(projectId);
		assertEquals(autoCompletionMap, actAutoCompletionMap, () -> "autoCompletionMaps of project must equal");
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
	
	private EntityId createModule(final EntityId projectId, final String suffix) {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setProject(projectId);
		module.setName("MOD" + suffix);
		module.setType(Type.PROGRAM);
		module.setTechnology(Technology.COBOL);
		module.setOrigin(Origin.CUSTOM);
		module.setStorage(Storage.FILE);
		module.setIdentification(Identification.MISSING);
		module.setPath("src/cobol/TESTCOBA" + suffix + ".cbl");
		module.setCreator(Creator.DISCOVERY);
		return moduleService.create(module);
	}
	
	private DataDictionaryPojo createFullDataDictionaryEntry(final String dataElementName, final EntityId moduleId, final String className,
			final String propertyName) {
		final Map<String, String> attributes = new HashMap<>();
		attributes.put("A", "B");
		attributes.put("C", "D");

		final DataDictionaryPojoPrototype pojoPrototype = new DataDictionaryPojoPrototype()
				.setName(dataElementName)
				.setScopes(
						Map.of(DataDictionaryVariableAttribute.FILE_DATASET.getScope(),
								Map.of(DataDictionaryVariableAttribute.FILE_DATASET.getKey(), "My data set name"),
						DataDictionaryVariableScope.SQL_DATABASE, Map.of(),
						DataDictionaryVariableScope.CICS_UI, Map.of(),
						DataDictionaryVariableScope.OTHER, attributes,
						DataDictionaryVariableScope.PARAMETER, Map.of()))
				.setModule(moduleId)
				.setLocation(new ModuleLocation(1, 10))
				.setDescription("MY description")
				.setFormat("PICX")
				.setLength(999L)
				.setCreatedByUserId("admin")
				.setPicClause("TEST PIC CLAUSE")
				.setDefinedLocation(DefinedLocation.PROGRAM)
				.setIsBusiness(true)
				.setState(WorkingState.CANDIDATE)
				.setFieldTransformation("TEST TRANSFORMATION")
				.setSourceInput("TEST INPUT")
				.setTargetOutput("TEST OUTPUT");
		
		/* Assign new property to DataDictionaryEntry */
		pojoPrototype.setCustomProperties(new HashMap<>(Map.of(className, new HashMap<>(Map.of(propertyName, Arrays.asList("red", "blue"))))));
		return dataDictionaryService.create(pojoPrototype);
	}
	
	@AfterAll
	void cleanUp() {
		resetCustomProperties(DD_ENTITY_NAME, assertNotNull(projectId));
	}
}

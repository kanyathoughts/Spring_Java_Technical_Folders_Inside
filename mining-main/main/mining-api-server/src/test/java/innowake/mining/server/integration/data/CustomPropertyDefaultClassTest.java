/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.data;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.datapoints.builder.MiningDataPointDefinitionWithCustomFetch;
import innowake.mining.data.datapoints.registry.DataPointRegistry;
import innowake.mining.server.event.CustomPropertiesModifiedEvent;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.CustomPropertiesService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.CustomPropertyFieldType;
import innowake.mining.shared.model.CustomPropertyMetadata;

/**
 * Test creation and deletion of custom properties on an entity's default custom property class. 
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class CustomPropertyDefaultClassTest extends DatabaseRelatedTest {

	private final EntityId TEST_PROJECT_ID = EntityId.of(1L);
	private final String ANNOTATION_TEST_ENTITY_NAME = "Annotation";
	private final String ANNOTATION_TEST_PROPERTY_CLASS = "AnnotationCustomProperties1";
	private final String MODULE_TEST_PROPERTY_CLASS = "ModuleCustomProperties1";

	@Autowired
	DataPointRegistry dataPointRegistry;

	@Autowired
	ApplicationEventPublisher eventPublisher;

	private boolean checkDataPointExists(final boolean refresh, @Nullable final CustomPropertyMetadata def) {
		if ( refresh ) {
			eventPublisher.publishEvent(new CustomPropertiesModifiedEvent(Optional.of(TEST_PROJECT_ID)));
		}
		String annotationTestDataPointName = "Annotation_CustomProperties_AnnotationCustomProperties1";
		final Map<String, MiningDataPointDefinitionWithCustomFetch> dps = dataPointRegistry.getDataPointDefinitions(Optional.of(TEST_PROJECT_ID.getNid()))
				.get(annotationTestDataPointName);
		if ( def == null ) {
			return dps != null;
		} else if ( dps != null ) {
			MiningDataPointDefinition dp = dps.get(def.getName());
			if ( dp != null ) {
				assertEquals(def.getLabel(), dp.getDisplayName());
				assertEquals(def.getDescription(), dp.getDescription());
			}
			return dp != null;
		}
		return false;
	}

	private boolean checkPropertyExists(final String cpClass, final String cpName) {
		return customPropertiesService.countPropertyDefinitions(q -> q.withParent(cpClass).withName(cpName)) == 1;
	}

	@Test
	@Order(1)
	void testMetamodelCreateModifyDelete() {
		final CustomPropertyMetadata def1 = customPropertyMetadataData("testProp1", "Test Label 1", "Test Description 1", 1, false);

		//this is flaky because something deletes the default Project 1 :(
		assertFalse("Assert that Custom Property does not exist.", checkPropertyExists(ANNOTATION_TEST_PROPERTY_CLASS, def1.getName()));
		assertFalse("Assert that data point does not exist", checkDataPointExists(false, def1));

		customPropertiesService.defineProperty(TEST_PROJECT_ID, ANNOTATION_TEST_ENTITY_NAME, def1.getName(), def1);
		assertTrue("Assert that Custom Property exists.", checkPropertyExists(ANNOTATION_TEST_PROPERTY_CLASS, def1.getName()));
		assertTrue("Assert that data point exists", checkDataPointExists(true, def1));

		final CustomPropertyMetadata def2 = customPropertyMetadataData("testProp2", "Test Label 2", "Test Description 2", 2, true);
		customPropertiesService.defineProperty(TEST_PROJECT_ID, ANNOTATION_TEST_ENTITY_NAME, def1.getName(), def2);
		assertFalse("Assert that Custom Property does not exist.", checkPropertyExists(ANNOTATION_TEST_PROPERTY_CLASS, def1.getName()));
		assertTrue("Assert that Custom Property exists.", checkPropertyExists(ANNOTATION_TEST_PROPERTY_CLASS, def2.getName()));
		assertTrue("Assert that data point exists", checkDataPointExists(true, null));
		assertFalse("Assert data point does not exist", checkDataPointExists(false, def1));
		assertTrue("Assert that data point exists", checkDataPointExists(false, def2));

		customPropertiesService.deleteProperty(TEST_PROJECT_ID, ANNOTATION_TEST_ENTITY_NAME, def2.getName());
		assertFalse("Assert that Custom Property does not exist.", checkPropertyExists(ANNOTATION_TEST_PROPERTY_CLASS, def2.getName()));
		assertFalse("Assert data point does not exist", checkDataPointExists(true, null));
	}

	@Test
	@Order(2)
	void testReorderCustomProperties() {
		final Map<String, Integer> viewIndexMap = new HashMap<>();

		final CustomPropertyMetadata def1 = customPropertyMetadataData("Sample1", "Sample1 Label", "Sample1 Description", 1, false);
		assertFalse("Assert that Custom Property does not exist.", checkPropertyExists(ANNOTATION_TEST_PROPERTY_CLASS, def1.getName()));
		final UUID prop1 = customPropertiesService.defineProperty(TEST_PROJECT_ID, ANNOTATION_TEST_ENTITY_NAME, def1.getName(), def1);
		viewIndexMap.put(def1.getName(), def1.getCustomViewIndex());

		final CustomPropertyMetadata def2 = customPropertyMetadataData("Sample2", "Sample2 Label", "Sample2 Description", 2, false);
		assertFalse("Assert that Custom Property does not exist.", checkPropertyExists(ANNOTATION_TEST_PROPERTY_CLASS, def2.getName()));
		final UUID prop2 = customPropertiesService.defineProperty(TEST_PROJECT_ID, ANNOTATION_TEST_ENTITY_NAME, def2.getName(), def2);
		viewIndexMap.put(def2.getName(), def2.getCustomViewIndex());

		List<CustomPropertyMetadata> metadataList = customPropertiesService.findPropertyDefinitions(q -> q.withParent(ANNOTATION_TEST_PROPERTY_CLASS));

		assertEquals(2, metadataList.size());
		assertViewIndex(metadataList, viewIndexMap);

		final CustomPropertyMetadata def3 = customPropertyMetadataData("Sample3", "Sample3 Label", "Sample3 Description", 2, false);
		assertFalse("Assert that Custom Property does not exist.", checkPropertyExists(ANNOTATION_TEST_PROPERTY_CLASS, def3.getName()));
		customPropertiesService.defineProperty(TEST_PROJECT_ID, ANNOTATION_TEST_ENTITY_NAME, def3.getName(), def3);
		viewIndexMap.put(def3.getName(), def3.getCustomViewIndex());
		customPropertiesService.updateProperty(prop2, null,
				BuildingConsumer.of(new HashMap<>(), m -> CustomPropertiesService.Properties.CUSTOM_VIEW_INDEX.setIn(m, 3)));
		viewIndexMap.put(def2.getName(), 3);

		metadataList = customPropertiesService.findPropertyDefinitions(q -> q.withParent(ANNOTATION_TEST_PROPERTY_CLASS));
		assertEquals(3, metadataList.size());
		assertViewIndex(metadataList, viewIndexMap);

		def3.setCustomViewIndex(1);
		customPropertiesService.defineProperty(TEST_PROJECT_ID, ANNOTATION_TEST_ENTITY_NAME, def3.getName(), def3);
		viewIndexMap.put(def3.getName(), def3.getCustomViewIndex());
		customPropertiesService.updateProperty(prop1, null,
				BuildingConsumer.of(new HashMap<>(), m -> CustomPropertiesService.Properties.CUSTOM_VIEW_INDEX.setIn(m, 2)));
		viewIndexMap.put(def1.getName(), 2);

		metadataList = customPropertiesService.findPropertyDefinitions(q -> q.withParent(ANNOTATION_TEST_PROPERTY_CLASS));
		assertEquals(3, metadataList.size());
		assertViewIndex(metadataList, viewIndexMap);

		customPropertiesService.deleteProperty(TEST_PROJECT_ID, ANNOTATION_TEST_ENTITY_NAME, def3.getName());
		viewIndexMap.remove(def3.getName());
		customPropertiesService.updateProperty(prop1, null,
				BuildingConsumer.of(new HashMap<>(), m -> CustomPropertiesService.Properties.CUSTOM_VIEW_INDEX.setIn(m, 1)));
		viewIndexMap.put(def1.getName(), 1);
		customPropertiesService.updateProperty(prop2, null,
				BuildingConsumer.of(new HashMap<>(), m -> CustomPropertiesService.Properties.CUSTOM_VIEW_INDEX.setIn(m, 2)));
		viewIndexMap.put(def2.getName(), 2);

		metadataList = customPropertiesService.findPropertyDefinitions(q -> q.withParent(ANNOTATION_TEST_PROPERTY_CLASS));
		assertEquals(2, metadataList.size());
		assertViewIndex(metadataList, viewIndexMap);
	}

	@Test
	@Order(3)
	void testCustomPropertiesPositionSwap() {
		final CustomPropertyMetadata def1 = customPropertyMetadataData("testCp1", "Test CP 1", "Test CP Description 1", 1, false);
		final CustomPropertyMetadata def2 = customPropertyMetadataData("testCp2", "Test CP 2", "Test CP Description 2", 2, false);
		final CustomPropertyMetadata def3 = customPropertyMetadataData("testCp3", "Test CP 3", "Test CP Description 3", 3, false);

		assertFalse("Assert that Custom Property does not exist.", checkPropertyExists(MODULE_TEST_PROPERTY_CLASS, def1.getName()));
		assertFalse("Assert that Custom Property does not exist.", checkPropertyExists(MODULE_TEST_PROPERTY_CLASS, def2.getName()));
		assertFalse("Assert that Custom Property does not exist.", checkPropertyExists(MODULE_TEST_PROPERTY_CLASS, def3.getName()));

		String moduleTestEntityName = "Module";
		customPropertiesService.defineProperty(TEST_PROJECT_ID, moduleTestEntityName, def1.getName(), def1);
		customPropertiesService.defineProperty(TEST_PROJECT_ID, moduleTestEntityName, def2.getName(), def2);
		customPropertiesService.defineProperty(TEST_PROJECT_ID, moduleTestEntityName, def3.getName(), def3);

		assertTrue("Assert that Custom Property exists.", checkPropertyExists(MODULE_TEST_PROPERTY_CLASS, def1.getName()));
		assertTrue("Assert that Custom Property exists.", checkPropertyExists(MODULE_TEST_PROPERTY_CLASS, def2.getName()));
		assertTrue("Assert that Custom Property exists.", checkPropertyExists(MODULE_TEST_PROPERTY_CLASS, def3.getName()));

		final CustomPropertyMetadata def4 = customPropertyMetadataData("testCp2", "Test CP 2", "Test CP Description 2", 1, false);
		customPropertiesService.defineProperty(TEST_PROJECT_ID, moduleTestEntityName, def2.getName(), def4);

		assertTrue("Assert that Custom Property exists.", checkPropertyExists(MODULE_TEST_PROPERTY_CLASS, def2.getName()));

		final List<CustomPropertyMetadata> filteredMetadata = customPropertiesService.findPropertyDefinitions(q -> q.withParent(MODULE_TEST_PROPERTY_CLASS))
				.stream().filter(metadata -> (metadata.getName().equals(def1.getName()) || metadata.getName().equals(def2.getName()) || metadata.getName()
						.equals(def3.getName()))).toList();
		assertEquals(3, filteredMetadata.size());
		filteredMetadata.forEach(metadata -> {
			if ( metadata.getName().equals(def1.getName()) ) {
				assertEquals(2, metadata.getCustomViewIndex());
			} else if ( metadata.getName().equals(def2.getName()) ) {
				assertEquals(1, metadata.getCustomViewIndex());
			} else if ( metadata.getName().equals(def3.getName()) ) {
				assertEquals(3, metadata.getCustomViewIndex());
			}
		});
	}

	private void assertViewIndex(final List<CustomPropertyMetadata> metadataList, final Map<String, Integer> viewIndexMap) {
		metadataList.forEach(metadata -> assertEquals(viewIndexMap.get(metadata.getName()), metadata.getCustomViewIndex()));
	}

	private CustomPropertyMetadata customPropertyMetadataData(final String name, final String label, final String description, final Integer viewIndex,
			final boolean plugin) {
		final CustomPropertyMetadata def = new CustomPropertyMetadata();
		def.setName(name);
		def.setDataType("STRING");
		def.setFieldType(CustomPropertyFieldType.DEFAULT);
		def.setLabel(label);
		def.setDescription(description);
		def.setPluginVisible(plugin);
		def.setCustomViewIndex(viewIndex);
		return def;
	}

}

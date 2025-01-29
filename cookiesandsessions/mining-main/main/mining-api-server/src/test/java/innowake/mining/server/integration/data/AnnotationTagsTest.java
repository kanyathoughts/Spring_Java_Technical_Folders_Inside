/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.data;

import static java.util.Collections.emptyList;
import static java.util.Collections.emptyMap;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.apache.commons.collections4.map.HashedMap;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import com.fasterxml.jackson.core.JsonProcessingException;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
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
 * Test annotation tagging and the collection of known tags.
 */
class AnnotationTagsTest extends DatabaseRelatedTest {
	
	private static final String ANNOTATION_CUSTOM_PROPERTIES = "AnnotationCustomProperties";
	private static final String COLOR_TAGS = "colorTags";
	private static final String RULE_TAGS = "ruleTags";

	@Autowired
	private ModuleService moduleService;
	
	@Autowired
	private AnnotationService annotationService;
	
	/**
	 * Tests that annotation and project have no CustomProperty if no tag was set.
	 *
	 * @throws JsonProcessingException if JSON processing failed
	 */
	@Test
	void testNoTags() throws JsonProcessingException {
		final EntityId projectId = createProject("projectNoAnnotationTags").identity();
		final EntityId module1Id = createModule(projectId, '1');

		final AnnotationPojo annotation = createAnnotation(module1Id, "Anno1", emptyList(), emptyList());
		assertAnnotationTags(annotation, emptyList(), emptyList());
		assertProjectTags(projectId, emptyMap());
	}

	/**
	 * Tests that multiple annotations and project are created together with CustomProperties. The project must contain the merged tags
	 * of both annotations.
	 * <p>Tests that duplicates and empty entries are not stored, which can also be the result of the trimming of leading and tailing whitespaces.</p>
	 *
	 * @throws JsonProcessingException if JSON processing failed
	 */
	@Test
	void testMultipleTagsOnModules() throws JsonProcessingException {
		final EntityId projectId = createProject("projectTestMultipleTagsOnModules").identity();
		customPropertiesService.createEnum(projectId, COLOR_TAGS);
		customPropertiesService.createEnum(projectId, RULE_TAGS);
		
		final EntityId module1Id = createModule(projectId, '1');
		final EntityId module2Id = createModule(projectId, '2');

		/* First annotation with tags for two different autoCompletionKeys */
		final List<String> colorTags1 = Arrays.asList("red", "blue ", "red", "  yellow ", "red", "", " ", "  ", "   ");
		final List<String> expColorTags1 = Arrays.asList("red", "blue", "yellow");
		final List<String> ruleTags1 = Arrays.asList("Rule 2", "Rule 1", "  Rule 2", " Rule x", "Rule x");
		final List<String> expRuleTags1 = Arrays.asList("Rule 2", "Rule 1", "Rule x");
		final Map<String, Set<String>> autoCompletionMap = new HashedMap<>();
		autoCompletionMap.put(COLOR_TAGS, new TreeSet<String>(Arrays.asList("blue", "red", "yellow")));
		autoCompletionMap.put(RULE_TAGS, new TreeSet<String>(Arrays.asList("Rule 1", "Rule 2", "Rule x")));
		final AnnotationPojo annotation1 = createAnnotation(module1Id, "Annotation_1", colorTags1, ruleTags1);

		assertAnnotationTags(annotation1, expColorTags1, expRuleTags1);
		assertProjectTags(projectId, autoCompletionMap);

		/* Second annotation with two new tags 'pink' and 'green' for the autoCompletionKey only */
		final List<String> colorTags2 = Arrays.asList("green ", "  yellow ", "", "pink");
		final List<String> expColorTags2 = Arrays.asList("green", "yellow", "pink");
		autoCompletionMap.put(COLOR_TAGS, new TreeSet<String>(Arrays.asList("blue", "green", "pink", "red", "yellow")));
		final AnnotationPojo annotation2 = createAnnotation(module2Id, "Annotation_2", colorTags2, emptyList());

		/* Test that colorTags of both annotations were merged into project autoCompletionMap */
		assertAnnotationTags(annotation2, expColorTags2, emptyList());
		assertProjectTags(projectId, autoCompletionMap);
	}
	
	/**
	 * Tests that annotation and project are created together with CustomProperties.
	 * <p>Tests also that tags can be removed from annotations which doesn't remove them from the project.</p>
	 * <p>Tests also that new tags can be added to an existing annotations which also adds it to the project.</p>
	 * <p>Tests also that duplicates after trimming leading and tailing whitespaces are not stored.</p>
	 *
	 * @throws JsonProcessingException if JSON processing failed
	 */
	@Test
	void testUpdateTags() throws JsonProcessingException {
		final EntityId projectId = createProject("projectUpdateTags").identity();
		customPropertiesService.createEnum(projectId, COLOR_TAGS);
		customPropertiesService.createEnum(projectId, RULE_TAGS);
		
		final EntityId module1Id = createModule(projectId, '1');

		/* Create new annotation only with colorTags */
		List<String> colorTags = Arrays.asList("  red   ", "red ", "  yellow ", " ", " ", "");
		List<String> expColorTags = Arrays.asList("red", "yellow");
		final Map<String, Set<String>> autoCompletionMap = new HashedMap<>();
		autoCompletionMap.put(COLOR_TAGS, new TreeSet<String>(expColorTags));
		AnnotationPojo annotation = createAnnotation(module1Id, "Annotation_1", colorTags, emptyList());

		/* Annotation and project must only contain color tags without duplicates */
		assertAnnotationTags(annotation, expColorTags, emptyList());
		assertProjectTags(projectId, autoCompletionMap);

		/* Remove colorTags yellow from annotation */
		final Object property = getCustomProperty(annotation, COLOR_TAGS);
		assertNotNull("Custom property for 'colorTags' must not be null", property);
		colorTags = Arrays.asList("red");
		expColorTags = Arrays.asList("red");
		annotation = annotationService.get(annotationService.update(new AnnotationPojoPrototype().withId(annotation.identity())
				.setCustomProperties(new NestedMap().set(ANNOTATION_CUSTOM_PROPERTIES, COLOR_TAGS, colorTags))
			));

		/* Annotation must only have colorTags red, project must still contain both tags, red and yellow */
		assertAnnotationTags(annotation, expColorTags, emptyList());
		assertProjectTags(projectId, autoCompletionMap);

		/* Add new ruleTags to annotation */
		final List<String> ruleTags = Arrays.asList("Rule1");
		autoCompletionMap.put(RULE_TAGS, new TreeSet<String>(ruleTags));
		
		/* CustomProperty must not yet be present since it was no value set for it when storing the annotation */
		assertNull("Custom property 'ruleTags' must be null", getCustomProperty(annotation, RULE_TAGS));
		final List<CustomPropertyMetadata> customPropertiesMetaData = customPropertiesService.findPropertyDefinitions(q -> q.withParent(ANNOTATION_CUSTOM_PROPERTIES));
		final boolean ruleTagsExists = customPropertiesMetaData.stream().anyMatch(meta -> RULE_TAGS.equals(meta.getName()));
		assertTrue(ruleTagsExists, "Custom property 'ruleTags' must exists");
		
		annotation = annotationService.get(annotationService.update(new AnnotationPojoPrototype().withId(annotation.identity())
				.setCustomProperties(new NestedMap().set(ANNOTATION_CUSTOM_PROPERTIES, RULE_TAGS, ruleTags))
			));

		/* Annotation and project must additionally contain ruleTags */
		assertAnnotationTags(annotation, expColorTags, ruleTags);
		assertProjectTags(projectId, autoCompletionMap);
	}

	/**
	 * Tests that new {@link CustomPropertyMetadata custom properties} can be created and assigned.
	 * <p>Tests also that new properties created creates respective class and property.</p>
	 */
	@Test
	void testAddNewCustomProperty() {
		final EntityId projectId = createProject("projectNewProperty").identity();
		final EntityId module1Id = createModule(projectId, '1');
		final String entityName = "Annotation";
		final String className = entityName + "CustomProperties" + projectId.getNid();
		final AnnotationPojo annotation = createAnnotation(module1Id, "Annotation_1", emptyList(), emptyList());
		
		/* Create new custom property */
		final CustomPropertyMetadata propertyMetaData = new CustomPropertyMetadata();
		propertyMetaData.setName("testAnnotationCustomProperty");
		propertyMetaData.setLabel("Test Annotation Custom Label");
		propertyMetaData.setDescription("Test Annotation Custom Property for graph ql test");
		propertyMetaData.setDataType("STRING");
		propertyMetaData.setFieldType(CustomPropertyFieldType.DEFAULT);
		propertyMetaData.setCustomViewIndex(1);
		propertyMetaData.setAutoCompletionKey("test");
		propertyMetaData.setPluginVisible(false);
		customPropertiesService.defineProperty(projectId, entityName, propertyMetaData.getName(), propertyMetaData);
		
		assertEquals(1L, customPropertiesService.countPropertyDefinitions(q -> q.ofParent(null).withName(className)), className + " should be existing in DB");
		assertEquals(1L, customPropertiesService.countPropertyDefinitions(q -> q.withParent(className).withName(propertyMetaData.getName())),
				propertyMetaData.getName() + " should exist in " + className);
		
		/* Assign new property to annotation */
		final String propertyValue = "Sample Property Value";
		final AnnotationPojo updatedAnnotation = annotationService.get(
			annotationService.update(new AnnotationPojoPrototype().withId(annotation.identity())
				.setCustomProperties(new NestedMap().set(className, propertyMetaData.getName(), propertyValue))
			));
		
		/* Assert custom property has been assigned as expected */
		assertEquals(annotation.getName(), updatedAnnotation.getName());
		final CustomPropertiesMap updatedCustomProperties = updatedAnnotation.getCustomProperties();
		assertTrue(updatedCustomProperties.containsKey(className), className + " should be present");
		assertEquals(1, updatedCustomProperties.size());
		final Object updatedValue = updatedCustomProperties.getValue(className, propertyMetaData.getName());
		assertEquals(propertyValue, updatedValue);
		
		resetCustomProperties(entityName, projectId);
	}

	@Override
	protected String getAdditionalScript() {
		return "DELETE FROM ProjectCustomProperties UNSAFE;"
				+ "DELETE FROM AnnotationCustomProperties UNSAFE;";
	}

	private ProjectPojo createProject(final String name) {
		return projectService.create(new ProjectPojoPrototype()
				.setName(name)
				.setClient(EntityId.of(1L))
				.setNatures(Collections.emptySet()));
	}

	private EntityId createModule(final EntityId projectId, final char suffix) {
		final ModulePojoPrototype mod = new ModulePojoPrototype();
		mod.setProject(projectId);
		mod.setName("MOD" + suffix);
		mod.setTechnology(Technology.COBOL);
		mod.setType(Type.PROGRAM);
		mod.setOrigin(Origin.CUSTOM);
		mod.setStorage(Storage.FILE);
		mod.setIdentification(Identification.MISSING);
		mod.setPath("src/cobol/TESTCOBA" + suffix + ".cbl");
		mod.setCreator(Creator.DISCOVERY);
		return moduleService.create(mod);
	}
	
	private AnnotationPojo createAnnotation(final EntityId moduleId, final String name, final List<String> colorTags, final List<String> ruleTags) {
		final var proto = new AnnotationPojoPrototype()
				.setModule(moduleId)
				.setType(AnnotationType.RULE)
				.setName(name)
				.setState(WorkingState.IN_ANALYSIS)
				.setCreatedByUserId("asdf")
				.setLocation(new ModuleLocation(0, 1));

		final var cpm = new NestedMap();
		if ( ! colorTags.isEmpty()) {
			cpm.set(ANNOTATION_CUSTOM_PROPERTIES, COLOR_TAGS, colorTags);
		}
		if ( ! ruleTags.isEmpty()) {
			cpm.set(ANNOTATION_CUSTOM_PROPERTIES, RULE_TAGS, ruleTags);
		}
		proto.setCustomProperties(cpm);

		return annotationService.get(annotationService.create(proto));
	}

	private void assertProjectTags(final EntityId projectId, final Map<String, Set<String>> autoCompletionMap) {
		final Map<String, Set<String>> actAutoCompletionMap = getAutoCompletionMap(projectId);
		assertEquals(autoCompletionMap, actAutoCompletionMap, () -> "autoCompletionMaps of project must equal");
	}

	private Map<String, Set<String>> getAutoCompletionMap(final EntityId projectId) {
		return customPropertiesService.getEnumsAndValues(projectId);
	}

	private void assertAnnotationTags(final AnnotationPojo annotation, final List<String> expColorTags, final List<String> expRuleTags) {
		@SuppressWarnings("unchecked")
		final List<String> actColorTags = (List<String>) getCustomProperty(annotation, COLOR_TAGS);
		if (actColorTags == null) {
			assertTrue(expColorTags.isEmpty(), "Custom property for 'colorTags' tags must be present");
		} else {
			assertEquals(expColorTags, actColorTags, "Colors tags must match");
		}

		final Object actRuleTags = getCustomProperty(annotation, RULE_TAGS);
		if (actRuleTags == null) {
			assertTrue(expRuleTags.isEmpty(), "Custom property for ruleTags must be present");
		} else {
			assertEquals(expRuleTags, actRuleTags, "Rules tags must match");
		}
	}

	@Nullable
	private static final Object getCustomProperty(final AnnotationPojo annotation, final String name) {
		return annotation.getCustomProperties().getOptionalAt(ANNOTATION_CUSTOM_PROPERTIES, name).orElse(null);
	}
	
	@Override
	protected List<String> getAdditionalPgScriptFile() {
		return List.of("test-data-custom-properties");
	}
}

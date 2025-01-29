/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.data;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.apache.commons.collections4.map.HashedMap;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.lang.NestedMap;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;

/**
 * Test Renaming and Deletion of Auto-completion list value
 */
class CustomPropertyDaoTest extends DatabaseResettingTest {
	
	private static final String COLOR_TAGS = "colorTags";
	private static final String GREEN = "green";
	private static final String PURPLE = "purple";
	private static final String ANNOTATION = "Annotation";
	private static final String ANNOTATION_CUSTOM_PROPERTIES = "AnnotationCustomProperties";
	
	@Autowired
	private ModuleService moduleService;
	
	@Autowired
	private AnnotationService annotationService;
	
	@Test
	void testRenamingTagValue() {
		final EntityId projectId = createProject("RenameTest");
		final EntityId moduleId = createModule(projectId, '1');
		final List<String> colorTags = new ArrayList<>(Arrays.asList("red", "blue", GREEN));
		final Map<String, Set<String>> autoCompletionMap = new HashedMap<>();
		autoCompletionMap.put(COLOR_TAGS, new TreeSet<String>(colorTags));
		AnnotationPojo annotation = createAnnotation(moduleId, ANNOTATION, colorTags);
		
		/* Assert if Project tags and Annotation tags are matching expected */
		assertProjectTags(projectId, autoCompletionMap);
		assertAnnotationTags(annotation, colorTags);
		
		/* Rename the auto-completion list value */
		customPropertiesService.renameEnumValue(projectId, COLOR_TAGS, GREEN, PURPLE);
		
		final List<String> expColorTags = new ArrayList<>(Arrays.asList("red", "blue", PURPLE));
		autoCompletionMap.put(COLOR_TAGS, new TreeSet<String>(expColorTags));
		AnnotationPojo annotation2 = annotationService.get(q -> q.ofProject(projectId).byId(annotation.identity()));
		/* Assert Renamed value change in Project and Annotation custom property*/
		assertProjectTags(projectId, autoCompletionMap);
		assertAnnotationTags(annotation2, expColorTags);
	}
	
	@Test
	void testdeletingTagValue() {
		final EntityId projectId = createProject("DeleteTest");
		final EntityId moduleId = createModule(projectId, '1');
		final List<String> colorTags = new ArrayList<>(Arrays.asList("yellow", "pink", GREEN));
		final Map<String, Set<String>> autoCompletionMap = new HashedMap<>();
		autoCompletionMap.put(COLOR_TAGS, new TreeSet<String>(colorTags));
		AnnotationPojo annotation = createAnnotation(moduleId, ANNOTATION, colorTags);
		
		/* Assert if Project tags and Annotation tags are matching expected */
		assertProjectTags(projectId, autoCompletionMap);
		assertAnnotationTags(annotation, colorTags);
		
		/* Delete auto-completion list value */
		customPropertiesService.removeEnumValues(projectId, COLOR_TAGS, Collections.singletonList(GREEN));
		
		final List<String> expColorTags = new ArrayList<>(Arrays.asList("yellow", "pink"));
		autoCompletionMap.put(COLOR_TAGS, new TreeSet<String>(expColorTags));
		final AnnotationPojo annotation2 = annotationService.get(q -> q.ofProject(projectId).byId(annotation.identity()));
		/* Assert that deleted value is removed in Project and Annotation custom property*/
		assertProjectTags(projectId, autoCompletionMap);
		assertAnnotationTags(annotation2, expColorTags);
	}
	
	private EntityId createProject(final String name) {
		EntityId projectId = projectService.create(new ProjectPojoPrototype()
				.setName(name)
				.setClient(EntityId.of(1L))
				.setNatures(Collections.emptySet())
			).identity();
		customPropertiesService.createEnum(projectId, COLOR_TAGS);
		customPropertiesService.assignProperty(projectId, MiningEnitityNames.ANNOTATION, ANNOTATION_CUSTOM_PROPERTIES);
		return projectId;
	}
	
	private EntityId createModule(final EntityId projectId, final char suffix) {
		final ModulePojoPrototype module = new ModulePojoPrototype()
				.setProject(projectId)
				.setName("MOD" + suffix)
				.setType(Type.PROGRAM)
				.setTechnology(Technology.COBOL)
				.setType(Type.PROGRAM)
				.setOrigin(Origin.CUSTOM)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.MISSING)
				.setPath("src/cobol/TESTCOBA" + suffix + ".cbl")
				.setCreator(Creator.DISCOVERY);
		return moduleService.create(module);
	}
	
	private AnnotationPojo createAnnotation(final EntityId moduleId, final String name, final List<String> colorTags) {
		return annotationService.get(annotationService.create(new AnnotationPojoPrototype()
				.setModule(moduleId)
				.setType(AnnotationType.RULE)
				.setName(name)
				.setState(WorkingState.IN_ANALYSIS)
				.setCreatedByUserId("user")
				.setLocation(new ModuleLocation(0, 1))
				.setCustomProperties(new NestedMap()
						.set(ANNOTATION_CUSTOM_PROPERTIES, COLOR_TAGS, colorTags)
				)
			));
	}
	
	private void assertProjectTags(final EntityId projectId, final Map<String, Set<String>> autoCompletionMap) {
		final Map<String, Set<String>> actAutoCompletionMap = getAutoCompletionMap(projectId);
		assertEquals(autoCompletionMap, actAutoCompletionMap, () -> "autoCompletionMaps of project must be equal");
	}

	private Map<String, Set<String>> getAutoCompletionMap(final EntityId projectId) {
		return customPropertiesService.getEnumsAndValues(projectId);
	}
	
	private void assertAnnotationTags(final AnnotationPojo annotation, final List<String> expectedColorTags) {
		final Object colorProperty = getCustomProperty(annotation, COLOR_TAGS);
		if (colorProperty == null) {
			assertTrue(expectedColorTags.isEmpty(), "Custom property for 'colorTags' tags must be present");
		} else {
			@SuppressWarnings("unchecked")
			final List<String> actualColorTags = (List<String>) colorProperty;
			assertEquals(expectedColorTags, actualColorTags, "Colors tags must match");
		}
	}
	
	@Nullable
	private static final Object getCustomProperty(final AnnotationPojo annotation, final String name) {
		final Map<String, Object> customProperties = annotation.getCustomProperties().getValue("AnnotationCustomProperties");
		return customProperties.get(name);
	}
	
	@Override
	protected List<String> getAdditionalPgScriptFile() {
		return List.of("test-data-custom-properties");
	}
}

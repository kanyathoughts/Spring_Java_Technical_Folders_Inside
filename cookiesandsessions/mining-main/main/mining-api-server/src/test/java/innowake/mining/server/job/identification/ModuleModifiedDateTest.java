/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.identification;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Arrays;
import java.util.Collections;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.event.CustomPropertiesModifiedEvent;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojoPrototype;
import innowake.mining.shared.lang.NestedMap;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.CustomPropertyFieldType;
import innowake.mining.shared.model.CustomPropertyMetadata;
import innowake.mining.shared.model.DataDictionaryVariableScope;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.WorkingState;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;

/**
 * Tests the Module modified date for any metadata change.
 */
@WithMockUser
class ModuleModifiedDateTest extends AbstractIdentificationTest {
	
	@Autowired
	private DataDictionaryService dataDictionaryService;
	@Autowired
	private AnnotationService annotationService;
	@Autowired
	private TaxonomyService taxonomyService;
	@Autowired
	private JobManager jobManager;
	@Autowired
	private ApplicationEventPublisher eventPublisher;
	
	@Test
	void testModuleModifiedDateForAnyDataDictionaryAndAnnotationChange() {
		final EntityId projectId = createProject().identity();
		final EntityId moduleId = createCobolProgram(projectId, "MMRS7100", "MMRS7100.cbl", RESOURCE_PATH);
		
		/* Create DataDictionaryEntry for the Cobol Module */
		Instant modifiedDate = resetModifiedDate(moduleId);
		final DataDictionaryPojo dataDictionaryEntry = createDataDictionaryEntry(moduleId, "Data Dictionary 1");
		assertModifiedDateAfter(moduleId, modifiedDate);
		
		/* Delete DataDictionaryEntry for the Cobol Module */
		modifiedDate = resetModifiedDate(moduleId);
		dataDictionaryService.delete(q -> q.byId(dataDictionaryEntry.identity()));
		assertModifiedDateAfter(moduleId, modifiedDate);
		
		/* Create Annotation for the Cobol Module */
		modifiedDate = resetModifiedDate(moduleId);
		final EntityId annotation = createAnnotation(moduleId);
		assertModifiedDateAfter(moduleId, modifiedDate);
		
		/* Delete Annotation for the Cobol Module */
		modifiedDate = resetModifiedDate(moduleId);
		annotationService.delete(projectId, annotation);
		assertModifiedDateAfter(moduleId, modifiedDate);
	}

	@Test
	void testModuleModifiedDateForCandidateIdentification() {
		final EntityId projectId = createProject().identity();
		final EntityId moduleId = createCobolProgram(projectId, "MMRS7100", "MMRS7100.cbl", RESOURCE_PATH);
		
		/* Run the Candidate Identification on the Cobol Module */
		final Instant modifiedDate = resetModifiedDate(moduleId);
		submitJob(jobManager, new IdentifyCandidatesJob(projectId, new ModuleMatcher(Arrays.asList(moduleId), Collections.emptyList())));
		assertModifiedDateAfter(moduleId, modifiedDate);
	}
	
	@Test
	void testModuleModifiedDateIdentifyModuleDescriptions() {
		final EntityId projectId = createProject().identity();
		final EntityId moduleId = createCobolProgram(projectId, "MMRS7100", "MMRS7100.cbl", RESOURCE_PATH);
		
		/* Run the Module Description Identification on the Cobol Module */
		final Instant modifiedDate = resetModifiedDate(moduleId);
		submitJob(jobManager, new IdentifyModuleDescriptionsJob(projectId, new ModuleMatcher(Collections.emptyList(), Collections.singletonList(RESOURCE_PATH + "MMRS7100.cbl"))));
		assertModifiedDateAfter(moduleId, modifiedDate);
	}
	
	@Test
	void testModuleModifiedDateForAnyTaxonomyChange() {
		final EntityId projectId = createProject().identity();
		final EntityId moduleId = createCobolProgram(projectId, "MMRS7100", "MMRS7100.cbl", RESOURCE_PATH);

		/* Run the Technical Taxonomy Identification on the Cobol Module */
		Instant modifiedDate = resetModifiedDate(moduleId);
		submitJob(jobManager, new IdentifyTechnicalTaxonomiesJob(projectId, new ModuleMatcher(Arrays.asList(moduleId), Collections.emptyList())));
		assertModifiedDateAfter(moduleId, modifiedDate);

		/* Create Taxonomy */
		final EntityId taxonomyId = createTaxonomy(projectId, "Taxonomy 1");

		/* Assign Taxonomy to the Cobol Module. */
		modifiedDate = resetModifiedDate(moduleId);
		taxonomyService.createModuleLink(moduleId.getUid(), taxonomyId);
		assertModifiedDateAfter(moduleId, modifiedDate);

		/* Unassign Taxonomy that already exists for the Cobol Module. It should delete the assignment. */
		modifiedDate = resetModifiedDate(moduleId);
		taxonomyService.deleteModuleLinks(q -> q.ofModule(moduleId).byId(taxonomyId));
		assertModifiedDateAfter(moduleId, modifiedDate);
	}

	@Test
	void testModuleModifiedDateForCustomPropertyChange() {
		final EntityId projectId = createProject().identity();
		final EntityId moduleId = createCobolProgram(projectId, "MMRS7100", "MMRS7100.cbl", RESOURCE_PATH);
		final var module = moduleService.findAnyModule(b -> b.byId(moduleId))
										.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module for id: " + moduleId));

		final String className = "Module" + "CustomProperties" + projectId.getNid();
		
		/* Creating a String repeater type custom property and adding this custom property to module */
		final CustomPropertyMetadata propertyMetaData1 = createCustomPropertyMetadata("StringRepeaterType",
				"Test Module String Repeater Custom Label", "Test Module Custom Property for graph ql test", "EMBEDDEDLIST",
				CustomPropertyFieldType.DEFAULT, 1, false, null, projectId, "Module", className);
		
		/* Assign new String Repeater type property to module */

		CustomPropertiesMap customPropertiesMap = module.getCustomProperties();
		assertEquals(0, customPropertiesMap.size());
		final var updateMap = new NestedMap(); 
		updateMap.put(className, Map.of(propertyMetaData1.getName(), Arrays.asList("red","blue","green")));

		final Instant modifiedDate = resetModifiedDate(moduleId);
		moduleService.update(new ModulePojoPrototype()
									.withId(module.identity())
									.setCustomProperties(customPropertiesMap));
		assertModifiedDateAfter(moduleId, modifiedDate);
		
		/* Cleaning the Custom property */
		resetCustomProperties("Module", projectId);
	}

	/**
	 * Loads the module for the given {@code moduleId} and updates its modification date to 5 seconds earlier to avoid test failures due to missing precision
	 * and returns the updated modification date.
	 *
	 * @param moduleId the id of the module 
	 * @return the updated modification date of the module
	 */
	private Instant resetModifiedDate(final EntityId moduleId) {
		final var modifiedDate = moduleService.findAnyModule(b -> b.byId(moduleId))
										.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module for id: " + moduleId))
										.getModifiedDate()
										.orElseThrow(() -> new IllegalStateException("Modification date must be present"));
		var newModifiedDate = modifiedDate.minus(5, ChronoUnit.SECONDS);

		moduleService.updateModules(q -> q.byId(moduleId), new ModulePojoPrototype()
																.setModifiedDate(newModifiedDate));

		final var actModifiedDate = moduleService.findAnyModule(b -> b.byId(moduleId))
												 .orElseThrow(() -> new MiningEntityNotFoundException("Could not find module for id: " + moduleId))
												 .getModifiedDate()
												 .orElseThrow(() -> new IllegalStateException("Modification date must be present"));

		assertEquals(newModifiedDate, actModifiedDate);

		return newModifiedDate;
	}

	private void assertModifiedDateAfter(final EntityId moduleId, final Instant expModifiedDate) {
		final var module = moduleService.findAnyModule(b -> b.byId(moduleId))
										.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module for id: " + moduleId));

		var actModifiedDate = module.getModifiedDate()
									.orElseThrow(() -> new IllegalStateException("Modification date must be present"));

		assertThat("timestamp", actModifiedDate, greaterThan(expModifiedDate));
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
		eventPublisher.publishEvent(new CustomPropertiesModifiedEvent(Optional.of(projectId)));
		
		assertEquals(1L, customPropertiesService.countPropertyDefinitions(q -> q.ofParent(null).withName(className)), className + " should be existing in DB");
		assertEquals(1L, customPropertiesService.countPropertyDefinitions(q -> q.withParent(className).withName(customPropertyMetadata.getName())),
				customPropertyMetadata.getName() + " should exist in " + className);
		
		return customPropertyMetadata;
	}

	private DataDictionaryPojo createDataDictionaryEntry(final EntityId moduleId, final String dataDictionaryEntryName) {
		final DataDictionaryPojoPrototype dataDictionaryEntry = new DataDictionaryPojoPrototype()
				.setName(dataDictionaryEntryName)
				.setCreatedByUserId("admin")
				.setDescription("Data Dictionary Entry")
				.setModule(moduleId)
				.setLocation(new ModuleLocation(47, 11))
				.setScopes(Map.of(DataDictionaryVariableScope.FILE, Map.of(), DataDictionaryVariableScope.SQL_DATABASE, Map.of()));
		return dataDictionaryService.create(dataDictionaryEntry);
	}

	private EntityId createTaxonomy(final EntityId projectId, final String taxonomyName) {
		final UUID type = taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("DataDomain").setProject(projectId));
		return taxonomyService.create(new TaxonomyPojoPrototype().setProject(projectId).setName(taxonomyName).setType(type));
	}

	private EntityId createAnnotation(final EntityId moduleId) {
		return annotationService.create(new AnnotationPojoPrototype()
				.setName("Annotation 1")
				.setCreatedByUserId("admin")
				.setState(WorkingState.IN_ANALYSIS)
				.setType(AnnotationType.RULE)
				.setModule(moduleId)
				.setLocation(new ModuleLocation(47, 11))
			);
	}
}

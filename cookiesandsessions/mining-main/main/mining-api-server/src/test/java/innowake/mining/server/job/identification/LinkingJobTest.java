/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.identification;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.job.LinkAnnotationToDataDictionaryJob;
import innowake.mining.server.job.LinkDataDictionaryToAnnotationJob;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.DefinedLocation;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;

/**
 * Linking job tests.
 */
@Disabled("Flaky (addresed in WMIN-11291)")
@WithMockUser
class LinkingJobTest extends AbstractIdentificationTest {

	@Autowired
	private JobManager jobManager;
	
	@Autowired
	private DataDictionaryService dataDictionaryService;
	
	@Autowired
	private AnnotationService annotationService;

	private EntityId moduleId = EntityId.VOID;
	private EntityId copyBookId = EntityId.VOID;
	
	@BeforeAll
	void init() {
		moduleId = createModule(PROJECT_ID_1, "MMRS71B1", "MMRS71B1.cbl", RESOURCE_PATH, Technology.COBOL, Type.PROGRAM);
		copyBookId = createCobolCopybook(PROJECT_ID_1, "MMRS71B", "MMRS71B.cpy", RESOURCE_PATH);
		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
												.setRelationship(RelationshipType.INCLUDES)
												.setSrcModule(moduleId)
												.setDstModule(copyBookId));
	}
	
	@Test
	void testLinkDataDictionaryToAnnotationJob() {
		createAnnotation(moduleId, "Annotation 1", 3488, 90, WorkingState.CANDIDATE, AnnotationType.RULE, "IDENTIFICATION");
		createAnnotation(moduleId, "Annotation 2", 8745, 214, WorkingState.CANDIDATE, AnnotationType.RULE, "IDENTIFICATION");

		final List<DataDictionaryPojo> dataDictionariesBeforeCreation = dataDictionaryService.find(q -> q.ofModule(moduleId));
		assertTrue(dataDictionariesBeforeCreation.isEmpty(), "dataDictionariesBeforeCreation should be empty since we did not created any");
		final DataDictionaryPojo manuallyCreatedDataDictionary = createFullDataDictionaryPojo("CICS-RESP", moduleId, 1984, 9,
				DefinedLocation.PROGRAM);
		assertNotNull(manuallyCreatedDataDictionary);
		final List<DataDictionaryPojo> dataDictionariesAfterCreation = dataDictionaryService.find(q -> q.ofModule(moduleId));
		assertEquals(1, dataDictionariesAfterCreation.size());
		assertTrue(dataDictionariesAfterCreation.get(0).getAnnotations().isEmpty(), "HasBusinessRules should be empty");
		
		submitJob(jobManager, new LinkDataDictionaryToAnnotationJob(manuallyCreatedDataDictionary.identity(), PROJECT_ID_1, moduleId));
		final List<DataDictionaryPojo> foundDd = dataDictionaryService.find(q -> q.ofModule(moduleId));
		assertEquals(1, foundDd.size());
		assertEquals(2, foundDd.get(0).getAnnotations().size());
	}
	
	@Test
	void testAnnotationToLinkDataDictionaryJob() {
		final DataDictionaryPojo manuallyCreatedDataDictionary = createFullDataDictionaryPojo("MY-TASK-NO", moduleId, 1579, 10,
				DefinedLocation.PROGRAM);
		assertNotNull(manuallyCreatedDataDictionary);
		
		final List<DataDictionaryPojo> dataDictionariesAfterCreation = dataDictionaryService.find(q -> q.ofModule(moduleId));
		assertEquals(1, dataDictionariesAfterCreation.size());
		
		assertTrue(dataDictionariesAfterCreation.get(0).getAnnotations().isEmpty(), "HasBusinessRules should be empty");
		
		final AnnotationPojo annotation = createAnnotation(moduleId, "Annotation", 8018, 72, WorkingState.CANDIDATE,
				AnnotationType.RULE, "IDENTIFICATION");
		assertNotNull(annotation);
		
		submitJob(jobManager, new LinkAnnotationToDataDictionaryJob(annotation.identity(), annotation.getLocation().orElseThrow(), PROJECT_ID_1, moduleId));
		
		final List<DataDictionaryPojo> dataDictionariesAfterJob = dataDictionaryService.find(q -> q.ofModule(moduleId));
		assertEquals(1, dataDictionariesAfterJob.size());
		final List<EntityId> ddAnnotations = dataDictionariesAfterJob.get(0).getAnnotations();
		assertEquals(1, ddAnnotations.size());
		assertEquals(annotation.identity(), ddAnnotations.get(0));
	}
	
	@Test
	void testLinkDataDictionaryToAnnotationJobForCopyBook() {
		final AnnotationPojo annotation = createAnnotation(moduleId, "New Annotation", 9233, 87, WorkingState.CANDIDATE,
				AnnotationType.RULE, "IDENTIFICATION");
		assertNotNull(annotation);
		
		final DataDictionaryPojo manuallyCreatedDataDictionary = createFullDataDictionaryPojo("USERIDI",  copyBookId, 459, 7,
				DefinedLocation.COPYBOOK);
		assertNotNull(manuallyCreatedDataDictionary);
		
		submitJob(jobManager, new LinkDataDictionaryToAnnotationJob(manuallyCreatedDataDictionary.identity(), PROJECT_ID_1, copyBookId));
		
		final List<DataDictionaryPojo> foundDd = dataDictionaryService.find(q -> q.ofModule(copyBookId));
		assertFalse(foundDd.isEmpty(), "foundDd must not be empty");
		final List<DataDictionaryPojo> actualEntry = foundDd.stream().filter(entry -> entry.getName().equals("USERIDI")).collect(Collectors.toList());
		assertEquals(1, actualEntry.size());
		assertFalse(actualEntry.get(0).getAnnotations().isEmpty(), "HasBusinessRules should not be empty");
		assertEquals(annotation.identity(), actualEntry.get(0).getAnnotations().get(0));
	}
	
	@Test
	void testLinkAnnotationToDataDictionaryJobForCopyBook() {
		final DataDictionaryPojo manuallyCreatedDataDictionary = createFullDataDictionaryPojo("PWTXTL", copyBookId, 493, 6,
				DefinedLocation.PROGRAM);
		assertNotNull(manuallyCreatedDataDictionary);
		
		final AnnotationPojo annotation = createAnnotation(copyBookId, "Annotation 3", 456, 59, WorkingState.CANDIDATE,
				AnnotationType.RULE, "IDENTIFICATION");
		assertNotNull(annotation);
		
		submitJob(jobManager, new LinkAnnotationToDataDictionaryJob(annotation.identity(), annotation.getLocation().orElseThrow(), PROJECT_ID_1, copyBookId));
		
		final List<DataDictionaryPojo> dataDictionariesAfterJob = dataDictionaryService.find(q -> q.ofModule(copyBookId));
		assertEquals(1, dataDictionariesAfterJob.size());
		final List<EntityId> ddAnnotations = dataDictionariesAfterJob.get(0).getAnnotations();
		assertEquals(1, ddAnnotations.size());
		assertEquals(annotation.identity(), ddAnnotations.get(0));
	}
	
	private AnnotationPojo createAnnotation(final EntityId moduleId, final String name, final int offset, final int length,
			final WorkingState state, final AnnotationType type, final String content) {
		final AnnotationPojoPrototype annotation = new AnnotationPojoPrototype();
		annotation.setName(name);
		annotation.setState(state);
		annotation.setType(type);
		annotation.setSourceAttachment(content);
		annotation.setCreatedByUserId("system_user");
		annotation.setUpdatedByUserId("admin");
		annotation.setLocation(new ModuleLocation(offset, length));
		annotation.setModule(moduleId);
		return annotationService.get(annotationService.create(annotation));
	}
	
	private DataDictionaryPojo createFullDataDictionaryPojo(final String dataElementName, final EntityId moduleId,
			final int offset, final int length, final DefinedLocation definedLocation) {
		final DataDictionaryPojoPrototype dataDictionaryEntry = new DataDictionaryPojoPrototype();
		dataDictionaryEntry.setName(dataElementName);
		dataDictionaryEntry.setDescription("MY description");
		dataDictionaryEntry.setFormat("PICX");
		dataDictionaryEntry.setCreatedByUserId("admin");
		dataDictionaryEntry.setCreatedByUserId("admin");
		dataDictionaryEntry.setDefinedLocation(definedLocation);
		dataDictionaryEntry.setIsBusiness(true);
		dataDictionaryEntry.setState(WorkingState.CANDIDATE);
		dataDictionaryEntry.setLocation(new ModuleLocation(offset, length));
		dataDictionaryEntry.setModule(moduleId);

		return dataDictionaryService.create(dataDictionaryEntry);
	}
}

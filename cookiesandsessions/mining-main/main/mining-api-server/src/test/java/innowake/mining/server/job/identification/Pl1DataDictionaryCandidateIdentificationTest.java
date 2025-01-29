/*
 * Copyright (c) 2023 Deloitte. All rights reserved..
 */
package innowake.mining.server.job.identification;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.collection.IsCollectionWithSize.hasSize;
import static org.hamcrest.collection.IsEmptyCollection.empty;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.junit.Before;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.job.LinkAnnotationToDataDictionaryJob;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.DefinedLocation;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;

/**
 * Data Dictionary Candidate Identification Job tests For PL1 Module.
 */
@WithMockUser
class Pl1DataDictionaryCandidateIdentificationTest extends AbstractIdentificationTest {
	
	@Autowired
	private DataDictionaryService dataDictionaryService;
	@Autowired
	private AnnotationService annotationService;
	@Autowired
	private JobManager jobManager;

	/**
	 * Creates the test data as following:
	 * <ul>
	 * <li>Creates a new project</li>
	 * <li>Creates new {@link Module Modules} in the new project</li>
	 * </ul>
	 */
	@BeforeAll
	void init() {
		createModule(PROJECT_ID_1, "ADD007A", "ADD007A.pl1m", RESOURCE_PATH_PL1, Technology.PL1, Type.PROGRAM);
		createModule(PROJECT_ID_1, "ADD012A", "ADD012A.pl1", RESOURCE_PATH_PL1, Technology.PL1, Type.PROGRAM);
		createModule(PROJECT_ID_1, "BAS001B", "BAS001B.pcpy", RESOURCE_PATH_PL1, Technology.PL1, Type.COPYBOOK);
		createModule(PROJECT_ID_1, "BrBranch", "BrBranch.pl1", RESOURCE_PATH_PL1, Technology.PL1, Type.PROGRAM);
		createModule(PROJECT_ID_1, "BeginLocation", "BeginLocation.pl1", RESOURCE_PATH_PL1, Technology.PL1, Type.PROGRAM);
		createModule(PROJECT_ID_1, "packageLocation", "packageLocation.pl1", RESOURCE_PATH_PL1, Technology.PL1, Type.PROGRAM);
		createModule(PROJECT_ID_1, "DataFormat", "DataFormat.pl1", RESOURCE_PATH_PL1, Technology.PL1, Type.PROGRAM);
	}
	
	@Before
	void resetData() {
		try {
			resetTestData();
		} catch (final IOException e) {
			e.printStackTrace();
		}
	}
	
	@Test
	void identifyDataDictionaryCandidatesInPL1mModule() {
		final ModuleLightweightPojo module = moduleService.findAnyModuleLightweight(
				q -> q.ofProject(PROJECT_ID_1).withPath(RESOURCE_PATH_PL1 + "ADD007A.pl1m")).orElseThrow();
		final EntityId moduleId = module.identity();
		final List<DataDictionaryPojo> dataDictionariesBeforeIdentification = dataDictionaryService.find(q -> q.ofModule(moduleId));
		assertThat(dataDictionariesBeforeIdentification, empty());

		/* Run the Candidate Identification on the PL1m Module */
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, RESOURCE_PATH_PL1 + "ADD007A.pl1m");

		/* Verify that the PL1m Module has the data dictionaries associated */
		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModule(moduleId));
		assertThat(dataDictionaries, hasSize(4));
		final List<String> dataElements = dataDictionaries.stream().map(DataDictionaryPojo::getName).collect(Collectors.toList());
		assertTrue("Identified data dictinary should contain GR_1", dataElements.contains("GR_1"));
		assertTrue("Identified data dictinary should contain FD_10", dataElements.contains("FD_10"));
		assertTrue("Identified data dictinary should contain FD_11", dataElements.contains("FD_11"));
		assertTrue("Identified data dictinary should contain FD_12", dataElements.contains("FD_12"));
		dataDictionaries.stream().forEach(entry -> {
			assertEquals(DefinedLocation.PROCEDURE, entry.getDefinedLocation().get());
		});
		assertEquals(0, dataDictionaries.get(0).getLength().orElseThrow(), "GR_1 should have Length 2");
		assertEquals(2, dataDictionaries.get(1).getLength().orElseThrow(), "FD_10 should have Length 2");
		assertEquals(7, dataDictionaries.get(2).getLength().orElseThrow(), "FD_11 should have Length 2");
		assertEquals(7, dataDictionaries.get(3).getLength().orElseThrow(), "FD_12 should have Length 2");
		assertTrue(dataDictionaries.stream().map(DataDictionaryPojo::getFormat).flatMap(Optional::stream)
				.collect(Collectors.toList()).containsAll(Arrays.asList("BINARY", "DECIMAL")));
	}
	
	@Test
	void identifyDataDictionaryCandidatesInPL1Module() {
		final ModuleLightweightPojo module = moduleService.findAnyModuleLightweight(
				q -> q.ofProject(PROJECT_ID_1).withPath(RESOURCE_PATH_PL1 + "BrBranch.pl1")).orElseThrow();
		final EntityId moduleId = module.identity();
		final List<DataDictionaryPojo> dataDictionariesBeforeIdentification = dataDictionaryService.find(q -> q.ofModule(moduleId));
		assertThat(dataDictionariesBeforeIdentification, empty());

		/* Run the Candidate Identification on the PL1 Module */
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, RESOURCE_PATH_PL1 + "BrBranch.pl1");

		/* Verify that the PL1 Module has the data dictionaries associated */
		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModule(moduleId));
		assertThat(dataDictionaries, hasSize(4));
		final List<String> dataElements = dataDictionaries.stream().map(DataDictionaryPojo::getName).collect(Collectors.toList());
		assertTrue("Identified data dictinary should contain U, V, C1, C2", dataElements.containsAll(Arrays.asList("U", "V", "C1", "C2")));
		dataDictionaries.stream().forEach(entry -> {
			assertEquals(DefinedLocation.PROCEDURE, entry.getDefinedLocation().get());
		});
		assertEquals(2, dataDictionaries.get(0).getLength().orElseThrow(), "U should have Length 2");
		assertEquals(2, dataDictionaries.get(1).getLength().orElseThrow(), "V should have Length 2");
		assertEquals(1, dataDictionaries.get(2).getLength().orElseThrow(), "C1 should have Length 2");
		assertEquals(1, dataDictionaries.get(3).getLength().orElseThrow(), "C2 should have Length 2");
		assertTrue(dataDictionaries.stream().map(DataDictionaryPojo::getFormat).flatMap(Optional::stream)
				.collect(Collectors.toList()).containsAll(Arrays.asList("BINARY", "BIN")));
	}
	
	@Test
	void testIdentifyDataDictionaryForCopyBookModule() {
		final ModuleLightweightPojo module = moduleService.findAnyModuleLightweight(
				q -> q.ofProject(PROJECT_ID_1).withPath(RESOURCE_PATH_PL1 + "ADD012A.pl1")).orElseThrow();
		final EntityId moduleId = module.identity();
		final ModuleLightweightPojo cpy = moduleService.findAnyModuleLightweight(
				q -> q.ofProject(PROJECT_ID_1).withPath(RESOURCE_PATH_PL1 + "BAS001B.pcpy")).orElseThrow();
		final EntityId copyBookId = cpy.identity();
		createReference(RelationshipType.INCLUDES, moduleId, copyBookId);
		final List<DataDictionaryPojo> dataDictionariesBeforeIdentification = dataDictionaryService.find(q -> q.ofModule(moduleId));
		assertThat(dataDictionariesBeforeIdentification, empty());

		/* Run the Candidate Identification on the PL1 Module */
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, RESOURCE_PATH_PL1 + "ADD012A.pl1");
		final var includes = new ArrayList<>(moduleService.findRelatedModules(moduleId, RelationshipType.INCLUDES, RelationshipDirection.OUT));
		includes.add(moduleId.getUid());
		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModuleUuids(includes));
		assertThat(dataDictionaries, hasSize(34));
		final List<String> dataElements = dataDictionaries.stream().map(DataDictionaryPojo::getName).collect(Collectors.toList());
		assertEquals(34, dataElements.size());
		final List<String> format = dataDictionaries.stream().map(DataDictionaryPojo::getFormat).flatMap(Optional::stream).collect(Collectors.toList());
		assertTrue("Identified data dictionary should have format contain DECIMAL, BINARY, POINTER, CHAR",
				format.containsAll(Arrays.asList("DECIMAL", "BINARY", "POINTER", "CHAR")));
	}
	
	@Test
	void testIdentifyBusinessVariablesInPl1CopyBook() {
		final EntityId moduleId = createModule(PROJECT_ID_1, "P1File1", "PlFile1.pl1", RESOURCE_PATH_PL1, Technology.PL1, Type.PROGRAM);
		final EntityId copyId = createModule(PROJECT_ID_1, "ADD", "ADD.pl1", RESOURCE_PATH_PL1, Technology.PL1, Type.COPYBOOK);
		createReference(RelationshipType.INCLUDES, moduleId, copyId);
		assertEquals(0, dataDictionaryService.find(q -> q.ofModule(moduleId)).size());

		/* Run the Data Dictionary Candidate Identification on the Pl1 Program Module */
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);
		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModule(moduleId));
		assertEquals(8, dataDictionaries.size());
		dataDictionaries.stream().forEach(entry -> {
			assertEquals(DefinedLocation.PROCEDURE, entry.getDefinedLocation().orElseThrow());
		});
		final List<String> format = dataDictionaries.stream().map(DataDictionaryPojo::getFormat).flatMap(Optional::stream).collect(Collectors.toList());
		assertTrue("Identified data dictionary should have format contain DEC", format.containsAll(Arrays.asList("DEC")));

		createAnnotation(moduleId, 400, 23);
		createAnnotation(moduleId, 168, 13);
		createAnnotation(moduleId, 206, 13);

		final List<DataDictionaryPojo> businessVariables = dataDictionaries.stream()
				.filter(dde -> ! annotationService.find(q -> q.ofProject(PROJECT_ID_1).ofDataDictionaryEntry(dde.identity())).isEmpty())
				.collect(Collectors.toList());

		assertEquals(6, businessVariables.size());
		assertTrue("Should contain WSNUM1, WSNUM3, WSNUME, ABCD, ABC, AB", businessVariables.stream().map(DataDictionaryPojo::getName)
				.collect(Collectors.toList()).containsAll(Arrays.asList("WSNUM1", "WSNUM3", "WSNUME", "ABCD", "ABC", "AB")));

		final List<DataDictionaryPojo> dataDictionariesCopy = dataDictionaryService.find(q -> q.ofModule(copyId));
		assertEquals(3, dataDictionariesCopy.size());
		assertTrue("Should contain FLD_1, FLD_2, FXD_1", dataDictionariesCopy.stream().map(DataDictionaryPojo::getName)
				.collect(Collectors.toList()).containsAll(Arrays.asList("FLD_1", "FLD_2", "FXD_1")));
		dataDictionaries.stream().forEach(entry -> {
			assertEquals(DefinedLocation.PROCEDURE, entry.getDefinedLocation().orElseThrow());
		});
		final List<String> formatCopy = dataDictionariesCopy.stream().map(DataDictionaryPojo::getFormat).flatMap(Optional::stream).collect(Collectors.toList());
		assertTrue("Identified data dictionary should have format contain DECIMAL", formatCopy.containsAll(Arrays.asList("DECIMAL")));
	}
	
	@Test
	void testIdentifyDataDictionaryDefinedLocation() {
		final ModuleLightweightPojo module = moduleService.findAnyModuleLightweight(
				q -> q.ofProject(PROJECT_ID_1).withPath(RESOURCE_PATH_PL1 + "packageLocation.pl1")).orElseThrow();
		final EntityId moduleId = module.identity();
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, RESOURCE_PATH_PL1 + "packageLocation.pl1");
		/* Verify that associated data dictionaries defined location is Package */
		List<DataDictionaryPojo> dictionaries = dataDictionaryService.find(q -> q.ofModule(moduleId));
		assertThat(dictionaries, hasSize(4));
		dictionaries.stream().forEach(entry -> {
			assertEquals(DefinedLocation.PACKAGE, entry.getDefinedLocation().orElseThrow());
		});

		final ModuleLightweightPojo module2 = moduleService.findAnyModuleLightweight(
				q -> q.ofProject(PROJECT_ID_1).withPath(RESOURCE_PATH_PL1 + "BeginLocation.pl1")).orElseThrow();
		final EntityId moduleId2 = module2.identity();
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, RESOURCE_PATH_PL1 + "BeginLocation.pl1");
		/* Verify that associated 3 data dictionaries defined location is Procedure and 2 is Begin */
		dictionaries = dataDictionaryService.find(q -> q.ofModule(moduleId2));
		assertThat(dictionaries, hasSize(5));
		assertTrue(dictionaries.stream().filter(entry -> entry.getDefinedLocation().orElse(null) == DefinedLocation.PROCEDURE)
				.map(DataDictionaryPojo::getName).collect(Collectors.toList()).containsAll(Arrays.asList("RESULT", "X", "Z")));
		
		assertTrue(dictionaries.stream().filter(entry -> entry.getDefinedLocation().orElse(null) == DefinedLocation.BEGIN)
				.map(DataDictionaryPojo::getName).collect(Collectors.toList()).containsAll(Arrays.asList("A", "B")));
	}
	
	@Test
	void testIdentifyDataDictionaryFormat() {
		final ModuleLightweightPojo module = moduleService.findAnyModuleLightweight(
				q -> q.ofProject(PROJECT_ID_1).withPath(RESOURCE_PATH_PL1 + "DataFormat.pl1")).orElseThrow();
		final EntityId moduleId = module.identity();
		final List<DataDictionaryPojo> dataDictionariesBeforeIdentification = dataDictionaryService.find(q -> q.ofModule(moduleId));
		assertThat(dataDictionariesBeforeIdentification, empty());

		/* Run the Candidate Identification on the PL1 Module */
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, RESOURCE_PATH_PL1 + "DataFormat.pl1");
		final var includes = new ArrayList<>(moduleService.findRelatedModules(moduleId, RelationshipType.INCLUDES, RelationshipDirection.OUT));
		includes.add(moduleId.getUid());
		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModuleUuids(includes));
		assertThat(dataDictionaries, hasSize(4));
		final List<String> format = dataDictionaries.stream().map(DataDictionaryPojo::getFormat).flatMap(Optional::stream).collect(Collectors.toList());
		assertTrue("Identified data dictionary should have format contain DECIMAL, BINARY, BIT, CHAR",
				format.containsAll(Arrays.asList("DECIMAL", "BINARY", "BIT", "CHAR")));	
	}
	
	@Test
	void testidentifyDataDictionariesAreIdentifiedInCorrectLocation() {
		final String fileName = "P4030G.pl1m";
		final EntityId moduleId = createModule(PROJECT_ID_1, "P4030G", fileName, RESOURCE_PATH_PL1, Technology.PL1, Type.PROGRAM);
		final List<DataDictionaryPojo> dataDictionariesBeforeIdentification = dataDictionaryService.find(q -> q.ofModule(moduleId));
		assertThat(dataDictionariesBeforeIdentification, empty());
		
		/* Run the Data Dictionary Candidate Identification on the PL1 Program Module */
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, RESOURCE_PATH_PL1 + fileName);
		final var includes = new ArrayList<>(moduleService.findRelatedModules(moduleId, RelationshipType.INCLUDES, RelationshipDirection.OUT));
		includes.add(moduleId.getUid());
		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModuleUuids(includes));
		assertThat(dataDictionaries, hasSize(6));
		
		final DataDictionaryPojo testDde = dataDictionaries.stream().filter(d -> d.getName().equals("DET_SEG_IO_AREA")).findAny().orElseThrow();
		assertEquals(new ModuleLocation(208, 15), testDde.getLocation().orElseThrow());
	}
	
	protected void submitIdentifyDataDictionaryCandidatesJob(final EntityId projectId, final EntityId moduleId) {
		submitJob(jobManager, new IdentifyCandidatesJob(projectId, new ModuleMatcher(Arrays.asList(moduleId), Collections.emptyList())));
	}

	protected void submitIdentifyDataDictionaryCandidatesJob(final EntityId projectId, final String path) {
		submitJob(jobManager, new IdentifyCandidatesJob(projectId, new ModuleMatcher(Collections.emptyList(), Arrays.asList(path))));
	}
	
	private AnnotationPojo createAnnotation(final EntityId moduleId, final int offset, final int length) {
		final AnnotationPojoPrototype annotation = new AnnotationPojoPrototype();
		annotation.setState(WorkingState.CANDIDATE);
		annotation.setType(AnnotationType.RULE);
		annotation.setName("Business Rule [System Identified]");
		annotation.setCreatedByUserId("");
		annotation.setModule(moduleId);
		annotation.setLocation(new ModuleLocation(offset, length));

		final AnnotationPojo pojo = annotationService.get(annotationService.create(annotation));

		submitJob(jobManager, new LinkAnnotationToDataDictionaryJob(pojo.identity(), pojo.getLocation().orElseThrow(), PROJECT_ID_1, moduleId));

		return pojo;
	}
	
}

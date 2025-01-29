/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.identification;

import static innowake.mining.shared.model.DatabaseAccessType.UPDATE;
import static innowake.mining.shared.model.ReferenceAttributes.DB_ACCESS_TYPES;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.READ;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.UI;
import static innowake.mining.shared.model.TechnicalTaxonomies.TypeName.DB_ACCESS;
import static innowake.mining.shared.model.TechnicalTaxonomies.TypeName.FILE_ACCESS;
import static innowake.mining.shared.model.TechnicalTaxonomies.TypeName.PROGRAM_TYPE;
import static innowake.mining.shared.model.Technology.BASIC;
import static innowake.mining.shared.model.Technology.COBOL;
import static innowake.mining.shared.model.Technology.C;
import static innowake.mining.shared.model.Technology.JCL;
import static innowake.mining.shared.model.Technology.NATURAL;
import static innowake.mining.shared.model.Technology.VMS;
import static innowake.mining.shared.model.Type.DCL;
import static innowake.mining.shared.model.Type.FUNCTION;
import static innowake.mining.shared.model.Type.JOB;
import static innowake.mining.shared.model.Type.OBJECT;
import static innowake.mining.shared.model.Type.PROGRAM;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.collection.IsCollectionWithSize.hasSize;
import static org.hamcrest.collection.IsEmptyCollection.empty;
import static org.hamcrest.core.IsEqual.equalTo;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.TechnicalTaxonomies;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;

/**
 * Integration tests for the Taxonomy identification functions.
 */
@WithMockUser
class TaxonomyIdentificationIntegrationTest extends AbstractIdentificationTest {
	
	private static final String IRRELEVANT_CONTENT = "IRRELEVANT CONTENT";
	private static final Long LONG_ONE = 1L;
	private static final Long LONG_THREE = 3L;
	
	@Autowired
	private JobManager jobManager;
	
	@Autowired
	private TaxonomyService taxonomyService;

	
	@Test
	void naturalProgramWithoutPropertiesDoesNotThrowNullPointerException() {
		final EntityId moduleId = createModule(PROJECT_ID_1, "WMIN2734", "WMIN2734.nsp", RESOURCE_PATH, NATURAL, PROGRAM);

		final List<TaxonomyPojo> taxonomiesBeforeIdentification = taxonomyService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleId));
		assertThat(taxonomiesBeforeIdentification, empty());

		/* Run the Technical Taxonomy Identification on the Natural Module */
		final IdentifyTechnicalTaxonomiesJob job = new IdentifyTechnicalTaxonomiesJob(PROJECT_ID_1, new ModuleMatcher(Arrays.asList(moduleId), Collections.emptyList()));
		submitJob(jobManager, job);
		
		/* Verify that the Natural Module has the UI Taxonomy associated */
		final List<TaxonomyPojo> taxonomies = taxonomyService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleId));
		assertThat(taxonomies, hasSize(1));
		final TaxonomyPojo taxonomy = taxonomies.get(0);
		assertThat(taxonomy.getName(), equalTo(UI.name()));
		assertThat(taxonomy.getType().getName(), equalTo(PROGRAM_TYPE.getDisplayName()));
	}
	
	@Test
	void testIdentifiedTaxonomyAlreadyAssociatedWithModule() {
		/* Check what happens when edge already exists between module and taxonomy */
		final EntityId moduleId = createFileSection(PROJECT_ID_3, RESOURCE_PATH, "WMIN538UI1.cbl", getContent("WMIN538UI1.cbl"), Technology.COBOL, Type.PROGRAM);
		final List<String> modulePaths = Collections.singletonList(RESOURCE_PATH + "WMIN538UI1.cbl");
		
		/* Run the Technical Taxonomy Identification on the Cobol Module */
		final IdentifyTechnicalTaxonomiesJob job = new IdentifyTechnicalTaxonomiesJob(PROJECT_ID_3, new ModuleMatcher(Arrays.asList(moduleId), Collections.emptyList()));
		submitJob(jobManager, job);
		
		final List<TaxonomyPojo> taxonomies = taxonomyService.find(q -> q.ofProject(PROJECT_ID_3).ofModule(moduleId));
		assertEquals(LONG_ONE, taxonomies.size());
		
		/* Re-run the Technical Taxonomy Identification */
		final IdentifyTechnicalTaxonomiesJob job2 = new IdentifyTechnicalTaxonomiesJob(PROJECT_ID_3, new ModuleMatcher(Collections.emptyList(), modulePaths));
		submitJob(jobManager, job2);
		final List<TaxonomyPojo> taxonomies2 = taxonomyService.find(q -> q.ofProject(PROJECT_ID_3).ofModule(moduleId));
		assertEquals(LONG_ONE, taxonomies2.size());
	}
	
	@Test
	void testIdentifiedTaxonomyAlreadyPresentInTheDatabase() {
		/* check what happens when Taxonomy already exists */
		final EntityId moduleIdA = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "WMIN538UI1.cbl", getContent("WMIN538UI1.cbl"), Technology.COBOL, Type.PROGRAM);
		final EntityId moduleIdB = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "WMIN538UI2.cbl", getContent("WMIN538UI2.cbl"), Technology.COBOL, Type.PROGRAM);

		/* Run the Technical Taxonomy Identification on the Cobol Module */
		final IdentifyTechnicalTaxonomiesJob jobA = new IdentifyTechnicalTaxonomiesJob(PROJECT_ID_1, new ModuleMatcher(Arrays.asList(moduleIdA), Collections.emptyList()));
		submitJob(jobManager, jobA);
		
		final List<TaxonomyPojo> taxonomiesA = taxonomyService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleIdA));
		assertEquals(LONG_ONE, taxonomiesA.size());
		
		/* Run the Technical Taxonomy Identification on the Cobol Module */
		final IdentifyTechnicalTaxonomiesJob jobB = new IdentifyTechnicalTaxonomiesJob(PROJECT_ID_1, new ModuleMatcher(Arrays.asList(moduleIdB), Collections.emptyList()));
		submitJob(jobManager, jobB);
		
		final List<TaxonomyPojo> taxonomiesB = taxonomyService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleIdB));
		assertEquals(LONG_ONE, taxonomiesB.size());
	}
	
	@Test
	void testUiProgramIdentification() {
		final EntityId moduleId = createFileSection(PROJECT_ID_2, RESOURCE_PATH, "WMIN538UI1.cbl", getContent("WMIN538UI1.cbl"), Technology.COBOL, Type.PROGRAM);

		/* Run the Technical Taxonomy Identification on the Cobol Module */
		final IdentifyTechnicalTaxonomiesJob job = new IdentifyTechnicalTaxonomiesJob(PROJECT_ID_2, new ModuleMatcher(Arrays.asList(moduleId), Collections.emptyList()));
		submitJob(jobManager, job);
		
		/* Verify that the Cobol Module has the UI Taxonomy associated */
		final List<TaxonomyPojo> taxonomies = taxonomyService.find(q -> q.ofProject(PROJECT_ID_2).ofModule(moduleId));
		assertEquals(LONG_ONE, taxonomies.size());
		final TaxonomyPojo taxonomy = taxonomies.get(0);
		assertEquals(UI.name(), taxonomy.getName());
		assertEquals(PROGRAM_TYPE.getDisplayName(), taxonomy.getType().getName());
	}
	
	@Test
	void testFileAccessAndLibraryProgramIdentification() {
		final EntityId moduleId = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "WMIN539FILE1.cbl", getContent("WMIN539FILE1.cbl"), Technology.COBOL, Type.PROGRAM);

		/* Run the Technical Taxonomy Identification on the Cobol Module */
		final IdentifyTechnicalTaxonomiesJob job = new IdentifyTechnicalTaxonomiesJob(PROJECT_ID_1, new ModuleMatcher(Arrays.asList(moduleId), Collections.emptyList()));
		submitJob(jobManager, job);
		
		final List<TechnicalTaxonomies.Name> expectedTaxonomiesNames = new ArrayList<>();
		final List<TechnicalTaxonomies.TypeName> expectedTypeNames = new ArrayList<>();
		
		expectedTaxonomiesNames.add(TechnicalTaxonomies.Name.WRITE);
		expectedTypeNames.add(TechnicalTaxonomies.TypeName.FILE_ACCESS);
		
		expectedTaxonomiesNames.add(TechnicalTaxonomies.Name.READ);
		expectedTypeNames.add(TechnicalTaxonomies.TypeName.FILE_ACCESS);
		
		expectedTaxonomiesNames.add(TechnicalTaxonomies.Name.LIBRARY);
		expectedTypeNames.add(TechnicalTaxonomies.TypeName.PROGRAM_TYPE);
		
		assertTaxonomies(expectedTaxonomiesNames, expectedTypeNames, taxonomyService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleId)));
		
	}

	static void assertTaxonomies(final List<TechnicalTaxonomies.Name> expectedTaxonomiesNames,
								 final List<TechnicalTaxonomies.TypeName> expectedTypeNames,
								 final List<TaxonomyPojo> actualTaxonomies) {
		assertEquals(expectedTaxonomiesNames.size(), actualTaxonomies.size());
		assertEquals(expectedTypeNames.size(), actualTaxonomies.size());

		final Map<String, TaxonomyPojo> taxonomies = actualTaxonomies.stream().collect(Collectors.toMap(taxonomy -> taxonomy.getName(), taxonomy -> taxonomy));
		
		for (int i = 0; i < expectedTypeNames.size(); i++) {
			final TaxonomyPojo taxonomy = taxonomies.get(expectedTaxonomiesNames.get(i).getDisplayName());
			Assertions.assertNotNull(taxonomy);

			assertEquals(expectedTypeNames.get(i).getDisplayName(), taxonomy.getType().getName());
		}
		
	}
	
	@Test
	void addIdentifiedTaxonomyToRootModuleEvenIfInformationComesFromCopybook() {
		/* check that taxonomy is added to the root module even though the information is coming from a copybook */
		final EntityId moduleIdA = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "WMIN538UI3.cbl", getContent("WMIN538UI3.cbl"), Technology.COBOL, Type.PROGRAM);
		final EntityId moduleIdB = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "WMIN538UI4.cpy", getContent("WMIN538UI4.cpy"), Technology.COBOL, Type.COPYBOOK);
		
		createReference(RelationshipType.INCLUDES, moduleIdA, moduleIdB);
		
		/* Run the Technical Taxonomy Identification on the Cobol Module */
		final IdentifyTechnicalTaxonomiesJob job = new IdentifyTechnicalTaxonomiesJob(PROJECT_ID_1, new ModuleMatcher(Arrays.asList(moduleIdA), Collections.emptyList()));
		submitJob(jobManager, job);
		
		final List<TaxonomyPojo> taxonomies = taxonomyService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleIdA));
		assertEquals(LONG_ONE, taxonomies.size());
	}
	
	@Test
	void testBatchProgramIdentification() {
		final EntityId moduleIdA = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "WMIN542A.cbl", IRRELEVANT_CONTENT, COBOL, PROGRAM);
		final EntityId moduleIdB = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "WMIN542B.job", IRRELEVANT_CONTENT, JCL, JOB);
		
		createReference(RelationshipType.CALLS, moduleIdA, moduleIdB);
		
		/* Run the Technical Taxonomy Identification on the Cobol Module */
		final IdentifyTechnicalTaxonomiesJob job = new IdentifyTechnicalTaxonomiesJob(PROJECT_ID_1, new ModuleMatcher(Arrays.asList(moduleIdA), Collections.emptyList()));
		submitJob(jobManager, job);
		
		final List<TaxonomyPojo> taxonomies = taxonomyService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleIdA));
		assertEquals(LONG_ONE, taxonomies.size());	
	}
	
	@Test
	void testBatchProgramIdentificationForBasic() {
		final EntityId moduleIdA = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "WMIN846A.bas", IRRELEVANT_CONTENT, BASIC, OBJECT);
		final EntityId moduleIdB = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "WMIN846B.com", IRRELEVANT_CONTENT, VMS, DCL);
		final EntityId moduleIdC = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "WMIN846C.bas", IRRELEVANT_CONTENT, BASIC, PROGRAM);
		
		createReference(RelationshipType.CALLS, moduleIdB, moduleIdC);
		createReference(RelationshipType.REFERENCES, moduleIdA, moduleIdC);
		
		/* Run the Technical Taxonomy Identification on the Basic Module */
		final IdentifyTechnicalTaxonomiesJob job = new IdentifyTechnicalTaxonomiesJob(PROJECT_ID_1, new ModuleMatcher(Arrays.asList(moduleIdA), Collections.emptyList()));
		submitJob(jobManager, job);
		
		final List<TaxonomyPojo> taxonomies = taxonomyService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleIdC));
		assertEquals(LONG_ONE, taxonomies.size());
		final TaxonomyPojo taxonomy = taxonomies.get(0);
		assertEquals(TechnicalTaxonomies.Name.BATCH.getDisplayName(), taxonomy.getName());
		assertEquals(PROGRAM_TYPE.getDisplayName(), taxonomy.getType().getName());
	}
	
	@Test
	void testUiProgramIdentificationForBasicAndCobolWithIfdl() {
		final EntityId basicFunctionId = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "BASICFUNCTION.bas", IRRELEVANT_CONTENT, BASIC, FUNCTION);
		final EntityId ifdlModuleId = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "IFDLFORM", IRRELEVANT_CONTENT, Technology.VMS, Type.IFDL_FORM);
		final EntityId cobolModuleId = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "WMIN846D.cbl", IRRELEVANT_CONTENT, COBOL, PROGRAM);
		final EntityId formEnableId = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "FORMS$ENABLE", IRRELEVANT_CONTENT, Technology.NONE, Type.UNKNOWN);
		final EntityId basicObjectId = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "BASICOBJECT.bas", IRRELEVANT_CONTENT, BASIC, OBJECT);
		
		createReference(RelationshipType.CALLS, ifdlModuleId, basicFunctionId);
		createReference(RelationshipType.CALLS, cobolModuleId, formEnableId);
		createReference(RelationshipType.REFERENCES, basicObjectId, basicFunctionId);
		
		/* Run the Technical Taxonomy Identification on the Cobol Module */
		final IdentifyTechnicalTaxonomiesJob job = new IdentifyTechnicalTaxonomiesJob(PROJECT_ID_1, new ModuleMatcher(Arrays.asList(cobolModuleId), Collections.emptyList()));
		submitJob(jobManager, job);
		
		final List<TaxonomyPojo> taxonomies = taxonomyService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(cobolModuleId));
		assertEquals(LONG_ONE, taxonomies.size());
		final TaxonomyPojo taxonomy = taxonomies.get(0);
		assertEquals(UI.name(),taxonomy.getName());
		assertEquals(PROGRAM_TYPE.getDisplayName(), taxonomy.getType().getName());
		
		final List<String> modulePaths2 = Collections.singletonList(RESOURCE_PATH + "BASICOBJECT.bas");
		/* Run the Technical Taxonomy Identification on the Basic Module */
		final IdentifyTechnicalTaxonomiesJob job2 = new IdentifyTechnicalTaxonomiesJob(PROJECT_ID_1, new ModuleMatcher(Collections.emptyList(), modulePaths2));
		submitJob(jobManager, job2);
		
		final List<TaxonomyPojo> taxonomies2 = taxonomyService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(basicFunctionId));
		assertEquals(LONG_ONE, taxonomies2.size());
		final TaxonomyPojo taxonomy2 = taxonomies2.get(0);
		assertEquals(UI.name(), taxonomy2.getName());
		assertEquals(PROGRAM_TYPE.getDisplayName(), taxonomy2.getType().getName());
	}
	
	@Test
	void testUiProgramIdentificationForCobolAndBasicWithIcscrget() {
		final EntityId cobolModuleId = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "WMIN846F.cbl", IRRELEVANT_CONTENT, COBOL, PROGRAM);
		final EntityId basicModuleId = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "WMIN4729A.bas", IRRELEVANT_CONTENT, BASIC, OBJECT);
		final EntityId basicProgramModuleId = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "WMIN4729.bas", IRRELEVANT_CONTENT, BASIC, PROGRAM);
		final EntityId icscrgetModuleId = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "ICSCRGET", IRRELEVANT_CONTENT, Technology.NONE, Type.UNKNOWN);
		
		createReference(RelationshipType.REFERENCES, basicModuleId, basicProgramModuleId);
		createReference(RelationshipType.CALLS, basicProgramModuleId, icscrgetModuleId);
		
		createReference(RelationshipType.CALLS, cobolModuleId, icscrgetModuleId);
		
		final List<String> modulePaths = Collections.singletonList(RESOURCE_PATH + "WMIN846F.cbl");
		/* Run the Technical Taxonomy Identification on the Cobol Module */
		final IdentifyTechnicalTaxonomiesJob job = new IdentifyTechnicalTaxonomiesJob(PROJECT_ID_1, new ModuleMatcher(Collections.emptyList(), modulePaths));
		submitJob(jobManager, job);
		
		final List<TaxonomyPojo> taxonomies = taxonomyService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(cobolModuleId));
		assertEquals(LONG_ONE, taxonomies.size());
		final TaxonomyPojo taxonomy = taxonomies.get(0);
		assertEquals(UI.name(),taxonomy.getName());
		assertEquals(PROGRAM_TYPE.getDisplayName(), taxonomy.getType().getName());
		
		final List<String> modulePaths2 = Collections.singletonList(RESOURCE_PATH + "WMIN4729A.bas");
		/* Run the Technical Taxonomy Identification on the Basic Module */
		final IdentifyTechnicalTaxonomiesJob job2 = new IdentifyTechnicalTaxonomiesJob(PROJECT_ID_1, new ModuleMatcher(Collections.emptyList(), modulePaths2));
		submitJob(jobManager, job2);
		
		final List<TaxonomyPojo> taxonomies2 = taxonomyService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(basicProgramModuleId));
		assertEquals(LONG_ONE, taxonomies2.size());
		final TaxonomyPojo taxonomy2 = taxonomies2.get(0);
		assertEquals(UI.name(), taxonomy2.getName());
		assertEquals(PROGRAM_TYPE.getDisplayName(), taxonomy2.getType().getName());
	}
	
	@Test
	void testMqProgramIdentification() {
		final EntityId moduleIdA = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "WMIN541A.cbl", IRRELEVANT_CONTENT, COBOL, PROGRAM);
		/* The actual type of the Module is irrelevant, as long as the name matches one of the predefined MQ programs */
		final EntityId moduleIdB = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "MQSET.cbl", IRRELEVANT_CONTENT, COBOL, PROGRAM);
		createReference(RelationshipType.CALLS, moduleIdA, moduleIdB);
		
		/* Run the Technical Taxonomy Identification on the Cobol Module */
		final IdentifyTechnicalTaxonomiesJob job = new IdentifyTechnicalTaxonomiesJob(PROJECT_ID_1, new ModuleMatcher(Arrays.asList(moduleIdA), Collections.emptyList()));
		submitJob(jobManager, job);
		
		final List<TaxonomyPojo> taxonomies = taxonomyService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleIdA));
		assertEquals(LONG_ONE, taxonomies.size());
	}
	
	@Test
	void testDbAccessAndLibraryModuleIdentification() {
		final EntityId moduleIdA = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "WMIN544A.cbl", IRRELEVANT_CONTENT, COBOL, PROGRAM);
		/* The actual type of the Module is irrelevant, as long as the properties in the ReadsWrites edge are correct */
		final EntityId moduleIdB = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "MYTABLE.table", IRRELEVANT_CONTENT, COBOL, PROGRAM);
		final Map<String, Object> properties = new HashMap<>();
		properties.put(DB_ACCESS_TYPES.getReferenceAttributeExcelName(), String.format("%s,%s", READ.name(), UPDATE.name()));
		final var reference = new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.ACCESSES)
				.setSrcModule(moduleIdA)
				.setDstModule(moduleIdB)
				.setProperties(properties);
		moduleService.createRelationship(reference);

		/* Run the Technical Taxonomy Identification on the Cobol Module */
		final IdentifyTechnicalTaxonomiesJob job = new IdentifyTechnicalTaxonomiesJob(PROJECT_ID_1, new ModuleMatcher(Arrays.asList(moduleIdA), Collections.emptyList()));
		submitJob(jobManager, job);
		
		final List<TaxonomyPojo> taxonomies = taxonomyService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleIdA));
		assertEquals(LONG_THREE, taxonomies.size());
	}
	
	@Test
	void testFileAccessIdentificationForBasic() {
		final EntityId moduleIdA = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "WMIN846FILE1.bas", getContent("WMIN846FILE1.bas"), Technology.BASIC, Type.OBJECT);
		
		/* Run the Technical Taxonomy Identification on the Basic Module */
		final IdentifyTechnicalTaxonomiesJob job = new IdentifyTechnicalTaxonomiesJob(PROJECT_ID_1, new ModuleMatcher(Arrays.asList(moduleIdA), Collections.emptyList()));
		submitJob(jobManager, job);
		
		final List<TechnicalTaxonomies.Name> expectedTaxonomiesNames = new ArrayList<>();
		final List<TechnicalTaxonomies.TypeName> expectedTypeNames = new ArrayList<>();
		
		expectedTaxonomiesNames.add(TechnicalTaxonomies.Name.WRITE);
		expectedTypeNames.add(TechnicalTaxonomies.TypeName.FILE_ACCESS);
		
		expectedTaxonomiesNames.add(TechnicalTaxonomies.Name.READ);
		expectedTypeNames.add(TechnicalTaxonomies.TypeName.FILE_ACCESS);
		
		assertTaxonomies(expectedTaxonomiesNames, expectedTypeNames, taxonomyService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleIdA)));
	
	}
	
	@Test
	void testFileAccessIdentificationForNatural() {
		final EntityId moduleIdA = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "WMIN1205A.nsp", getContent("WMIN1205A.nsp"), Technology.NATURAL, Type.PROGRAM);
		
		/* Run the Technical Taxonomy Identification on the Basic Module */
		final IdentifyTechnicalTaxonomiesJob job = new IdentifyTechnicalTaxonomiesJob(PROJECT_ID_1, new ModuleMatcher(Arrays.asList(moduleIdA), Collections.emptyList()));
		submitJob(jobManager, job);
		
		final List<TechnicalTaxonomies.Name> expectedTaxonomiesNames = new ArrayList<>();
		final List<TechnicalTaxonomies.TypeName> expectedTypeNames = new ArrayList<>();
		
		expectedTaxonomiesNames.add(TechnicalTaxonomies.Name.WRITE);
		expectedTypeNames.add(TechnicalTaxonomies.TypeName.FILE_ACCESS);
		
		expectedTaxonomiesNames.add(TechnicalTaxonomies.Name.READ);
		expectedTypeNames.add(TechnicalTaxonomies.TypeName.FILE_ACCESS);
		
		expectedTaxonomiesNames.add(TechnicalTaxonomies.Name.LIBRARY);
		expectedTypeNames.add(TechnicalTaxonomies.TypeName.PROGRAM_TYPE);
		
		assertTaxonomies(expectedTaxonomiesNames, expectedTypeNames, taxonomyService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleIdA)));
	}
	
	@Test
	void testUiProgramIdentificationForNatural() {

		final EntityId moduleIdA = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "WMIN1206A.nsp", getContent("WMIN1206A.nsp"), Technology.NATURAL, Type.PROGRAM);
		final EntityId moduleIdB = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "WMIN1206B.nsp", getContent("WMIN1206B.nsp"), Technology.NATURAL, Type.PROGRAM);
		final EntityId moduleIdC = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "WMIN1206C.nsp", getContent("WMIN1206C.nsp"), Technology.NATURAL, Type.PROGRAM);
		final EntityId moduleIdD = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "WMIN1206D.nsp", getContent("WMIN1206D.nsp"), Technology.NATURAL, Type.PROGRAM);
		final EntityId moduleIdE = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "WMIN1206E.nsp", getContent("WMIN1206E.nsp"), Technology.NATURAL, Type.PROGRAM);
		
		/* Run the Technical Taxonomy Identification on the Basic Module */
		final IdentifyTechnicalTaxonomiesJob jobA = new IdentifyTechnicalTaxonomiesJob(PROJECT_ID_1, new ModuleMatcher(Arrays.asList(moduleIdA), Collections.emptyList()));
		submitJob(jobManager, jobA);
		
		final List<TaxonomyPojo> taxonomiesA = taxonomyService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleIdA));
		assertEquals(LONG_ONE, taxonomiesA.size());
		final TaxonomyPojo taxonomyA = taxonomiesA.get(0);
		assertEquals(UI.name(),taxonomyA.getName());
		assertEquals(PROGRAM_TYPE.getDisplayName(), taxonomyA.getType().getName());
		
		/* Run the Technical Taxonomy Identification on the Basic Module */
		final IdentifyTechnicalTaxonomiesJob jobB = new IdentifyTechnicalTaxonomiesJob(PROJECT_ID_1, new ModuleMatcher(Arrays.asList(moduleIdB), Collections.emptyList()));
		submitJob(jobManager, jobB);
		final List<TaxonomyPojo> taxonomiesB = taxonomyService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleIdB));
		assertEquals(LONG_ONE, taxonomiesB.size());
		final TaxonomyPojo taxonomyB = taxonomiesB.get(0);
		assertEquals(UI.name(),taxonomyB.getName());
		assertEquals(PROGRAM_TYPE.getDisplayName(), taxonomyB.getType().getName());
		
		/* Run the Technical Taxonomy Identification on the Basic Module */
		final IdentifyTechnicalTaxonomiesJob jobC = new IdentifyTechnicalTaxonomiesJob(PROJECT_ID_1, new ModuleMatcher(Arrays.asList(moduleIdC), Collections.emptyList()));
		submitJob(jobManager, jobC);
		final List<TaxonomyPojo> taxonomiesC = taxonomyService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleIdC));
		assertEquals(LONG_ONE, taxonomiesC.size());
		final TaxonomyPojo taxonomyC = taxonomiesC.get(0);
		assertEquals(UI.name(),taxonomyC.getName());
		assertEquals(PROGRAM_TYPE.getDisplayName(), taxonomyC.getType().getName());
		
		/* Run the Technical Taxonomy Identification on the Basic Module */
		final IdentifyTechnicalTaxonomiesJob jobD = new IdentifyTechnicalTaxonomiesJob(PROJECT_ID_1, new ModuleMatcher(Arrays.asList(moduleIdD), Collections.emptyList()));
		submitJob(jobManager, jobD);
		final List<TaxonomyPojo> taxonomiesD = taxonomyService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleIdD));
		assertEquals(LONG_ONE, taxonomiesD.size());
		final TaxonomyPojo taxonomyD = taxonomiesD.get(0);
		assertEquals(UI.name(),taxonomyD.getName());
		assertEquals(PROGRAM_TYPE.getDisplayName(), taxonomyD.getType().getName());
		
		/* Run the Technical Taxonomy Identification on the Basic Module */
		final IdentifyTechnicalTaxonomiesJob jobE = new IdentifyTechnicalTaxonomiesJob(PROJECT_ID_1, new ModuleMatcher(Arrays.asList(moduleIdE), Collections.emptyList()));
		submitJob(jobManager, jobE);
		final List<TaxonomyPojo> taxonomiesE = taxonomyService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleIdE));
		assertEquals(LONG_ONE, taxonomiesE.size());
		final TaxonomyPojo taxonomyE = taxonomiesE.get(0);
		assertEquals(UI.name(),taxonomyE.getName());
		assertEquals(PROGRAM_TYPE.getDisplayName(), taxonomyE.getType().getName());
	}

	@Test
	void testDbAccessReadIdentification() {
		createModule(PROJECT_ID_4, "UnLoadsTable_ADUUMAIN", "UnLoadsTable_ADUUMAIN.job", RESOURCE_PATH, JCL, JOB);

		submitJob(jobManager, new DiscoverCodeJob(PROJECT_ID_4));

		/* Submit discover metrics job */
		submitJob(jobManager, new DiscoverMetricsJob(PROJECT_ID_4, false));

		final Optional<EntityId> moduleIdB = moduleService.findAnyModuleId(q -> q.ofProject(PROJECT_ID_4).withName("UnLoadsTable_ADUUMAIN"));
		assertTrue(moduleIdB.isPresent(), "UnLoadsTable_ADUUMAIN Module must be present as we have created");

		/* Run the Technical Taxonomy Identification on the Jcl Module */
		final IdentifyTechnicalTaxonomiesJob job = new IdentifyTechnicalTaxonomiesJob(PROJECT_ID_4,
				new ModuleMatcher(Collections.singletonList(moduleIdB.get()), Collections.emptyList()));
		submitJob(jobManager, job);
		
		final Optional<EntityId> dependentModule =
				moduleService.findAnyModuleId(q -> q.ofProject(PROJECT_ID_4).withName("UnLoadsTable_ADUUMAIN.STEP0080.EXEC_PGM"));
		assertTrue(dependentModule.isPresent(), "UnLoadsTable_ADUUMAIN.STEP0080.EXEC_PGM Virtual Module must be present as we have run DiscoverMetricsJob");

		final List<TaxonomyPojo> taxonomies = taxonomyService.find(q -> q.ofProject(PROJECT_ID_4).ofModule(dependentModule.get()));
		assertEquals(2L, taxonomies.size());
		final List<TaxonomyPojo> taxonomy =
				taxonomies.stream().filter(tax -> tax.getType().getName().equals(DB_ACCESS.getDisplayName()) && tax.getName()
						.equals(TechnicalTaxonomies.Name.READ.getDisplayName())).collect(Collectors.toList());
		assertEquals(1, taxonomy.size());
	}
	
	@Test
	void testFileAccessReadIdentification() {
		createModule(PROJECT_ID_4, "UnLoadsTable_ADUUMAIN", "UnLoadsTable_ADUUMAIN.job", RESOURCE_PATH, JCL, JOB);
		createModule(PROJECT_ID_4, "Exit", "Exit.c", RESOURCE_PATH, C, PROGRAM);

		submitJob(jobManager, new DiscoverCodeJob(PROJECT_ID_4));

		/* Submit discover metrics job */
		submitJob(jobManager, new DiscoverMetricsJob(PROJECT_ID_4, false));
		
		final Optional<EntityId> moduleIdA = moduleService.findAnyModuleId(q -> q.ofProject(PROJECT_ID_4).withName("Exit"));
		assertTrue(moduleIdA.isPresent(), "Exit Module must be present as we have created");

		final Optional<EntityId> moduleIdB = moduleService.findAnyModuleId(q -> q.ofProject(PROJECT_ID_4).withName("UnLoadsTable_ADUUMAIN"));
		assertTrue(moduleIdB.isPresent(), "UnLoadsTable_ADUUMAIN Module must be present as we have created");

		/* Running the Technical Taxonomy Identification on the Jcl Module which Identifies Db Acess Read*/
		final IdentifyTechnicalTaxonomiesJob job = new IdentifyTechnicalTaxonomiesJob(PROJECT_ID_4,
				new ModuleMatcher(Collections.singletonList(moduleIdB.get()), Collections.emptyList()));
		submitJob(jobManager, job);
		
		final Optional<EntityId> dependentModule =
				moduleService.findAnyModuleId(q -> q.ofProject(PROJECT_ID_4).withName("UnLoadsTable_ADUUMAIN.STEP0080.EXEC_PGM"));
		assertTrue(dependentModule.isPresent(), "UnLoadsTable_ADUUMAIN.STEP0080.EXEC_PGM Virtual Module must be present as we have run DiscoverMetricsJob");

		final List<TaxonomyPojo> taxonomies = taxonomyService.find(q -> q.ofProject(PROJECT_ID_4).ofModule(dependentModule.get()));
		assertEquals(2L, taxonomies.size());
		final List<TaxonomyPojo> taxonomy =
				taxonomies.stream().filter(tax -> tax.getType().getName().equals(DB_ACCESS.getDisplayName()) && tax.getName()
						.equals(TechnicalTaxonomies.Name.READ.getDisplayName())).collect(Collectors.toList());
		assertEquals(1, taxonomy.size());
		
		/* Run the Technical Taxonomy Identification on the Exit Module to ensure that the cache does not return 'Db Access Read', 
			which was identified for the JCL module, instead of 'File Access Read'. */
		final IdentifyTechnicalTaxonomiesJob job2 = new IdentifyTechnicalTaxonomiesJob(PROJECT_ID_4,
				new ModuleMatcher(Collections.singletonList(moduleIdA.get()), Collections.emptyList()));
		submitJob(jobManager, job2);
		final List<TaxonomyPojo> taxonomies2 = taxonomyService.find(q -> q.ofProject(PROJECT_ID_4).ofModule(moduleIdA.get()));
		assertEquals(3L, taxonomies2.size());
		final List<TaxonomyPojo> taxonomy2 =
				taxonomies2.stream().filter(tax1 -> tax1.getType().getName().equals(FILE_ACCESS.getDisplayName()) && tax1.getName()
						.equals(TechnicalTaxonomies.Name.READ.getDisplayName())).collect(Collectors.toList());
		assertEquals(1, taxonomy2.size());
		
	}
}

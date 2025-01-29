/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.identification;

import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.Before;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.lib.job.api.management.JobManager;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.server.job.LinkAnnotationToDataDictionaryJob;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;

/**
 * Business Variable Identification tests.
 */
@Disabled("Flaky (addresed in WMIN-11291)")
@WithMockUser
class BusinessVariableIdentificationTest extends AbstractIdentificationTest {

	@Autowired
	private JobManager jobManager;
	
	@Autowired
	private DataDictionaryService dataDictionaryService;
	@Autowired
	private AnnotationService annotationService;
	
	private static final String ANNOTATION_NAME = "Business Rule [System Identified]";
	
	@Before
	void resetData() {
		try {
			resetTestData();
		} catch (final IOException e) {
			e.printStackTrace();
		}
	}
	
	@Test
	void testIdentifyArithmaticDataDictionaryEntries() {
		final EntityId moduleId = createCobolProgram(PROJECT_ID_1, "MMRS71Z1", "MMRS71Z1.cbl", RESOURCE_PATH);

		assertEquals(0, dataDictionaryService.find(q -> q.ofModule(moduleId)).size());
		
		/* Run the Data Dictionary Candidate Identification on the COBOL Program Module */
		submitIdentifyBusinessVariablesJob(PROJECT_ID_1, RESOURCE_PATH + "MMRS71Z1.cbl");
		
		/* Verify that the COBOL Program Module has the data dictionaries associated */
		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModule(moduleId));
		final List<DataDictionaryPojo> businessVariables = dataDictionaries.stream().filter(d -> d.getIsBusiness().orElseThrow()).collect(Collectors.toList());
		assertEquals(27, dataDictionaries.size());
		assertEquals(3, businessVariables.size());
		assertTrue("Should contain MY-HEX-DIGIT, MY-HEX-ZONE, D",
				businessVariables.stream().map(DataDictionaryPojo::getName).collect(Collectors.toList())
						.containsAll(Arrays.asList("MY-HEX-DIGIT", "MY-HEX-ZONE", "D")));
	}
	
	@Test
	void testIdentifyArithmaticExpressionDataDictionaryEntries() {
		final EntityId moduleId = createCobolProgram(PROJECT_ID_1, "FUNC", "FUNC.cbl", RESOURCE_PATH);

		assertEquals(0, dataDictionaryService.find(q -> q.ofModule(moduleId)).size());
		
		/* Run the Data Dictionary Candidate Identification on the COBOL Program Module */
		submitIdentifyBusinessVariablesJob(PROJECT_ID_1, moduleId);
		
		/* Verify that the COBOL Program Module has the data dictionaries associated */
		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModule(moduleId));
		final List<DataDictionaryPojo> businessVariables = dataDictionaries.stream().filter(d -> d.getIsBusiness().orElseThrow()).collect(Collectors.toList());
		assertEquals(8, dataDictionaries.size());
		assertEquals(5, businessVariables.size());
		assertTrue("Should contain A, D, E, F, G",
				businessVariables.stream().map(DataDictionaryPojo::getName).collect(Collectors.toList())
						.containsAll(Arrays.asList("A", "D", "E", "F", "G")));
	}
	
	@Test
	void testIdentifyBusinessVariableExlcudesIndexes() {
		final EntityId moduleId = createCobolProgram(PROJECT_ID_1, "INDEXEXAMPLE.cbl", "INDEXEXAMPLE.cbl", RESOURCE_PATH);

		assertEquals(0, dataDictionaryService.find(q -> q.ofModule(moduleId)).size());
		
		/* Run the Data Dictionary Candidate Identification on the COBOL Program Module */
		submitIdentifyBusinessVariablesJob(PROJECT_ID_1, moduleId);
		
		/* Verify that the COBOL Program Module has the data dictionaries associated and
		 * Indexes are not identified as Business Variables*/
		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModule(moduleId));
		final List<DataDictionaryPojo> businessVariables = dataDictionaries.stream().filter(d -> d.getIsBusiness().orElseThrow()).collect(Collectors.toList());
		assertEquals(6, dataDictionaries.size());
		assertEquals(2, businessVariables.size());
		assertTrue("Should contain STD-DET, STD-INDEX, STD-MARKS, TOTAL-MARKS, STD-PERCENT, I",
				dataDictionaries.stream().map(DataDictionaryPojo::getName).collect(Collectors.toList())
						.containsAll(Arrays.asList("STD-DET", "STD-INDEX", "STD-MARKS", "TOTAL-MARKS", "STD-PERCENT", "I")));
		assertTrue("Should contain STD-PERCENT, TOTAL-MARKS but it should not contain STD-INDEX",
				businessVariables.stream().map(DataDictionaryPojo::getName).collect(Collectors.toList())
						.containsAll(Arrays.asList("STD-PERCENT", "TOTAL-MARKS")));
	}
	
	@Test
	void testIdentifyBusinessVariableExcludesCountersInsideInvocableOrJumpStatementIs() {
		final EntityId moduleId = createCobolProgram(PROJECT_ID_1, "ARITHMETICSTATEMENTS", "ARITHMETICSTATEMENTS.cbl", RESOURCE_PATH);

		assertEquals(0, dataDictionaryService.find(q -> q.ofModule(moduleId)).size());
		
		/* Run the Data Dictionary Candidate Identification on the COBOL Program Module */
		submitIdentifyBusinessVariablesJob(PROJECT_ID_1, moduleId);
		
		/* Verify that the COBOL Program Module has the data dictionaries associated and
		 * Counters inside Invocable or JumpSatetment are not identified as Business variable */
		
		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModule(moduleId));
		final List<DataDictionaryPojo> businessVariables = dataDictionaries.stream().filter(d -> d.getIsBusiness().orElseThrow()).collect(Collectors.toList());
		assertEquals(7, dataDictionaries.size());
		assertEquals(5, businessVariables.size());
		assertTrue("Should contain A, B, D, E, F but it should not contain C",
				businessVariables.stream().map(DataDictionaryPojo::getName).collect(Collectors.toList())
						.containsAll(Arrays.asList("A", "B", "D", "E", "F")));
	}
	
	@Test
	void testIdentifyBusinessVariablesExcludesCountersOutsideInvocableOrJumpStatement() {
		final EntityId moduleId = createCobolProgram(PROJECT_ID_1, "COUNTEROUTSIDEINVOCABLEORJUMP", "COUNTEROUTSIDEINVOCABLEORJUMP.cbl", RESOURCE_PATH);

		assertEquals(0, dataDictionaryService.find(q -> q.ofModule(moduleId)).size());
		
		/* Run the Data Dictionary Candidate Identification on the COBOL Program Module */
		submitIdentifyBusinessVariablesJob(PROJECT_ID_1, moduleId);
		
		/* Verify that the COBOL Program Module has the data dictionaries associated and
		 * Counters outside Invocable or JumpSatetment are identified as Business variable */
		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModule(moduleId));
		final List<DataDictionaryPojo> businessVariables = dataDictionaries.stream().filter(d -> d.getIsBusiness().orElseThrow()).collect(Collectors.toList());
		assertEquals(7, dataDictionaries.size());
		assertEquals(4, businessVariables.size());
		assertTrue("Should contain A, B, D, E",
				businessVariables.stream().map(DataDictionaryPojo::getName).collect(Collectors.toList())
						.containsAll(Arrays.asList("A", "B", "D", "E")));
	}
	
	@Test
	void testIdentifyFileDataDictionaryEntries() {
		final EntityId moduleId = createCobolProgram(PROJECT_ID_1, "CICSPGM", "CICSPGM.cbl", RESOURCE_PATH);

		assertEquals(0, dataDictionaryService.find(q -> q.ofModule(moduleId)).size());
		
		/* Verify that the COBOL Program Module has the data dictionaries associated */
		final List<DataDictionaryPojo> dataDictionariesAfter = identifyAndGetCandidates(moduleId);
		createAnnotation(moduleId, 2624, 8);
		createAnnotation(moduleId, 2675, 9);
		createAnnotation(moduleId, 2735, 17);
		createAnnotation(moduleId, 2799, 13);
		createAnnotation(moduleId, 2898, 9);
		createAnnotation(moduleId, 2851, 5);

		final List<DataDictionaryPojo> businessVariables = dataDictionariesAfter
				.stream()
				.filter(d -> d.getIsBusiness().orElseThrow())
				.collect(Collectors.toList());
		
		assertEquals(11, dataDictionariesAfter.size());
		assertEquals(6, businessVariables.size());
		assertTrue("Should contain FSTARTBR, FREADPREV, WS-VSAM-TABLE-KEY, WS-VSAM-TABLE, FREADNEXT, FREAD",
				businessVariables.stream().map(DataDictionaryPojo::getName).collect(Collectors.toList())
						.containsAll(Arrays.asList("FSTARTBR", "FREADPREV", "WS-VSAM-TABLE-KEY", "WS-VSAM-TABLE", "FREADNEXT", "FREAD")));
	}
	
	@Test
	void testIdentifyDatabaseDataDictionaryEntries() {
		final EntityId moduleId = createCobolProgram(PROJECT_ID_1, "TESTDBPRG", "TESTDBPRG.cbl", RESOURCE_PATH);
		final EntityId copyId = createCobolCopybook(PROJECT_ID_1, "EMPREC", "EMPREC.cpy", RESOURCE_PATH);
		createReference(RelationshipType.INCLUDES, moduleId, copyId);
		assertEquals(0, dataDictionaryService.find(q -> q.ofModule(moduleId)).size());
		
		/* Run the Data Dictionary Candidate Identification on the COBOL Program Module */
		submitIdentifyBusinessVariablesJob(PROJECT_ID_1, moduleId);
		
		/* Verify that the COBOL Program Module has the data dictionaries associated */
		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModule(moduleId));
		final List<DataDictionaryPojo> dataDictionariesCopyBook = dataDictionaryService.find(q -> q.ofModule(copyId));
		final List<DataDictionaryPojo> businessVariables = dataDictionaries.stream().filter(d -> d.getIsBusiness().orElseThrow()).collect(Collectors.toList());
		assertEquals(6, dataDictionaries.size());
		assertEquals(11, dataDictionariesCopyBook.size());
		assertEquals(1, businessVariables.size());
		assertEquals("COM-NULL-IND", businessVariables.get(0).getName());
		assertEquals(10, dataDictionariesCopyBook.stream().filter(d -> d.getIsBusiness().orElseThrow()).count());
	}
	
	@Test
	void testDoesNotIdentifyFileFlagDataDictionaryEntries() {
		final EntityId moduleId = createCobolProgram(PROJECT_ID_1, "BRE2_TECHNICAL3", "BRE2_TECHNICAL3.cbl", RESOURCE_PATH);
		assertEquals(0, dataDictionaryService.find(q -> q.ofModule(moduleId)).size());
		
		/* Run the Data Dictionary Candidate Identification on the COBOL Program Module */
		submitIdentifyBusinessVariablesJob(PROJECT_ID_1, moduleId);
		
		/* Verify that the COBOL Program Module has the data dictionaries associated */
		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModule(moduleId));
		final List<DataDictionaryPojo> businessVariables = dataDictionaries.stream().filter(d -> d.getIsBusiness().orElseThrow()).collect(Collectors.toList());
		assertEquals(7, dataDictionaries.size());
		assertEquals(1, businessVariables.size());
		assertEquals("WS-STUDENT", businessVariables.get(0).getName());
	}
	
	@Test
	void testIdentifyBusinessDataDictionaryEntriesInCopyBook() {
		final EntityId wsprogId = createCobolProgram(PROJECT_ID_1, "WSPROG", "WSPROG.cbl", RESOURCE_PATH);
		final EntityId wscopyId = createCobolCopybook(PROJECT_ID_1, "WSCOPY", "WSCOPY.cpy", RESOURCE_PATH);
		createReference(RelationshipType.INCLUDES, wsprogId, wscopyId);

		assertEquals(0, dataDictionaryService.find(q -> q.ofModule(wsprogId)).size());
		
		/* Run the Data Dictionary Candidate Identification on the COBOL Program Module */
		final List<DataDictionaryPojo> dataDictionariesAfter = identifyAndGetCandidates(wsprogId);
		createAnnotation(wsprogId, 38, 7);
		createAnnotation(wsprogId, 46, 7);
		createAnnotation(wsprogId, 147, 12);
		
		/* Verify that the COBOL Program Module has the data dictionaries associated */
		final List<DataDictionaryPojo> businessVariables = dataDictionariesAfter
				.stream()
				.filter(d -> d.getIsBusiness().orElseThrow())
				.collect(Collectors.toList());
		
		assertEquals(12, dataDictionariesAfter.size());
		assertEquals(3, businessVariables.size());
		assertTrue("Should contain WS-NUM3, WS-NUM4, ABCD and not contain WS-NUME because it's not referenced",
				businessVariables.stream().map(DataDictionaryPojo::getName).collect(Collectors.toList())
						.containsAll(Arrays.asList( "WS-NUM3", "WS-NUM4", "ABCD")));
	}
	
	@Test
	void testIdentifyArithmaticDataDictionaryEntriesInNatural() {
		final EntityId moduleId = createModule(PROJECT_ID_1, "ARITHM1", "ARITHM1.nsp", RESOURCE_PATH, Technology.NATURAL, Type.PROGRAM);

		final List<DataDictionaryPojo> dataDictionariesAfter = identifyAndGetCandidates(moduleId);
		createAnnotation(moduleId, 432, 2);
		createAnnotation(moduleId, 330, 5);
		createAnnotation(moduleId, 386, 2);
		createAnnotation(moduleId, 412, 2);
		createAnnotation(moduleId, 234, 2);
		createAnnotation(moduleId, 229, 1);
		createAnnotation(moduleId, 255, 3);
		createAnnotation(moduleId, 280, 4);
		createAnnotation(moduleId, 288, 3);
		createAnnotation(moduleId, 535, 1);
		
		final List<DataDictionaryPojo> businessVariables = dataDictionariesAfter
				.stream()
				.filter(d -> d.getIsBusiness().orElseThrow())
				.collect(Collectors.toList());
		
		assertEquals(10, businessVariables.size());
		assertTrue("Should contain #P, #INT2, #F, #E, #I, H, CDE, ABCD, ABC, T", businessVariables
				.stream()
				.map(DataDictionaryPojo::getName)
				.collect(Collectors.toList())
				.containsAll(Arrays.asList("#P", "#INT2","#F","#E", "#I", "H", "CDE", "ABCD", "ABC","T")));
	}
	
	@Test
	void testIdentifyBusinessVariablesExcludesCountersInsideInvocableInNatural() {
		final EntityId moduleId = createModule(PROJECT_ID_1, "COUNTER_IN_INVOCABLE", "COUNTER_IN_INVOCABLE.nsp", RESOURCE_PATH, Technology.NATURAL, Type.PROGRAM);

		final List<DataDictionaryPojo> dataDictionariesAfter = identifyAndGetCandidates(moduleId);
		createAnnotation(moduleId, 278, 2);
		
		final List<DataDictionaryPojo> businessVariables = dataDictionariesAfter
				.stream()
				.filter(d -> d.getIsBusiness().orElseThrow())
				.collect(Collectors.toList());
		
		assertEquals(1, businessVariables.size());
		assertTrue("Should contain #X but it should not contain #Y ", businessVariables
				.stream()
				.map(DataDictionaryPojo::getName)
				.collect(Collectors.toList())
				.containsAll(Arrays.asList("#X")));
	}
	
	@Test
	void testIdentifyFileDataDictionaryEntriesInNatural() {
		final EntityId moduleId = createModule(PROJECT_ID_1, "FILE_ACCESS", "FILE_ACCESS.nsp", RESOURCE_PATH, Technology.NATURAL, Type.PROGRAM);

		final List<DataDictionaryPojo> dataDictionariesAfter = identifyAndGetCandidates(moduleId);
		createAnnotation(moduleId, 100, 9);
		createAnnotation(moduleId, 139, 9);
		
		final List<DataDictionaryPojo> businessVariables = dataDictionariesAfter
				.stream()
				.filter(d -> d.getIsBusiness().orElseThrow())
				.collect(Collectors.toList());
		
		assertEquals(2, businessVariables.size());
		assertTrue("Should contain #WF-LINE1 , #WF-LINE2",
				businessVariables.stream()
				.map(DataDictionaryPojo::getName)
				.collect(Collectors.toList())
				.containsAll(Arrays.asList("#WF-LINE1","#WF-LINE2")));
	}

	@Test
	void testIdentifyDatabaseDataDictionaryEntriesInNatural() {
		final EntityId moduleId = createModule(PROJECT_ID_1, "DATABASE", "DATABASE.nsp", RESOURCE_PATH, Technology.NATURAL, Type.PROGRAM);

		final List<DataDictionaryPojo> dataDictionariesAfter = identifyAndGetCandidates(moduleId);
		createAnnotation(moduleId, 555, 6);
		createAnnotation(moduleId, 520, 9);
		
		final List<DataDictionaryPojo> businessVariables = dataDictionariesAfter
				.stream()
				.filter(d -> d.getIsBusiness().orElseThrow())
				.collect(Collectors.toList());
		
		assertEquals(2, businessVariables.size());
		assertTrue("Should contain MSN,TELEFONNR", businessVariables.stream()
				.map(DataDictionaryPojo::getName)
				.collect(Collectors.toList())
				.containsAll(Arrays.asList("MSN", "TELEFONNR")));
	}

	@Test
	void testIdentifyBusinessVariableExlcudesIndexesInNatural() {
		final EntityId moduleId = createModule(PROJECT_ID_1, "INDEX", "INDEX.nsp", RESOURCE_PATH, Technology.NATURAL, Type.PROGRAM);
		
		final List<DataDictionaryPojo> dataDictionariesAfter = identifyAndGetCandidates(moduleId);
		createAnnotation(moduleId, 133, 6);
		createAnnotation(moduleId, 105, 6);

		/* Verify that the Natural Program Module has the data dictionaries associated and
		 * Indexes are not identified as Business Variables*/
		final List<DataDictionaryPojo> businessVariables = dataDictionariesAfter
				.stream()
				.filter(d -> d.getIsBusiness().orElseThrow())
				.collect(Collectors.toList());

		assertEquals(2, businessVariables.size());
		assertTrue("Should contain PARAM3, PARAM2 but it should not contain #Index1", businessVariables.stream().map(DataDictionaryPojo::getName)
				.collect(Collectors.toList()).containsAll(Arrays.asList("PARAM3", "PARAM2")));
	}

	@Test
	void testIdentifyBusinessVariablesExcludesCountersOutsideInvocableInNatural() {
		final EntityId moduleId = createModule(PROJECT_ID_1, "COUNTEROUTSIDEINVOCABLE", "COUNTEROUTSIDEINVOCABLE.nsp", RESOURCE_PATH, Technology.NATURAL, Type.PROGRAM);

		final List<DataDictionaryPojo> dataDictionariesAfter = identifyAndGetCandidates(moduleId);
		createAnnotation(moduleId, 147, 5);
		createAnnotation(moduleId, 121, 5);

		final List<DataDictionaryPojo> businessVariables = dataDictionariesAfter
				.stream()
				.filter(d -> d.getIsBusiness().orElseThrow())
				.collect(Collectors.toList());

		assertEquals(2, businessVariables.size());
		assertTrue("Should contain #INT2, #INT1 not #COUNTER", businessVariables.stream().map(DataDictionaryPojo::getName)
				.collect(Collectors.toList()).containsAll(Arrays.asList("#INT2", "#INT1")));
	}
	
	@Test
	void testDoesNotIdentifyFileFlagDataDictionaryEntriesInNatral() {
		final EntityId moduleId = createModule(PROJECT_ID_1, "FILEFLAG", "FILEFLAG.nsp", RESOURCE_PATH, Technology.NATURAL, Type.PROGRAM);

		final List<DataDictionaryPojo> dataDictionariesAfter = identifyAndGetCandidates(moduleId);
		createAnnotation(moduleId, 70, 5);
		final List<DataDictionaryPojo> businessVariables = dataDictionariesAfter
				.stream()
				.filter(d -> d.getIsBusiness().orElseThrow())
				.collect(Collectors.toList());

		assertEquals(1, businessVariables.size());
		assertEquals("#DYNA", businessVariables.get(0).getName());
	}


	@Test
	void testIdentifyMultipleDataDictionriesWithSameNameInNatral() {
		final EntityId moduleId = createModule(PROJECT_ID_1, "GETUAM", "GETUAM.nsp", RESOURCE_PATH, Technology.NATURAL, Type.PROGRAM);

		final List<DataDictionaryPojo> dataDictionariesAfter = identifyAndGetCandidates(moduleId);
		createAnnotation(moduleId, 261, 6);
		createAnnotation(moduleId, 329, 7);
		
		final List<DataDictionaryPojo> businessVariables = dataDictionariesAfter
				.stream()
				.filter(d -> d.getIsBusiness().orElseThrow())
				.collect(Collectors.toList());
		
		assertEquals(2, businessVariables.size());
		assertTrue("Should contain UAMKEY, GETVIEW", businessVariables.stream().map(DataDictionaryPojo::getName)
				.collect(Collectors.toList()).containsAll(Arrays.asList("UAMKEY", "GETVIEW")));
		final DataDictionaryPojo uamkey = businessVariables.stream().filter(d -> "UAMKEY".equals(d.getName())).findAny().orElseThrow();
		assertEquals(new ModuleLocation(83, 6), uamkey.getLocation().orElseThrow());
	}
	
	@Test
	void testIdentifyDataDictionaryEntriesInPl1() {
		final EntityId moduleId = createModule(PROJECT_ID_1, "AM11853B", "AM11853B.pl1m", RESOURCE_PATH_PL1, Technology.PL1, Type.PROGRAM);

		final List<DataDictionaryPojo> dataDictionariesAfter = identifyAndGetCandidates(moduleId);
		createAnnotation(moduleId, 794, 9);
		createAnnotation(moduleId, 834, 6);
		createAnnotation(moduleId, 858, 11);
		
		final List<DataDictionaryPojo> businessVariables = dataDictionariesAfter
				.stream()
				.filter(d -> d.getIsBusiness().orElseThrow())
				.collect(Collectors.toList());
		
		assertEquals(24, dataDictionariesAfter.size());
		assertEquals(3, businessVariables.size());
		assertTrue("Should contain TESTCASE, ALPHA, ALPHA_NULL",
				businessVariables.stream().map(DataDictionaryPojo::getName).collect(Collectors.toList())
						.containsAll(Arrays.asList("TESTCASE", "ALPHA", "ALPHA_NULL")));
	}

	@Test
	void testdentifyArithmaticDataDictionaryEntriesInPL1() {
		final EntityId moduleId = createModule(PROJECT_ID_1, "ARITH", "ARITH.pl1", RESOURCE_PATH_PL1, Technology.PL1, Type.PROGRAM);

		final List<DataDictionaryPojo> dataDictionariesAfter = identifyAndGetCandidates(moduleId);
		createAnnotation(moduleId, 261, 6);
		createAnnotation(moduleId, 329, 7);
		
		final List<DataDictionaryPojo> businessVariables = dataDictionariesAfter
				.stream()
				.filter(d -> d.getIsBusiness().orElseThrow())
				.collect(Collectors.toList());
		
		assertEquals(5, dataDictionariesAfter.size());
		assertEquals(2, businessVariables.size());
		assertTrue("Should contain C, A",
				businessVariables.stream().map(DataDictionaryPojo::getName).collect(Collectors.toList()).containsAll(Arrays.asList("C", "A")));
	}

	@Test
	void testIdentifyBusinessVariablesExcludesCountersOutsideInvocableInPL1() {
		final EntityId moduleId = createModule(PROJECT_ID_1, "COUNTEROUTSIDE_INVOCABLE", "COUNTEROUTSIDE_INVOCABLE.pl1", RESOURCE_PATH_PL1, Technology.PL1, Type.PROGRAM);
		
		final List<DataDictionaryPojo> dataDictionariesAfter = identifyAndGetCandidates(moduleId);
		createAnnotation(moduleId, 213, 6);
		createAnnotation(moduleId, 221, 5);
		
		final List<DataDictionaryPojo> businessVariables = dataDictionariesAfter
				.stream()
				.filter(d -> d.getIsBusiness().orElseThrow())
				.collect(Collectors.toList());
		
		assertEquals(2, businessVariables.size());
		assertTrue("Should contain #INT2, #INT1 not #COUNTER", businessVariables.stream().map(DataDictionaryPojo::getName)
				.collect(Collectors.toList()).containsAll(Arrays.asList("#INT2", "#INT1")));
	}

	@Test
	void testDoesNotIdentifyFileFlagDataDictionaryEntriesInPL1() {
		final EntityId moduleId = createModule(PROJECT_ID_1, "File_Flag", "File_Flag.pl1m", RESOURCE_PATH_PL1, Technology.PL1, Type.PROGRAM);
		
		final List<DataDictionaryPojo> dataDictionariesAfter = identifyAndGetCandidates(moduleId);
		createAnnotation(moduleId, 334, 5);
		createAnnotation(moduleId, 294, 4);
		createAnnotation(moduleId, 271, 6);
		
		final List<DataDictionaryPojo> businessVariables = dataDictionariesAfter
				.stream()
				.filter(d -> d.getIsBusiness().orElseThrow())
				.collect(Collectors.toList());
		
		assertEquals(3, businessVariables.size());
		assertTrue("Should contain #DYNA, WORK, INFILE}",
				businessVariables.stream()
				.map(DataDictionaryPojo::getName)
				.collect(Collectors.toList())
				.containsAll(Arrays.asList("#DYNA", "WORK", "INFILE")));
	}

	@Test
	void testIdentifyFileDataDictionaryEntriesInPL1() {
		final EntityId moduleId = createModule(PROJECT_ID_1, "FileAccess", "FileAccess.pl1m", RESOURCE_PATH_PL1, Technology.PL1, Type.PROGRAM);
		
		final List<DataDictionaryPojo> dataDictionariesAfter = identifyAndGetCandidates(moduleId);
		createAnnotation(moduleId, 301, 5);
		createAnnotation(moduleId, 259, 7);
		createAnnotation(moduleId, 237, 6);
		
		final List<DataDictionaryPojo> businessVariables = dataDictionariesAfter
				.stream()
				.filter(d -> d.getIsBusiness().orElseThrow())
				.collect(Collectors.toList());

		assertEquals(3, businessVariables.size());
		assertTrue("Should contain RECRD, OUTFILE, INFILE}",
				businessVariables.stream()
				.map(DataDictionaryPojo::getName)
				.collect(Collectors.toList())
				.containsAll(Arrays.asList("RECRD", "OUTFILE", "INFILE")));
	}

	@Test
	void testIdentifyBusinessVariableExlcudesIndexesInPL1() {
		final EntityId moduleId = createModule(PROJECT_ID_1, "EXCLUDE_INDEX", "EXCLUDE_INDEX.pl1m", RESOURCE_PATH_PL1, Technology.PL1, Type.PROGRAM);
		
		final List<DataDictionaryPojo> dataDictionariesAfter = identifyAndGetCandidates(moduleId);
		createAnnotation(moduleId, 195, 8);
		createAnnotation(moduleId, 426, 9);
		
		final List<DataDictionaryPojo> businessVariables = dataDictionariesAfter
				.stream()
				.filter(d -> d.getIsBusiness().orElseThrow())
				.collect(Collectors.toList());
		
		assertEquals(2, businessVariables.size());
		assertTrue("Should contain #Result, #Message but it should not contain #Index1", businessVariables.stream().map(DataDictionaryPojo::getName)
				.collect(Collectors.toList()).containsAll(Arrays.asList("#Result", "#Message")));
	}

	@Test
	void testIdentifyDatabaseDataDictionaryEntriesInPL1() {
		final EntityId moduleId = createModule(PROJECT_ID_1, "DbAccess", "DbAccess.pl1", RESOURCE_PATH_PL1, Technology.PL1, Type.PROGRAM);
		
		final List<DataDictionaryPojo> dataDictionariesAfter = identifyAndGetCandidates(moduleId);
		createAnnotation(moduleId, 522, 2);
		createAnnotation(moduleId, 530, 1);
		createAnnotation(moduleId, 218, 4);
		createAnnotation(moduleId, 231, 12);
		
		final List<DataDictionaryPojo> businessVariables = dataDictionariesAfter
				.stream()
				.filter(d -> d.getIsBusiness().orElseThrow())
				.collect(Collectors.toList());

		assertEquals(4, businessVariables.size());
		assertTrue("Should contain A, C, cnt, alpha_short", businessVariables.stream().map(DataDictionaryPojo::getName)
				.collect(Collectors.toList()).containsAll(Arrays.asList("A", "C", "cnt", "alpha_short")));
	}

	private List<DataDictionaryPojo> identifyAndGetCandidates(final EntityId moduleId) {
		assertEquals(0, dataDictionaryService.find(q -> q.ofModule(moduleId)).size());

		/* Run the Data Dictionary Candidate Identification on the Program Module */
		submitIdentifyBusinessVariablesJob(PROJECT_ID_1, moduleId);
		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModule(moduleId));
		return dataDictionaries;
	}

	private void submitIdentifyBusinessVariablesJob(final EntityId projectId, final EntityId moduleId) {
		submitJob(jobManager, new IdentifyCandidatesJob(projectId, new ModuleMatcher(Arrays.asList(moduleId), Collections.emptyList())));
	}

	private void submitIdentifyBusinessVariablesJob(final EntityId projectId, final String path) {
		submitJob(jobManager, new IdentifyCandidatesJob(projectId, new ModuleMatcher(Collections.emptyList(), Arrays.asList(path))));
	}
	

	private AnnotationPojo createAnnotation(final EntityId moduleId, final int offset, final int length) {
		final AnnotationPojoPrototype annotation = new AnnotationPojoPrototype();
		annotation.setState(WorkingState.CANDIDATE);
		annotation.setType(AnnotationType.RULE);
		annotation.setName(ANNOTATION_NAME);
		annotation.setCreatedByUserId("");
		annotation.setLocation(new ModuleLocation(offset, length));
		annotation.setModule(moduleId);

		final EntityId id = annotationService.create(annotation);
		final AnnotationPojo annotationPojo = annotationService.get(id);

		submitJob(jobManager, new LinkAnnotationToDataDictionaryJob(id, annotationPojo.getLocation().orElseThrow(), PROJECT_ID_1, moduleId));

		return annotationPojo;
	}
}

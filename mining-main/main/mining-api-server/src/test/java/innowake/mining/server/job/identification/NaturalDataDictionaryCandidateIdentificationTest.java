/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.job.identification;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.lib.job.api.management.JobManager;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.model.DefinedLocation;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;

/**
 * Data Dictionary Candidate Identification job tests for Natural language.
 */
@WithMockUser
class NaturalDataDictionaryCandidateIdentificationTest extends AbstractIdentificationTest {

	@Autowired
	private JobManager jobManager;

	@Autowired
	private DataDictionaryService dataDictionaryService;

	@Test
	void identifyNaturalProgramDataDictionaries() {
		final EntityId moduleId = createModule(PROJECT_ID_1, "MAIN1", "MAIN1.nsp", RESOURCE_PATH, Technology.NATURAL, Type.PROGRAM);
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);
		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModule(moduleId));

		assertEquals(24, dataDictionaries.size(), "Number of dataDictionaries sould be correct");
		assertEquals(DefinedLocation.PROGRAM, dataDictionaries.get(0).getDefinedLocation().get(), "Defined location is PROGRAM");
		assertEquals(1, dataDictionaries.stream().filter(dde -> "#PROG1".equals(dde.getName())).count(), "DataDictionaries should contain #PROG1");
		assertEquals(1, dataDictionaries.stream().filter(dde -> "#VAR1".equals(dde.getName())).count(), "DataDictionaries should contain #VAR1");
		final DataDictionaryPojo dataDictionary1 = dataDictionaries.stream().filter(dde -> "#PROG1".equals(dde.getName())).findFirst().get();
		final DataDictionaryPojo dataDictionary2 = dataDictionaries.stream().filter(dde -> "#DATE".equals(dde.getName())).findFirst().get();
		final DataDictionaryPojo dataDictionary3 = dataDictionaries.stream().filter(dde -> "#INT1".equals(dde.getName())).findFirst().get();
		final DataDictionaryPojo dataDictionary4 = dataDictionaries.stream().filter(dde -> "#A4".equals(dde.getName())).findFirst().get();

		assertEquals("A", dataDictionary1.getFormat().get(), "Format of #PROG1 should be Alphanumeric");
		assertEquals(8, dataDictionary1.getLength().get(), "Length of #PROG1 should be correct");
		assertEquals("D", dataDictionary2.getFormat().get(), "Format of #Date should be Packed numeric");
		assertEquals(4, dataDictionary2.getLength().get(), "Length of #Date should be correct");
		assertEquals("I", dataDictionary3.getFormat().get(), "Format of #INT1 should be Integer");
		assertEquals(1, dataDictionary3.getLength().get(), "Length of #INT1 should be correct");
		assertEquals("N", dataDictionary4.getFormat().get(), "Format of #A4 should be Numeric");
		assertEquals(9, dataDictionary4.getLength().get(), "Length of #A4 should be correct");

	}

	@Test
	void identifyNaturalSubProgramDataDictionaries() {
		final EntityId moduleIdSubProgram = assertNotNull(createModule(PROJECT_ID_1, "SUB1", "SUB1.nsn", RESOURCE_PATH, Technology.NATURAL, Type.SUBPROGRAM));
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleIdSubProgram);

		final List<DataDictionaryPojo> dataDictionariesSubProgram = dataDictionaryService.find(q -> q.ofModule(moduleIdSubProgram));
		assertEquals(2, dataDictionariesSubProgram.size(), "Number of dataDictionaries sould be correct");
		assertEquals(DefinedLocation.SUBPROGRAM, dataDictionariesSubProgram.get(0).getDefinedLocation().get(), "Defined location is SUBPROGRAM");
		assertEquals(1, dataDictionariesSubProgram.stream().filter(dde -> "#A".equals(dde.getName())).count(),
				"DataDictionaries should contain #VAR1");
	}

	@Test
	void identifyNaturalSubRoutineDataDictionaries() {
		final EntityId moduleIdSubRoutine = assertNotNull(createModule(PROJECT_ID_1, "SUBR1", "SUBR1.nss", RESOURCE_PATH, Technology.NATURAL, Type.SUBROUTINE));
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleIdSubRoutine);

		final List<DataDictionaryPojo> dataDictionariesSubRutine = dataDictionaryService.find(q -> q.ofModule(moduleIdSubRoutine));
		assertEquals(DefinedLocation.SUBROUTINE, dataDictionariesSubRutine.get(0).getDefinedLocation().get(), "Defined location is SUBROUTINE");
		assertEquals(2, dataDictionariesSubRutine.size(), "Number of dataDictionaries sould be correct");
		assertEquals(1, dataDictionariesSubRutine.stream().filter(dde -> "#A".equals(dde.getName())).count(),
				"DataDictionaries should contain #VAR1");
	}

	private void submitIdentifyDataDictionaryCandidatesJob(final EntityId projectId, final EntityId moduleId) {
		submitJob(jobManager, new IdentifyCandidatesJob(projectId, new ModuleMatcher(Arrays.asList(moduleId), Collections.emptyList())));
	}
}
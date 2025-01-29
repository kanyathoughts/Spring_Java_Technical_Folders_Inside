/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.StringContains.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import innowake.mining.shared.model.job.Message;
import org.junit.jupiter.api.Test;

import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.candidate.CandidateServiceProvider;
import innowake.mining.client.service.job.JobServiceProvider;
import innowake.mining.data.access.postgres.ModulePgDao;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.JobStatus;
import innowake.mining.test.util.JobStatusUtil;

/**
 * Integration tests for the {@link CandidateServiceProvider}.
 */
class IdentifyCandidatesTest extends IntegrationTest {

	private final CandidateServiceProvider candidateServiceProvier = MiningApiClient.candidateService(getConnectionInfo());
	private final JobServiceProvider jobServiceProvider = MiningApiClient.jobService(getConnectionInfo());
	
	private static final Long ONE = Long.valueOf(1);
	private final String IDENTIFIED_MODULE_MSG = "Identified 1 Module(s) in selection which are supported by the Candidate identification, i.e. Cobol and Natural Program modules.";

	/**
	 * Tests that the REST service invocation for the candidate identification properly triggers a job that successfully executes.
	 * 
	 * @throws IOException if the REST calls cannot be executed
	 */
	@Test
	void testCandidateIdentification() throws IOException {
		/* submit job */
		final List<String> modulePaths = new ArrayList<>();
		modulePaths.add("src/cobol/programs/PRGA.cbl");
		modulePaths.add("src/cobol/programs/PRGB.cbl");
		final Result<String> result = candidateServiceProvier.identifyAllCandidates().setProjectId(ONE).setModulePaths(modulePaths).execute();
		assertNotNull(result);
		assertEquals(202, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		final String jobId = result.getValue().get();
		
		/* wait for the job to finish */
		final Instant start = Instant.now();
		JobInformation jobInfo = null;
		while (Duration.between(start, Instant.now()).toMinutes() < 2) {
			final Result<JobInformation> jobInfoResult = jobServiceProvider.getJobInfo().setJobId(jobId).execute();
			assertNotNull(jobInfoResult);
			assertTrue(jobInfoResult.getValue().isPresent());
			jobInfo = jobInfoResult.getValue().get();
			
			final JobStatus status = jobInfo.getStatus();
			if (status != JobStatus.RUNNING && status != JobStatus.SCHEDULED) {
				break;
			}
		}
		
		assertNotNull(jobInfo);
		assertEquals(jobId, jobInfo.getJobId());
		assertEquals(JobStatus.SUCCESS, jobInfo.getStatus());
	}

	/**
	 * checks the log output {@link JobInformation} of the candidate identification for a module without sourceCode.
	 * 
	 * @throws IOException if the REST call was not successful
	 */
	@Test
	void testCandidateIdentificationWithoutSourceCode() throws IOException {
		final String modulePath = "src/cobol/programs/PRGTEST.cbl";
		/* Submit job */
		final Result<String> result = candidateServiceProvier.identifyAllCandidates().setProjectId(ONE).setModulePaths(Arrays.asList(modulePath)).execute();
		assertNotNull(result);
		assertEquals(202, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		final String jobId = result.getValue().get();
		final JobInformation jobInformation = JobStatusUtil.checkJobStatus(jobId, jobServiceProvider);
		final List<Message> messages = jobInformation.getMessages();
		final var module = new ModulePgDao(getDataSource()).findAnyModuleId(q -> q.withPath(modulePath)).orElseThrow();
		assertEquals(3, messages.size());
		final var textWithoutUid = messages.get(0).getText().replaceAll("uid=[0-9a-z-]+", "uid=someUid");
		assertEquals(String.format(SOURCE_CODE_NOT_FOUND, module.getNid(), "PRGTEST"), textWithoutUid);
		assertEquals(IDENTIFIED_MODULE_MSG, messages.get(1).getText());
		assertThat(messages.get(2).getText(), containsString("1 module(s) were successful."));
	}
	
	/**
	 * Tests Identify Candidate support for CICS BMS_MAPSET.
	 * 
	 * @throws IOException if the REST call was not successful
	 */
	@Test
	void testCandidateIdentificationOfBmsMapSet() throws IOException {
		final Result<String> result = candidateServiceProvier.identifyAllCandidates()
				.setProjectId(ONE)
				.setModulePaths(Arrays.asList("src/cobol/maps/UISCOPE.map"))
				.execute();
		assertNotNull(result);
		assertEquals(202, result.getStatusCode());
		assertTrue("identify candidate should return proper jobId", result.getValue().isPresent());
		final String jobId = result.getValue().get();
		final JobInformation jobInformation = JobStatusUtil.checkJobStatus(jobId, jobServiceProvider);
		assertEquals(2, jobInformation.getMessages().size());
		assertEquals("UISCOPE.map should be identified as a candidate", IDENTIFIED_MODULE_MSG, jobInformation.getMessages().get(0).getText());
		assertThat(jobInformation.getMessages().get(1).getText(), containsString("1 module(s) were successful."));
	}
}


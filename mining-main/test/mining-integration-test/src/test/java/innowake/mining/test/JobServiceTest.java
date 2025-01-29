/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test;

import static org.hamcrest.core.StringContains.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.time.Duration;
import java.time.Instant;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

import org.hamcrest.CoreMatchers;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;

import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.job.JobServiceProvider;
import innowake.mining.client.service.module.ModuleServiceProvider;
import innowake.mining.shared.datapoints.definition.filters.FilterOperators;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.model.JobInfoFieldName;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.JobStatus;

/**
 * Tests for the Job REST services.
 */
class JobServiceTest extends IntegrationTest {

	private final ModuleServiceProvider moduleServiceProvider = MiningApiClient.moduleService(getConnectionInfo());
	private final JobServiceProvider jobServiceProvider = MiningApiClient.jobService(getConnectionInfo());
	private static final Long ONE = Long.valueOf(1);
	private static final String MODULE_PATH_1 = "src/cobol/programs/EXECSQL.cbl";

	private static final String MODULE_PATH_2 =  "src/cobol/programs/PRGA.cbl";
	
	/**
	 * Tests querying of the {@link JobInformation} for a single specific job.
	 * 
	 * @throws IOException if the REST call was not successful
	 */
	@Test
	void testRequestSingleJobInfoByModulePath() throws IOException {
		/* Submit job */
		final Result<String> result = moduleServiceProvider.identifyModuleDescriptions().setProjectId(ONE).setModulePaths(Arrays.asList(MODULE_PATH_1)).execute();
		assertNotNull(result);
		assertEquals(202, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		final String jobId = result.getValue().get();
		
		/* Wait until job is done by polling the info */
		final Instant start = Instant.now();
		JobInformation jobInfo = null;
		boolean jobIsDone = false;
		do {
			final Result<JobInformation> jobInfoResult = jobServiceProvider.getJobInfo().setJobId(jobId).execute();
			assertNotNull(jobInfoResult);
			assertTrue(jobInfoResult.getValue().isPresent());
			jobInfo = jobInfoResult.getValue().get();
			assertNotNull(jobInfo);

			final JobStatus status = jobInfo.getStatus();
			if (status == JobStatus.RUNNING || status == JobStatus.SCHEDULED) {
				assertNull(jobInfo.getResultStatus());
				assertNull(jobInfo.getFinishTime());
			} else if (status == JobStatus.SUCCESS || status == JobStatus.FAILURE ||
						status == JobStatus.CANCELED || status == JobStatus.TIMEOUT) {
				jobIsDone = true;
			}
		} while ( ! jobIsDone && Duration.between(start, Instant.now()).toMinutes() < 2);

		/* Check for meaningful job information */
		final Result<JobInformation> jobInfoResult = jobServiceProvider.getJobInfo().setJobId(jobId).execute();
		assertNotNull(jobInfoResult);
		assertTrue(jobInfoResult.getValue().isPresent());
		jobInfo = jobInfoResult.getValue().get();
		assertNotNull(jobInfo);
		assertEquals(jobId, jobInfo.getJobId());
		assertEquals(JobStatus.SUCCESS, jobInfo.getStatus());
		assertNotNull(jobInfo.getSubmitTime());
		assertNotNull(jobInfo.getScheduledStartTime());
		assertNotNull(jobInfo.getStartTime());
		assertNotNull(jobInfo.getFinishTime());
		assertThat(jobInfo.getJobDescription(), CoreMatchers.containsString("Module description identification for the project Demo Project A"));
		assertNotNull(jobInfo.getStepDescription());
		assertNotNull(jobInfo.getUserName());
		assertNull(jobInfo.getEta());
		assertNotNull(jobInfo.getMessages());
		assertEquals(2, jobInfo.getMessages().size());
		MatcherAssert.assertThat(jobInfo.getMessages().get(1).getText(), containsString("1 module(s) were successful."));
	}

	/**
	 * Tests querying of the {@link JobInformation} for a single specific job.
	 *
	 * @throws IOException if the REST call was not successful
	 */
	@Test
	void testRequestSingleJobInfoByModuleIds() throws IOException {

		final Result<ModulePojo> moduleResult = moduleServiceProvider.findModuleByPath().setProjectId(ONE).setPath(MODULE_PATH_2).execute();
		final ModulePojo module = moduleResult.getValue().get();
		/* Submit job */
		final Result<String> result = moduleServiceProvider.identifyModuleDescriptions().setProjectId(ONE).setModuleIds(Arrays.asList(module.identity())).execute();
		assertNotNull(result);
		assertEquals(202, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		final String jobId = result.getValue().get();

		/* Wait until job is done by polling the info */
		final Instant start = Instant.now();
		JobInformation jobInfo = null;
		boolean jobIsDone = false;
		do {
			final Result<JobInformation> jobInfoResult = jobServiceProvider.getJobInfo().setJobId(jobId).execute();
			assertNotNull(jobInfoResult);
			assertTrue(jobInfoResult.getValue().isPresent());
			jobInfo = jobInfoResult.getValue().get();
			assertNotNull(jobInfo);

			final JobStatus status = jobInfo.getStatus();
			if (status == JobStatus.RUNNING || status == JobStatus.SCHEDULED) {
				assertNull(jobInfo.getResultStatus());
				assertNull(jobInfo.getFinishTime());
			} else if (status == JobStatus.SUCCESS || status == JobStatus.FAILURE ||
					status == JobStatus.CANCELED || status == JobStatus.TIMEOUT) {
					jobIsDone = true;
			}
		} while ( ! jobIsDone && Duration.between(start, Instant.now()).toMinutes() < 2);

		/* Check for meaningful job information */
		final Result<JobInformation> jobInfoResult = jobServiceProvider.getJobInfo().setJobId(jobId).execute();
		assertNotNull(jobInfoResult);
		final Result<byte[]> streamedLogResult = jobServiceProvider.getJobLogStreamed().setJobId(jobId).execute();
		assertNotNull(streamedLogResult);
		assertTrue(streamedLogResult.getValue().isPresent());
		assertTrue(jobInfoResult.getValue().isPresent());
		jobInfo = jobInfoResult.getValue().get();
		assertNotNull(jobInfo);
		assertEquals(jobId, jobInfo.getJobId());
		assertEquals(JobStatus.SUCCESS, jobInfo.getStatus());
		assertNotNull(jobInfo.getSubmitTime());
		assertNotNull(jobInfo.getScheduledStartTime());
		assertNotNull(jobInfo.getStartTime());
		assertNotNull(jobInfo.getFinishTime());
		assertThat(jobInfo.getJobDescription(), CoreMatchers.containsString("Module description identification for the project Demo Project A"));
		assertNotNull(jobInfo.getStepDescription());
		assertNotNull(jobInfo.getUserName());
		assertNull(jobInfo.getEta());
		assertNotNull(jobInfo.getMessages());
		assertEquals(2, jobInfo.getMessages().size());
		MatcherAssert.assertThat(jobInfo.getMessages().get(1).getText(), containsString("1 module(s) were successful."));
	}
	
	/**
	 * Tests querying of two {@link JobInformation}s with the {@link FilterOperators} IN operator on the job Ids. 
	 * 
	 * @throws IOException if the REST call was not successful
	 */
	@Test
	void testFilterObjectQueryWithTwoJobIds() throws IOException {
		final String[] jobIds = submitTwoJobs();
		
		/* Filter against the information contained in the database */
		final Map<JobInfoFieldName, Map<String, Object>> filterObject = new HashMap<>();
		filterObject.put(JobInfoFieldName.ID, Map.of(FilterOperators.OPERATOR_IN, List.of(jobIds[0], jobIds[1])));
		
		final Result<JobInformation[]> jobIdInResult = jobServiceProvider.getJobInfos().setFilter(filterObject).execute();
		assertJobIds(jobIdInResult, jobIds[0], jobIds[1]);
	}
	
	/**
	 * Tests querying of two {@link JobInformation}s with the {@link FilterOperators} IN operator on the job Ids and status value. 
	 * 
	 * @throws IOException if the REST call was not successful
	 */
	@Test
	void testFilterObjectQueryWithTwoJobIdsAndStatus() throws IOException {
		final String[] jobIds = submitTwoJobs();
		
		/* Filter against the information contained in the database */
		final Map<JobInfoFieldName, Map<String, Object>> filterObject = new HashMap<>();
		filterObject.put(JobInfoFieldName.ID, Map.of(FilterOperators.OPERATOR_IN, List.of(jobIds[0], jobIds[1])));
		filterObject.put(JobInfoFieldName.STATUS, Map.of(FilterOperators.OPERATOR_EQ, JobStatus.SUCCESS.name()));

		final Result<JobInformation[]> jobIdInResult = jobServiceProvider.getJobInfos().setFilter(filterObject).execute();
		assertJobIds(jobIdInResult, jobIds[0], jobIds[1]);
	}
	
	/**
	 * Tests querying of two {@link JobInformation}s with the RSQL IN operator on the job Ids and finish time value. 
	 * 
	 * @throws IOException if the REST call was not successful
	 */
	@Test
	void testFilterObjectQueryWithMultipleJobIdsAndFinishTime() throws IOException {
		final String[] jobIds = submitTwoJobs();
		
		/* Filter against the information contained in the database */
		final Map<JobInfoFieldName, Map<String, Object>> filterObject = new HashMap<>();
		filterObject.put(JobInfoFieldName.ID, Map.of(FilterOperators.OPERATOR_IN, List.of(jobIds[0], jobIds[1])));
		filterObject.put(JobInfoFieldName.FINISH_TIME, Map.of(FilterOperators.OPERATOR_LTE, Instant.now().toString()));

		final Result<JobInformation[]> jobIdInResult = jobServiceProvider.getJobInfos().setFilter(filterObject).execute();
		assertJobIds(jobIdInResult, jobIds[0], jobIds[1]);
	}
	
	private void assertJobIds(final Result<JobInformation[]> jobIdResult, final String expectedJobId1, final String expectedJobId2) {
		assertTrue(jobIdResult.getValue().isPresent());
		final JobInformation[] jobIds = jobIdResult.getValue().get();
		assertEquals(2, jobIds.length);
		assertContainsJobId(jobIds, expectedJobId1);
		assertContainsJobId(jobIds, expectedJobId2);
	}
	
	private void assertContainsJobId(final JobInformation[] jobInfos, final String expectedJobId) {
		final List<JobInformation> jobInfoList = Arrays.asList(jobInfos);
		final Optional<JobInformation> jobInfo = jobInfoList.stream().filter(info -> expectedJobId.equals(info.getJobId())).findAny();
		assertTrue(jobInfo.isPresent());
	}
	
	private String[] submitTwoJobs() throws IOException {
		/* Submit two jobs */
		final Result<String> result1 = moduleServiceProvider.identifyModuleDescriptions().setProjectId(ONE)
				.setModulePaths(Arrays.asList(MODULE_PATH_1)).execute();
		final Result<String> result2 = moduleServiceProvider.identifyModuleDescriptions().setProjectId(ONE)
				.setModulePaths(Arrays.asList(MODULE_PATH_2)).execute();
		
		assertNotNull(result1);
		assertEquals(202, result1.getStatusCode());
		assertTrue(result1.getValue().isPresent());
		final String jobId1 = result1.getValue().get();
		
		assertNotNull(result2);
		assertEquals(202, result2.getStatusCode());
		assertTrue(result2.getValue().isPresent());
		final String jobId2 = result2.getValue().get();
		
		/* Wait for both jobs to finish */
		final Instant start = Instant.now();
		JobInformation jobInfo1 = null;
		JobInformation jobInfo2 = null;
		while (Duration.between(start, Instant.now()).toMinutes() < 2) {
			try {
				TimeUnit.MILLISECONDS.sleep(100);
			} catch (final InterruptedException e) {
				throw new IllegalStateException(e);
			}
			
			/* FilterObject against the information contained in the cluster */
			final Map<JobInfoFieldName, Map<String, Object>> filterObject = new HashMap<>();
			filterObject.put(JobInfoFieldName.ID, Map.of(FilterOperators.OPERATOR_IN, List.of(jobId1, jobId2)));
			filterObject.put(JobInfoFieldName.SUBMIT_TIME, Map.of(FilterOperators.OPERATOR_LTE, Instant.now().toString()));

			final Result<JobInformation[]> jobIdInResult = jobServiceProvider.getJobInfos().setFilter(filterObject).execute();
			assertTrue(jobIdInResult.getValue().isPresent());
			final JobInformation[] jobIdIn = jobIdInResult.getValue().get();
			assertEquals(2, jobIdIn.length);
			
			jobInfo1 = jobIdIn[1];
			jobInfo2 = jobIdIn[0];
			final JobStatus jobStatus1 = jobInfo1.getStatus();
			final JobStatus jobStatus2 = jobInfo2.getStatus();
			if (jobStatus1 != JobStatus.RUNNING && jobStatus1 != JobStatus.SCHEDULED
					&& jobStatus2 != JobStatus.RUNNING && jobStatus2 != JobStatus.SCHEDULED) {
				break;
			}
		}
		
		assertNotNull(jobInfo1);
		assertEquals(jobId1, jobInfo1.getJobId());
		assertEquals(JobStatus.SUCCESS, jobInfo1.getStatus());
		
		assertNotNull(jobInfo2);
		assertEquals(jobId2, jobInfo2.getJobId());
		assertEquals(JobStatus.SUCCESS, jobInfo2.getStatus());
		
		return new String[] { jobId1, jobId2 };
	}
}

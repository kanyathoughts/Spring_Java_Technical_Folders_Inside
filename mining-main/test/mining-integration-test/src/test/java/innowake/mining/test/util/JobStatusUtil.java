/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.time.Duration;
import java.time.Instant;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import innowake.mining.client.service.Result;
import innowake.mining.client.service.job.JobServiceProvider;
import innowake.mining.shared.datapoints.definition.filters.FilterOperators;
import innowake.mining.shared.model.JobInfoFieldName;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.JobStatus;

/**
 * This utility class can be used to check status of job.
 */
public class JobStatusUtil {

	/**
	 * Checks the job status and returns the {@link JobInformation} of the job id. 
	 *
	 * @param jobId the Id of the job
	 * @param jobServiceProvider the {@link JobServiceProvider}
	 * @return {@link JobInformation} of the submitted job id
	 * @throws IOException if the REST call was not successful 
	 */
	public static JobInformation checkJobStatus(final String jobId, final JobServiceProvider jobServiceProvider) throws IOException {
		final Instant start = Instant.now();
		JobInformation jobInfo = null;
		while (Duration.between(start, Instant.now()).toMinutes() < 2) {
			try {
				TimeUnit.MILLISECONDS.sleep(100);
			} catch (final InterruptedException e) {
				throw new IllegalStateException(e);
			}
			
			/* Filter against the information contained in the cluster */
			final Map<JobInfoFieldName, Map<String, Object>> filterObject = new HashMap<>();
			filterObject.put(JobInfoFieldName.ID, Map.of(FilterOperators.OPERATOR_EQ, jobId));
			filterObject.put(JobInfoFieldName.SUBMIT_TIME, Map.of(FilterOperators.OPERATOR_LTE, Instant.now().toString()));
			final Result<JobInformation[]> jobIdInResult = jobServiceProvider.getJobInfos().setFilter(filterObject).execute();
			assertTrue(jobIdInResult.getValue().isPresent());
			final JobInformation[] jobIdIn = jobIdInResult.getValue().get();
			assertEquals(1, jobIdIn.length);
			
			jobInfo = jobIdIn[0];
			final JobStatus jobStatus = jobInfo.getStatus();
			if (jobStatus != JobStatus.RUNNING && jobStatus != JobStatus.SCHEDULED) {
				break;
			}
		}
		
		assertNotNull(jobInfo);
		assertEquals(jobId, jobInfo.getJobId());
		assertEquals(JobStatus.SUCCESS, jobInfo.getStatus());
		/* Check for meaningful job information */
		final Result<JobInformation> jobInfoResult = jobServiceProvider.getJobInfo().setJobId(jobId).execute();
		assertNotNull(jobInfoResult);
		assertTrue(jobInfoResult.getValue().isPresent());
		jobInfo = jobInfoResult.getValue().get();
		assertNotNull(jobInfo);
		return jobInfo;
	}
	
	/**
	 * Checks if the job status is FAILING and returns the {@link JobInformation} of the job id. 
	 *
	 * @param jobId the Id of the job
	 * @param jobServiceProvider the {@link JobServiceProvider}
	 * @return {@link JobInformation} of the submitted job id
	 * @throws IOException if the REST call was not successful 
	 */
	public static JobInformation checkFailingJobStatus(final String jobId, final JobServiceProvider jobServiceProvider) throws IOException {
		final Instant start = Instant.now();
		JobInformation jobInfo = null;
		while (Duration.between(start, Instant.now()).toMinutes() < 2) {
			try {
				TimeUnit.MILLISECONDS.sleep(100);
			} catch (final InterruptedException e) {
				throw new IllegalStateException(e);
			}

			/* Filter against the information contained in the cluster */
			final Map<JobInfoFieldName, Map<String, Object>> filterObject = new HashMap<>();
			filterObject.put(JobInfoFieldName.ID, Map.of(FilterOperators.OPERATOR_EQ, jobId));
			filterObject.put(JobInfoFieldName.SUBMIT_TIME, Map.of(FilterOperators.OPERATOR_LTE, Instant.now().toString()));
			final Result<JobInformation[]> jobIdInResult = jobServiceProvider.getJobInfos().setFilter(filterObject).execute();
			assertTrue(jobIdInResult.getValue().isPresent());
			final JobInformation[] jobIdIn = jobIdInResult.getValue().get();
			assertEquals(1, jobIdIn.length);
			jobInfo = jobIdIn[0];
			final JobStatus jobStatus = jobInfo.getStatus();
			if (jobStatus != JobStatus.RUNNING && jobStatus != JobStatus.SCHEDULED) {
				break;
			}
		}
		assertNotNull(jobInfo);
		assertEquals(jobId, jobInfo.getJobId());
		assertEquals(JobStatus.FAILURE, jobInfo.getStatus());
		return jobInfo;
	}
}

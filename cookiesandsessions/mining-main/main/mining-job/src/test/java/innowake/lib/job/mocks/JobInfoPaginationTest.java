/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.lib.job.mocks;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.Assert.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

import com.hazelcast.map.IMap;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.TestApplication;
import innowake.lib.job.api.JobInfoService;
import innowake.lib.job.api.JobInfoService.JobInfoInquiryBuilder;
import innowake.lib.job.api.management.JobInformation;
import innowake.lib.job.internal.JobConfiguration;
import innowake.lib.job.internal.JobInfo;
import innowake.lib.job.internal.hazelcast.HzJobManager;
import innowake.lib.junit.extend.TestLevelCondition;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.entities.JobInfoPojo;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.JobInfoFieldName;
import innowake.mining.shared.model.job.JobStatus;

/**
 * Tests pagination and sorting for jobs.
 */
@SpringBootTest
@ContextConfiguration(classes = TestApplication.class)
@ExtendWith(TestLevelCondition.class)
@RunWith(SpringRunner.class)
class JobInfoPaginationTest {
	
	@Nullable
	@Mock
	private JobInfoService jobInfoService;
	
	@Nullable
	@Mock
	private IMap<String, JobInfo> jobs;

	@Autowired
	@Qualifier(JobConfiguration.JOB_MANAGER_ID)
	protected HzJobManager jobManager;
	
	@BeforeEach
	void setUp() {
		MockitoAnnotations.openMocks(this);
		assertNotNull(jobManager).setJobInfoService(assertNotNull(jobInfoService));
		assertNotNull(jobManager).setJobs(assertNotNull(jobs));
		when(assertNotNull(jobInfoService).find(any())).thenReturn(mockDataForJobInfoService());
		when(assertNotNull(jobs).values())
				.thenReturn(Collections.singleton(createJobInfo(new UUID(4l, 4l), "RunningJob", JobStatus.RUNNING, 3, Instant.now().plus(4, ChronoUnit.MINUTES))));
	}
	
	/**
	 * Tests pagination and sorting on jobs for page 1.
	 */
	@Test
	void testPaginationAndSortingForJobsWithPage1() {
		testAndAssertPagination(0, "RunningJob", "TimeoutJob");
	}
	
	/**
	 * Tests pagination and sorting on jobs for page 2.
	 */
	@Test
	void testPaginationAndSortingForJobsWithPage2() {
		testAndAssertPagination(1, "FailureJob", "CancelJob");
	}
	
	/**
	 * Tests pagination and sorting on jobs for page 3 without any jobs to show.
	 */
	@Test
	void testPaginationAndSortingForForJobsWithPage3() {
		final Pagination pagination = Pagination.at(2, 2);
		final Paged<JobInformation> jobsPage = jobManager.getJobs(pagination, q -> q.sortBy(JobInfoFieldName.SUBMIT_TIME, SortDirection.DESCENDING));
		assertEquals(Long.valueOf(4l), jobsPage.getTotalElements());
		assertEquals(0, jobsPage.getSize());
		assertEquals(Long.valueOf(5), jobsPage.getFirstElement());
		assertTrue(jobsPage.getContent().isEmpty());
	}
	
	/**
	 * Tests pagination and sorting on jobs for first page without any jobs to show.
	 */
	@Test
	void testPaginationAndSortingForNoJobs() {
		when(assertNotNull(jobInfoService).find(any())).thenReturn(Collections.emptyList());
		when(assertNotNull(jobs).values()).thenReturn(Collections.emptyList());
		final Pagination pagination = Pagination.at(0, 2);
		final Paged<JobInformation> jobsPage = jobManager.getJobs(pagination, q -> q.sortBy(JobInfoFieldName.SUBMIT_TIME, SortDirection.DESCENDING));
		assertEquals(Long.valueOf(0l), jobsPage.getTotalElements());
		assertTrue(jobsPage.getContent().isEmpty());
	}
	
	/**
	 * Tests sorting on jobs by job id.
	 */
	@Test
	void testPaginationAndSortByJobId() {
		assertPageAndJobsInfo("RunningJob", "TimeoutJob", Pagination.at(0, 2), q -> q.sortBy(JobInfoFieldName.ID, SortDirection.DESCENDING));
	}
	
	/**
	 * Tests sorting on jobs by job name.
	 */
	@Test
	void testPaginationAndSortByJobName() {
		assertPageAndJobsInfo("TimeoutJob", "RunningJob", Pagination.at(0, 2), q -> q.sortBy(JobInfoFieldName.NAME, SortDirection.DESCENDING));
	}
	
	/**
	 * Tests sorting on jobs by submit time.
	 */
	@Test
	void testPaginationAndSortBySubmitTime() {
		testForSortBy(q -> q.sortBy(JobInfoFieldName.SUBMIT_TIME, SortDirection.ASCENDING));
	}
	
	/**
	 * Tests sorting on jobs by scheduled start time.
	 */
	@Test
	void testPaginationAndSortByScheduledStartTime() {
		testForSortBy(q -> q.sortBy(JobInfoFieldName.SCHEDULED_START_TIME, SortDirection.ASCENDING));
	}
	
	/**
	 * Tests sorting on jobs by start time.
	 */
	@Test
	void testPaginationAndSortByStartTime() {
		assertPageAndJobsInfo("FailureJob", "CancelJob", Pagination.at(1, 2), q -> q.sortBy(JobInfoFieldName.START_TIME, SortDirection.DESCENDING));
	}
	
	/**
	 * Tests sorting on jobs by finish time.
	 */
	@Test
	void testPaginationAndSortByFinishTime() {
		assertPageAndJobsInfo("RunningJob", "TimeoutJob", Pagination.at(0, 2), q -> q.sortBy(JobInfoFieldName.FINISH_TIME, SortDirection.DESCENDING));
	}
	
	/**
	 * Tests sorting on jobs by status.
	 */
	@Test
	void testPaginationAndSortByStatus() {
		assertPageAndJobsInfo("CancelJob", "TimeoutJob", Pagination.at(0, 2), q -> q.sortBy(JobInfoFieldName.STATUS, SortDirection.DESCENDING));
	}
	
	/**
	 * Tests sorting on jobs by total working units.
	 */
	@Test
	void testPaginationAndSortByTotalWorkUnits() {
		assertPageAndJobsInfo("FailureJob", "RunningJob", Pagination.at(1, 2), q -> q.sortBy(JobInfoFieldName.TOTAL_WORK_UNITS, SortDirection.DESCENDING));
	}
	
	/**
	 * Tests sorting on jobs by multiple fields like user name and submit time in ascending order with duplicate user names on jobs.
	 */
	@Test
	void testPaginationAndSortByUserNameAndSubmitTimeAsc() {
		when(assertNotNull(jobInfoService).find(any())).thenReturn(mockDataForJobInfoService());
		when(assertNotNull(jobs).values())
				.thenReturn(Collections.singleton(createJobInfo(new UUID(4l, 4l), "RunningJob", JobStatus.RUNNING, 3, Instant.now().plus(4, ChronoUnit.MINUTES))));
		assertPageAndJobsInfo("CancelJob", "FailureJob", Pagination.at(0, 2), q -> q.sortBy(JobInfoFieldName.CREATED_BY_USER_ID, SortDirection.ASCENDING)
																					.sortBy(JobInfoFieldName.SUBMIT_TIME, SortDirection.ASCENDING));
	}

	/**
	 * Tests sorting on jobs by multiple columns like user name and submit time descending order with duplicate user names on jobs.
	 */
	@Test
	void testPaginationAndSortByUserNameAndSubmitTimeDesc() {
		when(assertNotNull(jobInfoService).find(any())).thenReturn(mockDataForJobInfoService());
		when(assertNotNull(jobs).values())
				.thenReturn(Collections.singleton(createJobInfo(new UUID(4l, 4l), "RunningJob", JobStatus.RUNNING, 3, Instant.now().plus(4, ChronoUnit.MINUTES))));
		assertPageAndJobsInfo("RunningJob", "TimeoutJob", Pagination.at(0, 2), q -> q.sortBy(JobInfoFieldName.CREATED_BY_USER_ID, SortDirection.ASCENDING)
																					 .sortBy(JobInfoFieldName.SUBMIT_TIME,SortDirection.DESCENDING));
	}
	
	/**
	 * Tests pagination on jobs without sort.
	 */
	@Test
	void testPaginationWithoutSortBy() {
		assertPageAndJobsInfo("CancelJob", "FailureJob", Pagination.at(0, 2), q -> {});
	}
	
	private void testForSortBy(final BuildingConsumer<JobInfoInquiryBuilder> builder) {
		assertPageAndJobsInfo("CancelJob", "FailureJob", Pagination.at(0, 2), builder);
	}
	
	private void testAndAssertPagination(final int pageNumber, final String expJobName1, final String expJobName2) {
		assertPageAndJobsInfo(expJobName1, expJobName2,
								Pagination.at(pageNumber, 2),
								q -> q.sortBy(JobInfoFieldName.SUBMIT_TIME, SortDirection.DESCENDING));
	}
	
	private void assertPageAndJobsInfo(final String expJobName1, final String expJobName2, final Pagination pageable, final BuildingConsumer<JobInfoInquiryBuilder> builder) {
		final Paged<JobInformation> jobsPage = jobManager.getJobs(pageable, builder);
		assertEquals(Long.valueOf(4l), jobsPage.getTotalElements());
		assertEquals(2, jobsPage.getSize());
		final List<JobInformation> jobs = jobsPage.getContent();
		assertEquals(2, jobs.size());
		assertEquals(expJobName1, jobs.get(0).getJobName());
		assertEquals(expJobName2, jobs.get(1).getJobName());
	}
	
	private JobInfo createJobInfo(final UUID jobId, final String jobName, final JobStatus status, final Integer numberOfTask, final Instant time) {
		final JobInfo jobInfo = new JobInfo(jobId.toString(), jobName, "test");
		jobInfo.setSubmitTime(time);
		jobInfo.setScheduledStartTime(time);
		jobInfo.setStartTime(time);
		jobInfo.setFinishTime(time);
		jobInfo.setStatus(status);
		jobInfo.setTotalWorkUnits(numberOfTask.intValue());
		jobInfo.setProcessedWorkUnits(numberOfTask.doubleValue() - 1);
		return jobInfo;
	}

	private List<JobInfoPojo> mockDataForJobInfoService() {
		final List<JobInfoPojo> jobsList = new ArrayList<>();
		jobsList.add(createPojo(new UUID(1l, 1l), "CancelJob", JobStatus.CANCELED, 10, Instant.now().plus(1, ChronoUnit.MINUTES)));
		jobsList.add(createPojo(new UUID(2l, 2l), "FailureJob", JobStatus.FAILURE, 5, Instant.now().plus(2, ChronoUnit.MINUTES)));
		jobsList.add(createPojo(new UUID(3l, 3l), "TimeoutJob", JobStatus.TIMEOUT, 15, Instant.now().plus(3, ChronoUnit.MINUTES)));

		return jobsList;
	}

	private JobInfoPojo createPojo(final UUID jobId, final String jobName, final JobStatus status, final Integer numberOfTask, final Instant time) {
		return new JobInfoPojo(jobId,
								jobName,
								"test",
								null,
								status,
								Collections.emptyList(),
								null,
								0,
								numberOfTask.intValue(),
								numberOfTask.doubleValue() - 1,
								time,
								time,
								time,
								time,
								"test");
	}
}

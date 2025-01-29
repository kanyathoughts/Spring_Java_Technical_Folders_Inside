/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.controller;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.shared.access.SortDirection.ASCENDING;
import static innowake.mining.shared.access.SortDirection.DESCENDING;
import static innowake.mining.shared.model.JobInfoFieldName.CREATED_BY_USER_ID;
import static innowake.mining.shared.model.JobInfoFieldName.FINISH_TIME;
import static innowake.mining.shared.model.JobInfoFieldName.NAME;
import static innowake.mining.shared.model.JobInfoFieldName.PROCESSED_WORK_UNITS;
import static innowake.mining.shared.model.JobInfoFieldName.SCHEDULED_START_TIME;
import static innowake.mining.shared.model.JobInfoFieldName.START_TIME;
import static innowake.mining.shared.model.JobInfoFieldName.STATUS;
import static innowake.mining.shared.model.JobInfoFieldName.SUBMIT_TIME;
import static innowake.mining.shared.model.JobInfoFieldName.TOTAL_WORK_UNITS;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.fail;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.multipart;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import innowake.mining.server.controller.cfg.CalculateCFGJob;
import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectReader;

import brave.Span;
import brave.Tracer;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.JobExecutionCallback;
import innowake.lib.job.api.config.properties.JobConfigurationProperties;
import innowake.lib.job.api.config.properties.LogProperties;
import innowake.lib.job.api.management.JobInformation;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.DummyTestJob;
import innowake.mining.server.LongDummyTestJob;
import innowake.mining.server.TestJob;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.controller.discovery.DiscoveryController;
import innowake.mining.server.controller.job.JobController;
import innowake.mining.server.integration.authorization.AbstractUserNameTest;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.MiningJobInfoService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.JobInfoFieldName;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.job.JobStatus;

/**
 * Contains tests for JobController.
 */
@AutoConfigureMockMvc
@TestMethodOrder(OrderAnnotation.class)
@ActiveProfiles(value = Profiles.AUTH_TEST, inheritProfiles = false )
@WithMockUser
class JobControllerTest extends AbstractUserNameTest {

	@Autowired
	private MockMvc mvc;
	
	@Autowired
	private JobManager jobManager;
	
	@Autowired
	private MiningJobInfoService miningJobInfoService;

	@Autowired
	protected Tracer tracer;
	
	@Autowired
	protected ModuleService moduleService;

	private String jobId = "";
	
	@Autowired
	private JobConfigurationProperties jobConfigProperties;

	@BeforeEach
	void initLocal() {
		mockResponseExpectOk("user");
		jobId = submitJob(jobManager, tracer, new TestJob("Example"), true); /* execute some job */
	}
	
	@AfterEach
	void deleteAllJobs() throws Exception {
		/* delete all the jobs */
		mvc.perform(delete("/api" + JobController.JOB_COLLECTION_URL)
			.with(miningUser(roles(admin()))))
			.andExpect(status().is2xxSuccessful());
	}

	@Test
	@Order(12)
	void testUserNameFromKeycloak() throws Exception {
		mvc.perform(get("/api" + JobController.JOB_INFO_URL, jobId))
				.andExpect(status().isOk())
				.andExpect(jsonPath("$.userName").value("Unknown User"));

	}

	@Test
	@Order(13)
	void testJobName() throws Exception {
		mvc.perform(get("/api" + JobController.JOB_INFO_URL, jobId))
				.andExpect(status().isOk())
				.andExpect(jsonPath("$.jobName").value("Example"));
	}

	@Test
	@Order(14)
	void testEmptyJobLog() throws Exception {
		mvc.perform(get("/api" + JobController.JOB_LOG_URL, jobId))
				.andExpect(status().isOk());
		mvc.perform(get("/api" + JobController.JOB_LOG_URL_V2, jobId))
				.andExpect(status().isOk());

	}

	@Test
	@Order(15)
	void testJobLogWhenJobIdNotExist() throws Exception {
		mvc.perform(get("/api" + JobController.JOB_LOG_URL, UUID.randomUUID()))
				.andExpect(status().isNotFound());
		mvc.perform(get("/api" + JobController.JOB_LOG_URL_V2, UUID.randomUUID()))
				.andExpect(status().isNotFound());
	}

	@Test
	@Order(16)
	void testCancellingCompletedJob() throws Exception {
		mvc.perform(put("/api" + JobController.JOB_CANCEL_URL, jobId))
			.andExpect(status().isBadRequest()).andExpect(jsonPath("$.message")
					.value("400 BAD_REQUEST \"Job is not active and cannot be cancelled\""));
	}

	@Test
	@Order(17)
	void testCancellingRunningJob() throws Exception {
		final String jobId = submitJob(jobManager, tracer, new DummyTestJob(), false);
		mvc.perform(put("/api" + JobController.JOB_CANCEL_URL, jobId))
				.andExpect(status().is2xxSuccessful());

		final JobStatus jobStatus = waitForCompletion(jobId);
		assertEquals(JobStatus.CANCELED, jobStatus, "Job must have canceled");

		mvc.perform(put("/api" + JobController.JOB_CANCEL_URL, jobId))
			.andExpect(status().is2xxSuccessful());
	}

	@Test
	@Order(18)
	void testDeletingCompletedJob() throws Exception {
		mvc.perform(get("/api" + JobController.JOB_INFO_URL, jobId))
			.andExpect(status().is2xxSuccessful());

		mvc.perform(delete("/api" + JobController.JOB_URL, jobId))
			.andExpect(status().is2xxSuccessful());

		mvc.perform(get("/api" + JobController.JOB_INFO_URL, jobId))
			.andExpect(status().isNotFound());
	}

	@Test
	@Order(19)
	void testDeletingRunningJob() throws Exception {
		final String jobId = submitJob(jobManager, tracer, new DummyTestJob(), false);
		try {
			mvc.perform(get("/api" + JobController.JOB_INFO_URL, jobId))
				.andExpect(status().is2xxSuccessful());

			mvc.perform(delete("/api" + JobController.JOB_URL, jobId))
				.andExpect(status().is4xxClientError());

			mvc.perform(get("/api" + JobController.JOB_INFO_URL, jobId))
				.andExpect(status().is2xxSuccessful());
		} finally {
			mvc.perform(put("/api" + JobController.JOB_CANCEL_URL, jobId));
			waitForCompletion(jobId);
		}
	}

	@Test
	@Order(20)
	void testDeletingMultipleJobs() throws Exception {
		mockResponseExpectOk("user2");
		final String runningJobId = submitJob(jobManager, tracer, new LongDummyTestJob(), false);
		final String jobIdSubmittedWithDifferentUser = submitSomeJobWithDifferentUser("user2");
		try {
			/* Check if the Jobs exists */
			mvc.perform(get("/api" + JobController.JOB_INFO_URL, runningJobId))
				.andExpect(status().is2xxSuccessful());

			mvc.perform(get("/api" + JobController.JOB_INFO_URL, jobIdSubmittedWithDifferentUser)
				.with(miningUser(roles(admin()))))
				.andExpect(status().is2xxSuccessful());

			/* delete all the jobs */
			mvc.perform(delete("/api" + JobController.JOB_COLLECTION_URL)
				.with(miningUser(roles(admin()))))
				.andExpect(status().is2xxSuccessful());

			/* running job should not get deleted */
			mvc.perform(get("/api" + JobController.JOB_INFO_URL, runningJobId))
				.andExpect(status().is2xxSuccessful());

			/* jobs submitted by a different user should also get deleted */
			mvc.perform(get("/api" + JobController.JOB_INFO_URL, jobIdSubmittedWithDifferentUser)
				.with(miningUser(roles(admin()))))
				.andExpect(status().isNotFound());

			/* completed job should also get deleted */
			mvc.perform(get("/api" + JobController.JOB_INFO_URL, jobId))
				.andExpect(status().isNotFound());
		} finally {
			mvc.perform(put("/api" + JobController.JOB_CANCEL_URL, runningJobId));
			waitForCompletion(runningJobId);
		}
	}
	
	@Test
	@Order(1)
	void testForPaginationForJobsWithPage1() throws Exception {
		final MockHttpServletResponse response = testPaginationAndSortingForJobs("0", "2", List.of(Map.of(SUBMIT_TIME, DESCENDING)));
		processResultAndAssert(response, "4", "2", "2", "Discover Code", "TEST2");	
	}
	
	@Test
	@Order(2)
	void testForPaginationForJobsWithPage2() throws Exception {
		final MockHttpServletResponse response = testPaginationAndSortingForJobs("1", "2", List.of(Map.of(SUBMIT_TIME, DESCENDING)));
		processResultAndAssert(response, "4", "2", "2", "TEST1", "Example");	
	}
	
	@Test
	@Order(3)
	void testForPaginationForJobsWithPage3() throws Exception {
		final MockHttpServletResponse response = testPaginationAndSortingForJobs("3", "2", List.of(Map.of(SUBMIT_TIME, DESCENDING)));
		processResultAndAssert(response, "4", "2", "0");	
	}
	
	@Test
	@Order(21)
	void testForPaginationWithoutJobs() throws Exception {
		deleteAllJobs();
		final MockHttpServletResponse response = mvc.perform(get("/api" + JobController.JOB_COLLECTION_URL)
			    .param("page", "0")
			    .param("size", "2")
			    .param("sortBy", PojoMapper.jsonWriter().writeValueAsString(List.of(Map.of(SUBMIT_TIME, ASCENDING))))
				.with(miningUser(roles(admin()))))
				.andExpect(status().is2xxSuccessful())
				.andReturn()
				.getResponse();
		processResultAndAssert(response, "0", "2", "0");
	}
	
	@Test
	@Order(4)
	void testForSortingofJobsByJobName() throws Exception {
		final MockHttpServletResponse response = testPaginationAndSortingForJobs("0", "2", List.of(Map.of(NAME, DESCENDING)));
		processResultAndAssert(response, "4", "2", "2", "TEST2", "TEST1");	
	}
	
	@Test
	@Order(5)
	void testForSortingofJobsByScheduledStartTime() throws Exception {
		final MockHttpServletResponse response = testPaginationAndSortingForJobs("0", "2", List.of(Map.of(SCHEDULED_START_TIME, ASCENDING)));
		processResultAndAssert(response, "4", "2", "2", "Example", "TEST1");	
	}
	
	@Test
	@Order(6)
	void testForSortingofJobsByStartTime() throws Exception {
		final MockHttpServletResponse response = testPaginationAndSortingForJobs("0", "2", List.of(Map.of(START_TIME, ASCENDING)));
		processResultAndAssert(response, "4", "2", "2", "Example", "TEST1");	
	}
	
	@Test
	@Order(7)
	void testForSortingofJobsByFinishTime() throws Exception {
		final MockHttpServletResponse response = testPaginationAndSortingForJobs("0", "2", List.of(Map.of(FINISH_TIME, ASCENDING)));
		processResultAndAssert(response, "4", "2", "2", "Example", "TEST1");	
	}
	
	@Test
	@Order(8)
	void testForSortingofJobsByStatus() throws Exception {
		final var sortObj = List.of(Map.of(STATUS, ASCENDING), Map.of(SUBMIT_TIME, ASCENDING));
		final MockHttpServletResponse response = testPaginationAndSortingForJobs("0", "4", sortObj);
		processResultAndAssert(response, "4", "4", "4", "Example", "TEST1");	
	}
	
	@Test
	@Order(9)
	void testForSortingofJobsByTotalWorkUnits() throws Exception {
		final var sortObj = List.of(Map.of(TOTAL_WORK_UNITS, DESCENDING), Map.of(NAME, DESCENDING));
		final MockHttpServletResponse response = testPaginationAndSortingForJobs("0", "2", sortObj);
		processResultAndAssert(response, "4", "2", "2", "TEST2", "TEST1");	
	}
	
	@Test
	@Order(10)
	void testForSortingofJobsByProcessedWorkUnits() throws Exception {
		final var sortObj = List.of(Map.of(PROCESSED_WORK_UNITS, DESCENDING), Map.of(NAME, DESCENDING));
		final MockHttpServletResponse response = testPaginationAndSortingForJobs("0", "2", sortObj);
		processResultAndAssert(response, "4", "2", "2", "TEST2", "TEST1");	
	}
	
	@Test
	@Order(11)
	void testForSortingofJobsByUserNameAndSubmitTime() throws Exception {
		final var sortObj = List.of(Map.of(CREATED_BY_USER_ID, ASCENDING), Map.of(SUBMIT_TIME, ASCENDING));
		final MockHttpServletResponse response = testPaginationAndSortingForJobs("0", "2", sortObj);
		processResultAndAssert(response, "4", "2", "2", "Example", "TEST1");	
	}
	
	@Test
	@Order(22)
	void testForSortingofJobsByInvalidColumn() throws Exception {
		triggerTestJobs();
		mvc.perform(get("/api" + JobController.JOB_COLLECTION_URL)
			    .param("page", "0")
			    .param("size", "2")
			    .param("sortBy", PojoMapper.jsonWriter().writeValueAsString(List.of(Map.of("ABC", ASCENDING))))
				.with(miningUser(roles(admin()))))
				.andExpect(status().isBadRequest()).andExpect(jsonPath("$.message")
						.exists());
	}
	
	@Test
	@Order(23)
	void testMiningJobWithoutProjectIdModuleId() throws Exception {
		testAndAssertMiningJob(null, null);
	}

	@Test
	@Order(24)
	void testMiningJobWithProjectId() throws Exception {
		testAndAssertMiningJob(EntityId.of(2L), null);
	}

	@Test
	@Order(25)
	void testMiningJobWithProjectIdModuleId() throws Exception {
		final EntityId testModuleId = createTestModule();
		testAndAssertMiningJob(EntityId.of(2L), testModuleId);
	}
	
	@Test
	@Order(26)
	void testMiningJobInfoDeletionWithJobId() throws Exception {
		final String miningJobId = submitJobAndAssert(null, null, false);
		mvc.perform(delete("/api" + JobController.JOB_URL, miningJobId))
		.andExpect(status().is2xxSuccessful());
		assertEquals(0, getMiningJobInfoCount());
	}
	
	@Test
	@Order(27)
	void testMiningJobInfoDeletionWithSpecificUser() throws Exception {
		submitJobAndAssert(null, null, true);
		/* deleting all jobs with user3 */
		mvc.perform(delete("/api" + JobController.JOB_COLLECTION_URL)
				.with(SecurityMockMvcRequestPostProcessors.user("user3").authorities(clientAdmin())))
				.andExpect(status().is2xxSuccessful());
		assertEquals(0, getMiningJobInfoCount());
	}

	@Test
	@Order(28)
	void testJobExtentionV2WithoutFile() throws Exception {
		/* testing JobExtentionV2 without file */
		mvc.perform(post("/api" +
				JobController.JOB_EXTENSION_V2_URL, 2l, "callchain")
				.with(miningUser(roles(admin()))))
				.andExpect(status().is2xxSuccessful())
				.andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON))
				.andReturn().getResponse();
	}
	
	@Test
	@Order(29)
	void testMiningJobInfoDeletionWithAdminRole() throws Exception {
		deleteAllMiningJobs();
		assertEquals(0, getMiningJobInfoCount());
		/* Submitting multiple jobs */
		submitJob(jobManager, tracer, new DummyTestJob(), true);
		submitJob(jobManager, tracer, new DummyTestJob(), true);
		mockResponseExpectOk("user3");
		submitSomeJobWithDifferentUser("user3");

		assertEquals(3, getMiningJobInfoCount());
		/* deleting all jobs with admin role */
		mvc.perform(delete("/api" + JobController.JOB_COLLECTION_URL)
				.with(miningUser(roles(admin()))))
				.andExpect(status().is2xxSuccessful());
		assertEquals(0, getMiningJobInfoCount());
	}
	
	@Test
	@Order(30)
	void whenFileUploaded_thenVerifyStatus() throws Exception {
		MockMultipartFile file = new MockMultipartFile("file", "hello.txt", MediaType.TEXT_PLAIN_VALUE, "Hello, World!".getBytes());
		mvc.perform(multipart("/api" + JobController.JOB_EXTENSION_V2_URL, 2l, "callchain")
				.file(file)
				.contentType(MediaType.MULTIPART_FORM_DATA)
				.with(miningUser(roles(admin()))))
				.andExpect(status().is2xxSuccessful())
				.andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON));
	}
	
	@Test
	@Order(31)
	void testJobLogStream() throws Exception {
		final LogProperties properties = jobConfigProperties.getLog();
		final File logFile = Paths.get(properties.getLogFolder(), properties.getLogFilePrefix() + jobId + ".log").toFile();
		FileUtils.writeStringToFile(logFile, "Log output for "+ jobId, StandardCharsets.UTF_8);
		final MvcResult result = mvc.perform(get("/api" + JobController.JOB_LOG_URL_V2, jobId)
				.with(miningUser(roles(admin()))))
				.andExpect(status().isOk())
				.andReturn();
		final byte[] zipData = result.getResponse().getContentAsByteArray();
		try (final ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(zipData);
			final ZipInputStream zipInputStream = new ZipInputStream(byteArrayInputStream)) {
			ZipEntry entry;
			while ((entry = zipInputStream.getNextEntry()) != null) {
				if ( ! entry.isDirectory()) {
					final BufferedReader reader = new BufferedReader(new InputStreamReader(zipInputStream, StandardCharsets.UTF_8));
					final StringBuilder logContents = new StringBuilder();
					String line;
					while ((line = reader.readLine()) != null) {
						logContents.append(line);
					}
					final String logContentsString = logContents.toString();
					assertEquals("Log output for "+ jobId, logContentsString);
				}
			}
		}
		logFile.delete();
	}

	@Test
	@Order(32)
	void testCalculateCFGJob() throws Exception {
		final EntityId projectId = EntityId.of(1L);
		final EntityId moduleId = createTestModule();
		final String jobId = submitJob(jobManager, tracer, new CalculateCFGJob(projectId, moduleId), true);
		final JobStatus jobStatus = waitForCompletion(jobId);
		assertEquals(JobStatus.SUCCESS, jobStatus, "Job must have completed successfully");
		assertMiningJob(projectId, moduleId, jobId);
	}

	private String submitSomeJobWithDifferentUser(final String userName) throws Exception {
		final String jobId = mvc.perform(post("/api" + DiscoveryController.DISCOVER_CODE_URL, 1)
				.with(SecurityMockMvcRequestPostProcessors.user(userName).authorities(admin())))
				.andExpect(status().is2xxSuccessful())
				.andReturn().getResponse().getContentAsString().replace("\"", "");

		int times = 5;
		JobInformation info = null;
		while (times-- > 0) {
			TimeUnit.SECONDS.sleep(3);
			final List<JobInformation> infos = jobManager.getJobs(q -> q.byId(UUID.fromString(jobId)));
			info = infos.stream().filter(jobInfo -> jobInfo.getJobId().equals(jobId)).findAny().get();
			if (info.getStatus() == JobStatus.SUCCESS) {
				break;
			}
		}
		if (assertNotNull(info).getStatus() != JobStatus.SUCCESS) {
			fail("Job could not be completed");
		}
		return jobId;
	}

	private JobStatus waitForCompletion(final String jobId) throws Exception {
		JobStatus jobStatus = getJobStatus(jobId);
		for (int i = 0; i < 10 && JobStatus.isActive(jobStatus); i++) {
			TimeUnit.SECONDS.sleep(1);
			jobStatus = getJobStatus(jobId);
		}
		if (JobStatus.isActive(jobStatus)) {
			fail("Job is active even after 10 seconds. Failing the test to prevent the test cases from stalling");
		}
		return jobStatus;
	}

	private JobStatus getJobStatus(final String jobId) {
		final List<JobInformation> jobInfos = jobManager.getJobs(q -> q.byId(UUID.fromString(jobId)));
		assertFalse(jobInfos.isEmpty(), "Could not find any job");

		return jobInfos.get(0).getStatus();
	}

	private static String submitJob(final JobManager jobManager, final Tracer tracer, final Job<?> job, final boolean waitUntilJobCompletes) {
		final Span rootSpan = tracer.newTrace();
		String jobId = "";
		try (final Tracer.SpanInScope scope = tracer.withSpanInScope(rootSpan)) {
			final CountDownLatch latch = new CountDownLatch(1);
			final Throwable[] error = new Throwable[1];
			jobId = jobManager.submit(job, new JobExecutionCallback() {

				@Override
				public void onCompletion() {
					latch.countDown();
				}

				@Override
				public void onFailure(@Nullable final Throwable throwable) {
					error[0] = throwable;
					latch.countDown();
				}
			}).getJobId();

			if (waitUntilJobCompletes) {
				try {
					final boolean countReachedZero = latch.await(10, TimeUnit.MINUTES);
					if ( ! countReachedZero) {
						throw new IllegalStateException(
								"CountDownLatch timed out in JobController.submitJob(), possible deadlock! (" + latch.toString() + ")");
					}
				} catch (final InterruptedException e) {
					throw new IllegalStateException(e);
				}
			}

			if (error[0] != null) {
				throw new IllegalStateException(error[0]);
			}
		} finally {
			rootSpan.finish();
		}
		return jobId;
	}
	
	private MockHttpServletResponse testPaginationAndSortingForJobs(final String page, final String size,
			final List<Map<JobInfoFieldName, SortDirection>> sortObject) throws Exception {
		triggerTestJobs();
		final MockHttpServletResponse response = mvc.perform(get("/api" + JobController.JOB_COLLECTION_URL)
			    .param("page", page)
			    .param("size", size)
			    .param("sortBy", PojoMapper.jsonWriter().writeValueAsString(sortObject))
				.with(miningUser(roles(admin()))))
				.andExpect(status().is2xxSuccessful())
				.andReturn()
				.getResponse();
		return response;
	}
	
	private void processResultAndAssert(final MockHttpServletResponse response, final String... expJobInfo) throws Exception {
		final String jobInfosJson = response.getContentAsString();
		final var mapper = PojoMapper.jsonReader();
		final JsonNode jsonNode = mapper.readTree(jobInfosJson);
		final List<innowake.mining.shared.model.job.JobInformation> jobInfos = mapper.readValue(jsonNode.get("content").traverse(),
				new TypeReference<List<innowake.mining.shared.model.job.JobInformation>>() {});
		assertEquals(Integer.parseInt(expJobInfo[0]), jsonNode.get("totalElements").asInt());
		assertEquals(Integer.parseInt(expJobInfo[1]), jsonNode.get("limit").asInt());
		final int expectedJobInfoSize = Integer.parseInt(expJobInfo[2]);
		assertEquals(expectedJobInfoSize, jobInfos.size());
		if (expectedJobInfoSize > 0) {
			assertEquals(expJobInfo[3], jobInfos.get(0).getJobName());
			assertEquals(expJobInfo[4], jobInfos.get(1).getJobName());
		}
	}
	
	private void triggerTestJobs() throws Exception{
		submitJob(jobManager, tracer, new TestJob("TEST1"), true);
		submitJob(jobManager, tracer, new TestJob("TEST2"), true);
		mockResponseExpectOk("user2");
		submitSomeJobWithDifferentUser("user2");	
	}
	
	private void testAndAssertMiningJob(@Nullable final EntityId projectId, @Nullable final EntityId moduleId)
			throws Exception {
		final String miningJobId = submitJobAndAssert(projectId, moduleId, false);
		assertMiningJob(projectId, moduleId, miningJobId);
	}

	private void assertMiningJob(@Nullable EntityId projectId, @Nullable EntityId moduleId, String miningJobId) throws Exception {
		final MockHttpServletResponse response = mvc.perform(get("/api" + JobController.JOB_INFO_URL, miningJobId))
				.andExpect(status().isOk())
				.andReturn()
				.getResponse();
		final ObjectReader jsonReader = PojoMapper.jsonReader();
		final innowake.mining.shared.model.job.JobInformation jobInfo = jsonReader
				.forType(innowake.mining.shared.model.job.JobInformation.class)
				.readValue(response.getContentAsString());
		assertEquals(miningJobId, jobInfo.getJobId());
		assertEquals(projectId != null ? projectId.getNid() : null, jobInfo.getProjectId());
		assertEquals(moduleId != null ? moduleId.getNid() : null, jobInfo.getModuleId());
	}

	private String submitJobAndAssert(@Nullable final EntityId projectId, @Nullable final EntityId moduleId,
			final boolean withDifferentUser) throws Exception {
		deleteAllMiningJobs();
		assertEquals(0, getMiningJobInfoCount());
		final String miningJobId;
		if (withDifferentUser) {
			mockResponseExpectOk("user3");
			miningJobId = submitSomeJobWithDifferentUser("user3");
		} else {
			final TestJob testJob = projectId == null ? new TestJob("test") : new TestJob(projectId, moduleId, "test");
			miningJobId = submitJob(jobManager, tracer, testJob, true);
		}
		assertEquals(1, getMiningJobInfoCount());
		return miningJobId;
	}

	private EntityId createTestModule() {
		return moduleService.create(new ModulePojoPrototype()
				.setProject(EntityId.of(2L))
				.setName("test")
				.setTechnology(Technology.COBOL)
				.setType(Type.PROGRAM)
				.setOrigin(Origin.CUSTOM)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setCreator(Creator.DISCOVERY)
			);
	}

	private long getMiningJobInfoCount() {
		return miningJobInfoService.count(q -> {});
	}

	private void deleteAllMiningJobs() {
		miningJobInfoService.delete(q -> {});
	}
}



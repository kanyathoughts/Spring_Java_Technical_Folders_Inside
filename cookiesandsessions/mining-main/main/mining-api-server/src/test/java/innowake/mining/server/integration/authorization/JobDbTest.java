/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Paths;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.hamcrest.CoreMatchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;

import innowake.lib.job.internal.JobConfiguration;
import innowake.lib.job.api.config.properties.JobConfigurationProperties;
import innowake.lib.job.api.config.properties.LogProperties;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.config.security.MiningRole;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.tags.AuthorizationTest;

/**
 * Tests to validate the Job operations in IAM profile
 * 
 */
@ActiveProfiles(value = Profiles.AUTH_TEST, inheritProfiles = false )
@AutoConfigureMockMvc
@WithMockUser
@AuthorizationTest
class JobDbTest extends AbstractUserNameTest {
	private static final String ADMIN_ROLE = "admin";
	private static final String EDITOR_ROLE = "client-1-project-2-editor";
	private static final String MINING_NATURE = "client-1-project-2-mining";
	private static final String DISCOVERY_NATURE = "client-1-project-2-discovery";
	private static final String DISCOVERY_LIGHT_NATURE = "client-1-project-2-discovery-light";
	private static final String USER_1 = "5678";
	private static final String USER_2 = "5679";
	
	@Autowired
	private MockMvc mvc;
	
	@Autowired
	private JobConfigurationProperties jobConfigProperties;
	@Autowired
	@Qualifier(JobConfiguration.NODE_NAME)
	private String nodeName;
	
	private String testJobId = "";
	
	/**
	 * Method to submit a test job that can then be used in test cases to validate
	 * the endpoints.
	 * 
	 * @throws Exception while making the HTTP call / retrieving the content from the response
	 */
	@BeforeEach
	public void submitTestJobs() throws Exception {
		mockResponseExpectOk(USER_1);
		mockResponseExpectOk(USER_2);
		final MvcResult postResult = mvc
				.perform(post("/api/v1/projects/2/control-flow/2003").contentType(MediaType.APPLICATION_JSON)
						.with(SecurityMockMvcRequestPostProcessors.user(USER_1)
					.authorities(new MiningRole(EDITOR_ROLE),
							new MiningRole(MINING_NATURE))))
				.andExpect(status().isAccepted()).andReturn();
		testJobId = postResult.getResponse().getContentAsString().replaceAll("\"", "").trim();
	}

	/**
	 * Reset the test data after every test method.
	 * @throws IOException If an IO related error occurs.
	 */
	@AfterEach
	public void resetData() throws IOException {
		resetTestData();
	}

	/**
	 * Validates - Users with Admin role can access the jobs of other users
	 */
	@Test
	void testAdminShouldBeAbleToRetrieveAllJobs() throws Exception {
		final MvcResult result = mvc
				.perform(get("/api/v1/jobs").with(SecurityMockMvcRequestPostProcessors.user(USER_2).authorities(new MiningRole(ADMIN_ROLE))))
				.andExpect(status().isOk()).andReturn();

		final String jobInfosJson = result.getResponse().getContentAsString();
		final List<JobInformation> jobInfos = mapJobInfos(jobInfosJson);
		assertTrue(jobInfos.stream().map(JobInformation::getJobId).anyMatch(jobId -> jobId.equals(testJobId)));
	}

	/**
	 * Validates - User with Admin role can access the jobs information of other user
	 */
	@Test
	void testShouldGetJobInfoAsAdmin() throws Exception {
		final MvcResult result = mvc.perform(
				get("/api/v1/jobs/{submittedJobId}/info", testJobId).contentType(MediaType.APPLICATION_JSON)
						.with(SecurityMockMvcRequestPostProcessors.user(USER_2)
								.authorities(new MiningRole(ADMIN_ROLE))))
				.andExpect(status().isOk()).andReturn();
		
		final String jobInfosJson = result.getResponse().getContentAsString();
		final JobInformation jobInfos = PojoMapper.jsonReaderFor(JobInformation.class).readValue(jobInfosJson);
		assertEquals(testJobId, jobInfos.getJobId()); 
	}

	/**
	 * Validates - Forbidden when non-authorized user with regular role cancels the
	 * job
	 */
	@Test
	void testShouldNotCancelJobForUnauthorizedUser() throws Exception {
		mvc.perform(put("/api/v1/jobs/{submittedJobId}/cancel", testJobId)
				.with(SecurityMockMvcRequestPostProcessors.user(USER_2)
						.authorities(new MiningRole(EDITOR_ROLE), new MiningRole(MINING_NATURE))))
				.andExpect(status().isForbidden());
	
	}
	
	/**
	 * Validates - Authorized user with regular role can cancel the job
	 */
	@Test
	void testAuthorizedUserCancelJob() throws Exception {
		mvc.perform(put("/api/v1/jobs/{submittedJobId}/cancel", testJobId)
				.with(SecurityMockMvcRequestPostProcessors.user(USER_1)
						.authorities(new MiningRole(EDITOR_ROLE), new MiningRole(MINING_NATURE))))
				.andExpect(status().isOk());
	}
	
	/**
	 * Validates - Forbidden when non-authorized user with regular role access the job log
	 */
	@Test
	void testShouldNotGetJobLogForUnauthorizedUser() throws Exception {
			mvc.perform(get("/api/v1/jobs/{submittedJobId}/log", testJobId)
							.with(SecurityMockMvcRequestPostProcessors.user(USER_2)
											.authorities(new MiningRole(EDITOR_ROLE), new MiningRole(MINING_NATURE))))
							.andExpect(status().isForbidden());
	}

	/**
	 * Validates - Authorized user with regular role can access the job log
	 */
	@Test
	void testAuthenticatedUserJobLog() throws Exception {
		final String log = "Job used to test the endpoints";
		final LogProperties properties = jobConfigProperties.getLog();
		final File logFile = Paths.get(properties.getLogFolder(), properties.getLogFilePrefix() + testJobId + ".log").toFile();
		FileUtils.writeStringToFile(logFile, log, Charset.forName("Cp1252"));
		
		final MvcResult result = mvc.perform(get("/api/v1/jobs/{submittedJobId}/log", testJobId)
						.with(SecurityMockMvcRequestPostProcessors.user(USER_1)
										.authorities(new MiningRole(EDITOR_ROLE), new MiningRole(MINING_NATURE))))
						.andExpect(status().isOk()).andReturn();
		
		/* {USSNDJPK8Q13-1:Job used to test the endpoints2021-11-26 16:02:42.529 [hz.fervent_joliot.cached.thread-6] INFO  innowake.mining.data.core.Utils - Parsing of MMRS7102 (#292:210) took 0 ms.\r\n} */
		final String actual = result.getResponse().getContentAsString().replaceAll("\"", "").trim();
		/* test that job log contains the expected log entry */
		assertThat(actual, CoreMatchers.containsString("{" + nodeName + ":" + log));
		logFile.delete();
	}

	/**
	 * Validates - That it returns the jobs that are submitted only by him, as the Job is submitted with USER_1,
	 * it won't return that job when accessed with USER_2
	 */
	@Test
	void testGetJobInformationWithDiffUser() throws Exception {
		final MvcResult result = mvc
				.perform(get("/api/v1/jobs").with(SecurityMockMvcRequestPostProcessors.user(USER_2)
						.authorities(new MiningRole(EDITOR_ROLE), new MiningRole(MINING_NATURE))))
				.andExpect(status().isOk()).andReturn();

		final String jobInfosJson = result.getResponse().getContentAsString();
		final List<JobInformation> jobInfos = mapJobInfos(jobInfosJson);
		assertEquals(0, jobInfos.size());
	}
	
	/**
	 * Validates - That it returns information for jobs the user is authorized for.
	 */
	@Test
	void testShouldGetAllJobsForAuthorizedUser() throws Exception {
		final MvcResult result = mvc
				.perform(get("/api/v1/jobs").with(SecurityMockMvcRequestPostProcessors.user(USER_1)
						.authorities(new MiningRole(EDITOR_ROLE), new MiningRole(MINING_NATURE))))
				.andExpect(status().isOk()).andReturn();

		final String jobInfosJson = result.getResponse().getContentAsString();
		final List<JobInformation> jobInfos = mapJobInfos(jobInfosJson);
		assertTrue(jobInfos.stream().map(JobInformation::getJobId).anyMatch(jobId -> jobId.equals(testJobId)));
	}
	
	/**
	 * Validates - That it returns information for jobs of the user with diff natures.
	 */
	@Test
	void testShouldGetJobsForDiscoveryNature() throws Exception {
		final MvcResult result = mvc
				.perform(get("/api/v1/jobs").with(SecurityMockMvcRequestPostProcessors.user(USER_1)
						.authorities(new MiningRole(EDITOR_ROLE), new MiningRole(DISCOVERY_NATURE))))
				.andExpect(status().isOk()).andReturn();

		final String jobInfosJson = result.getResponse().getContentAsString();
		final List<JobInformation> jobInfos = mapJobInfos(jobInfosJson);
		assertTrue(jobInfos.stream().map(JobInformation::getJobId).anyMatch(jobId -> jobId.equals(testJobId)));
	}
	
	/**
	 * Validates - That it returns information for jobs of the user with diff natures.
	 */
	@Test
	void testShouldGetJobsForDiscoveryLightNature() throws Exception {
		final MvcResult result = mvc
				.perform(get("/api/v1/jobs").with(SecurityMockMvcRequestPostProcessors.user(USER_1)
						.authorities(new MiningRole(EDITOR_ROLE), new MiningRole(DISCOVERY_LIGHT_NATURE))))
				.andExpect(status().isOk()).andReturn();

		final String jobInfosJson = result.getResponse().getContentAsString();
		final List<JobInformation> jobInfos = mapJobInfos(jobInfosJson);
		assertTrue(jobInfos.stream().map(JobInformation::getJobId).anyMatch(jobId -> jobId.equals(testJobId)));
	}

	/**
	 * Validates only authorized user with regular role can access the Job
	 * information
	 */
	@Test
	void testShouldGetJobInfoForAuthorizedUser() throws Exception {
		final MvcResult result = mvc.perform(get("/api/v1/jobs/{submittedJobId}/info", testJobId).contentType(MediaType.APPLICATION_JSON)
				.with(SecurityMockMvcRequestPostProcessors.user(USER_1)
						.authorities(new MiningRole(EDITOR_ROLE), new MiningRole(MINING_NATURE))))
				.andExpect(status().isOk()).andReturn();
		
		final String jobInfosJson = result.getResponse().getContentAsString();
		final JobInformation jobInfos = PojoMapper.jsonReaderFor(JobInformation.class).readValue(jobInfosJson);
		assertEquals(testJobId, jobInfos.getJobId()); 
	}
	
	/**
	 * Validates - Forbidden when non-authorized user with regular role access the
	 * job result
	 */
	@Test
	void testShouldNotGetJobResultForUnauthorizedUser() throws Exception {
		mvc.perform(get("/api/v1/jobs/{submittedJobId}/result", testJobId).contentType(MediaType.APPLICATION_JSON)
				.with(SecurityMockMvcRequestPostProcessors.user(USER_2)
				.authorities(new MiningRole(EDITOR_ROLE), new MiningRole(MINING_NATURE)))).andExpect(status().isForbidden());
	}

	/**
	 * Validates - Forbidden when non-authorized user with regular role access the
	 * job info
	 */
	@Test
	void testShouldNotGetJobInfoForUnauthorizedUser() throws Exception {
		mvc.perform(get("/api/v1/jobs/{submittedJobId}/info", testJobId).contentType(MediaType.APPLICATION_JSON)
				.with(SecurityMockMvcRequestPostProcessors.user(USER_2)
						.authorities(new MiningRole(EDITOR_ROLE), new MiningRole(MINING_NATURE))))
				.andExpect(status().isForbidden());
	}
	
	private List<JobInformation> mapJobInfos(final String jobInfosJson) throws Exception {
		final var mapper = PojoMapper.jsonReader();
		final JsonNode jsonNode = mapper.readTree(jobInfosJson);
		final List<JobInformation> jobInfos = mapper.readValue(jsonNode.get("content").traverse(), new TypeReference<List<JobInformation>>() {});
		return jobInfos;
	}

	@Override
	protected ResetScriptFile getScriptFile() {
		return ResetScriptFile.COMPLETE;
	}

}

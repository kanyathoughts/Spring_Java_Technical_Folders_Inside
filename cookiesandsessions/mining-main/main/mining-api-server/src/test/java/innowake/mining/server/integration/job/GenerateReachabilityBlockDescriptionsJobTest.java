/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.job;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import innowake.mining.server.error.UserFacingException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;

import brave.Tracer;
import innowake.lib.job.api.JobInfoService;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Severity;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.integration.discovery.BaseDiscoveryTest;
import innowake.mining.server.job.genai.GenerateReachabilityBlockDescriptionsJob;
import innowake.mining.server.job.genai.MonitoredTaskManager;
import innowake.mining.server.job.genai.MonitoredTaskManagerService;
import innowake.mining.server.service.GenAIAvailabilityService;
import innowake.mining.server.service.GenerativeReachabilityBlockDescriptionService;
import innowake.mining.server.service.GenerativeReachabilityBlockDescriptionService.ReachabilityDescriptionResult;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;

/**
 * Tests the bulk reachability block description generation. We don't actually create any blocks in the DB:
 * We mock the {@linkplain FunctionalBlockService} and make it return certain blocks for certain uids.
 * The {@linkplain GenerativeReachabilityBlockDescriptionService} and {@linkplain GenAIAvailabilityService} are also mocked
 * and we check that the {@link GenerateReachabilityBlockDescriptionsJob} calls them correctly.
 */
@WithMockUser
class GenerateReachabilityBlockDescriptionsJobTest extends DatabaseRelatedTest {
	
	private static final EntityId PROJECT_ID = EntityId.of(1l);
	
	private static final UUID UUID_EMPTY_DESCRIPTION = UUID.fromString("b44ca2ce-6b77-458f-b871-d2110bcc029e");
	private static final UUID UUID_NULL_DESCRIPTION = UUID.fromString("e0e9217c-1c93-48bf-a2d3-d86a0c514520");
	private static final UUID UUID_EXISTING_DESCRIPTION = UUID.fromString("47d463cd-3652-473e-96b0-cee6427e7dfa");
	private static final UUID UUID_BLANK_DESCRIPTION = UUID.fromString("b69a3910-7f3c-4cfb-b448-447a626ff96d");
	private static final UUID UUID_NO_TYPE = UUID.fromString("99fb4782-5fea-433a-a5be-8e55af33fed2");
	private static final List<String> UUIDS_VALID = Arrays.asList(UUID_EMPTY_DESCRIPTION.toString(), UUID_NULL_DESCRIPTION.toString(), UUID_EXISTING_DESCRIPTION.toString(), UUID_BLANK_DESCRIPTION.toString());
	private static final List<String> UUIDS_INVALID = Arrays.asList(UUID_NO_TYPE.toString(), "abc", "00000000-0000-0000-0000-000000000000");

	private FunctionalBlockPojo blockEmptyDescription;
	private FunctionalBlockPojo blockNullDescription;
	private FunctionalBlockPojo blockExistingDescription;
	private FunctionalBlockPojo blockBlankDescription;
	private FunctionalBlockPojo blockWithoutType;

	@MockBean
	private transient GenerativeReachabilityBlockDescriptionService generativeReachabilityBlockDescriptionService;
	
	@MockBean
	private transient GenAIAvailabilityService availabilityService;

	@MockBean
	private transient FunctionalBlockService blockService;

	@MockBean
	private transient MonitoredTaskManagerService monitoredTaskManagerService;

	@MockBean
	private ProgressMonitor progressMonitor;

	@Autowired
	private JobManager jobManager;

	@Autowired
	private JobInfoService jobInfoService;
	
	@Autowired
	private Tracer tracer;

	@BeforeEach
	void setupTestDataAndMocks() {
		final Map<String, Object> flags = new HashMap<>();
		final Map<String, Object> secondFlags = new HashMap<>();
		flags.put("TYPE", Collections.singletonList(FunctionalBlockType.REACHABILITY));
		secondFlags.put("TYPE", Collections.singletonList(FunctionalBlockType.RA_TOP_DOWN));
		blockEmptyDescription = new FunctionalBlockPojo(UUID_EMPTY_DESCRIPTION, null, PROJECT_ID, null, null, null, "block1", "", flags, null);
		blockNullDescription = new FunctionalBlockPojo(UUID_NULL_DESCRIPTION, null, PROJECT_ID, null, null, null, "block2", null, flags, null);
		blockExistingDescription = new FunctionalBlockPojo(UUID_EXISTING_DESCRIPTION, null, PROJECT_ID, null, null, null, "block3",
				"this is an awesome block", flags, null);
		blockBlankDescription = new FunctionalBlockPojo(UUID_BLANK_DESCRIPTION, null, PROJECT_ID, null, null, null, "block4", " 	", flags, null);
		blockWithoutType = new FunctionalBlockPojo(UUID_NO_TYPE, null, PROJECT_ID, null, null, null, "block5", "", null, null);
		Mockito.when(blockService.find(UUID_EMPTY_DESCRIPTION)).thenReturn(Optional.of(blockEmptyDescription));
		Mockito.when(blockService.find(UUID_NULL_DESCRIPTION)).thenReturn(Optional.of(blockNullDescription));
		Mockito.when(blockService.find(UUID_EXISTING_DESCRIPTION)).thenReturn(Optional.of(blockExistingDescription));
		Mockito.when(blockService.find(UUID_BLANK_DESCRIPTION)).thenReturn(Optional.of(blockBlankDescription));
		Mockito.when(blockService.find(UUID_NO_TYPE)).thenReturn(Optional.of(blockWithoutType));
		ReachabilityDescriptionResult reachabilityDescriptionResult = new ReachabilityDescriptionResult("AI generated description");
		Mockito.when(generativeReachabilityBlockDescriptionService.generateDescription(any(EntityId.class), any(FunctionalBlockPojo.class), anyBoolean(), any(),
				any(), any())).thenReturn(reachabilityDescriptionResult);
		final ExecutorService executorService = Executors.newFixedThreadPool(5);
		MonitoredTaskManager monitoredTaskManager = new MonitoredTaskManager(progressMonitor, executorService, 4);
		Mockito.when(monitoredTaskManagerService.newTaskManager(any(), anyInt())).thenReturn(monitoredTaskManager);
	}
	
	@Test
	void testServiceUnavailable() {
		Mockito.doReturn(false).when(availabilityService).isGenAIServiceAvailable();
		final var parameters = new HashMap<String, List<String>>();
		parameters.put("uids", UUIDS_VALID);
		final GenerateReachabilityBlockDescriptionsJob job = new GenerateReachabilityBlockDescriptionsJob(PROJECT_ID, parameters);
		final String jobId = BaseDiscoveryTest.submitJob(jobManager, tracer, job);

		verify(availabilityService, times(1)).isGenAIServiceAvailable();
		verify(generativeReachabilityBlockDescriptionService, times(0)).generateDescription(any(EntityId.class), any(UUID.class), anyBoolean());
		verify(generativeReachabilityBlockDescriptionService, times(0)).generateDescription(any(EntityId.class), any(FunctionalBlockPojo.class), anyBoolean(), any(),
				any(), any());
		verify(monitoredTaskManagerService, times(0)).newTaskManager(any(), anyInt());


		final Serializable jobResult = jobManager.getJobResult(jobId);
		assertNotNull(jobResult);
		assertEquals(Result.class, jobResult.getClass());
		final Result<?> result = (Result<?>) jobResult;
		assertEquals(Severity.ERROR, result.status.getSeverity());
	}
	
	@Test
	void testRunJobNoOverwrite() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		final var parameters = new HashMap<String, List<String>>();
		parameters.put("uids", UUIDS_VALID);
		final GenerateReachabilityBlockDescriptionsJob job = new GenerateReachabilityBlockDescriptionsJob(PROJECT_ID, parameters);
		final String jobId = BaseDiscoveryTest.submitJob(jobManager, tracer, job);
		verify(generativeReachabilityBlockDescriptionService, times(1)).generateDescription(any(EntityId.class), eq(blockEmptyDescription), anyBoolean(), any(), any(), any());
		verify(generativeReachabilityBlockDescriptionService, times(1)).generateDescription(any(EntityId.class), eq(blockNullDescription), anyBoolean(), any(), any(), any());
		verify(generativeReachabilityBlockDescriptionService, times(0)).generateDescription(any(EntityId.class), eq(blockExistingDescription), anyBoolean(), any(), any(), any());
		verify(generativeReachabilityBlockDescriptionService, times(1)).generateDescription(any(EntityId.class), eq(blockBlankDescription), anyBoolean(), any(), any(), any());
		verify(monitoredTaskManagerService, times(1)).newTaskManager(any(), anyInt());

		final Serializable jobResult = jobManager.getJobResult(jobId);
		assertNotNull(jobResult);
		assertEquals(Result.class, jobResult.getClass());
		final Result<?> result = (Result<?>) jobResult;
		assertEquals(Severity.OK, result.status.getSeverity());
	}
	
	@Test
	void testRunJobOverwrite() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		final var parameters = new HashMap<String, List<String>>();
		parameters.put("uids", UUIDS_VALID);
		parameters.put("overwrite", Collections.singletonList("true"));
		final GenerateReachabilityBlockDescriptionsJob job = new GenerateReachabilityBlockDescriptionsJob(PROJECT_ID, parameters);
		final String jobId = BaseDiscoveryTest.submitJob(jobManager, tracer, job);
		verify(generativeReachabilityBlockDescriptionService, times(4)).generateDescription(any(EntityId.class), any(FunctionalBlockPojo.class), anyBoolean(), any(), any(), any());
		verify(monitoredTaskManagerService, times(1)).newTaskManager(any(), anyInt());
		final Serializable jobResult = jobManager.getJobResult(jobId);
		assertNotNull(jobResult);
		assertEquals(Result.class, jobResult.getClass());
		final Result<?> result = (Result<?>) jobResult;
		assertEquals(Severity.OK, result.status.getSeverity());
	}
	
	@Test
	void testRunJobInvalidIds() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		final var parameters = new HashMap<String, List<String>>();
		parameters.put("uids", UUIDS_INVALID);
		final GenerateReachabilityBlockDescriptionsJob job = new GenerateReachabilityBlockDescriptionsJob(PROJECT_ID, parameters);
		final String jobId = BaseDiscoveryTest.submitJob(jobManager, tracer, job);
		verify(generativeReachabilityBlockDescriptionService, times(0)).generateDescription(any(EntityId.class), any(UUID.class), anyBoolean());
		verify(generativeReachabilityBlockDescriptionService, times(0)).generateDescription(any(EntityId.class), any(FunctionalBlockPojo.class), anyBoolean(), any(), any(), any());
		verify(monitoredTaskManagerService, times(1)).newTaskManager(any(), anyInt());
		final var jobInfo = jobInfoService.get(UUID.fromString(jobId));
		assertNotNull(jobInfo);
		assertEquals(3, jobInfo.getMessages().size());
		
		final Serializable jobResult = jobManager.getJobResult(jobId);
		assertNotNull(jobResult);
		assertEquals(Result.class, jobResult.getClass());
		final Result<?> result = (Result<?>) jobResult;
		assertEquals(Severity.ERROR, result.status.getSeverity());
	}

	@Test
	void testRunJobFailedInGenAi() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		Mockito.when(generativeReachabilityBlockDescriptionService.generateDescription(any(), any(), anyBoolean())).thenThrow(UserFacingException.class);
		Mockito.when(generativeReachabilityBlockDescriptionService.generateDescription(any(), any(), anyBoolean(), any(), any(), any())).thenThrow(
				UserFacingException.class);
		final var parameters = new HashMap<String, List<String>>();
		parameters.put("uids", UUIDS_VALID);
		parameters.put("overwrite", Collections.singletonList("true"));
		final GenerateReachabilityBlockDescriptionsJob job = new GenerateReachabilityBlockDescriptionsJob(EntityId.of(1l), parameters);
		final String jobId = JobUtil.submitJob(jobManager, tracer, job);
		verify(availabilityService, times(1)).isGenAIServiceAvailable();
		verify(generativeReachabilityBlockDescriptionService, times(4)).generateDescription(any(), any(), anyBoolean(), any(), any(), any());
		verify(monitoredTaskManagerService, times(1)).newTaskManager(any(), anyInt());
		final var jobInfo = jobInfoService.get(UUID.fromString(jobId));
		assertNotNull(jobInfo);
		assertEquals(4, jobInfo.getMessages().size());

		final Serializable jobResult = jobManager.getJobResult(jobId);
		assertNotNull(jobResult);
		assertEquals(Result.class, jobResult.getClass());
		final Result<?> result = (Result<?>) jobResult;
		assertEquals(Severity.ERROR, result.status.getSeverity());
	}

	@Test
	void testRunJobWithErrorInGenAi() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		ReachabilityDescriptionResult reachabilityDescriptionResult = new ReachabilityDescriptionResult(Optional.of("Unable to generate a description for the reachability block because none of the included modules have descriptions."));
		Mockito.when(generativeReachabilityBlockDescriptionService.generateDescription(any(), any(), anyBoolean(), any(), any(), any())).thenReturn(reachabilityDescriptionResult);
		final var parameters = new HashMap<String, List<String>>();
		parameters.put("uids", UUIDS_VALID);
		parameters.put("overwrite", Collections.singletonList("true"));
		final GenerateReachabilityBlockDescriptionsJob job = new GenerateReachabilityBlockDescriptionsJob(EntityId.of(1l), parameters);
		final String jobId = JobUtil.submitJob(jobManager, tracer, job);
		verify(availabilityService, times(1)).isGenAIServiceAvailable();
		verify(generativeReachabilityBlockDescriptionService, times(4)).generateDescription(any(), any(), anyBoolean(), any(), any(), any());

		final var jobInfo = jobInfoService.get(UUID.fromString(jobId));
		assertNotNull(jobInfo);
		assertEquals(4, jobInfo.getMessages().size());

		final Serializable jobResult = jobManager.getJobResult(jobId);
		assertNotNull(jobResult);
		assertEquals(Result.class, jobResult.getClass());
		final Result<?> result = (Result<?>) jobResult;
		assertEquals(Severity.ERROR, result.status.getSeverity());
	}

}

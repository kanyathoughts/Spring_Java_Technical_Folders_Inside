/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.job;

import brave.Tracer;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Severity;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.integration.discovery.BaseDiscoveryTest;
import innowake.mining.server.job.genai.GenerateSingleReachabilityBlockDescriptionJob;
import innowake.mining.server.job.genai.MonitoredTaskManager;
import innowake.mining.server.job.genai.MonitoredTaskManagerService;
import innowake.mining.server.service.GenAIAvailabilityService;
import innowake.mining.server.service.GenerativeReachabilityBlockDescriptionService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * Tests the single reachability block description generation job.
 */
@WithMockUser
class GenerateSingleReachabilityBlockDescriptionJobTest extends DatabaseRelatedTest {

	private static final EntityId PROJECT_ID = EntityId.of(1L);

	private static final UUID UUID_BLOCK = UUID.fromString("b44ca2ce-6b77-458f-b871-d2110bcc029e");
	private static final List<String> UUIDS_BLOCK = Collections.singletonList(UUID_BLOCK.toString());

	private static final String DESCRIPTION = "AI generated description";

	private FunctionalBlockPojo block;

	@MockBean
	private transient FunctionalBlockService blockService;

	@MockBean
	private transient GenerativeReachabilityBlockDescriptionService generativeReachabilityBlockDescriptionService;

	@MockBean
	private transient GenAIAvailabilityService availabilityService;

	@MockBean
	private transient MonitoredTaskManagerService monitoredTaskManagerService;

	@MockBean
	private ProgressMonitor progressMonitor;

	@Autowired
	private JobManager jobManager;

	@Autowired
	private Tracer tracer;

	@BeforeEach
	void setupTestDataAndMocks() {
		final Map<String, Object> flags = new HashMap<>();
		flags.put("TYPE", Collections.singletonList(FunctionalBlockType.REACHABILITY));
		block = new FunctionalBlockPojo(UUID_BLOCK, null, PROJECT_ID, null, null, null, "Block", "", flags, null);
		Mockito.when(blockService.find(UUID_BLOCK)).thenReturn(Optional.of(block));

		final GenerativeReachabilityBlockDescriptionService.ReachabilityDescriptionResult reachabilityDescriptionResult = new GenerativeReachabilityBlockDescriptionService.ReachabilityDescriptionResult(DESCRIPTION);
		Mockito.when(generativeReachabilityBlockDescriptionService.generateDescription(any(EntityId.class), any(FunctionalBlockPojo.class), anyBoolean(), any(), any(), any())).thenReturn(reachabilityDescriptionResult);

		final ExecutorService executorService = Executors.newFixedThreadPool(5);
		final MonitoredTaskManager monitoredTaskManager = new MonitoredTaskManager(progressMonitor, executorService, 4);
		Mockito.when(monitoredTaskManagerService.newTaskManager(any(), anyInt())).thenReturn(monitoredTaskManager);
	}

	@Test
	void testServiceUnavailable() {
		Mockito.doReturn(false).when(availabilityService).isGenAIServiceAvailable();
		final var parameters = new HashMap<String, List<String>>();
		parameters.put("uids", UUIDS_BLOCK);
		final GenerateSingleReachabilityBlockDescriptionJob job = new GenerateSingleReachabilityBlockDescriptionJob(PROJECT_ID, parameters);
		final String jobId = BaseDiscoveryTest.submitJob(jobManager, tracer, job);

		verify(availabilityService, times(1)).isGenAIServiceAvailable();
		verify(generativeReachabilityBlockDescriptionService, times(0)).generateDescription(any(EntityId.class), any(FunctionalBlockPojo.class), anyBoolean(), any(), any(), any());
		verify(monitoredTaskManagerService, times(0)).newTaskManager(any(), anyInt());

		final Serializable jobResult = jobManager.getJobResult(jobId);
		assertNotNull(jobResult);
		assertEquals(Result.class, jobResult.getClass());
		final Result<?> result = (Result<?>) jobResult;
		assertEquals(Severity.ERROR, result.status.getSeverity());
	}

	@Test
	void testServiceAvailable() throws JsonProcessingException {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		final var parameters = new HashMap<String, List<String>>();
		parameters.put("uids", UUIDS_BLOCK);
		final GenerateSingleReachabilityBlockDescriptionJob job = new GenerateSingleReachabilityBlockDescriptionJob(PROJECT_ID, parameters);
		final String jobId = BaseDiscoveryTest.submitJob(jobManager, tracer, job);

		verify(availabilityService, times(1)).isGenAIServiceAvailable();
		verify(generativeReachabilityBlockDescriptionService, times(1)).generateDescription(eq(PROJECT_ID), eq(block), anyBoolean(), any(), any(), any());
		verify(monitoredTaskManagerService, times(1)).newTaskManager(any(), anyInt());
		verify(blockService, times(0)).update(any());

		final Serializable jobResult = jobManager.getJobResult(jobId);
		assertNotNull(jobResult);
		assertEquals(Result.class, jobResult.getClass());
		final Result<?> result = (Result<?>) jobResult;

		assertNotNull(result.value);

		final Map<String, String> map = new ObjectMapper().readValue(result.value.toString(), new TypeReference<HashMap<String, String>>() {
		});
		assertEquals(DESCRIPTION, map.get("description"));
		assertEquals(UUID_BLOCK.toString(), map.get("uid"));
	}

	@Test
	void testMultipleUids() {
		final var parameters = new HashMap<String, List<String>>();
		parameters.put("uids", List.of(UUID.randomUUID().toString(), UUID.randomUUID().toString()));
		final GenerateSingleReachabilityBlockDescriptionJob job = new GenerateSingleReachabilityBlockDescriptionJob(PROJECT_ID, parameters);
		final String jobId = BaseDiscoveryTest.submitJob(jobManager, tracer, job);

		verify(availabilityService, times(0)).isGenAIServiceAvailable();
		verify(generativeReachabilityBlockDescriptionService, times(0)).generateDescription(any(EntityId.class), any(FunctionalBlockPojo.class), anyBoolean(), any(), any(), any());
		verify(monitoredTaskManagerService, times(0)).newTaskManager(any(), anyInt());

		final Serializable jobResult = jobManager.getJobResult(jobId);
		assertNotNull(jobResult);
		assertEquals(Result.class, jobResult.getClass());
		final Result<?> result = (Result<?>) jobResult;
		assertEquals(Severity.ERROR, result.status.getSeverity());
	}
}

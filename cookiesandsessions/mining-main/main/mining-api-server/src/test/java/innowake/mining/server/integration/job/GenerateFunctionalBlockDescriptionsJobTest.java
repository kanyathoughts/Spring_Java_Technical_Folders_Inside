/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.job;

import brave.Tracer;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Severity;
import innowake.lib.job.api.management.JobManager;

import innowake.mining.data.core.SchemaConstants;

import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.integration.discovery.BaseDiscoveryTest;
import innowake.mining.server.job.genai.GenerateFunctionalBlockDescriptionsJob;
import innowake.mining.server.job.genai.GenerateReachabilityBlockDescriptionsJob;
import innowake.mining.server.job.genai.MonitoredTaskManager;
import innowake.mining.server.job.genai.MonitoredTaskManagerService;
import innowake.mining.server.job.genai.MonitoredTaskParameter;
import innowake.mining.server.service.GenAIAvailabilityService;
import innowake.mining.server.service.GenerativeFunctionalBlocksDescriptionService;
import innowake.mining.server.service.GenerativeReachabilityBlockDescriptionService;
import innowake.mining.server.service.GenerativeReachabilityBlockDescriptionService.ReachabilityDescriptionResult;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.GeneratedFrom;
import innowake.mining.shared.entities.testing.AnnotationPojoDummy;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.mockito.ArgumentMatchers.*;
import org.mockito.Mockito;
import static org.mockito.Mockito.anyInt;
import static org.mockito.Mockito.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
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

/**
 * Tests the bulk reachability block description generation. We don't actually create any blocks in the DB:
 * We mock the {@linkplain FunctionalBlockService} and make it return certain blocks for certain uids.
 * The {@linkplain GenerativeReachabilityBlockDescriptionService} and {@linkplain GenAIAvailabilityService} are also mocked
 * and we check that the {@link GenerateReachabilityBlockDescriptionsJob} calls them correctly.
 */
@WithMockUser
class GenerateFunctionalBlockDescriptionsJobTest extends DatabaseRelatedTest {
	
	private static final EntityId PROJECT_ID = EntityId.of(1l);
	
	private static final UUID UUID1 = UUID.fromString("b44ca2ce-6b77-458f-b871-d2110bcc029e");
	private static final UUID UUID2 = UUID.fromString("e0e9217c-1c93-48bf-a2d3-d86a0c514520");
	private static final List<String> UUIDS_VALID = Arrays.asList(UUID1.toString(), UUID2.toString());

	private FunctionalBlockPojo block1;
	private FunctionalBlockPojo block2;
	private UUID moduleUid = UUID.randomUUID();
	private UUID MODULE_1_ID = UUID.randomUUID();
	private EntityId ANNOTATION_1_ID = EntityId.of(11l);
	private EntityId ANNOTATION_2_ID = EntityId.of(12l);

	@MockBean
	private transient GenerativeFunctionalBlocksDescriptionService generativeFunctionalBlocksDescriptionService;
	
	@MockBean
	private transient GenAIAvailabilityService availabilityService;

	@MockBean
	private transient FunctionalBlockService blockService;
	@MockBean
	private transient AnnotationService annotationService;
	@MockBean
	private transient MonitoredTaskManagerService monitoredTaskManagerService;

    @MockBean
	private transient MonitoredTaskParameter monitoredTaskParameter;

	@MockBean
	private ProgressMonitor progressMonitor;

	@Autowired
	private JobManager jobManager;


	@Autowired
	private Tracer tracer;

	@BeforeEach
	void setupTestDataAndMocks() {
		final Map<String, Object> flags = new HashMap<>();
		final Map<String, Object> secondFlags = new HashMap<>();
		flags.put("TYPE", Collections.singletonList(FunctionalBlockType.REACHABILITY));
		secondFlags.put("TYPE", Collections.singletonList(FunctionalBlockType.RA_TOP_DOWN));
		block1 = new FunctionalBlockPojo(UUID1, null, PROJECT_ID, null, null, null, "block1", "", flags, null);
		block2 = new FunctionalBlockPojo(UUID2, null, PROJECT_ID, null, null, null, "block2", null, flags, null);
		Mockito.when(blockService.find(UUID1)).thenReturn(Optional.of(block1));
		Mockito.when(blockService.find(UUID2)).thenReturn(Optional.of(block2));
		var annotation1b = AnnotationPojoDummy.build(new AnnotationPojoPrototype()
				.setId(ANNOTATION_1_ID.getNid().toString())
				.setSourceAttachment("content 1")
				.setCreatedByUserId(SchemaConstants.SYSTEM_USER)
				.setUpdatedByUserId(SchemaConstants.SYSTEM_USER)
				.setModule(EntityId.of(MODULE_1_ID))
				.setName("description of annotation 1")
		);
		var annotation2b = AnnotationPojoDummy.build(new AnnotationPojoPrototype()
				.setId(ANNOTATION_2_ID.getNid().toString())
				.setSourceAttachment("content 2")
				.setCreatedByUserId(SchemaConstants.SYSTEM_USER)
				.setUpdatedByUserId(SchemaConstants.SYSTEM_USER)
				.setModule(EntityId.of(MODULE_1_ID))
				.setName("description of annotation 2")
		);

		ReachabilityDescriptionResult reachabilityDescriptionResult = new ReachabilityDescriptionResult("AI generated description");
		Map<UUID, List<FunctionalBlockPojo>> map = new HashMap<>();
		map.put(UUID1 ,List.of(block1));
		map.put(UUID2,List.of(block2));
		when(blockService.findChildrenDeep(anyList(), anyInt(), any())).thenReturn(map);

		when(blockService.getGeneratedFrom(anyList()))
				.thenReturn(Map.of(moduleUid, GeneratedFrom.fromAnnotation(ANNOTATION_1_ID)));

		when(annotationService.find(any())).thenReturn(List.of(annotation1b, annotation2b));
		reachabilityDescriptionResult.getResult();
		Mockito.when(generativeFunctionalBlocksDescriptionService.generateAnnotationsDescription(any(), any(), anyList(), any(), anyBoolean())).thenReturn("AI generated description");
		final ExecutorService executorService =  Executors.newFixedThreadPool(5);
		MonitoredTaskManager monitoredTaskManager = new MonitoredTaskManager( progressMonitor, executorService, 4);
		Mockito.when(monitoredTaskManagerService.newTaskManager(any(), anyInt())).thenReturn(monitoredTaskManager);
	}
	
	@Test
	void testServiceUnavailable() {
		Mockito.doReturn(false).when(availabilityService).isGenAIServiceAvailable();
		final var parameters = new HashMap<String, List<String>>();
		parameters.put("uids", UUIDS_VALID);
		final GenerateFunctionalBlockDescriptionsJob job = new GenerateFunctionalBlockDescriptionsJob(PROJECT_ID, parameters);
		final String jobId = BaseDiscoveryTest.submitJob(jobManager, tracer, job);

		verify(availabilityService, times(1)).isGenAIServiceAvailable();
		verify(generativeFunctionalBlocksDescriptionService, times(0)).generateAnnotationsDescription(any(), any(), anyList(), any(), anyBoolean());
		verify(monitoredTaskManagerService, times(0)).newTaskManager(any(), anyInt());

		final Serializable jobResult = jobManager.getJobResult(jobId);
		assertNotNull(jobResult);
		assertEquals(Result.class, jobResult.getClass());
		final Result<?> result = (Result<?>) jobResult;
		assertEquals(Severity.ERROR, result.status.getSeverity());
	}

	@Test
	void testRunJobForTwoFunctionalBlock() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		final var parameters = new HashMap<String, List<String>>();
		parameters.put("functionalBlockIds", UUIDS_VALID);
		final GenerateFunctionalBlockDescriptionsJob job = new GenerateFunctionalBlockDescriptionsJob(PROJECT_ID, parameters);
		final String jobId = BaseDiscoveryTest.submitJob(jobManager, tracer, job);

		verify(generativeFunctionalBlocksDescriptionService, times(2)).generateAnnotationsDescription(any(), any(), anyList(), any(), anyBoolean());
		verify(monitoredTaskManagerService, times(1)).newTaskManager(any(), anyInt());

		final Serializable jobResult = jobManager.getJobResult(jobId);
		assertNotNull(jobResult);
		assertEquals(Result.class, jobResult.getClass());
		final Result<?> result = (Result<?>) jobResult;
		assertEquals(Severity.OK, result.status.getSeverity());
	}

}

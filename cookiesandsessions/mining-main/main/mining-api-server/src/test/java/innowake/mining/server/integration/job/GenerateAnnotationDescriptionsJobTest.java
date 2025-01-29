/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.job;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

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
import innowake.mining.data.core.SchemaConstants;
import innowake.mining.server.error.PermissionException;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.integration.discovery.BaseDiscoveryTest;
import innowake.mining.server.job.genai.GenerateAnnotationDescriptionsJob;
import innowake.mining.server.job.genai.MonitoredTaskManager;
import innowake.mining.server.job.genai.MonitoredTaskManagerService;
import innowake.mining.server.service.GenAIAvailabilityService;
import innowake.mining.server.service.GenerativeAnnotationTranslationService;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.testing.AnnotationPojoDummy;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.WorkingState;

/**
 * Tests the bulk annotation description generation.
 * The {@linkplain GenerativeAnnotationTranslationService} is also mocked and we check that the {@link GenerateAnnotationDescriptionsJob} calls it correctly.
 */
@WithMockUser
class GenerateAnnotationDescriptionsJobTest extends DatabaseRelatedTest {
	
	/**
	 * OF.
	 */
	private static final EntityId PROJECT_ID = EntityId.of(1L);
	private AnnotationPojo systemIdentifiedAnnotation1;
	private AnnotationPojo systemIdentifiedAnnotation2;
	private AnnotationPojo systemIdentifiedUserModifiedAnnotation;
	
	private Long annotation1Id = 20000l;
	private Long annotation2Id = 20001l;
	
	@MockBean
	AnnotationService annotationService;
	
	@MockBean
	GenerativeAnnotationTranslationService translateService;

	@MockBean
	GenAIAvailabilityService availabilityService;

	@MockBean
	MonitoredTaskManagerService monitoredTaskManagerService;

	@MockBean
	ProgressMonitor progressMonitor;

	@Autowired
	private JobManager jobManager;
	
	@Autowired
	private Tracer tracer;
	
	@Autowired
	private JobInfoService jobInfoService;
	
	@BeforeEach
	void setupTestDataAndMocks() {
		systemIdentifiedAnnotation1 = AnnotationPojoDummy.build(
				new AnnotationPojoPrototype()
						.setName("Busienss Rule Candidate [System Identified]")
						.setCreatedByUserId(SchemaConstants.SYSTEM_USER)
						.setUpdatedByUserId("")
						.setState(WorkingState.CANDIDATE)
						.setType(AnnotationType.RULE)
						.setSourceAttachment("ABC")
						.setNid(1l)
		);
		systemIdentifiedAnnotation2 = AnnotationPojoDummy.build(
				new AnnotationPojoPrototype()
						.setName("Busienss Rule Candidate [System Identified]")
						.setCreatedByUserId(SchemaConstants.SYSTEM_USER)
						.setUpdatedByUserId("")
						.setState(WorkingState.CANDIDATE)
						.setType(AnnotationType.RULE)
						.setSourceAttachment("DEF")
						.setNid(2l)
		);
		systemIdentifiedUserModifiedAnnotation = AnnotationPojoDummy.build(
				new AnnotationPojoPrototype()
						.setName("This is a user written description")
						.setCreatedByUserId(SchemaConstants.SYSTEM_USER)
						.setUpdatedByUserId("Some user")
						.setState(WorkingState.CANDIDATE)
						.setType(AnnotationType.RULE)
						.setSourceAttachment("DEF")
						.setNid(3l)
		);

		when(annotationService.find(any())).thenReturn(List.of(systemIdentifiedAnnotation1, systemIdentifiedAnnotation2,systemIdentifiedUserModifiedAnnotation));
		when(annotationService.find(any(), any())).thenReturn(Paged.empty());
		Mockito.when(annotationService.getContent(any(), any(), any())).thenReturn("some test code");
		Mockito.when(annotationService.update(any())).thenReturn(EntityId.VOID);
		Mockito.when(translateService.translateAnnotationUsingGenAI(any(AnnotationPojo.class))).thenReturn("abc");
		final ExecutorService executorService =  Executors.newFixedThreadPool(5);
		MonitoredTaskManager monitoredTaskManagerAnnotation = new MonitoredTaskManager(progressMonitor, executorService , 1);
		Mockito.when(monitoredTaskManagerService.newTaskManager(any(), anyInt())).thenReturn(monitoredTaskManagerAnnotation);
	}

	
	/**
	 * Sets the availabilityService service to return false on call of {@linkplain GenAIAvailabilityService#isGenAIServiceAvailable()} and runs the Job.
	 * We expect the job to return a {@linkplain Severity#ERROR} result and we expect {@linkplain GenerativeAnnotationTranslationService#(String)}
	 * to never be called.
	 */
	@Test
	void testServiceUnavailable() {
		Mockito.doReturn(false).when(availabilityService).isGenAIServiceAvailable();
		final var parameters = new HashMap<String, List<String>>();
		parameters.put("ids", Collections.singletonList(annotation1Id.toString()));
		final GenerateAnnotationDescriptionsJob job = new GenerateAnnotationDescriptionsJob(PROJECT_ID, parameters);
		final String jobId = BaseDiscoveryTest.submitJob(jobManager, tracer, job);

		verify(availabilityService, times(1)).isGenAIServiceAvailable();
		verify(translateService, times(0)).translateAnnotationUsingGenAI(any(AnnotationPojo.class));
		
		final Serializable jobResult = jobManager.getJobResult(jobId);
		assertNotNull(jobResult);
		assertEquals(Result.class, jobResult.getClass());
		final Result<?> result = (Result<?>) jobResult;
		assertEquals(Severity.ERROR, result.status.getSeverity());
	}
	
	/**
	 * Sets the availabilityService service to return true on call of {@linkplain GenAIAvailabilityService#isGenAIServiceAvailable()} and runs the Job.
	 * annotation1 and annotation2 are passed as annotation parameters and the overwrite flag is set to false.
	 * We expect {@linkplain GenerativeAnnotationTranslationService#(String)} to be called twice for the two system identified annotations.
	 */
	@Test
	void testRunJobNoOverwrite() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		final var parameters = new HashMap<String, List<String>>();
		final List<String> annotationIds = new ArrayList<>();
		annotationIds.add(annotation1Id.toString());
		annotationIds.add(annotation2Id.toString());
		parameters.put("ids", annotationIds);
		final GenerateAnnotationDescriptionsJob job = new GenerateAnnotationDescriptionsJob(PROJECT_ID, parameters);
		final String jobId = BaseDiscoveryTest.submitJob(jobManager, tracer, job);

		verify(monitoredTaskManagerService, times(1)).newTaskManager(any(), anyInt());
		verify(availabilityService, times(1)).isGenAIServiceAvailable();
		verify(translateService, times(2)).translateAnnotationUsingGenAI(any(AnnotationPojo.class));
		
		final Serializable jobResult = jobManager.getJobResult(jobId);
		assertNotNull(jobResult);
		assertEquals(Result.class, jobResult.getClass());
		final Result<?> result = (Result<?>) jobResult;
		assertEquals(Severity.OK, result.status.getSeverity());
	}
	
	/**
	 * Sets the availabilityService service to return true on call of {@linkplain GenAIAvailabilityService#isGenAIServiceAvailable()} and runs the Job.
	 * annotation1 and annotation2 are passed as annotation parameters and the overwrite flag is set to true.
	 * We expect {@linkplain GenerativeAnnotationTranslationService#(String)} to be called three times for the two system identified annotations
	 * and the user modified one.
	 */
	@Test
	void testRunJobOverwrite() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		final var parameters = new HashMap<String, List<String>>();
		final List<String> annotationIds = new ArrayList<>();
		annotationIds.add(annotation1Id.toString());
		annotationIds.add(annotation2Id.toString());
		parameters.put("ids", annotationIds);
		parameters.put("overwrite", Collections.singletonList("true"));
		final GenerateAnnotationDescriptionsJob job = new GenerateAnnotationDescriptionsJob(PROJECT_ID, parameters);
		final String jobId = BaseDiscoveryTest.submitJob(jobManager, tracer, job);

		verify(monitoredTaskManagerService, times(1)).newTaskManager(any(), anyInt());
		verify(availabilityService, times(1)).isGenAIServiceAvailable();
		verify(translateService, times(3)).translateAnnotationUsingGenAI(any(AnnotationPojo.class));
		final Serializable jobResult = jobManager.getJobResult(jobId);
		assertNotNull(jobResult);
		assertEquals(Result.class, jobResult.getClass());
		final Result<?> result = (Result<?>) jobResult;
		assertEquals(Severity.OK, result.status.getSeverity());
	}
	
	@Test
	void testRunJobNoPermission() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		Mockito.when(translateService.translateAnnotationUsingGenAI(any(AnnotationPojo.class))).thenThrow(PermissionException.class);
		final var parameters = new HashMap<String, List<String>>();
		final List<String> annotationIds = new ArrayList<>();
		annotationIds.add(annotation1Id.toString());
		annotationIds.add(annotation2Id.toString());
		parameters.put("ids", annotationIds);
		final GenerateAnnotationDescriptionsJob job = new GenerateAnnotationDescriptionsJob(PROJECT_ID, parameters);
		final String jobId = JobUtil.submitJob(jobManager, tracer, job);

		verify(monitoredTaskManagerService, times(1)).newTaskManager(any(), anyInt());
		verify(availabilityService, times(1)).isGenAIServiceAvailable();
		verify(translateService, times(2)).translateAnnotationUsingGenAI(any(AnnotationPojo.class));
		
		final var jobInfo = jobInfoService.get(UUID.fromString(jobId));
		assertNotNull(jobInfo);
		
		assertEquals(2, jobInfo.getMessages().size());
		
		final Serializable jobResult = jobManager.getJobResult(jobId);
		assertNotNull(jobResult);
		assertEquals(Result.class, jobResult.getClass());
		final Result<?> result = (Result<?>) jobResult;
		assertEquals(Severity.WARNING, result.status.getSeverity());
	}

}

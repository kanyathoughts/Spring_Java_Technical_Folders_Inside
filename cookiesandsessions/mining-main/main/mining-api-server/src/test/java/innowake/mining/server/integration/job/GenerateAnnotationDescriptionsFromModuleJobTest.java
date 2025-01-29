/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.job;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
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
import innowake.mining.server.job.genai.GenerateAnnotationDescriptionsFromModuleJob;
import innowake.mining.server.job.genai.MonitoredTaskManager;
import innowake.mining.server.job.genai.MonitoredTaskManagerService;
import innowake.mining.server.service.GenAIAvailabilityService;
import innowake.mining.server.service.GenerativeAnnotationTranslationService;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.JobInfoPojo;
import innowake.mining.shared.entities.testing.AnnotationPojoDummy;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.WorkingState;

/**
 * Tests the bulk annotation description generation. We don't actually create any Annotations or modules in the DB:
 * We mock the {@linkplain AnnotationService} and make it return certain Annotations for certain module IDs.
 * The {@linkplain GenerativeAnnotationTranslationService} is also mocked and we check that the {@link GenerateAnnotationDescriptionsFromModuleJob} calls it correctly.
 */
@WithMockUser
class GenerateAnnotationDescriptionsFromModuleJobTest extends DatabaseRelatedTest {
	
	/**
	 * OF.
	 */
	private static final EntityId PROJECT_ID = EntityId.of(1L);
	private AnnotationPojo systemIdentifiedAnnotation1;
	private AnnotationPojo systemIdentifiedAnnotation2;
	private AnnotationPojo systemIdentifiedUserModifiedAnnotation;
	
	private Long module1Id = 20000l;
	private Long module2Id = 20001l;
	
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
	private JobInfoService jobinfoService;
	
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

		final AnnotationPojo[] annotations = { systemIdentifiedAnnotation2, systemIdentifiedUserModifiedAnnotation };
		when(annotationService.find(any())).then(i -> {
			@SuppressWarnings("unchecked")
			final BuildingConsumer<AnnotationService.AnnotationInquiryBuilder> builderConsumer = (BuildingConsumer<AnnotationService.AnnotationInquiryBuilder>) i.getArgument(0);
			final AnnotationService.AnnotationInquiryBuilder builder = Mockito.mock(AnnotationService.AnnotationInquiryBuilder.class);
			final ArgumentCaptor<EntityId> moduleIdCaptor = ArgumentCaptor.forClass(EntityId.class);

			/* Avoid NPEs when chaining is used when calling filter methods */
			when(builder.ofProject(any())).thenReturn(builder);
			when(builder.ofModule(eq(EntityId.of(module1Id)))).thenReturn(builder);
			when(builder.ofModule(eq(EntityId.of(module2Id)))).thenReturn(builder);

			builderConsumer.prepare(builder);
			Mockito.verify(builder).ofModule(moduleIdCaptor.capture());

			final var captorValue = moduleIdCaptor.getValue();
			if (module1Id.equals(captorValue.getNid())) {
				return Collections.singletonList(systemIdentifiedAnnotation1);
			} else if (module2Id.equals(captorValue.getNid())) {
				return Arrays.asList(annotations);
			} else {
				throw new IllegalStateException("Illegal Value: " + captorValue);
			}
		});
		
		when(annotationService.find(any(), any())).thenReturn(Paged.empty());
		Mockito.when(annotationService.getContent(any(), any(), any())).thenReturn("some test code");
		Mockito.when(annotationService.update(any())).thenReturn(EntityId.VOID);
		Mockito.when(translateService.translateUsingGenAI(any(AnnotationPojo.class), any(EntityId.class), any(EntityId.class))).thenReturn("abc");
		final ExecutorService executorService =  Executors.newFixedThreadPool(5);
		MonitoredTaskManager monitoredTaskManagerModules = new MonitoredTaskManager(progressMonitor, executorService , 1);
		MonitoredTaskManager monitoredTaskManagerAnnotations = new MonitoredTaskManager(progressMonitor, executorService, 1);
		Mockito.when(monitoredTaskManagerService.newTaskManager(any(), anyInt())).thenReturn(monitoredTaskManagerModules);
		Mockito.when(monitoredTaskManagerService.newSubTaskManager(any(), anyInt())).thenReturn(monitoredTaskManagerAnnotations);
	}

	
	/**
	 * Sets the availabilityService service to return false on call of {@linkplain GenAIAvailabilityService#isGenAIServiceAvailable()} and runs the Job.
	 * We expect the job to return a {@linkplain Severity#ERROR} result and we expect {@linkplain GenerativeAnnotationTranslationService#translateUsingGenAI(AnnotationPojoPrototype, EntityId, EntityId)}
	 * to never be called.
	 */
	@Test
	void testServiceUnavailable() {
		Mockito.doReturn(false).when(availabilityService).isGenAIServiceAvailable();
		final var parameters = new HashMap<String, List<String>>();
		parameters.put("ids", Collections.singletonList(module1Id.toString()));
		final GenerateAnnotationDescriptionsFromModuleJob job = new GenerateAnnotationDescriptionsFromModuleJob(PROJECT_ID, parameters);
		final String jobId = BaseDiscoveryTest.submitJob(jobManager, tracer, job);

		verify(availabilityService, times(1)).isGenAIServiceAvailable();
		verify(translateService, times(0)).translateUsingGenAI(any(AnnotationPojo.class), any(EntityId.class), any(EntityId.class));
		
		final Serializable jobResult = jobManager.getJobResult(jobId);
		assertNotNull(jobResult);
		assertEquals(Result.class, jobResult.getClass());
		final Result<?> result = (Result<?>) jobResult;
		assertEquals(Severity.ERROR, result.status.getSeverity());
	}
	
	/**
	 * Sets the availabilityService service to return true on call of {@linkplain GenAIAvailabilityService#isGenAIServiceAvailable()} and runs the Job.
	 * module1 and module2 are passed as module parameters and the overwrite flag is set to false. 
	 * We expect {@linkplain GenerativeAnnotationTranslationService#translateUsingGenAI(AnnotationPojo, EntityId, EntityId)} to be called twice for the two system identified annotations.
	 */
	@Test
	void testRunJobNoOverwrite() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		final var parameters = new HashMap<String, List<String>>();
		final List<String> moduleIds = new ArrayList<>();
		moduleIds.add(module1Id.toString());
		moduleIds.add(module2Id.toString());
		parameters.put("ids", moduleIds);
		final GenerateAnnotationDescriptionsFromModuleJob job = new GenerateAnnotationDescriptionsFromModuleJob(PROJECT_ID, parameters);
		final String jobId = BaseDiscoveryTest.submitJob(jobManager, tracer, job);

		verify(monitoredTaskManagerService, times(1)).newTaskManager(any(), anyInt());
		verify(monitoredTaskManagerService, times(2)).newSubTaskManager(any(), anyInt());
		verify(availabilityService, times(1)).isGenAIServiceAvailable();
		verify(translateService, times(2)).translateUsingGenAI(any(AnnotationPojo.class), any(EntityId.class), any(EntityId.class));
		
		final Serializable jobResult = jobManager.getJobResult(jobId);
		assertNotNull(jobResult);
		assertEquals(Result.class, jobResult.getClass());
		final Result<?> result = (Result<?>) jobResult;
		assertEquals(Severity.OK, result.status.getSeverity());
	}
	
	/**
	 * Sets the availabilityService service to return true on call of {@linkplain GenAIAvailabilityService#isGenAIServiceAvailable()} and runs the Job.
	 * module1 and module2 are passed as module parameters and the overwrite flag is set to true. 
	 * We expect {@linkplain GenerativeAnnotationTranslationService#translateUsingGenAI(AnnotationPojo, EntityId, EntityId)} to be called three times for the two system identified annotations
	 * and the user modified one.
	 */
	@Test
	void testRunJobOverwrite() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		final var parameters = new HashMap<String, List<String>>();
		final List<String> moduleIds = new ArrayList<>();
		moduleIds.add(module1Id.toString());
		moduleIds.add(module2Id.toString());
		parameters.put("ids", moduleIds);
		parameters.put("overwrite", Collections.singletonList("true"));
		final GenerateAnnotationDescriptionsFromModuleJob job = new GenerateAnnotationDescriptionsFromModuleJob(PROJECT_ID, parameters);
		final String jobId = BaseDiscoveryTest.submitJob(jobManager, tracer, job);

		verify(monitoredTaskManagerService, times(1)).newTaskManager(any(), anyInt());
		verify(monitoredTaskManagerService, times(2)).newSubTaskManager(any(), anyInt());
		verify(availabilityService, times(1)).isGenAIServiceAvailable();
		verify(translateService, times(3)).translateUsingGenAI(any(AnnotationPojo.class), any(EntityId.class), any(EntityId.class));
		
		final Serializable jobResult = jobManager.getJobResult(jobId);
		assertNotNull(jobResult);
		assertEquals(Result.class, jobResult.getClass());
		final Result<?> result = (Result<?>) jobResult;
		assertEquals(Severity.OK, result.status.getSeverity());
	}
	
	@Test
	void testRunJobNoPermission() {
		Mockito.doReturn(true).when(availabilityService).isGenAIServiceAvailable();
		Mockito.when(translateService.translateUsingGenAI(any(AnnotationPojo.class), any(EntityId.class), any(EntityId.class))).thenThrow(PermissionException.class);
		final var parameters = new HashMap<String, List<String>>();
		final List<String> moduleIds = new ArrayList<>();
		moduleIds.add(module1Id.toString());
		moduleIds.add(module2Id.toString());
		parameters.put("ids", moduleIds);
		final GenerateAnnotationDescriptionsFromModuleJob job = new GenerateAnnotationDescriptionsFromModuleJob(PROJECT_ID, parameters);
		final UUID jobId = UUID.fromString(JobUtil.submitJob(jobManager, tracer, job));

		verify(monitoredTaskManagerService, times(1)).newTaskManager(any(), anyInt());
		verify(monitoredTaskManagerService, times(2)).newSubTaskManager(any(), anyInt());
		verify(availabilityService, times(1)).isGenAIServiceAvailable();
		verify(translateService, times(2)).translateUsingGenAI(any(AnnotationPojo.class), any(EntityId.class), any(EntityId.class)); /* Once per module */
		
		final JobInfoPojo jobInfo = jobinfoService.get(jobId);
		assertNotNull(jobInfo);
		
		assertEquals(2, jobInfo.getMessages().size());
		
		final Serializable jobResult = jobManager.getJobResult(jobId.toString());
		assertNotNull(jobResult);
		assertEquals(Result.class, jobResult.getClass());
		final Result<?> result = (Result<?>) jobResult;
		assertEquals(Severity.WARNING, result.status.getSeverity());
	}

}

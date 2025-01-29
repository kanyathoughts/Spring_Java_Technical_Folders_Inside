package innowake.mining.server.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.atMost;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.List;
import java.util.UUID;
import java.util.function.Consumer;

import innowake.mining.shared.entities.scheduler.SchedulerImportPojoPrototype;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.server.scheduler.SchedulerConditionEntryResolver;
import innowake.mining.server.scheduler.SchedulerEntryResolver;
import innowake.mining.server.scheduler.SchedulerJobEntryResolver;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.SchedulerInfoService;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.scheduler.SchedulerEntryPojoPrototype;
import innowake.mining.shared.entities.scheduler.SchedulerEntryType;

/**
 * Tests for {@link ControlMImportService}.
 */
@Tag("mocked")
class ControlMImportServiceTest {

	@SuppressWarnings("unchecked")
	@Test
	void multipleJobEntryResolvers() {
		final SchedulerJobEntryResolver resolver1 = Mockito.mock(SchedulerJobEntryResolver.class);
		when(resolver1.resolverSchedulerIdentifier()).thenReturn("identifier1");
		final SchedulerJobEntryResolver resolver2 = Mockito.mock(SchedulerJobEntryResolver.class);
		when(resolver2.resolverSchedulerIdentifier()).thenReturn("identifier1");
		final List<SchedulerJobEntryResolver> jobEntryResolvers = List.of(resolver1, resolver2);

		final ControlMImportService service = mockService(null, null, jobEntryResolvers, null, null, null);
		final SchedulerImportPojoPrototype prototype = new SchedulerImportPojoPrototype()
				.setProject(Mockito.mock(EntityId.class))
				.setImporterUsed("identifier1")
				.setIdentifier("file1")
				.setDescription("description")
				.setSource(UUID.randomUUID());
		assertThrows(IllegalStateException.class,
				() -> service.importData(prototype, Mockito.mock(ProgressMonitor.class), Mockito.mock(Consumer.class)));
	}

	@SuppressWarnings("unchecked")
	@Test
	void multipleConditionEntryResolvers() {
		final SchedulerConditionEntryResolver resolver1 = Mockito.mock(SchedulerConditionEntryResolver.class);
		when(resolver1.resolverSchedulerIdentifier()).thenReturn("identifier1");
		final SchedulerConditionEntryResolver resolver2 = Mockito.mock(SchedulerConditionEntryResolver.class);
		when(resolver2.resolverSchedulerIdentifier()).thenReturn("identifier1");
		final List<SchedulerConditionEntryResolver> conditionEntryResolvers = List.of(resolver1, resolver2);

		final ControlMImportService service = mockService(null, null, null, conditionEntryResolvers, null, null);
		final SchedulerImportPojoPrototype prototype = new SchedulerImportPojoPrototype()
				.setProject(Mockito.mock(EntityId.class))
				.setImporterUsed("identifier1")
				.setIdentifier("file1")
				.setDescription("description")
				.setSource(UUID.randomUUID());
		assertThrows(IllegalStateException.class,
				() -> service.importData(prototype, Mockito.mock(ProgressMonitor.class), Mockito.mock(Consumer.class)));
	}

	@SuppressWarnings("unchecked")
	@Test
	void multipleEntryResolvers() throws IOException {
		final SchedulerImportPojoPrototype context = new SchedulerImportPojoPrototype()
				.setProject(Mockito.mock(EntityId.class))
				.setImporterUsed("identifier1")
				.setIdentifier("file1")
				.setDescription("description")
				.setSource(UUID.randomUUID());

		final SchedulerConditionEntryResolver resolver1 = Mockito.mock(SchedulerConditionEntryResolver.class);
		when(resolver1.resolverSchedulerIdentifier()).thenReturn("identifier1");
		when(resolver1.resolveSchedulerEntryType(Mockito.any(), Mockito.any())).thenReturn(SchedulerEntryType.UNKNOWN);
		final SchedulerJobEntryResolver resolver2 = Mockito.mock(SchedulerJobEntryResolver.class);
		when(resolver2.resolverSchedulerIdentifier()).thenReturn("identifier2");
		when(resolver2.resolveSchedulerEntryType(Mockito.any(), Mockito.any())).thenReturn(SchedulerEntryType.JOB);
		final SchedulerJobEntryResolver resolver3 = Mockito.mock(SchedulerJobEntryResolver.class);
		when(resolver3.resolverSchedulerIdentifier()).thenReturn("identifier1");
		when(resolver3.resolveSchedulerEntryType(Mockito.any(), Mockito.any())).thenReturn(SchedulerEntryType.FOLDER);
		final SchedulerJobEntryResolver resolver4 = Mockito.mock(SchedulerJobEntryResolver.class);
		when(resolver4.resolverSchedulerIdentifier()).thenReturn("identifier1");
		when(resolver4.resolveSchedulerEntryType(Mockito.any(), Mockito.any())).thenReturn(SchedulerEntryType.CONDITION);
		final List<SchedulerEntryResolver> entryResolvers = List.of(resolver1, resolver2, resolver3, resolver4);

		final SchedulerInfoService schedulerInfoService = Mockito.mock(SchedulerInfoService.class);
		when(schedulerInfoService.createSchedulerImport(any())).thenReturn(UUID.randomUUID());
		when(schedulerInfoService.findEntries(any(), any())).thenReturn(Paged.empty());

		final SourceService sourceService = Mockito.mock(SourceService.class);
		when(sourceService.getContent(context.source.getNonNull(), context.project.getNonNull())).thenReturn(new BinaryString("content"));

		final ControlMImportService service = mockService(null, schedulerInfoService, null, null, entryResolvers, sourceService);

		service.importData(context, Mockito.mock(ProgressMonitor.class), Mockito.mock(Consumer.class));

		final ArgumentCaptor<SchedulerEntryPojoPrototype> entryCaptor = ArgumentCaptor.forClass(SchedulerEntryPojoPrototype.class);
		verify(schedulerInfoService, atMost(2)).createSchedulerEntry(entryCaptor.capture());
		/* Based on the mocked entryResolvers, the identifier3 should be used. Although both identifier3 & identifier4 are applicable,
		since identifier3 appears on the list first, it is considered */
		assertEquals(SchedulerEntryType.FOLDER, entryCaptor.getValue().type.get());
	}

	private ControlMImportService mockService(@Nullable ModuleService moduleService, @Nullable SchedulerInfoService schedulerInfoService, @Nullable List<SchedulerJobEntryResolver> jobEntryResolvers,
			@Nullable List<SchedulerConditionEntryResolver> conditionEntryResolvers, @Nullable List<SchedulerEntryResolver> entryResolvers, @Nullable SourceService sourceService) {
		if (moduleService == null) {
			moduleService = Mockito.mock(ModuleService.class);
		}
		if (schedulerInfoService == null) {
			schedulerInfoService = Mockito.mock(SchedulerInfoService.class);
		}
		if (jobEntryResolvers == null) {
			final SchedulerJobEntryResolver resolver1 = Mockito.mock(SchedulerJobEntryResolver.class);
			when(resolver1.resolverSchedulerIdentifier()).thenReturn("identifier1");
			jobEntryResolvers = List.of(resolver1);
		}
		if (conditionEntryResolvers == null) {
			final SchedulerConditionEntryResolver resolver3 = Mockito.mock(SchedulerConditionEntryResolver.class);
			when(resolver3.resolverSchedulerIdentifier()).thenReturn("identifier1");
			conditionEntryResolvers = List.of(resolver3);
		}
		if (entryResolvers == null) {
			final SchedulerEntryResolver resolver4 = Mockito.mock(SchedulerEntryResolver.class);
			when(resolver4.resolverSchedulerIdentifier()).thenReturn("identifier1");
			entryResolvers = List.of(resolver4);
		}
		if (sourceService == null) {
			sourceService = Mockito.mock(SourceService.class);
			final SourcePojo source = Mockito.mock(SourcePojo.class);
			final BinaryString string = Mockito.mock(BinaryString.class);
			final String simpleXml = "<?xml version=\"1.0\" encoding=\"utf-8\"?><root><job></job></root>";
			when(string.get()).thenReturn(simpleXml.getBytes());
			when(source.getContent()).thenReturn(string);
			when(source.identity()).thenReturn(Mockito.mock(EntityId.class));
			when(assertNotNull(sourceService).getContent(Mockito.any())).thenReturn(string);
			when(assertNotNull(sourceService).put(Mockito.any(), Mockito.any())).thenReturn(UUID.randomUUID());
		}
		return new ControlMImportService(assertNotNull(moduleService), assertNotNull(schedulerInfoService), assertNotNull(jobEntryResolvers), 
				assertNotNull(conditionEntryResolvers), assertNotNull(entryResolvers), assertNotNull(sourceService));
	}
}

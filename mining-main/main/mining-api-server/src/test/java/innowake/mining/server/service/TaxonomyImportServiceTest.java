/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.service;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Phaser;

import org.apache.commons.collections4.map.HashedMap;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.test.context.support.WithMockUser;

import brave.Span;
import brave.Tracer;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.JobExecutionCallback;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.error.TaxonomyImportException;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.job.TaxonomyImportJob;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.ProjectNature;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Tests for the {@link TaxonomyImportException}.
 */
@TestInstance(Lifecycle.PER_CLASS)
@WithMockUser
class TaxonomyImportServiceTest  extends DatabaseRelatedTest{

	@Autowired
	private ModuleService moduleService;

	@Autowired
	private SourceService sourceService;
	
	@Autowired
	private JobManager jobManager;
	
	@Autowired
	private Tracer tracer;
	
	@Value("${mining.taxonomies.fetchModulePartitionSize: 10000}")
	private int fetchModulePartitionSize;
	
	/**
	 * Tests that the {@link TaxonomyImportException} loads the modules only once per name.
	 *
	 * @throws TaxonomyImportException
	 */
	@Test
	void testNonAmbiguousModules() throws TaxonomyImportException {
		final ProjectPojo project = createTestProject();
		createTestModule(project.identity());

		final String[] keys = { "Module Name", "Module Path", "Technology", "Type" };
		final Map<String, String> lineAsMap = createLineAsMap(keys, "IDWAD00", "src/ims/dbd/IDWAD00.dbd","IMS","DBD");
		
		final List<Map<String, String>> importLines = new ArrayList<>();
		for (int i = 0; i < fetchModulePartitionSize + 1; i++) {
			importLines.add(lineAsMap);
		}
		
		final Span rootSpan = tracer.newTrace();
		try (final Tracer.SpanInScope scope = tracer.withSpanInScope(rootSpan)) {
			final Phaser jobFinishedPhase = new Phaser(2);
			final Throwable[] error = new Throwable[1];
			jobManager.submit(new TaxonomyImportJob(project.identity(), importLines), new JobExecutionCallback() {
				
				@Override
				public void onFailure(@Nullable Throwable throwable) {
					error[0] = throwable;
					jobFinishedPhase.arrive();
				}
				
				@Override
				public void onCompletion() {
					jobFinishedPhase.arrive();
				}
			});
			jobFinishedPhase.arriveAndAwaitAdvance();
			if (error[0] != null) {
				throw new IllegalStateException("Error occured while importing taxonomies", error[0]);
			}
		} finally {
			rootSpan.finish();
		}
	}

	private static Map<String, String> createLineAsMap(final String[] keys, final String...values) {
		assertEquals(keys.length, values.length, "Length of values must match length of keys");

		final Map<String, String> map = new HashedMap<>();
		for (int i = 0; i < keys.length; i++) {
			map.put(keys[i], values[i]);
		}

		return map;
	}

	private void createTestModule(final EntityId projectId) {
		sourceService.create(new SourcePojoPrototype()
				.setProject(projectId)
				.setName("IDWAD00.dbd")
				.setPath("src/ims/dbd/IDWAD00.dbd")
				.setTechnology(Technology.IMS)
				.setType(Type.DBD)
				.setContent(BinaryString.EMPTY));
		
		final ModulePojoPrototype dbdFile = new ModulePojoPrototype();
		dbdFile.setProject(projectId);
		dbdFile.setName("IDWAD00");
		dbdFile.setTechnology(Technology.IMS);
		dbdFile.setType(Type.DBD);
		dbdFile.setOrigin(Origin.CUSTOM);
		dbdFile.setStorage(Storage.FILE);
		dbdFile.setIdentification(Identification.IDENTIFIED);
		dbdFile.setPath("src/ims/dbd/IDWAD00.dbd");
		dbdFile.setCreator(Creator.DISCOVERY);
		moduleService.create(dbdFile);
	}

	private ProjectPojo createTestProject() {
		return projectService.create(new ProjectPojoPrototype()
				.setName("Taxonomy Test Project")
				.setClient(EntityId.of(1L))
				.setNatures(new HashSet<>(Arrays.asList(ProjectNature.DISCOVERY))));
	}

}

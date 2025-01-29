/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.base;

import static innowake.mining.shared.model.Identification.IDENTIFIED;
import static innowake.mining.shared.model.Origin.CUSTOM;
import static org.junit.Assert.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import brave.Tracer;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.integration.job.JobUtil;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.job.JobSummary;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;

/**
 * Test for {@link JobSummary}.
 */
@WithMockUser
class JobSummaryTest extends DatabaseRelatedTest {

	private final static Long PROJECT_ID = Long.valueOf(1);
	
	@Autowired
	private JobManager jobManager;
	
	@Autowired
	private ModuleService moduleService;
	
	@Autowired
	private Tracer tracer;
		
	@Test
	public void testSummary() {
		final List<EntityId> ids = createModules(PROJECT_ID, Technology.COBOL, Type.PROGRAM, 10);
		final String jobId = JobUtil.submitJob(jobManager, tracer, new MockModulesJob(EntityId.of(Long.valueOf(1)), new ModuleMatcher(ids, new ArrayList<String>())));
		
		final Serializable jobResult = jobManager.getJobResult(jobId);
		
		final Result<?> result = assertInstanceOf(Result.class, jobResult);
		final JobSummary summary = assertInstanceOf(JobSummary.class, result.value);
		
		assertEquals(5, summary.getSuccessfulModules().size());
		assertEquals(2, summary.getUnsuccessfulModules().size());
		assertEquals(3, summary.getUnsupportedModules().size());
	}
	
	protected List<EntityId> createModules(final Long projectId, final Technology technology, final Type type, final int count) {
		final ArrayList<EntityId> ids = new ArrayList<EntityId>();
		for (int x = 1; x <= count; x++) {
			ids.add(createModule(EntityId.of(projectId), x, technology, type));
		}
		
		return ids;
	}
	
	protected EntityId createModule(final EntityId projectId, final int name, final Technology technology, final Type type) {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setName(Integer.valueOf(name).toString());
		module.setProject(projectId);
		module.setTechnology(technology);
		module.setType(type);
		module.setStorage(Storage.FILE);
		module.setIdentification(IDENTIFIED);
		module.setOrigin(CUSTOM);
		module.setCreator(Creator.DISCOVERY);
		return moduleService.create(module);
	}
}
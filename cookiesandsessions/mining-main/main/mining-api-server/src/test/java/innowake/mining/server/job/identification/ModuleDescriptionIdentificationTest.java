/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.identification;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Collections;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;

/**
 * Integration test for identifying Module Descriptions.
 */
@WithMockUser
class ModuleDescriptionIdentificationTest extends AbstractIdentificationTest {
	
	@Autowired
	private JobManager jobManager;
	
	/**
	 * Test making sure {@link Technology#NATURAL} {@link Type#PROGRAM} Modules are supported and generate the expected description.
	 */
	@Test
	void testNaturalModule() {
		final EntityId naturalModuleId = createModule(PROJECT_ID_1, "WMIN1170A", "WMIN1170A.nsp", RESOURCE_PATH, Technology.NATURAL, Type.PROGRAM);
		/* Check that description is empty on newly created Module. */
		assertEmptyDescription(PROJECT_ID_1, naturalModuleId);
		
		final List<String> modulePaths = Collections.singletonList(RESOURCE_PATH + "WMIN1170A.nsp");
		
		/* Run the Description identification. */
		runModuleDescription(modulePaths, PROJECT_ID_1);
		
		/* Check that description is not empty any more. */
		assertDescription(PROJECT_ID_1, naturalModuleId, "WMIN1170A.nsp.exp");
	}
	
	/**
	 * Test making sure {@link Technology#JCL} {@link Type#JOB} is supported and generates the expected description.
	 */
	@Test
	void testJclJob() {
		final EntityId jclModuleId = createModule(PROJECT_ID_1, "WMIN1173A", "WMIN1173A.job", RESOURCE_PATH, Technology.JCL, Type.JOB);
		/* Check that description is empty on newly created Module. */
		assertEmptyDescription(PROJECT_ID_1, jclModuleId);
		
		/* Run the Description identification. */
		runModuleDescriptionWithIds(Collections.singletonList(jclModuleId), PROJECT_ID_1);
		
		/* Check that description is not empty any more. */
		assertDescription(PROJECT_ID_1, jclModuleId, "WMIN1173A.job.exp");
	}
	
	private void assertEmptyDescription(final EntityId projectId, final EntityId moduleId) {
		final ModulePojo module = moduleService.findAnyModule(b -> b.ofProject(projectId).byId(moduleId))
				.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with id: " + moduleId + " in project: " + projectId));
		assertEquals(StringUtils.EMPTY, StringUtils.trimToEmpty(module.getDescription().orElse(null)));
	}
	
	private void assertDescription(final EntityId projectId, final EntityId moduleId, final String expectedFileName) {
		final ModulePojo module = moduleService.findAnyModule(b -> b.ofProject(projectId).byId(moduleId))
												.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with id: " + moduleId + " in project: " + projectId));
		final String expected = getContent(expectedFileName);
		assertEquals(expected, module.getDescription().get());
	}
	
	private void runModuleDescription(final List<String> modulePaths, final EntityId projectId) {
		final IdentifyModuleDescriptionsJob job = new IdentifyModuleDescriptionsJob(projectId, new ModuleMatcher(Collections.emptyList(), modulePaths));
		submitJob(jobManager, job);
	}

	private void runModuleDescriptionWithIds(final List<EntityId> moduleIds, final EntityId projectId) {
		final IdentifyModuleDescriptionsJob job = new IdentifyModuleDescriptionsJob(projectId, new ModuleMatcher(moduleIds, Collections.emptyList()));
		submitJob(jobManager, job);
	}
}
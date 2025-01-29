/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.discovery;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.lib.core.lang.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.junit.jupiter.api.Test;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;

import innowake.mining.server.config.Profiles;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;

/**
 * Tests to check whether the PreLoaded modules regarding to utilities are deleted or not and also checks whether the project specific utility module 
 * have created and dependencies are in place as earlier or not.
 */
@AutoConfigureMockMvc
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false )
@WithMockUser
class MigrateUtilitiesFromProject0ToProjectSpecificTest extends BaseDiscoveryTest {
	
	/**
	 * Tests the Project specific module creation and utilities in project 0
	 */
	@Test
	void doesProjectSpecificModuleIsCreated() {
		final ProjectPojo testProject = createProject();
		projectId = testProject.identity();
		/*Checks whether all utilities in project 0 is deleted or not*/
		final long moduleCount = moduleService.countModules(b -> b.ofProject(EntityId.of(Long.valueOf(0))));
		assertEquals(0, moduleCount);
		
		sourceService.resetCaches();
		uploadResources(testProject.identity());
		submitJob(jobManager, tracer, new DiscoverCodeJob(testProject.identity()));
		submitJob(jobManager, tracer, new DiscoverMetricsJob(testProject.identity(), false));
		
		/*This will check whether project specific module is created or not*/
		final List<ModulePojo> modules = moduleService.findModules(builder -> builder.ofProject(testProject.identity()));
		final Optional<ModulePojo> toModule = modules.stream()
				.filter(module -> module.getName().equals("SQLCA"))
				.findAny();
		assertTrue(toModule.isPresent(), "SQLCA module should present");
		final Optional<ModulePojo> fromModule = modules.stream()
				.filter(module -> module.getName().equals("UTPSPA66"))
				.findAny();
		assertTrue(fromModule.isPresent(), "UTPSPA66 module should present");
		assertEquals(fromModule.get().getProject(), toModule.get().getProject());
		
		final var references = moduleService.findRelationship(q -> q.ofProject(testProject.identity())
				.withTypes(Collections.singletonList(RelationshipType.INCLUDES)).ofModuleInDirection(toModule.get().identity(), RelationshipDirection.IN));
		assertEquals(1, references.size(), "The dependency should not be null");
		
		var dependency = references.get(0);
		assertNotNull(dependency);
		/* This will verify that the dependency created between SQLCA and UTPSPA66 in same project*/
		assertEquals(dependency.getSrcModule(), fromModule.get().getUid());
		assertEquals(dependency.getDstModule(), toModule.get().getUid());
	}
	
	@Override
	protected String getTestFolder() {
		return "WMIN6693";
	}
}
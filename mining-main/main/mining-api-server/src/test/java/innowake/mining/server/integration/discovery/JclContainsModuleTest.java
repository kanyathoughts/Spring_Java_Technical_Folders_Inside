/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.discovery;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Technology;

@WithMockUser
class JclContainsModuleTest extends BaseDiscoveryTest {

	/**
	 * Tests whether there is a ContainsModule edge to every virtual JCL Module from a physical one
	 */
	@Test
	void testContainsModuleEdgesExist() {
		final EntityId projectId = performDiscovery();
		final List<ModulePojo> modules = moduleService.findModules(builder -> builder.ofProject(projectId));

		assertFalse(modules.isEmpty(), "Modules must be present");

		final Set<Long> physicalModuleRids = modules.stream()
				.filter(module -> Representation.PHYSICAL == module.getRepresentation().orElse(null))
				.map(ModulePojo::getId)
				.collect(Collectors.toSet());
		for (final ModulePojo module : modules) {
			if (Representation.VIRTUAL == module.getRepresentation().orElse(null) && Technology.JCL == module.getTechnology() && 
					module.getIdentification() != Identification.MISSING) {
				assertNotNull(module.getParent(), "VIRTUAL JCL module must have a parent module");
				assertTrue(physicalModuleRids.contains(module.getParent().orElseThrow().getNid()), "Parent module must be present in project");
			}
		}
	}

	/**
	 * Tests that there are no excessive ContainsModule edges to non-JCL modules. (E.g. Resources, SQL, ...)
	 */
	@Test
	void testNoExcessContainsModuleEdges() {
		final EntityId projectId = performDiscovery();
		final List<ModulePojo> modules = moduleService.findModules(q -> q.ofProject(projectId));
		final List<ModulePojo> notJcls = modules.stream()
												.filter(module -> Technology.JCL != module.getTechnology())
												.collect(Collectors.toList());
		assertTrue(notJcls.size() > 0);
		for (final ModulePojo notJcl : notJcls) {
			assertTrue(notJcl.getParent().isEmpty(), "non-JCL file must have no parent module");
		}
	}

	/**
	 * Tests that there are no duplicate JCL_PROCs when the same JCL_PROC is called by two JCL_JOBs.
	 */
	@Test
	void testNoDuplicateProcs() {
		final EntityId projectId = performDiscovery();
		final List<ModulePojo> job1 = moduleService.findModules(q -> q.ofProject(projectId).withName("MMRS712K"));
		final List<ModulePojo> job2 = moduleService.findModules(q -> q.ofProject(projectId).withName("712KDUP"));
		final List<ModulePojo> proc = moduleService.findModules(q -> q.ofProject(projectId).withName("MMRS712P"));
		final List<ModulePojo> job1ProcCall = moduleService.findModules(q -> q.ofProject(projectId).withName("MMRS712K.STEPCP.EXEC"));
		final List<ModulePojo> job2ProcCall = moduleService.findModules(q -> q.ofProject(projectId).withName("712KDUP.STEPCP.EXEC"));
		assertEquals(1, job1.size(), "MMRS712K");
		assertEquals(1, job2.size(), "712KDUP");
		assertEquals(1, proc.size(), "MMRS712P");
		assertEquals(1, job1ProcCall.size(), "MMRS712K.STEPCP.EXEC");
		assertEquals(1, job2ProcCall.size(), "712KDUP.STEPCP.EXEC");
	}

	public EntityId performDiscovery() {
		sourceService.resetCaches();
		final EntityId projectId = createProject().identity();
		uploadResources(projectId);
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		submitJob(jobManager, tracer, new DiscoverMetricsJob(projectId, false));
		return projectId;
	}

	@Override
	public String getTestFolder() {
		return "jclContainsModule";
	}
}
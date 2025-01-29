/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.categorize.jcl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Optional;

import org.junit.jupiter.api.Test;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.integration.discovery.BaseDiscoveryTest;
import innowake.mining.shared.entities.SourcePojo;

/**
 * Tests whether the JCL_INCLUDE file discovered without extension and adding .proc after discovery.
 */
@WithMockUser
class JclIncludeDiscoveryTest extends BaseDiscoveryTest {

	@Override
	protected String getTestFolder() {
		return "WMIN13161";
	}
	
	@Test
	void testJclIncludeFileDiscoveryWithoutExtension() {
		final var project = createProject();
		final var projectId = project.identity();
		uploadResources(projectId);
		final Optional<SourcePojo> sourceObjectBeforeDiscovery = sourceService.findAny(builder -> builder.ofProject(projectId).withName("ABEND"));
		assertTrue(sourceObjectBeforeDiscovery.isPresent());
		assertEquals("temp/WMIN13161/ABEND", sourceObjectBeforeDiscovery.get().getPath());
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		final Optional<SourcePojo> sourceObjectAfterDiscovery = sourceService.findAny(builder -> builder.ofProject(projectId).withName("ABEND"));
		assertTrue(sourceObjectAfterDiscovery.isPresent());
		assertEquals("src/jcl/WMIN13161/include/ABEND.proc", sourceObjectAfterDiscovery.get().getPath());
	}

}

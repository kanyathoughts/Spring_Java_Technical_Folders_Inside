/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.discovery;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.nio.file.Path;

import org.junit.jupiter.api.Test;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.model.ModuleType;

/**
 * Tests for WMIN-7726: Discovery code should continue to proceed and identify the files even when placed in any folder starting with src.
 */
@WithMockUser
class DiscoveryCodeFolderNameTest extends BaseDiscoveryTest {

	@Override
	protected String getTestFolder() {
		return "src-cobol";
	}

	@Test
	void createTestData() {
		final EntityId projectId = createProject().identity();
		uploadResources(projectId);
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		/* We are submitted the metrics job to validate the discover code is successful for the provided folder, as he can't only check if the identification
		 * is success or not */
		submitJob(jobManager, tracer, new DiscoverMetricsJob(projectId, true));
		
		/* Module creation confirms the cobol file is identified correctly for the given folder and the metrics is available */
		assertEquals(1, moduleService.countModules(b -> b.ofProject(projectId).withName("AAA")));
	}
	
	/* Overriding this because the parent method prefix "temp" to the path but we require the path as is, which is starting with src */
	@Override
	protected SourcePojoPrototype createSourceObject(final EntityId projectId, final Path path, final String content, final ModuleType moduleType) {
		return new SourcePojoPrototype()
				.setProject(projectId)
				.setName(path.getFileName().toString())
				.setPath(getSourcePath().getParent().relativize(path).toString())
				.setTechnology(moduleType.getTechnology())
				.setType(moduleType.getType())
				.setMetaDataRevision(Long.valueOf(1))
				.setContentRevision(Long.valueOf(1))
				.setContent(new BinaryString(content));
	}

}

/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.discovery;

import innowake.mining.server.discovery.dawn.metrics.test.contributors.multiplematchperformance.MultipleMatchResolveAllPerformanceContributor;
import innowake.mining.server.discovery.dawn.metrics.test.contributors.multiplematchperformance.MultipleMatchResolveAllPerformanceContributorsConfiguration;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.context.annotation.Import;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.TestPropertySource;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.server.discovery.dawn.metrics.test.contributors.multiplematchperformance.MultipleMatchResolveAllPerformanceContributor.DEPENDENCY_TARGET_NAME;

/**
 * Performance test for dependency resolution flag {@code MULTIPLE_MATCH_RESOLVE_ALL}.
 * @see MultipleMatchResolveAllPerformanceContributor
 */
@TestPropertySource(properties ="configuration.discovery-enable-dawn-contributors=false") /* disable auto-registration of Dawn contributors ... */
@TestPropertySource(properties ="configuration.discovery-enable-legacy-contributors=false") /* disable execution of legacy contributors ... */
@Import(MultipleMatchResolveAllPerformanceContributorsConfiguration.class) /* ... and only load the test contributors instead */
@WithMockUser
@Disabled /* manual performance test */
public class MultipleMatchResolveAllPerformanceTest extends BaseDiscoveryTest {

	private static final int NUMBER_OF_TARGETS = 1_000;
	private static final String TEST_FOLDER = "resolve-all-performance";

	@Test
	void testMultipleMatchResolveAllPerformance() {
		projectId = assertNotNull(createProject()).identity();

		/* create 1000 Modules with the same name that serve as dependency target */
		for (int i = 0; i < NUMBER_OF_TARGETS; i++) {
			moduleService.create(new ModulePojoPrototype()
					.setProject(assertNotNull(projectId))
					.setName(DEPENDENCY_TARGET_NAME)
					.setPath("fake/path/" + i)
					.setCreator(Creator.API)  /* creator API is important, or DiscoverMetricsJob will delete the module */
					.setTechnology(Technology.UNKNOWN)
					.setType(Type.UNKNOWN)
					.setStorage(Storage.FILE)
					.setOrigin(Origin.CUSTOM)
					.setIdentification(Identification.IDENTIFIED));
		}

		submitJob(jobManager, tracer, new DiscoverMetricsJob(assertNotNull(projectId), false));
		System.out.println("done");
	}

	@Override
	protected String getTestFolder() {
		return TEST_FOLDER;
	}
}

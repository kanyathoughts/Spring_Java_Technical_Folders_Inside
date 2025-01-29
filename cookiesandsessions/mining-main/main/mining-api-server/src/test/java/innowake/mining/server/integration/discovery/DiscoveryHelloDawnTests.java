/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.discovery;

import innowake.mining.server.discovery.dawn.metrics.test.contributors.hello.HelloContributorsConfiguration;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.EntityId;

import org.springframework.context.annotation.Import;
import org.springframework.test.context.TestPropertySource;

import com.google.common.collect.ImmutableSet;

import java.nio.file.Paths;
import java.util.Set;

/**
 * End-to-end tests for Dawn discovery using simple contributor implementations.
 */
@TestPropertySource(properties ="configuration.discovery-enable-dawn-contributors=false") /* disable auto-registration of Dawn contributors ... */
@TestPropertySource(properties ="configuration.discovery-enable-legacy-contributors=false") /* disable execution of legacy contributors ... */
@Import(HelloContributorsConfiguration.class) /* ... and only load the "hello" contributors instead */
public class DiscoveryHelloDawnTests extends DiscoveryBulkTest {

	@Override
	protected String folder() {
		return "hello-dawn";
	}

	@Override
	protected BaseDiscoveryTest createTestInstance(final String testFolder, final boolean skipDiscoveryFeatureValidation) {
		return new BaseDiscoveryTest(skipDiscoveryFeatureValidation) {

			@Override
			protected DiscoverMetricsJob createDiscoverMetricsJob(final EntityId projectId) {
				return new DiscoverMetricsJob(projectId, true);
			}

			@Override
			protected String getTestFolder() {
				return Paths.get(folder()).resolve(testFolder).toString();
			}
		};
	}

	@Override
	protected Set<String> skipDiscoveryFeatureValidation() {
		/* Will be addressed as part of WMIN-7331 */
		return ImmutableSet.of("dependency-creation", "simple-files");
	}
}

/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.discovery;

import java.nio.file.Paths;
import java.util.Set;

import com.google.common.collect.ImmutableSet;

import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.EntityId;

/**
 * Tests discovery for IMS.
 *
 * After discovery, PL/I lightweight parsing and IMS discovery are default, then these tests should be merged with the normal test runs.
 */
public class ImsDiscoveryTest extends DiscoveryBulkTest {

	@Override
	protected String folder() {
		return "ims";
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
		return ImmutableSet.of("WMIN4431A", "WMIN4431B"); /* Will be addressed as part of WMIN-7331 */
	}
}

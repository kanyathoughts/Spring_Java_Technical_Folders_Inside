/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.discovery;

import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;

import com.google.common.collect.ImmutableSet;

import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.server.integration.discovery.BaseDiscoveryTest.AssertionValues;
import innowake.mining.shared.access.EntityId;

/**
 * This executes all existing tests of {@link DiscoveryIrisTest} that are supported in the clusterable discover metrics implementation.
 */
public class DiscoveryIrisTest extends DiscoveryBulkTest {

	@Autowired
	private DiscoveryIrisCustomAssertions discoveryIrisCustomAssertions;

	protected final Map<String, BiConsumer<EntityId, AssertionValues>> customAssertions = new HashMap<>();

	@Override
	protected String folder() {
		return "iris";
	}

	@Override
	protected BaseDiscoveryTest createTestInstance(final String testFolder, final boolean skipDiscoveryFeatureValidation) {

		if (customAssertions.containsKey(testFolder)) {

			return new BaseDiscoveryTest(customAssertions.get(testFolder), skipDiscoveryFeatureValidation) {

				@Override
				protected String getTestFolder() {
					return Paths.get(folder()).resolve(testFolder).toString();
				}
			};
		} else {

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
	}

	@PostConstruct
	private void populateCustomAssertions() {
		customAssertions.putAll(discoveryIrisCustomAssertions.getCustomAssertions());
	}

	@Override
	protected Set<String> skipDiscoveryFeatureValidation() {
		/* Will be addressed as part of WMIN-7331 */
		return ImmutableSet.of(
				"WCFD230",
				"WCFD269",
				"WCFD32",
				"WCFD535",
				"WCFD649",
				"WCFD666",
				"WCFD771",
				"WCFD815",
				"WDIS502",
				"WDIS504",
				"WDIS528",
				"WDIS529",
				"WDIS529A",
				"WDIS529B",
				"WDIS529C",
				"WMIN2831",
				"WMIN2841",
				"WMIN2956",
				"WMIN3985A",
				"WMIN5639",
				"WNDT3010",
				"WNDT3020",
				"WMIN8388",
				"WMIN8388A",
				"WMIN9602"
				);
	}
}

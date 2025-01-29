/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.discovery;

import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import innowake.mining.data.model.discovery.ModelDependency;
import innowake.mining.data.model.discovery.ModelStatement;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.EntityId;

/**
 * The {@link DiscoveryConcurrencyTest} executes certain discover metrics test several times to verify that no concurrency
 * issues exists e.g. when creating {@link ModelDependency ModelDependencies} or {@link ModelStatement ModelStatements}.
 */
public class DiscoveryConcurrencyTest extends DiscoveryBulkTest {
	
	private static final int TEST_RUNS = 5;
	private static final List<String> TEST_CASES = Arrays.asList("WMIN2514A", "WMIN2514B");
	
	@Override
	protected String folder() {
		return "iris";
	}
	
	@Override
	protected List<String> includeThisTestsOnly() {
		final List<String> list = new ArrayList<>(TEST_CASES.size() * TEST_RUNS);
		TEST_CASES.forEach(testCase -> {
			for (int i = 0; i < TEST_RUNS; i++) {
				list.add(testCase);
			}
		});
		
		return list;
	}

	private static int i = 0;

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
			
			@Override
			protected String getProjectName() {
				return super.getProjectName() + i++;
			}
		};
	}
}

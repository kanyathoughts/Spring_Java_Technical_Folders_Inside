/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.discovery;

import java.io.Serializable;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.junit.Assert;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.SourceMetricsPojo;

/**
 * This executes all existing tests of {@link DiscoveryIrisDeadCodeTest} that are supported in the clusterable discover metrics implementation.
 */
public class DiscoveryIrisDeadCodeTest extends DiscoveryBulkTest {
	
	@Override
	protected String folder() {
		return "iris";
	}
	
	@Override
	protected List<String> includeThisTestsOnly() {
		return Arrays.asList("DEAD6");
	}
	
	@Override
	protected BaseDiscoveryTest createTestInstance(final String testFolder, final boolean skipDiscoveryFeatureValidation) {
		return new BaseDiscoveryTest(skipDiscoveryFeatureValidation) {
			
			@Override
			protected DiscoverMetricsJob createDiscoverMetricsJob(final EntityId projectId) {
				return new DeadCodeJob(projectId);
			}
			
			@Override
			protected String getTestFolder() {
				return Paths.get(folder()).resolve(testFolder).toString();
			}
		};
	}
}

class DeadCodeJob extends DiscoverMetricsJob {
	@Autowired
	private transient ModuleService moduleService;

	public DeadCodeJob(final EntityId projectId) {
		super(projectId, true);
	}
	
	@Override
	protected Result<Serializable> run(ProgressMonitor progressMonitor) {
		final Result<Serializable> result = super.run(progressMonitor);
		final List<String> moduleNames = Arrays.asList("DEAD9", "DEAD10", "DEAD11", "DEAD12");
		final Set<String> nameVerificationSet = new HashSet<>(moduleNames);
		final List<ModulePojo> modules = moduleService.findModules(b -> b.ofProject(projectId).withNames(moduleNames));
		Assert.assertEquals(4, modules.size());
		for (final ModulePojo module : modules) {
			final Integer linesOfDeadCode = module.getSourceMetrics().map(SourceMetricsPojo::getDeadCodeLines).orElse(Integer.valueOf(-1));
			final String name = module.getName();
			Assert.assertTrue("Module " + name + "was not expected.", nameVerificationSet.remove(name));
			switch (name) {
				case "DEAD9":
					Assert.assertEquals(0, linesOfDeadCode.intValue());
					break;
				case "DEAD10":
					Assert.assertEquals(4, linesOfDeadCode.intValue());
					break;
				case "DEAD11":
					Assert.assertEquals(0, linesOfDeadCode.intValue());
					break;
				case "DEAD12":
					Assert.assertEquals(0, linesOfDeadCode.intValue());
					break;
				default:
					Assert.fail("Module " + name + "was not expected.");
			}
		}
		Assert.assertTrue(nameVerificationSet.isEmpty());
		return result;
	}
}

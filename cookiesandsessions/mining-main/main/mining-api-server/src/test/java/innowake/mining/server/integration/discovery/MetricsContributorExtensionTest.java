/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.discovery;
import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;
import java.nio.file.Path;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.jupiter.api.Test;
import org.springframework.context.annotation.Import;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.data.discovery.metrics.ContributorParameters;
import innowake.mining.data.discovery.metrics.MetricsContributor;
import innowake.mining.data.discovery.metrics.MetricsContributor.Phase;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.data.model.discovery.ModelDependency;
import innowake.mining.extensions.MetricsContributorExtension;
import innowake.mining.server.discovery.DiscoveryExcelUtil;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.server.integration.discovery.MetricsContributorExtensionTest.JavaMetricsContributorExtensionTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;

/**
 * Tests the {@link MetricsContributorExtension} by running Discover code and metrics.
 */
@WithMockUser
@Import(JavaMetricsContributorExtensionTest.class)
class MetricsContributorExtensionTest extends BaseDiscoveryTest {

	@Override
	protected String getTestFolder() {
		return "metricsContributorExtension";
	}

	@Override
	protected DiscoverMetricsJob createDiscoverMetricsJob(final EntityId projectId) {
		return new DiscoverMetricsJob(projectId, true);
	}

	/**
	 * Tests if the {@code JavaMetricsContributorExtensionTest} is rightly invoked.
	 * 
	 * @throws IOException any exception while accessing the content
	 */
	@Test
	void testDiscoveryOutput() throws IOException {
		doTest();
	}

	private void doTest() throws IOException {
		final EntityId projectId = performDiscovery();
		final Path expectedFile = getExpectedFile(getExpectedFileName());
		if (isWriteExpected()) {
			writeExpected(expectedFile, getMetricsContentAsCsv(projectId));
		} else {
			DiscoveryExcelUtil.compareIgnoreOrder(read(expectedFile), getMetricsContentAsCsv(projectId));
		}
	}

	private EntityId performDiscovery() {
		sourceService.resetCaches();
		final EntityId projectId = assertNotNull(createProject()).identity();
		uploadResources(projectId);
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		submitJob(jobManager, tracer, createDiscoverMetricsJob(projectId));
		return projectId;
	}

	/**
	 * This class is a test MetricsContributorExtension that demonstrates how an extension class have to be implemented.
	 */
	@SuppressWarnings("removal")
	public static class JavaMetricsContributorExtensionTest implements MetricsContributorExtension {
		
		@Override
		public boolean accept(final ModelArtifact entry, final Phase phase) {
			return entry.getType() == ResolveTarget.JAVA_COMPILATION_UNIT;
		}
		
		@Override
		public MetricsContributor init(final ContributorParameters parameters, final SourceService sourceService, final @Nullable TimedWorker timedWorker) {
			return new JavaMetricsContributorTest(sourceService, parameters);
		}
	}
	
	
	public static class JavaMetricsContributorTest implements MetricsContributor {

		private static final Pattern PATTERN = Pattern.compile("@StoredProcedure\\(\"(\\w+)\"\\)");

		private final SourceService sourceService;
		private final ContributorParameters parameters;

		public JavaMetricsContributorTest(final SourceService sourceService, final ContributorParameters parameters) {
			this.sourceService = sourceService;
			this.parameters = parameters;
		}

		@Override
		public void calculateDependentMetrics(final ModelArtifact artifact) throws DiscoveryException {
			final String content = getSourceObject(artifact).getContent().toString();
			final Matcher matcher = PATTERN.matcher(content);
			if (matcher.find()) {
				final String storedProcedure = matcher.toMatchResult().group(1);
				artifact.addDependency(new ModelDependency()
												.setLateBinding()
												.setTarget(new ModelArtifact()
																.setName(storedProcedure)
																.setType(ResolveTarget.SQL_STORED_PROCEDURE)
																.validate())
												.validate());
			}
		}

		private SourcePojo getSourceObject(final ModelArtifact artifact) throws DiscoveryException {
			final String path = artifact.getPath().orElseThrow(() -> new DiscoveryException("Entry file is not present: " + artifact.getName()));

			final SourcePojo sourceObject = Assert.assertNotNull(sourceService)
					.get(q -> q.ofProject(Assert.assertNotNull(parameters).getProjectId()).withPath(path));
			return sourceObject;
		}
	}

}

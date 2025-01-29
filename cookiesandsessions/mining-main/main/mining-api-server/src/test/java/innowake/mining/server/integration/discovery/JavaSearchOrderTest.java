/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.discovery;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;
import java.nio.file.Path;
import java.util.stream.Stream;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.server.discovery.DiscoveryExcelUtil;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.FeatureId;

/**
 * Tests the Java dependencies are rightly resolved based on the search-order.
 */
@WithMockUser
class JavaSearchOrderTest extends BaseDiscoveryTest {
	
	private String expectedTestFolder = StringUtils.EMPTY;

	@BeforeAll
	public void setFeature() {
		ff4j.disable(FeatureId.JAVA_COLLECT_METHOD_CALLS.getId());
	}

	@Override
	protected String getTestFolder() {
		return "configTests/WMIN3202";
	}
	
	/**
	 * Required for {@link ParameterizedTest}.
	 * 
	 * @return the paths of the test cases
	 */
	public Stream<Arguments> testCases() {
		return Stream.of(
				/* Tests if the dependencies are rightly resolved. The search-order rightly points to all the projects. */
				Arguments.of("a"),
				/* Tests only if the dependencies from project: pro2 is resolved. The search-order only points to the project: pro2. */
				Arguments.of("b"),
				/* Same as 'a' and the only difference is search-order source pattern points to all the source objects. */
				Arguments.of("c"),
				/* Tests none of the dependencies are resolved. The search-order target pattern doesn't points to the proper project. */
				Arguments.of("d"),
				/* Tests for source pattern configured incorrectly which starts with '/'*/
				Arguments.of("e"),
				/* Test which checks for a file with incorrect package */
				Arguments.of("f"),
				/* Tests if the source pattern does not match with the path and package of the java source file.*/
				Arguments.of("g"));
	}

	@DisplayName("Tests Java dependencies are resolved based on the search-order")
	@ParameterizedTest(name = "{0}")
	@MethodSource("testCases")
	void testSearchOrders(final String path) {
		doTest(path);
	}

	private void doTest(final String path) {
		try {
			sourceService.resetCaches();
			final EntityId projectId = assertNotNull(createProject()).identity();
			assertNotNull(projectService.get(projectId));
			final Path expectedFile = EXPECTED_FOLDER.resolve(getTestFolder()).resolve(path).resolve(getExpectedFileName());
			final Path expectedEffortFile = EXPECTED_FOLDER.resolve(getTestFolder()).resolve(path).resolve(getExpectedEffortFileName());
			
			this.expectedTestFolder = path;
			uploadResources(projectId);
			submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));		
			submitJob(jobManager, tracer, new DiscoverMetricsJob(projectId, false));

			if (isWriteExpected()) {
				writeExpected(expectedFile, getMetricsContentAsCsv(projectId));
				writeExpected(expectedEffortFile, getEffortContentAsCsv(projectId));
			} else {
				DiscoveryExcelUtil.compareIgnoreOrder(read(expectedFile), getMetricsContentAsCsv(projectId));
				DiscoveryExcelUtil.compareIgnoreOrder(read(expectedEffortFile), getEffortContentAsCsv(projectId));
			}
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}
	}
	
	@Override
	protected Path getConfigFolder() {
		return EXPECTED_FOLDER.resolve(getTestFolder()).resolve(expectedTestFolder).resolve(DISCOVERY_CONFIG_FOLDER);
	}

	@Override
	protected boolean normalizeFileContent() {
		return true;
	}
}

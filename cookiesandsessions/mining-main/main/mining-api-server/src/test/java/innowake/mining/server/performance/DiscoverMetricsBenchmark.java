/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.performance;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.io.FilenameUtils;
import org.junit.jupiter.api.AfterAll;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;

import brave.Tracer;
import innowake.lib.core.profile.DefaultProfiler;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.io.sourceobject.FileExtension;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.integration.discovery.BaseDiscoveryTest;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.model.ModuleType;

@ActiveProfiles(value = Profiles.AUTH_TEST, inheritProfiles = false )
@AutoConfigureMockMvc
@WithMockUser
public class DiscoverMetricsBenchmark extends DatabaseRelatedTest {

	private static final Path LOG_DIRECTORY = Paths.get("./logs/");
	
	@Autowired
	private ModuleService moduleService;
	@Autowired
	private SourceService sourceService;
	@Autowired
	private JobManager jobManager;
	@Autowired
	private Tracer tracer;
	private static EntityId projectId = null;
	
	public void loadSourceCode(final String name) {
		if (projectId == null) {
			final ProjectPojo projectResult = createProject();
			projectId = projectResult.identity();
		}
		final Path sourcePath = Paths.get("./test-resources/innowake/mining/server/discovery/source/" + name);
		loadSourceIntoProject(sourcePath, projectId);
	}

	public static void enableProfiling() {
		/* to enable the profiling either call DefaultProfiler.setProfilingEnabled(true) 
		 * or set the property "innowake.lib.core.profile.ProfilingEnabled" */
		DefaultProfiler.setProfilingEnabled(true);
		ByteBuddyProfiling.startProfiling();
	}
	
	@AfterAll
	public static void printCsv() {
		ByteBuddyProfiling.writeToCsv(LOG_DIRECTORY.resolve("bytebuddy-out.csv"));
	}
	
//  Performance test should only be run manually.
//	@Test
	void testPerformanceDiscoverMetrics() {
		enableProfiling();

		loadSourceCode("DemoDez18DiscoveredCode");
		new TestExecutor.Builder()
			.test(this::runMetrics)
			.warmup(0)
			.testCount(5)
			.sysout(true)
			.appendResultTo(LOG_DIRECTORY.resolve("performance-benchmarks-dez18.csv"))
			.resultNote("Dez18 - Not Optimized")
			.build()
			.runTest();

		moduleService.deleteModules(q -> q.ofProject(projectId));
		loadSourceCode("IrisDiscoveredCode");
		new TestExecutor.Builder()
			.test(this::runMetrics)
			.warmup(0)
			.testCount(5)
			.sysout(true)
			.appendResultTo(LOG_DIRECTORY.resolve("performance-benchmarks-iris.csv"))
			.resultNote("Iris - Not Optimized")
			.build()
			.runTest();
	}

	private void runMetrics() {
		BaseDiscoveryTest.submitJob(jobManager, tracer, new DiscoverMetricsJob(projectId, false));
	}

	private ProjectPojo createProject() {
		return projectService.create(new ProjectPojoPrototype()
				.setName("Performance Test Project")
				.setClient(EntityId.of(1L)));
	}

	private List<SourcePojo> loadSourceIntoProject(final Path sourcePath, final EntityId projectId) {
		try (final Stream<Path> fileStream = Files.walk(sourcePath)) {
			return fileStream.filter(Files::isRegularFile)
				.map(file -> createSourceObject(file, projectId))
				.map(so -> sourceService.get(sourceService.create(so)))
				.collect(Collectors.toList());
		} catch (IOException e) {
			throw new IllegalStateException(e);
		}
	}
	
	private static SourcePojoPrototype createSourceObject(final Path filePath, final EntityId projectId) {
		final String path = filePath.toString();
		final String name = FilenameUtils.getBaseName(path);
		final ModuleType moduleType = FileExtension.resolve(path);
		try {
			final BinaryString content = new BinaryString(Files.readAllBytes(filePath));
			return new SourcePojoPrototype() 
				.setProject(projectId)
				.setName(name)
				.setPath(path)
				.setTechnology(moduleType.getTechnology())
				.setType(moduleType.getType())
				.setContent(content);
		} catch (IOException e) {
			throw new IllegalStateException(e);
		}
	}
}

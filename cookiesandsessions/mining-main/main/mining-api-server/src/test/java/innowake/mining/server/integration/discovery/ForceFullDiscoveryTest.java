/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.discovery;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.server.discovery.DiscoveryExcelUtil;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.entities.testing.SourcePojoDummy;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.ModuleType;

/**
 * Tests for discovery on a project with incremental changes
 */
@WithMockUser
class ForceFullDiscoveryTest extends BaseDiscoveryTest {

	private static final String TEST_FOLDER = "incremental";
	private static final Path TEST_SOURCE_FOLDER = SOURCE_FOLDER.resolve(TEST_FOLDER);

	/** The {@code true} (default) if incremental scan is enabled by API. {@code false} to force a full scan */
	private boolean isIncrementalScanEnabled = true;

	@Nullable
	private Path testFolder;

	private String testFolderName = "";

	@SuppressWarnings("hiding")
	@Nullable
	@SpyBean
	private SourceCachingService sourceService;

	@BeforeAll
	public void before() {
		ff4j.enable(FeatureId.INCREMENTAL_SCAN.getId());
	}

	@AfterAll
	public void after() {
		ff4j.disable(FeatureId.INCREMENTAL_SCAN.getId());
	}

	/**
	 * Tests that with the {@code INCREMENTAL_SCAN} feature enabled and when {@link DiscoverMetricsJobClustered#DiscoverMetricsJobClustered(Long, boolean)}
	 * is called with {@code incremental=true} that an incremental discover metrics scan is done.
	 * 
	 * @throws IOException in case of an I/O error when creating or reading expected data
	 */
	@Test
	void testWMIN2030Incremental() throws IOException {
		isIncrementalScanEnabled = true;
		doTest("WMIN2030");
	}

	/**
	 * Tests that with the {@code INCREMENTAL_SCAN} feature enabled and when {@link DiscoverMetricsJobClustered#DiscoverMetricsJobClustered(Long, boolean)}
	 * is called with {@code incremental=false} that a full discover metrics scan is done.
	 * 
	 * @throws IOException in case of an I/O error when creating or reading expected data
	 */
	@Test
	void testWMIN2030ForceFull() throws IOException {
		isIncrementalScanEnabled = false;
		doTest("WMIN2030");
	}

	@Override
	protected String getTestFolder() {
		return testFolderName;
	}

	@Override
	protected Path getSourcePath() {
		return Assert.assertNotNull(testFolder);
	}

	/**
	 * Prepares the source object with the given parameters
	 *
	 * @param projectId the project id
	 * @param path the file path
	 * @param content the file content
	 * @param moduleType {@link ModuleType}
	 * @return the source object created from the given parameters
	 */
	@Override
	protected SourcePojoPrototype createSourceObject(final EntityId projectId, final Path path, final String content, final ModuleType moduleType) {
		return new SourcePojoDummy()
				.setProject(projectId)
				.setName(path.getFileName().toString())
				.setPath(Paths.get("temp").resolve(getSourcePath().relativize(path)).toString())
				.setTechnology(moduleType.getTechnology())
				.setType(moduleType.getType())
				.setContent(new BinaryString(content));
	}

	@Override
	protected final DiscoverMetricsJob createDiscoverMetricsJob(final EntityId projectId) {
		return new DiscoverMetricsJob(projectId, isIncrementalScanEnabled);
	}

	private void doTest(final String folder) throws IOException {
		testFolderName = TEST_FOLDER + "/" + folder;
		testFolder =  assertResource(TEST_SOURCE_FOLDER.resolve(folder));
		Objects.requireNonNull(sourceService).resetCaches();
		final Path expectedFile = getExpectedFile(getExpectedFileName());
		final EntityId projectId = createProject().identity();

		uploadResources(projectId);
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		submitJob(jobManager, tracer, createDiscoverMetricsJob(projectId));

		if (isWriteExpected()) {
			writeExpected(expectedFile, getMetricsContentAsCsv(projectId));
		} else {
			final Map<Long, ModulePojo> modules1 = moduleService.findModules(builder -> builder.ofProject(projectId))
																.stream().collect(Collectors.toMap(ModulePojo::getId, module -> module));

			submitJob(jobManager, tracer, createDiscoverMetricsJob(projectId));

			/* isIncrementalScanEnabled => true: Method must have been called once by first discovery and twice by incremental scan
			 * isIncrementalScanEnabled => false: Method must have have been called once by first discovery and once by 2nd non-incremental scan */
			verify(sourceService, isIncrementalScanEnabled ? times(4) : times(2)).findIDs(any());

			final Map<Long, ModulePojo> modules2 = moduleService.findModules(builder -> builder.ofProject(projectId)).stream()
																.collect(Collectors.toMap(ModulePojo::getId, module -> module));
			assertEquals(modules1.size(), modules2.size(), "Number of modules before and after second metrics discovery must be the same");
			
			if (isIncrementalScanEnabled) {
				final int[] cnt = { 0 };
				modules2.forEach((id, module) -> {
					final ModulePojo module1 = modules1.get(id);
					if (module1 != null) {
						assertEquals(module1.getId(), modules2.get(id).getId(),
									 String.format("Modules with id %d must be equal after incremental scan without revision changes", id));
						cnt[0]++;
					}
				});
				assertEquals(11, cnt[0], "Number of modules without revision changes must match");
			} else {
				modules2.keySet().forEach(id -> assertNull(modules1.get(id),
							String.format("Module with id %d must not be present after incremental scan with enabled force full scan", id)));
			}

			DiscoveryExcelUtil.compareIgnoreOrder(read(expectedFile), getMetricsContentAsCsv(projectId));
		}
	}
}

/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.discovery;

import static innowake.lib.core.lang.Assert.fail;
import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.springframework.security.test.context.support.WithMockUser;
import org.testcontainers.shaded.org.apache.commons.io.FilenameUtils;

import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.data.io.sourceobject.FileExtension;
import innowake.mining.server.discovery.DiscoveryExcelUtil;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Test class for testing proc file under includes folder in source code identified as module with
 * technology and type as JCL and INCLUDE respectively.
 */
@WithMockUser
class JclIncludeAndProcModuleTest extends BaseDiscoveryTest {

	/**
	 * Tests if proc file under includes folder in source code identified as module with
	 * technology and type as JCL and INCLUDE respectively.
	 */
	@Test
	void testJclIncludeAndProcs() {
		final Path expectedFile = getExpectedFile(getExpectedFileName());
		final Path expectedEffortFile = getExpectedFile(getExpectedEffortFileName());
		sourceService.resetCaches();
		final EntityId projectId = createProject().identity();
		uploadResources(projectId);
		submitJob(jobManager, tracer, new DiscoverMetricsJob(projectId, false));
		final ModulePojo procModule = moduleService.findAnyModule(b -> b.ofProject(projectId).withPath("src/jcl/PROD.PROC/procs/TESTPARM.proc"))
				.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with path: src/jcl/PROD.PROC/procs/TESTPARM.proc in project: " + projectId));
		assertEquals(Technology.JCL, procModule.getTechnology());
		assertEquals(Type.PROC, procModule.getType());
		final ModulePojo includeModule = moduleService.findAnyModule(b -> b.ofProject(projectId).withPath("src/jcl/TEST.PROD.PROCLIB/include/TESTPARM.proc"))
				.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with path: src/jcl/TEST.PROD.PROCLIB/include/TESTPARM.proc in project: " + projectId));
		assertEquals(Technology.JCL, includeModule.getTechnology());
		assertEquals(Type.INCLUDE, includeModule.getType());
		
		try {
			final String actualWorkbook = getMetricsContentAsCsv(projectId);
			final String actualEffortContent = getEffortContentAsCsv(projectId);
			if (isWriteExpected()) {
				writeExpected(expectedFile, actualWorkbook);
				writeExpected(expectedEffortFile, actualEffortContent);
			} else {
				DiscoveryExcelUtil.compareIgnoreOrder(read(expectedFile), actualWorkbook);
				DiscoveryExcelUtil.compareIgnoreOrder(read(expectedEffortFile), actualEffortContent);
			}
		} catch (final IOException e) {
			fail("Fail to export excel . Exception occurred : " + e.getMessage());
			e.printStackTrace();
		}
	}
	
	@Override
	protected void uploadResources(final EntityId projectId, final Predicate<Path> pathFilter) {
		try (final Stream<Path> walk = Files.walk(getSourcePath())) {
			walk.filter(pathFilter).map(path -> {
				final String content;
				try {
					content = getFileContent(path);
				} catch (final IOException e) {
					throw new IllegalStateException(e);
				}
				String pathToSet = Paths.get("temp").resolve(getSourcePath().getParent().relativize(path)).toString();

				if (pathToSet.contains("TESTJOB1.job")) {
					pathToSet = "src/jcl/PROD.JCL/jobs/TESTJOB1.job";
				} else {
					pathToSet = pathToSet.contains("TEST.PROD.PROCLIB") ? "src/jcl/TEST.PROD.PROCLIB/include/TESTPARM.proc"
							: "src/jcl/PROD.PROC/procs/TESTPARM.proc";
				}
				final ModuleType moduleType = FileExtension.resolve(pathToSet);
				final SourcePojoPrototype sourceObject = createSourceObject(projectId, path, content, moduleType);
				sourceObject.setPath(pathToSet);
				sourceObject.setName(FilenameUtils.getBaseName(pathToSet));
				return sourceObject;
			}).forEach(this::uploadSourceObject);

			importConfiguration(projectId);
		} catch (final Exception e) {
			e.printStackTrace();
			fail("Error while uploading resource: " + e.getMessage());
		}
	}

	@Override
	protected String getTestFolder() {
		return "WMIN7920";
	}
}

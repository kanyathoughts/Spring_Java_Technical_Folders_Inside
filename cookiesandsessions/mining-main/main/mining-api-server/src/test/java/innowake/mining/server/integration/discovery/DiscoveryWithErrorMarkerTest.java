/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.discovery;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.lib.core.lang.Assert.fail;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.discovery.DiscoveryExcelUtil;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ErrorMarkerPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.model.discovery.Severity;

/**
 * Tests whether the errors got saved in newly introduced ErrorMarker table and linking these errors with module using 
 * HasErrorMarker edge or not.
 */
@WithMockUser
class DiscoveryWithErrorMarkerTest extends BaseDiscoveryTest {

	@Override
	protected String getTestFolder() {
		return "WMIN7071";
	}

	@Test
	void testForErrorMarkers() {
		final Path expectedFile = getExpectedFile(getExpectedFileName());
		final Path expectedEffortFile = getExpectedFile(getExpectedEffortFileName());
		final EntityId projectId = createProject().identity();
		assertEquals(0, moduleService.countErrorMarkers(q -> q.ofProject(projectId)));
		sourceService.resetCaches();
		uploadResources(projectId);
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		submitJob(jobManager, tracer, new DiscoverMetricsJob(projectId, false));
		final ModulePojo module = moduleService.findAnyModule(b -> b.ofProject(projectId).withPath("src/cobol/WMIN7071/programs/WMIN7071.cbl"))
				.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with path: src/cobol/WMIN7071/programs/WMIN7071.cbl in project: " + projectId));
		/* As we are getting the Error markers with Module's id , that means there is an edge created between module and ErrorMaker i.e HasErrorMarker.*/
		final List<ErrorMarkerPojo> errorMarkers = moduleService.findErrorMarkers(q -> q.ofModule(module.identity())).stream()
				.filter(error -> error.getSeverity() == Severity.ERROR)
				.collect(Collectors.toList());
		assertEquals(3, errorMarkers.size());
		final var moduleLocation = assertNotNull(errorMarkers.get(0).getLocation());
		assertTrue(moduleLocation.getAssembledOffset().isPresent());
		assertTrue(moduleLocation.getAssembledLength().isPresent());
		assertTrue(moduleLocation.getAssembledOffset().get() > 0, "The Offset of ErrorMarker should be greater than 0");
		assertTrue(moduleLocation.getAssembledLength().get() > 0, "The Length of ErrorMarker should be greater than 0");
		
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
			fail("Fail to export excel . Exception occured : " + e.getMessage());
			e.printStackTrace();
		}
	}
}

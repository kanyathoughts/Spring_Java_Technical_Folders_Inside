/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.importer.csv;

import static innowake.lib.core.lang.Assert.fail;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Optional;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.data.model.discovery.ErrorMarker;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ErrorMarkerPojo;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.SourceMetricsPojo;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;

/**
 * Tests Discovery snapshot import functionalities. 
 */
class DiscoveryCsvImportTest extends DatabaseResettingTest {

	private static final EntityId PROJECT_ONE = EntityId.of(1L);
	private static final Path SNAPSHOT_CSV = Paths.get("./test-resources/innowake/mining/server/integration/import-discovery.csv");
	
	private static final String WMIN_7099_CSV_FILE = "import-discovery-WMIN-7099.csv";
	private static final Path WMIN_7099_CSV_PATH = Paths.get("./test-resources/innowake/mining/server/integration", WMIN_7099_CSV_FILE);

	private static final String WMIN_8321_CSV_FILE = "import-discovery-WMIN-8321.csv";
	private static final Path WMIN_8321_CSV_PATH = Paths.get("./test-resources/innowake/mining/server/integration", WMIN_8321_CSV_FILE);

	@Autowired
	private CSVImportService importCsvService;	
	@Autowired
	private ModuleService moduleService;

	/**
	 * Tests CSV snapshot import.
	 */
	@Test
	void importCsv() {
		try {
			
			final int moduleCountBeforeImport = moduleService.findModules(builder -> builder.ofProject(PROJECT_ONE)).size();
			importCsvService.importCsv(PROJECT_ONE, "import-discovery.csv", Files.newInputStream(SNAPSHOT_CSV));
			
			final List<ModulePojo> modules = moduleService.findModules(builder -> builder.ofProject(PROJECT_ONE));
			assertEquals(moduleCountBeforeImport + 20, modules.size());
			
			modules.forEach(DiscoveryCsvImportTest::assertSourceMetrics);
			assertJspTypeIsReferenced(PROJECT_ONE);
		} catch (final IOException e) {
			fail("Failed to read " + SNAPSHOT_CSV.toString() + ". Exception: " + e.getMessage());
		}
	}

	private static void assertSourceMetrics(final ModulePojo module) {
		final SourceMetricsPojo sourceMetrics = module.getSourceMetrics().orElse(null);
		/* Module with a path must have SourceMetrics, modules without must have none */
		if (module.getPath().isPresent()) {
			assertNotNull(sourceMetrics, "SourceMetrics of module must not be null");
			/* test physical lines */
			switch (module.getName()) {
				case "TESTSQL1":
					assertEquals(Integer.valueOf(123), sourceMetrics.getPhysicalLines(), "Number of physical lines must match");
					break;
				case "TESTJCL1":
					assertEquals(Integer.valueOf(42), sourceMetrics.getPhysicalLines(), "Number of physical lines must match");
					break;
				case "jclconfig":
					assertEquals(Integer.valueOf(0), sourceMetrics.getPhysicalLines(), "Number of physical lines must match");
					break;
				case "ExampleClass":
					assertEquals(Integer.valueOf(16), sourceMetrics.getPhysicalLines(), "Number of physical lines must match");
					break;
				case "ExampleJsp":
					assertEquals(Integer.valueOf(16), sourceMetrics.getPhysicalLines(), "Number of physical lines must match");
					break;
				default:
					assertNull(sourceMetrics.getPhysicalLines(), "Number of physical lines must not be set");
					break;
			}
		} else {
			assertNull(sourceMetrics);
		}
	}
	
	/**
	 * Test for WMIN-8826. Asserts the relationship exists between the Java and JSP module after the import.
	 * 
	 * @param projectId the project id
	 */
	private void assertJspTypeIsReferenced(final EntityId projectId) {
		final Optional<ModuleLightweightPojo> match = moduleService.findAnyModuleLightweight(q -> q.ofProject(projectId)
																		.withName("ExampleJsp")
																		.withDestinationRelationships(RelationshipType.REFERENCES));

		assertTrue(match.isPresent(), "REFERENCES relationship does not exist between Java and JSP modules");
	}

	/**
	 * Tests the backwards compatibility of the {@link CSVImportService} for LoC metrics.
	 * In WMIN-7099 the LoC metrics column headers were changed.
	 */
	@Test
	void testBackwardsCompatibilityLoCImport() {
		try {
			final int moduleCountBeforeImport = moduleService.findModules(builder -> builder.ofProject(PROJECT_ONE)).size();
			importCsvService.importCsv(PROJECT_ONE, WMIN_7099_CSV_FILE, Files.newInputStream(WMIN_7099_CSV_PATH));

			final List<ModulePojo> modules = moduleService.findModules(builder -> builder.ofProject(PROJECT_ONE));
			assertEquals(moduleCountBeforeImport + 16, modules.size());

			boolean checked = false;
			for (final ModulePojo module : modules) {
				if ("TESTSQL1".equals(module.getName())) {
					final SourceMetricsPojo sourceMetrics = module.getSourceMetrics().orElse(null);
					assertNotNull(sourceMetrics, "SourceMetrics of module must not be null");
					assertEquals(Integer.valueOf(16), sourceMetrics.getCodeLines(), "Code lines must match");
					assertEquals(Integer.valueOf(21), sourceMetrics.getCommentLines(), "Comment lines must match");
					assertEquals(Integer.valueOf(123), sourceMetrics.getPhysicalLines(), "Physical lines must match");
					checked = true;
				}
			}

			assertTrue(checked, "Source metrics import must have been tested");

		} catch (final IOException e) {
			fail("Failed to read file " + WMIN_7099_CSV_PATH.toString() + ". Exception: " + e.getMessage());
		}
	}
	
	/**
	 * Tests the backward compatibility of the {@link ErrorMarker} for Error metrics.
	 * In WMIN-7071 the Error column headers were changed.
	 */
	@Test
	void testBackwardCompatibilityErrorMarker() {
		try {
			final long errorsCountBeforeImport = moduleService.countErrorMarkers(q -> q.ofProject(PROJECT_ONE));
			importCsvService.importCsv(PROJECT_ONE, WMIN_8321_CSV_FILE, Files.newInputStream(WMIN_8321_CSV_PATH));

			final List<ErrorMarkerPojo> errors = moduleService.findErrorMarkers(q -> q.ofProject(PROJECT_ONE));
			assertEquals(errorsCountBeforeImport + 8, errors.size());

			for (final ErrorMarkerPojo error : errors) {
				if (ErrorKey.UNDISCOVERED_DEPENDENCY == error.getKey()) {
					assertEquals(Severity.WARNING, error.getSeverity());
					assertEquals("Unable to determine name of dependency target. The target likely has the type COBOL_PROGRAM", error.getCause());
					final var moduleLocation = error.getLocation();
					assertNotNull(moduleLocation, "modulelocation must not be null");
					assertEquals(10, moduleLocation.getAssembledOffset().orElseThrow());
					assertEquals(20, moduleLocation.getAssembledLength().orElseThrow());
				}
				if (ErrorKey.MODULE_ABORT == error.getKey()) {
					assertEquals(Severity.ERROR, error.getSeverity());
					assertEquals("Unable to resolve dependency < BAS001B, [PL1_COPYBOOK]>: Multiple possible candidates found: [1069, 1107]", error.getCause());
					final var moduleLocation = error.getLocation();
					assertNotNull(moduleLocation, "modulelocation must not be null");
					assertEquals(45, moduleLocation.getAssembledOffset().orElseThrow());
					assertEquals(25, moduleLocation.getAssembledLength().orElseThrow());
				}
			}
		} catch (final IOException e) {
			fail("Failed to import file " + WMIN_8321_CSV_PATH.toString() + ". Exception: " + e.getMessage());
		}
	}
}

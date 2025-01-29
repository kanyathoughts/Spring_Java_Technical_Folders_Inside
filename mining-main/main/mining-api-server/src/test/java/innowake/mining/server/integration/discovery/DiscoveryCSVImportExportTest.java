/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.discovery;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.lib.core.lang.Assert.fail;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.server.discovery.DiscoveryExcelUtil;
import innowake.mining.server.importer.csv.CSVImportService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.model.RelationshipType;

/**
 * Tests helps to validate the csv import works as expected by exporting the data again and checking reference import also
 */
@WithMockUser
class DiscoveryCSVImportExportTest extends BaseDiscoveryTest {
	private static final String DUMP_FILE = "import-discovery.csv";

	@Autowired
	private CSVImportService importCsvService;	

	@Test
	void discoveryCSVImportTest() throws IOException {
		final Path expectedFile = getExpectedFile(getExpectedFileName());
		final ProjectPojo project = assertNotNull(createProject());
		/* Imports the csv data to the backend */
		importCsvService.importCsv(project.identity(), "import-discovery.csv", Files.newInputStream(expectedFile));
		try {
			/* Validate the imported data is correct by exporting it */
			DiscoveryExcelUtil.compareIgnoreOrder(read(expectedFile), getMetricsContentAsCsv(project.identity()));

			doReferenceTest(project.identity(), RelationshipType.CALLS);
			doReferenceTest(project.identity(), RelationshipType.INCLUDES);
			doReferenceTest(project.identity(), RelationshipType.ACCESSES);
			doReferenceTest(project.identity(), RelationshipType.REFERENCES);
		} catch (final IOException e) {
			fail("Failed to export csv. Exception occured : " + e.getMessage());
			e.printStackTrace();
		}
	}

	private void doReferenceTest(final EntityId projectId, final RelationshipType referenceType) {
		assertTrue(moduleService.findAnyRelationship(q -> q.ofProject(projectId).withType(referenceType)).isPresent());
	}

	@Override
	protected String getTestFolder() {
		return "importExportTest";
	}
	
	@Override
	protected String getExpectedFileName() {
		return DUMP_FILE;
	}

}

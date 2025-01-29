/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.discovery;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.multipart;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

import innowake.mining.server.config.Profiles;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;

/**
 * Tests if the Lines of dead code field in Modules table is populated when running Discover metrics and while importing Csv.
 */
@WithMockUser
@AutoConfigureMockMvc
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false )
class ModulesLinesOfDeadCodeTest extends BaseDiscoveryTest {

	@Autowired
	private MockMvc mockMvc;

	@Override
	protected String getTestFolder() {
		return "WMIN1439";
	}

	@Test
	void testDeadCodeByImportingCsv() throws IOException, Exception {
		final Map<String, ModulePojo> modulesImported = importCsv();
		assertEquals(30, modulesImported.get("COBDEAD").getSourceMetrics().orElseThrow().getDeadCodeLines());
		assertEquals(0, modulesImported.get("JOBALIVE").getSourceMetrics().orElseThrow().getDeadCodeLines());
		/* NOTAPPL is a virtual module that has no path and no source so SourceMetrics must be null */
		assertTrue(modulesImported.get("NOTAPPL").getSourceMetrics().isEmpty());
	}

	private Map<String, ModulePojo> importCsv() throws IOException, Exception {
		final EntityId projectId = createProject().identity();
		final Path testCsv = Paths.get("./test-resources/innowake/mining/server/integration/import-discovery-deadcode.csv");
		mockMvc.perform(multipart("/api/v1/projects/{projectId}/csv",String.valueOf(projectId.getNid())).file("file", Files.readAllBytes(testCsv)))
				.andExpect(status().is2xxSuccessful());
		return moduleService.findModules(builder -> builder.ofProject(projectId)).stream().collect(Collectors.toMap(ModulePojo::getName, module -> module));
	}
}

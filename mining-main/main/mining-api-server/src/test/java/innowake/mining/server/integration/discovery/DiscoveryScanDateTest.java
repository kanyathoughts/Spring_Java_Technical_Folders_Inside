/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.discovery;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.multipart;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Date;
import java.util.List;

import innowake.mining.shared.model.Creator;
import org.apache.commons.collections4.map.HashedMap;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

import innowake.mining.server.config.Profiles;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Tests if the {@link Project#getMetricsDate()}, {@link ModulePojo#getModifiedDate()} & {@link Module#getMetricsDate()} are populated properly.
 */
@WithMockUser
@AutoConfigureMockMvc
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false )
class DiscoveryScanDateTest extends BaseDiscoveryTest {

	@Autowired
	private MockMvc mockMvc;

	@Override
	protected String getTestFolder() {
		return "WMIN1439";
	}

	@Test
	void testScanDateByRunningDiscovery() {
		sourceService.resetCaches();
		ProjectPojo project = assertNotNull(createProject());
		final EntityId projectId = project.identity();
		assertNull(project.getMetricsDate());

		uploadResources(projectId);
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		project = assertNotNull(projectService.get(projectId));
		assertNull(project.getMetricsDate());

		submitJob(jobManager, tracer, new DiscoverMetricsJob(projectId, false));
		/* New Project variable created since it has to be used inside lambda */
		final ProjectPojo project1 = assertNotNull(projectService.get(projectId));

		final List<ModulePojo> modules = moduleService.findModules(builder -> builder.ofProject(projectId));
		modules.forEach(module -> {
			assertEquals(assertNotNull(project1.getMetricsDate()).truncatedTo(ChronoUnit.SECONDS),
					module.getMetricsDate().orElseThrow().truncatedTo(ChronoUnit.SECONDS));
		});
	}
	
	@Test
	void testScanDateByImportingCsv() throws IOException, Exception {
		final ProjectPojo project = assertNotNull(createProject());
		final EntityId projectId = project.identity();
		assertNull(project.getMetricsDate());

		final Path testCsv = Paths.get("./test-resources/innowake/mining/server/integration/import-discovery.csv");
		mockMvc.perform(multipart("/api/v1/projects/{projectId}/csv", projectId.getNid()).file("file", Files.readAllBytes(testCsv)))
				.andExpect(status().is2xxSuccessful());
		final ProjectPojo project1 = assertNotNull(projectService.get(projectId));

		final List<ModulePojo> modules = moduleService.findModules(builder -> builder.ofProject(projectId));
		modules.forEach(module -> {
			assertEquals(assertNotNull(project1.getMetricsDate()).truncatedTo(ChronoUnit.SECONDS),
					module.getMetricsDate().orElseThrow().truncatedTo(ChronoUnit.SECONDS));
			assertEquals(assertNotNull(project1.getMetricsDate()).truncatedTo(ChronoUnit.SECONDS),
					module.getModifiedDate().orElseThrow().truncatedTo(ChronoUnit.SECONDS));
		});
	}
	
	@Test
	void testScanDateByImportingExcel() throws IOException, Exception {
		final ProjectPojo project = assertNotNull(createProject());
		final EntityId projectId = project.identity();
		assertNull(project.getMetricsDate());

		final Path testCsv = Paths.get("./test-resources/innowake/mining/server/integration/import-discovery.xlsx");
		mockMvc.perform(multipart("/api/v1/projects/{projectId}/excel", projectId.getNid()).file("file", Files.readAllBytes(testCsv)))
				.andExpect(status().is2xxSuccessful());
		final ProjectPojo project1 = assertNotNull(projectService.get(projectId));

		final List<ModulePojo> modules = moduleService.findModules(builder -> builder.ofProject(projectId));
		modules.forEach(module -> {
			assertEquals(assertNotNull(project1.getMetricsDate()).truncatedTo(ChronoUnit.SECONDS),
					module.getMetricsDate().orElseThrow().truncatedTo(ChronoUnit.SECONDS));
			assertEquals(assertNotNull(project1.getMetricsDate()).truncatedTo(ChronoUnit.SECONDS),
					module.getModifiedDate().orElseThrow().truncatedTo(ChronoUnit.SECONDS));
		});
	}

	@Test
	void testModuleCreateAndUpdate() throws IOException, Exception {
		final EntityId projectId = createProject().identity();

		final ModulePojoPrototype module = new ModulePojoPrototype()
				.setName("Test Module")
				.setProject(projectId)
				.setTechnology(Technology.COBOL)
				.setType(Type.PROGRAM)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setCreator(Creator.DISCOVERY)
				.setInfo(new HashedMap<>());

		final Date timeBeforeModuleCreation = new Date();
		
		/* creates new module and test the modified date */
		mockMvc.perform(post("/api/v1/projects/{projectId}/modules", projectId.getUid())
				.contentType(MediaType.APPLICATION_JSON).content(PojoMapper.jsonWriter().writeValueAsString(module)))
				.andExpect(status().is2xxSuccessful());
		
		List<ModulePojo> modules = moduleService.findModules(q -> q.ofProject(projectId).withName("Test Module"));
		assertEquals(1, modules.size());
		final Instant moduleCreationDate = modules.get(0).getModifiedDate().orElseThrow();
		/* Do not use Date#compareTo as it is not comparing milliseconds correctly */
		assertTrue(moduleCreationDate.getEpochSecond() >= timeBeforeModuleCreation.toInstant().getEpochSecond()); 
		
		/* update new module and test if the modified date is greater than the module creation date */
		final ModulePojo updatedModule = modules.get(0);
		final ModulePojoPrototype module2 = new ModulePojoPrototype()
				.withId(updatedModule.identity())
				.setTechnology(Technology.BASIC);
		
		mockMvc.perform(put("/api/v1/projects/{projectId}/modules/{moduleId}", projectId.getUid(), updatedModule.getId())
				.contentType(MediaType.APPLICATION_JSON).content(PojoMapper.jsonWriter().writeValueAsString(module2)))
				.andExpect(status().is2xxSuccessful());
		
		modules = moduleService.findModules(q -> q.ofProject(projectId).withName("Test Module"));
		assertEquals(1, modules.size());
		final Instant updatedModuleModifiedDate = updatedModule.getModifiedDate().orElseThrow();
		assertTrue(moduleCreationDate.getEpochSecond() <= updatedModuleModifiedDate.getEpochSecond());
	}

}

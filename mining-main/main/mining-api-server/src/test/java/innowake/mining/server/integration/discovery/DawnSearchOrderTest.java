/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.discovery;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.lib.core.lang.Assert.fail;
import static innowake.mining.server.JobTestHelper.waitForJobCompletion;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.context.annotation.Import;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.controller.DataSchemaController;
import innowake.mining.server.discovery.DiscoveryExcelUtil;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.dawn.metrics.test.contributors.hello.HelloContributorsConfiguration;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;

/**
 * Tests for WMIN-7160: Tests for correct dependency resolution using the configured search order.
 */
@AutoConfigureMockMvc
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false )
@WithMockUser
@TestPropertySource(properties ="configuration.discovery-enable-dawn-contributors=false") /* disable auto-registration of Dawn contributors ... */
@TestPropertySource(properties ="configuration.discovery-enable-legacy-contributors=false") /* disable execution of legacy contributors ... */
@Import(HelloContributorsConfiguration.class) /* ... and only load the "hello" contributors instead */
class DawnSearchOrderTest extends BaseDiscoveryTest {	
	
	final String legacyDatabase = "{\"name\":\"jdbc:sqlserver://localhost;databaseName=unit_test_1\",\"schemes\":[{\"name\":\"TEST_SCHEMA_1\",\"tables\":"
			+ "[{\"name\":\"TABLE_1\",\"columns\":[{\"name\":\"optname\",\"dataType\":\"sysname\",\"defaultValue\":null,\"autoIncremented\":false},"
			+ "{\"name\":\"value\",\"dataType\":\"bit\",\"defaultValue\":null,\"autoIncremented\":false}],\"indices\":[],\"primaryKey\":null,\"foreignKeys\":[]"
			+ "}],\"views\":[],\"routines\":[],\"triggers\":[]},{\"name\":\"TEST_SCHEMA_2\",\"tables\":[{\"name\":\"TABLE_1\",\"columns\":"
			+ "[{\"name\":\"optname\",\"dataType\":\"sysname\",\"defaultValue\":null,\"autoIncremented\":false},{\"name\":\"value\",\"dataType\":\"bit\","
			+ "\"defaultValue\":null,\"autoIncremented\":false}],\"indices\":[],\"primaryKey\":null,\"foreignKeys\":[]}],\"views\":[],\"routines\":[],"
			+ "\"triggers\":[]}]}";

	@Autowired
	private MockMvc mvc;

	@Override
	public void resetData() {
		/* Do not reset test data as it deletes the data we create in importDatabaseSchema() */
	}

	@BeforeAll
	void importDatabaseSchema() throws Exception {
		super.resetData();

		projectId = assertNotNull(createProject()).identity();
		final MvcResult mvcResult = mvc.perform(post("/api" + DataSchemaController.SCHEMA_IMPORT_URL, projectId.getNid()).contentType("application/json")
				.content(legacyDatabase)).andExpect(status().isOk()).andReturn();
		final String testJobId = mvcResult.getResponse().getContentAsString().replaceAll("\"", "").trim();
		waitForJobCompletion(testJobId, jobManager, 1, TimeUnit.MINUTES);
	}
	
	/**
	 * Tests whether correct dependencies are resolved based on configured search orders.
	 */
	@Test
	void testSearchOrderDependencyResolution() {
		final Path expectedFile = getExpectedFile(getExpectedFileName());
		final Path expectedEffortFile = getExpectedFile(getExpectedEffortFileName());
		final EntityId nonNullProjectId = assertNotNull(projectId);
		assertEquals(4, getModuleCount(Creator.API));
		sourceService.resetCaches();
		uploadResources(assertNotNull(projectId));
		submitJob(jobManager, tracer, new DiscoverCodeJob(assertNotNull(projectId)));
		submitJob(jobManager, tracer, new DiscoverMetricsJob(assertNotNull(projectId), false));
		
		/* This will verify that the modules created through DB schema import are preserved or not. */
		assertEquals(4, getModuleCount(Creator.API));
		
		final List<ModulePojo> modules = moduleService.findModules(b -> b.ofProject(nonNullProjectId).withName("DEPTEST"));
		assertEquals(1, modules.size());
		final ModulePojo depTestModule = modules.get(0);
		/* Check whether correct Table module has been resolved */
		final List<UUID> readWritesdependentIds = moduleService.findRelationship(q -> q.ofProject(nonNullProjectId)
				.ofModuleInDirection(depTestModule.identity(), RelationshipDirection.OUT) 
				.withType(RelationshipType.ACCESSES)).stream().map(ModuleRelationshipPojo::getDstModule).collect(Collectors.toList());
		final Optional<ModulePojo> optionalTableModule = moduleService.findModules(b -> b.ofProject(nonNullProjectId).byUids(readWritesdependentIds)).stream()
				.filter(module -> module.getName().equals("TABLE_1")).findFirst();
		assertTrue(optionalTableModule.isPresent());
		final ModulePojo tableModule = optionalTableModule.get();
		assertEquals(Creator.API, tableModule.getCreator());
		final Optional<EntityId> parent = tableModule.getParent();
		assertTrue(parent.isPresent(), "Parent module must be present");
		final ModulePojo schemaModule = moduleService.findAnyModule(b -> b.ofProject(nonNullProjectId).byId(parent.get()))
				.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with id: " + parent.get() + " in project: " + nonNullProjectId));
		assertEquals("TEST_SCHEMA_2", assertNotNull(schemaModule).getName());
		
		/* check whether correct copy module based on path and type is resolved */
		final List<UUID> includesDependentIds = moduleService.findRelationship(q -> q.ofProject(nonNullProjectId)
				.ofModuleInDirection(depTestModule.identity(), RelationshipDirection.OUT) 
				.withType(RelationshipType.INCLUDES)).stream().map(ModuleRelationshipPojo::getDstModule).collect(Collectors.toList());
		final Optional<ModulePojo> optionalCopyModule = moduleService.findModules(b -> b.ofProject(nonNullProjectId).byUids(includesDependentIds)).stream()
				.filter(module -> module.getName().equals("foo")).findFirst();
		assertTrue(optionalCopyModule.isPresent());
		final ModulePojo copyModule = optionalCopyModule.get();
		assertEquals("src/cobol/dawnSearchOrder/copybooks/folderB/copies/foo.cpy", copyModule.getPath().get());
		try {
			final String actualWorkbook = getMetricsContentAsCsv(nonNullProjectId);
			final String actualEffortContent = getEffortContentAsCsv(nonNullProjectId);
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

	@Override
	protected String getTestFolder() {
		return "dawnSearchOrder";
	}
	
	private long getModuleCount(final Creator creator) {
		return moduleService.countModules(q -> q.ofProject(assertNotNull(projectId)).withCreator(creator));
	}
}

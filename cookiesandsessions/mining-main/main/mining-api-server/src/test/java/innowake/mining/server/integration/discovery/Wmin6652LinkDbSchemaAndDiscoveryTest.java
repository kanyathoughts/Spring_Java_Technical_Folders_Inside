/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.discovery;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.lib.core.lang.Assert.assertTrue;
import static innowake.lib.core.lang.Assert.fail;
import static innowake.mining.server.JobTestHelper.waitForJobCompletion;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import innowake.mining.server.config.Profiles;
import innowake.mining.server.controller.DataSchemaController;
import innowake.mining.server.discovery.DiscoveryExcelUtil;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.RelationshipType;

/**
 * Tests for WMIN-6652: Ensure the linking of database schema and discovery and extended search order support.
 */
@AutoConfigureMockMvc
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false )
@WithMockUser
class Wmin6652LinkDbSchemaAndDiscoveryTest extends BaseDiscoveryTest {	
	
	final String legacyDatabase = "{\"name\":\"jdbc:sqlserver://localhost;databaseName=unit_test_1\",\"schemes\":[{\"name\":\"TEST_SCHEMA_1\",\"tables\":"
			+ "[{\"name\":\"TABLE_1\",\"columns\":[{\"name\":\"optname\",\"dataType\":\"sysname\",\"size\":1,\"defaultValue\":null,\"autoIncremented\":false},"
			+ "{\"name\":\"value\",\"dataType\":\"bit\",\"size\":2,\"defaultValue\":null,\"autoIncremented\":false}],\"indices\":[],\"primaryKey\":null,"
			+ "\"foreignKeys\":[]}],\"views\":[],\"routines\":[],\"triggers\":[]},{\"name\":\"TEST_SCHEMA_2\",\"tables\":[{\"name\":\"TABLE_1\",\"columns\":"
			+ "[{\"name\":\"optname\",\"dataType\":\"sysname\",\"size\":3,\"defaultValue\":null,\"autoIncremented\":false},{\"name\":\"value\","
			+ "\"dataType\":\"bit\",\"size\":4,\"defaultValue\":null,\"autoIncremented\":false}],\"indices\":[],\"primaryKey\":null,\"foreignKeys\":[]}],"
			+ "\"views\":[],\"routines\":[],\"triggers\":[]}]}";

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
	 * Tests the linking of database schema and discovery and extended search order support.
	 */
	@Test
	void testLinkOfDbSchemaAndDiscoveryAndExtendedSearchOrder() {
		final Path expectedFile = getExpectedFile(getExpectedFileName());
		final Path expectedEffortFile = getExpectedFile(getExpectedEffortFileName());
		final EntityId nonNullProjectId = assertNotNull(projectId);
		assertEquals(4, getModuleCount(Creator.API));
		sourceService.resetCaches();
		uploadResources(nonNullProjectId);
		submitJob(jobManager, tracer, new DiscoverCodeJob(nonNullProjectId));
		submitJob(jobManager, tracer, new DiscoverMetricsJob(assertNotNull(projectId), false));
		
		/* This will verify that the modules created through DB schema import are preserved or not. */
		assertEquals(4, getModuleCount(Creator.API));
		
		final List<ModulePojo> modules = moduleService.findModules(b -> b.ofProject(nonNullProjectId).withNames(Arrays.asList("WMIN6652", "TEST_SCHEMA_2", "TABLE_1")));
		final Optional<ModulePojo> programModule = modules.stream()
				.filter(module -> module.getName().equals("WMIN6652"))
				.findAny();
		assertTrue(programModule.isPresent(), "WMIN6652 module should present");
		final Optional<ModulePojo> schema2Module = modules.stream()
				.filter(module -> module.getName().equals("TEST_SCHEMA_2"))
				.findAny();
		assertTrue(schema2Module.isPresent(), "TEST_SCHEMA2 module should present");
		final Optional<ModulePojo> table1Module = modules.stream()
				.filter(module -> module.getName().equals("TABLE_1") && schema2Module.get().getUid().equals(module.getParent().get().getUid()))
				.findAny();
		
		assertTrue(table1Module.isPresent(), "TABLE_1 should present with parent module of TEST_SCHEMA2");
		
		final EntityId table1ModuleId = table1Module.get().identity();
		final var references = moduleService.findRelationship(q -> q.ofDestination(table1ModuleId).withType(RelationshipType.ACCESSES));
		
		assertEquals(1, references.size(), "The ReadsWrites dependency should be created between WMIN6652 and TABLE_1.");
		
		final var readWriteEdge = references.get(0);
		
		/* This will verify that the dependency created between program module (WMIN6652) and module created from schema import (TABLE_1) 
		 * with parent module (TEST_SCHEMA_2), according to the search order */
		assertEquals(readWriteEdge.getSrcModule(), programModule.get().getUid());
		assertEquals(readWriteEdge.getDstModule(), table1ModuleId.getUid());
		
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
		return "WMIN6652";
	}
	
	private long getModuleCount(final Creator creator) {
		return moduleService.countModules(q -> q.ofProject(assertNotNull(projectId)).withCreator(creator));
	}
}

/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.job;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.innowake.innovationlab.commons.model.Column;
import com.innowake.innovationlab.commons.model.LegacyDatabase;
import com.innowake.innovationlab.commons.model.Schema;
import com.innowake.innovationlab.commons.model.Table;

import brave.Tracer;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.integration.discovery.BaseDiscoveryTest;
import innowake.mining.shared.access.FieldInfoService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.FieldInfoPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.SchemaInfoPojo;

/**
 * Validate the changes for DbSchemaImporterJob.
 */
@WithMockUser
class DbSchemaImportJobTest extends DatabaseRelatedTest {
	
	private static final String RESOURCE_FILE_PATH = "./test-resources/innowake/mining/server/job/DbSchema/";
	private static final File RESOURCE_FILE = new File(RESOURCE_FILE_PATH + "Dbschema.json");
	private boolean tableDataPresent = false;
	
	@Autowired
	private Tracer tracer;
	
	@Autowired
	private JobManager jobManager;
	
	@Autowired
	private FieldInfoService dataSchemaService;
	
	@Autowired
	private ModuleService moduleService;
	
	@Test
	void testDbSchemaImporterJob() throws IOException {
		final var projectId = createProject().identity();
		final LegacyDatabase legacyDatabase = createLegacyDatabase();
		final List<Schema> expectedSchema = legacyDatabase.getSchemes();
		assertNotNull(expectedSchema);
		BaseDiscoveryTest.submitJob(jobManager, tracer, new DbSchemaImporterJob(projectId, legacyDatabase));
		final List<SchemaInfoPojo> actualSchema = moduleService.findSchemaInfos(projectId);
		final Map<String, SchemaInfoPojo> actualSchemaMap = new HashMap<>();
		actualSchema.forEach(k -> actualSchemaMap.put(k.name, k));
		assertEquals(actualSchema.size(), expectedSchema.size());
		expectedSchema.forEach(k -> assertDbSchema(k, actualSchemaMap.get(k.getName()), projectId));
		assertTrue("Table data should be present in atleast one Schema.", tableDataPresent);
	}
	
	private void assertDbSchema(final Schema expectedSchema, final SchemaInfoPojo actualSchema, final EntityId projectId) {
		assertEquals(expectedSchema.getName(), actualSchema.name);
		assertEquals(expectedSchema.getTables().size(), actualSchema.tables);
		assertEquals(expectedSchema.getRoutines().size(), actualSchema.procedures);
		assertEquals(expectedSchema.getViews().size(), actualSchema.views);
		assertEquals(expectedSchema.getTriggers().size(), actualSchema.triggers);
		
		if (expectedSchema.getTables().size() > 0) {
			final Table expectedTable = expectedSchema.getTables().get(0);
			final List<ModulePojo> modules = moduleService.findModules(b -> b.ofProject(projectId).withName(expectedTable.getName()));
			
			final ModulePojo actualTable = modules.get(0);
			final List<FieldInfoPojo> fieldInfos = dataSchemaService.find(q -> q.ofProject(projectId)
																				.ofModule(actualTable.identity()));
			final List<Column> columns = expectedTable.getColumns();
			
			final Map<String, FieldInfoPojo> fieldInfoMap = new HashMap<>();
			fieldInfoMap.put(fieldInfos.get(0).getName(), fieldInfos.get(0));
			fieldInfoMap.put(fieldInfos.get(1).getName(), fieldInfos.get(1));
			fieldInfoMap.put(fieldInfos.get(2).getName(), fieldInfos.get(2));
			fieldInfoMap.put(fieldInfos.get(3).getName(), fieldInfos.get(3));
			fieldInfoMap.put(fieldInfos.get(4).getName(), fieldInfos.get(4));
			
			assertEquals(columns.size(), fieldInfos.size());
			assertTableColumns(columns.get(0), fieldInfoMap.get(columns.get(0).getName()));
			assertTableColumns(columns.get(1), fieldInfoMap.get(columns.get(1).getName()));
			assertTableColumns(columns.get(2), fieldInfoMap.get(columns.get(2).getName()));
			assertTableColumns(columns.get(3), fieldInfoMap.get(columns.get(3).getName()));
			assertTableColumns(columns.get(4), fieldInfoMap.get(columns.get(4).getName()));
			tableDataPresent = true;
		}
	}
	
	private void assertTableColumns(final Column column, final FieldInfoPojo fieldInfo) {
		final Map<String, Object> properties = fieldInfo.getProperties()
														.orElseThrow(() -> new IllegalStateException("Properties should not be null.")); 
		assertEquals(column.getName(), fieldInfo.getName());
		assertEquals(column.getDataType(), properties.get("type"));
		assertEquals(column.getDefaultValue(), properties.get("default"));
		assertEquals(String.valueOf(column.getSize()), properties.get("size"));
	}
	
	private LegacyDatabase createLegacyDatabase() throws IOException {
		final String dbSchemaBackupJson = FileUtils.readFileToString(RESOURCE_FILE, StandardCharsets.UTF_8);
		final ObjectMapper mapper = new ObjectMapper();
		return mapper.readValue(dbSchemaBackupJson, LegacyDatabase.class);
	}
	
	private ProjectPojo createProject() {
		final ProjectPojoPrototype project = new ProjectPojoPrototype();
		project.setName("Test Project");
		project.setClient(EntityId.of(1L));
		project.setNatures(Collections.emptySet());
		return projectService.create(project);
	}
}

/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.integration.data;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.innowake.innovationlab.commons.model.LegacyDatabase;

import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.service.DataSchemaImportService;
import innowake.mining.shared.access.FieldInfoService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.FieldInfoPojo;
import innowake.mining.shared.entities.FieldInfoPojoPrototype;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.hashing.LinkHash;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Tests {@link DataSchemaDao}
 */
class DataSchemaServiceTest extends DatabaseRelatedTest {

	@Autowired
	private FieldInfoService fieldInfoService;
	@Autowired
	private DataSchemaImportService dataSchemaImportService;

	@Autowired
	private ModuleService moduleService;
	
	private EntityId projectId = EntityId.VOID;
	private EntityId moduleId = EntityId.VOID;

	private static final String COMMENT = "comment";

	private static final String LEGECY_DATABASE_STRING = "{\"name\":\"jdbc:sqlserver://localhost;databaseName=unit_test_1\","
			+ "\"schemes\":[{\"name\":\"TEST_SCHEMA_1\",\"tables\":[{\"name\":\"TABLE_1\",\"columns\":"
			+ "[{\"name\":\"optname\",\"dataType\":\"sysname\",\"defaultValue\":null,\"autoIncremented\":false},"
			+ "{\"name\":\"value\",\"dataType\":\"bit\",\"defaultValue\":null,\"autoIncremented\":false}],\"indices\":[],\"primaryKey\":null,\"foreignKeys\":[]"
			+ "}],\"views\":[],\"routines\":[],\"triggers\":[]},{\"name\":\"TEST_SCHEMA_2\",\"tables\":[{\"name\":\"TABLE_2\","
			+ "\"columns\":[{\"name\":\"optname\",\"dataType\":\"sysname\",\"defaultValue\":null,\"autoIncremented\":false},"
			+ "{\"name\":\"value\",\"dataType\":\"bit\",\"defaultValue\":null,\"autoIncremented\":false}],\"indices\":[],\"primaryKey\":null,\"foreignKeys\":[]"
			+ "}],\"views\":[],\"routines\":[],\"triggers\":[]}]}";

	@BeforeAll
	void initialize() {
		projectId = createProject("Test Project 1", Long.valueOf(1));
		moduleId = createTestModule("Test Module 1", projectId);
		fieldInfoService.create(new FieldInfoPojoPrototype()
												.setModule(moduleId)
												.setOrdinal(1)
												.setName("Test Field"));
	}

	/*
	 * Find FieldInfo By Module Id and Project Id
	 */
	@Test
	void testFetchFieldInfoByModuleIdAndByProjectId() {
		final List<FieldInfoPojo> fieldinfo = fieldInfoService.find(q -> q.ofProject(projectId).ofModule(moduleId));
		assertEquals(1, fieldinfo.size());
	}

	/*
	 * Test For Insert FieldInfo
	 */
	@Test
	void testInsertFieldInfo() {
		fieldInfoService.create(new FieldInfoPojoPrototype()
				.setModule(moduleId)
				.setOrdinal(2)
				.setName("Test Field 2")
				.setComment(COMMENT));
		
		assertEquals(2, fieldInfoService.find(q -> q.ofProject(projectId).ofModule(moduleId)).size());
	}

	/*
	 * Test For update fieldInfo
	 */
	@Test
	void testUpdateFieldInfo() {
		final List<FieldInfoPojo> fieldInfoList = fieldInfoService.find(q -> q.ofProject(projectId).ofModule(moduleId));
		assertEquals(1, fieldInfoList.size());
		fieldInfoService.update(new FieldInfoPojoPrototype()
							.setId(fieldInfoList.get(0).getId())
							.setModule(moduleId)
							.setOrdinal(1)
							.setName("Updated Field 1")
							.setComment(COMMENT));

		final List<FieldInfoPojo> fieldInfo = fieldInfoService.find(q -> q.ofProject(projectId).ofModule(moduleId));
		assertEquals(1, fieldInfo.size());
		assertEquals("Updated Field 1", fieldInfo.get(0).getName());
		assertEquals(COMMENT, fieldInfo.get(0).getComment().orElse(null));
	}

	/*
	 * Deleting the Specific FieldInfo by ProjectId and ModuleId
	 */
	@Test
	void testDeleteFieldInfos() {
		final EntityId projectId2 = createProject("Test Project 2", Long.valueOf(2));
		final EntityId moduleId2 = createTestModule("Test Module 2", projectId2);
		fieldInfoService.create(new FieldInfoPojoPrototype()
												.setModule(moduleId2)
												.setOrdinal(1)
												.setName("Module For deletion"));

		assertEquals(1, fieldInfoService.find(q -> q.ofProject(projectId2).ofModule(moduleId2)).size());
		fieldInfoService.delete(q -> q.ofProject(projectId2).ofModule(moduleId2));
		assertEquals(0, fieldInfoService.find(q -> q.ofProject(projectId2).ofModule(moduleId2)).size());
	}

	@Test
	void testContainingModuleLinkHash() throws JsonProcessingException {
		final String firstSchemaExpectedHash = LinkHash.calculateLinkHash("TEST_SCHEMA_1", "SQL", "SCHEMA", null, null, null);
		final String secondSchemaExpectedHash = LinkHash.calculateLinkHash("TEST_SCHEMA_2", "SQL", "SCHEMA", null, null, null);
		final LegacyDatabase legacyDatabase = new ObjectMapper().readValue(LEGECY_DATABASE_STRING, LegacyDatabase.class);
		dataSchemaImportService.importSchema(projectId, legacyDatabase);

		final List<ModulePojo> firstTableModule = moduleService.findModules(b -> b.ofProject(projectId).withName("TABLE_1"));
		assertEquals(1, firstTableModule.size(), "Only one module is present with name TABLE_1");
		assertEquals("TABLE_1", firstTableModule.get(0).getName());
		assertTrue(firstTableModule.get(0).getParent().isPresent());
		final ModulePojo firstTableParentModule = moduleService.getModule(firstTableModule.get(0).getParent().get());
		assertEquals(firstSchemaExpectedHash, firstTableParentModule.getLinkHash());

		final List<ModulePojo> secondTableModule = moduleService.findModules(b -> b.ofProject(projectId).withName("TABLE_2"));
		assertEquals(1, secondTableModule.size(), "Only one module is present with name TABLE_2");
		assertEquals("TABLE_2", secondTableModule.get(0).getName());
		final ModulePojo secondTableParentModule = moduleService.getModule(secondTableModule.get(0).getParent().get());
		assertEquals(secondSchemaExpectedHash, secondTableParentModule.getLinkHash());
	}
	
	@Test
	void testModuleLinkHash() throws JsonProcessingException {
		final String schemaLinkHash = LinkHash.calculateLinkHash("TEST_SCHEMA_1", "SQL", "SCHEMA", null, null, null);
		final String tableLinkHash = LinkHash.calculateLinkHash("TABLE_1", "SQL", "TABLE", null, null, schemaLinkHash);
		final LegacyDatabase legacyDatabase = new ObjectMapper().readValue(LEGECY_DATABASE_STRING, LegacyDatabase.class);
		dataSchemaImportService.importSchema(projectId, legacyDatabase);

		final List<ModulePojo> tableModules = moduleService.findModules(q -> q.ofProject(projectId).withName("TABLE_1"));
		assertEquals(1, tableModules.size(), "Only one module is present with name TABLE_1");
		final ModulePojo tableModule = tableModules.get(0);
		assertEquals("TABLE_1", tableModule.getName());
		assertEquals(tableLinkHash, tableModule.getLinkHash());
	}

	private EntityId createProject(final String name , final Long clientId) {
		return projectService.create(new ProjectPojoPrototype()
				.setName(name)
				.setClient(EntityId.of(clientId))
				.setNatures(Collections.emptySet())
			).identity();
	}

	private EntityId createTestModule(final String name, final EntityId projectId) {
		final ModulePojoPrototype cobolProgram = new ModulePojoPrototype();
		cobolProgram.setProject(projectId);
		cobolProgram.setName(name);
		cobolProgram.setOrigin(Origin.CUSTOM);
		cobolProgram.setStorage(Storage.FILE);
		cobolProgram.setIdentification(Identification.IDENTIFIED);
		cobolProgram.setPath("path");
		cobolProgram.setTechnology(Technology.COBOL);
		cobolProgram.setType(Type.PROGRAM);
		cobolProgram.setCreator(Creator.DISCOVERY);
		return moduleService.create(cobolProgram);
	}

}

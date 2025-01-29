/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.Collections;
import java.util.UUID;

import innowake.mining.data.core.MiningContentProvider;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.testing.ModulePojoDummy;
import innowake.mining.shared.model.*;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.ndt.core.assembling.cobol.CobolAssembler.CobolAssemblingObjectType;

/**
 * Tests for {@link MiningContentProvider}.
 */
class OContentProviderTest extends DatabaseRelatedTest {

	private static final EntityId TEST_PROJECT_ID = EntityId.of(4L);
	
	@Autowired
	private ModuleService moduleService;
	@Autowired
	private MiningDataCoreService core;

	private ModulePojo dummy = ModulePojoDummy.build(new ModulePojoPrototype());

	private ModulePojo rootJCLJob = dummy;
	private ModulePojo programA = dummy;
	private ModulePojo copy1 = dummy;
	private ModulePojo copy2 = dummy;
	private ModulePojo copy3 = dummy;
	private ModulePojo proc = dummy;
	private ModulePojo include = dummy;
	private ModulePojo copy4 = dummy;

	@BeforeAll
	void insertTestData() {
		
		rootJCLJob = createModule(TEST_PROJECT_ID, "ROOTJOB", Technology.JCL, Type.JOB, Storage.FILE, "Test jcl job module");
		programA = createModule(TEST_PROJECT_ID, "PROGRAMA", Technology.COBOL, Type.PROGRAM, Storage.FILE, "Test programA module");
		copy1 = createModule(TEST_PROJECT_ID, "COPY1", Technology.COBOL, Type.COPYBOOK, Storage.FILE, "Test copyBook1 module");
		copy2 = createModule(TEST_PROJECT_ID, "COPY2", Technology.COBOL, Type.COPYBOOK, Storage.FILE, "Test copyBook2 module");
		copy3 = createModule(TEST_PROJECT_ID, "COPY3", Technology.COBOL, Type.COPYLIB, Storage.FILE, "Test copyBook3 module");
		proc = createModule(TEST_PROJECT_ID, "PROC", Technology.JCL, Type.PROC, Storage.FILE, "Test proc module");
		include = createModule(TEST_PROJECT_ID, "INCLUDE", Technology.JCL, Type.INCLUDE, Storage.FILE, "Test Include module");
		copy4 = createModule(TEST_PROJECT_ID, "COPY4", Technology.COBOL, Type.COPYLIB, Storage.FILE, "Test copyBook4 module");

		createReference(RelationshipType.CALLS, rootJCLJob.identity(), proc.identity());
		createReference(RelationshipType.INCLUDES, rootJCLJob.identity(), include.identity());
		createReference(RelationshipType.INCLUDES, programA.identity(), copy1.identity());
		createReference(RelationshipType.INCLUDES, programA.identity(), copy2.identity());
		createReference(RelationshipType.INCLUDES, programA.identity(), copy3.identity());
		createReference(RelationshipType.INCLUDES, programA.identity(), copy4.identity());
	}

	/* Find if the included module from the candidates obtained through references is filtered properly based on one CobolAssemblingObjectType */
	@Test
	void testFind() {
		final MiningContentProvider data = new MiningContentProvider(core, programA);
		final ModulePojo module = data.find(programA, copy1.getName(), CobolAssemblingObjectType.COPYBOOK);
		assertEquals(copy1.getContent().orElse(null), assertNotNull(module).getContent().orElse(null));
		assertEquals(copy1.getTechnology(), assertNotNull(module).getTechnology());
		assertEquals(copy1.getType(), assertNotNull(module).getType());
	}

	/* Find if the included module from the candidates obtained through references is filtered properly based on multiple CobolAssemblingObjectType */
	@Test
	void testFind2() {
		final MiningContentProvider data = new MiningContentProvider(core, programA);
		final ModulePojo module = data.find2(programA, copy3.getName(), CobolAssemblingObjectType.COPYBOOK, CobolAssemblingObjectType.COPYLIB);

		assertEquals(copy3.getContent().orElse(null), assertNotNull(module).getContent().orElse(null));
		assertEquals(copy3.getTechnology(), assertNotNull(module).getTechnology());
		assertEquals(copy3.getType(), assertNotNull(module).getType());
	}

	/* Get the content of JCL Proc module from the candidates obtained through references */
	@Test
	void testGetProc() {
		final MiningContentProvider data = new MiningContentProvider(core, rootJCLJob);
		final String content = data.getProc(proc.getName(), Collections.emptyList());
		assertEquals(proc.getContent().orElse(null), content);
	}
	
	/* Get the content of include module from the candidates obtained through references */
	@Test
	void testGetIncludeMember() {
		final MiningContentProvider data = new MiningContentProvider(core, rootJCLJob);
		final String content = data.getIncludeMember(include.getName(), Collections.emptyList());
		assertEquals(include.getContent().orElse(null), content);
	}
	
	/* Test if the included module referenced by the given relationship from the candidates obtained through references is filtered properly */
	@Test
	void testCreateModuleDescription() {
		final MiningContentProvider data = new MiningContentProvider(core, rootJCLJob);
		final ModulePojo module = data.createModuleDescription(programA, copy2.getName() , RelationshipType.INCLUDES, Type.COPYBOOK);
		assertEquals(copy2.getContent().orElse(null), assertNotNull(module).getContent().orElse(null));
		assertEquals(copy2.getTechnology(), assertNotNull(module).getTechnology());
		assertEquals(copy2.getType(), assertNotNull(module).getType());
	}

	/* Test if the included module having different case referenced by the given relationship from the candidates obtained through references is filtered 
	 * properly */
	@Test
	void testModuleDescriptionForCaseSensitiveInculdeName() {
		final MiningContentProvider data = new MiningContentProvider(core, programA);
		final ModulePojo module = data.createModuleDescription(programA, "copy1" , RelationshipType.INCLUDES, Type.COPYBOOK);
		assertEquals(copy1.getContent().orElse(null), assertNotNull(module).getContent().orElse(null));
		assertEquals(copy1.getTechnology(), assertNotNull(module).getTechnology());
		assertEquals(copy1.getType(), assertNotNull(module).getType());
	}

	private ModulePojo createModule(final EntityId projectId, final String name, final Technology technology, final Type type, final Storage storage,
									final String content, final String... path) {
		final ModulePojoPrototype module = new ModulePojoPrototype()
			.setProject(projectId)
			.setName(name)
			.setTechnology(technology)
			.setType(type)
			.setPath(path.toString())
			.setStorage(storage)
			.setIdentification(Identification.IDENTIFIED)
			.setOrigin(Origin.CUSTOM)
			.setContent(content)
			.setCreator(Creator.DISCOVERY);
		final EntityId moduleId = moduleService.create(module);
		return moduleService.findAnyModule(q -> q.byId(moduleId).includeContent(true)).get();
	}
	
	private UUID createReference(final RelationshipType relationship, final EntityId srcModule, final EntityId dstModule) {
		final var reference = new ModuleRelationshipPojoPrototype()
				.setSrcModule(srcModule)
				.setDstModule(dstModule)
				.setRelationship(relationship);

		return moduleService.createRelationship(reference);
	}
}

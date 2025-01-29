/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.dao;

import brave.Tracer;
import graphql.AssertException;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.core.datadictionary.AstNodeToDataDictionaryEntryUtil;
import innowake.mining.data.core.storeast.DefaultStoreAstExecutor;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.job.identification.IdentifyCandidatesJob;
import innowake.mining.server.service.MiningDataCoreService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static innowake.mining.server.integration.discovery.BaseDiscoveryTest.submitJob;
import static innowake.mining.shared.model.Identification.IDENTIFIED;
import static innowake.mining.shared.model.Origin.CUSTOM;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

/**
 * Test cases for {@linkplain AstNodeToDataDictionaryEntryUtil}.
 */
@WithMockUser
@TestMethodOrder(OrderAnnotation.class)
class AstNodeToDataDictionaryEntryDaoTest extends DatabaseRelatedTest {

	private static final String PATH = "/test-resources/innowake/mining/server/job/identification/";
	
	@Autowired
	private JobManager jobManager;

	@Autowired
	private Tracer tracer;
	
	@Autowired
	private MiningDataCoreService core;

	private EntityId moduleAId;
	private EntityId moduleBId;
	private EntityId cpyGId;
	private EntityId cpyHId;
	private AstNodeToDataDictionaryEntryUtil nodeToDDE;
	private List<DataDictionaryPojo> dataDictionariesForModuleA;
	private List<DataDictionaryPojo> dataDictionariesForModuleB;
	private List<DataDictionaryPojo> dataDictionariesForCopyBookA;
	private List<DataDictionaryPojo> dataDictionariesForCopyBookD;
	private List<DataDictionaryPojo> dataDictionariesForCopyBookE;
	private List<DataDictionaryPojo> dataDictionariesForCopyBookF;
	private List<DataDictionaryPojo> dataDictionariesForCopyBookG;
	private List<DataDictionaryPojo> dataDictionariesForCopyBookH;
	private boolean testDataCreated;
	
	private void createTestDataIfRequired() {
		if ( ! testDataCreated) {
			testDataCreated = true;
			final EntityId projectId = createProject().identity();
			moduleAId = createModule(projectId, "MMRS7100", "MMRS7100.cbl", Type.PROGRAM);
			moduleBId = createModule(projectId, "MMRS71012", "MMRS71012.cbl", Type.PROGRAM);
			final EntityId cpyAId = createModule(projectId, "MMRS710A", "MMRS710A.cpy", Type.COPYBOOK);
			final EntityId cpyDId = createModule(projectId, "MMRS710D", "MMRS710D.cpy", Type.COPYBOOK);
			final EntityId cpyEId = createModule(projectId, "MMRS710E", "MMRS710E.cpy", Type.COPYBOOK);
			final EntityId cpyFId = createModule(projectId, "MMRS710F", "MMRS710F.cpy", Type.COPYBOOK);
			cpyGId = createModule(projectId, "MMRS710G", "MMRS710G.cpy", Type.COPYBOOK);
			cpyHId = createModule(projectId, "MMRS710H", "MMRS710H.cpy", Type.COPYBOOK);
			createRelationship(moduleBId, cpyAId);
			createRelationship(moduleBId, cpyDId);
			createRelationship(moduleAId, cpyGId);
			createRelationship(cpyDId, cpyEId);
			createRelationship(cpyDId, cpyFId);
			createRelationship(cpyFId, cpyGId);
			createRelationship(cpyFId, cpyHId);
			submitJob(jobManager, tracer, new IdentifyCandidatesJob(projectId, new ModuleMatcher(Collections.singletonList(moduleAId), List.of(PATH + "MMRS71012.cbl"))));

			dataDictionariesForModuleA = core.dataDictionaryService.find(q -> q.ofModule(moduleAId));
			dataDictionariesForModuleB = core.dataDictionaryService.find(q -> q.ofModule(moduleBId));
			dataDictionariesForCopyBookA = core.dataDictionaryService.find(q -> q.ofModule(cpyAId));
			dataDictionariesForCopyBookD = core.dataDictionaryService.find(q -> q.ofModule(cpyDId));
			dataDictionariesForCopyBookE = core.dataDictionaryService.find(q -> q.ofModule(cpyEId));
			dataDictionariesForCopyBookF = core.dataDictionaryService.find(q -> q.ofModule(cpyFId));
			dataDictionariesForCopyBookG = core.dataDictionaryService.find(q -> q.ofModule(cpyGId));
			dataDictionariesForCopyBookH = core.dataDictionaryService.find(q -> q.ofModule(cpyHId));

			nodeToDDE = new AstNodeToDataDictionaryEntryUtil(new DefaultStoreAstExecutor(), core);
			assertEquals(6, dataDictionariesForModuleA.size());
			assertEquals(13, dataDictionariesForModuleB.size());
			assertEquals(5, dataDictionariesForCopyBookG.size());
			assertEquals(5, dataDictionariesForCopyBookH.size());
		}
	}
	
	@Test
	@Order(1)
	void testFindAstNodeForDdeDefinedInProgram() {
		createTestDataIfRequired();
		final DataDictionaryPojo ddeInProgram = dataDictionariesForModuleA.stream().findAny().orElseThrow(
				() -> new AssertException("Expected a dde to be found"));
		final List<AstNodePojo> astNode = nodeToDDE.findAstNode(ddeInProgram);
		assertEquals(1, astNode.size());
		assertTrue(astNode.get(0).getLabel().contains(ddeInProgram.getName()));
		
		final DataDictionaryPojo retrievedDde = nodeToDDE.findDatatDictionaryEntry(astNode.get(0));
		assertEquals(ddeInProgram.identity(), retrievedDde.identity());
		assertEquals(ddeInProgram.getName(), retrievedDde.getName());
	}
	
	@Test
	@Order(2)
	void testFindAstNodeForDdeDefinedInCopybook() {
		createTestDataIfRequired();
		final DataDictionaryPojo ddeInCpy = dataDictionariesForCopyBookH.stream()
				.filter(c -> c.getName().equals("MMRS-CURRENT-TIME-HHMMSSMS"))
				.findAny().orElseThrow(() -> new AssertException("Expected a dde to be found"));
		final List<AstNodePojo> astNode = nodeToDDE.findAstNode(ddeInCpy);
		assertEquals(1, astNode.size());
		assertTrue(astNode.get(0).getLabel().contains(ddeInCpy.getName()));
		
		final DataDictionaryPojo retrievedDde = nodeToDDE.findDatatDictionaryEntry(astNode.get(0));
		assertEquals(ddeInCpy.identity(), retrievedDde.identity());
		assertEquals(ddeInCpy.getName(), retrievedDde.getName());
	}
	
	@Test
	@Order(3)
	void testFindAstNodeForDdeDefinedInCopybookIncludedInMultiplePrograms() {
		createTestDataIfRequired();
		final DataDictionaryPojo ddeInCpy = dataDictionariesForCopyBookG.stream()
				.filter(c -> c.getName().equals("MMRS-CURRENT-DATE-JHJJMMDD"))
				.findAny().orElseThrow(() -> new AssertException("Expected a dde to be found"));
		final List<AstNodePojo> astNodes = nodeToDDE.findAstNode(ddeInCpy);
		assertEquals(2, astNodes.size());
		final AstNodePojo astNode1 = astNodes.get(0);
		final AstNodePojo astNode2 = astNodes.get(1);
		assertTrue(astNode1.getLabel().contains(ddeInCpy.getName()));
		assertTrue(astNode2.getLabel().contains(ddeInCpy.getName()));
		
		final DataDictionaryPojo retrievedDde1 = nodeToDDE.findDatatDictionaryEntry(astNode1);
		final DataDictionaryPojo retrievedDde2 = nodeToDDE.findDatatDictionaryEntry(astNode2);
		
		assertEquals(ddeInCpy.identity(), retrievedDde1.identity());
		assertEquals(ddeInCpy.getName(), retrievedDde1.getName());
		assertEquals(ddeInCpy.identity(), retrievedDde2.identity());
		assertEquals(ddeInCpy.getName(), retrievedDde2.getName());
	}
	
	@Test
	@Order(4)
	void testFindConnectionForModuleWithCopybooks() {
		createTestDataIfRequired();
		final List<Tuple2<AstNodePojo, DataDictionaryPojo>> connections = nodeToDDE.findConnections(moduleAId);
		assertEquals(dataDictionariesForModuleA.size() + dataDictionariesForCopyBookG.size(), connections.size());
		connections.forEach(t -> assertTrue(t.a.getLabel().contains(t.b.getName())));
	}
	
	@Test
	@Order(5)
	void testFindConnectionForModuleIncludingOtherModules() {
		createTestDataIfRequired();
		final List<DataDictionaryPojo> ddes = new ArrayList<>();
		ddes.addAll(dataDictionariesForCopyBookA);
		ddes.addAll(dataDictionariesForCopyBookD);
		ddes.addAll(dataDictionariesForCopyBookE);
		ddes.addAll(dataDictionariesForCopyBookF);
		ddes.addAll(dataDictionariesForCopyBookG);
		ddes.addAll(dataDictionariesForCopyBookH);
		ddes.addAll(dataDictionariesForModuleB);
		final AstNodeToDataDictionaryEntryUtil dao = new AstNodeToDataDictionaryEntryUtil(new DefaultStoreAstExecutor(), core);
		final List<Tuple2<AstNodePojo, DataDictionaryPojo>> connections = dao.findConnections(moduleBId);
		assertEquals(59, ddes.size());
		assertEquals(ddes.size(), connections.size());
		connections.forEach(t -> assertTrue(t.a.getLabel().contains(t.b.getName()), () -> String.format("Matching DDE %s to AstNode %s failed", t.b.getName(), t.a.getLabel())));
	}

	private ProjectPojo createProject() {
		return projectService.create(new ProjectPojoPrototype()
				.setName("Test Project")
				.setClient(EntityId.of(1L))
				.setNatures(Collections.emptySet()));
	}
	
	private EntityId createModule(final EntityId projectId, final String name, final String file, final Type type) {
		final String content = getContent(file);
		return core.moduleService.create(new ModulePojoPrototype()
				.setName(name)
				.setProject(projectId)
				.setTechnology(Technology.COBOL)
				.setType(type)
				.setStorage(Storage.FILE)
				.setIdentification(IDENTIFIED)
				.setOrigin(CUSTOM)
				.setPath(AstNodeToDataDictionaryEntryDaoTest.PATH + file)
				.setContent(content)
				.setCreator(Creator.DISCOVERY)
			);
	}
	
	private void createRelationship(final EntityId from, final EntityId to) {
		core.moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.INCLUDES)
				.setSrcModule(from)
				.setDstModule(to)
			);
	}
	
	private String getContent(final String file) {
		final Path path = Paths.get(System.getProperty("user.dir"), PATH, file);
		try {
			return Files.readString(path, Charset.forName("Cp1252"));
		} catch (final IOException e) {
			fail(e.getLocalizedMessage());
			throw new IllegalStateException("Exception occured while file reading", e);
		}
	}
}

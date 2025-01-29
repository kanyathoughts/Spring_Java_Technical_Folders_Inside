/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.service;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.server.job.identification.AbstractIdentificationTest;
import innowake.mining.shared.access.FieldInfoService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.FieldInfoPojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.codeviewer.AssembledContent;
import innowake.mining.shared.model.codeviewer.CodeViewerLink;
import innowake.mining.shared.model.codeviewer.CodeViewerLinkModel;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraph;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode;
import innowake.mining.shared.model.datalineage.graph.FieldNode;
import innowake.mining.shared.model.datalineage.graph.ModuleNode;
import innowake.mining.shared.model.datalineage.graph.StatementNode;

/**
 * Contains tests for CodeViewerLinkService
 */
@WithMockUser
@TestMethodOrder(OrderAnnotation.class)
class CodeViewerLinkServiceTest extends AbstractIdentificationTest {
	
	@Autowired
	private ExecutorService executorService;
	
	@Autowired
	private CodeViewerLinkService codeViewerLinkService;
	
	@Autowired
	private CodeViewerDataLineageService codeViewerDataLineageService;
	
	@Autowired
	private ContentAssemblingService contentAssemblingService;
	
	@Autowired
	private FieldInfoService dataSchemaService;
	
	protected static final String RESOURCE_PATH2 = "/test-resources/innowake/mining/server/job/identification/wmin-5715/";
	protected static final String RESOURCE_PATH_DL = "/test-resources/innowake/mining/server/job/identification/WMIN10724/";

	@Test
	@Order(1)
	void testGetAssembledContentAndLinks() {
		final EntityId moduleIdA = createCobolCopybook(PROJECT_ID_1, "WSCOPY", "WSCOPY.cpy", RESOURCE_PATH);
		final EntityId moduleIdB = createCobolProgram(PROJECT_ID_1, "WSPROG", "WSPROG.cbl", RESOURCE_PATH);
		moduleService.createRelationship(new ModuleRelationshipPojoPrototype().setRelationship(RelationshipType.INCLUDES)
				.setSrcModule(moduleIdB).setDstModule(moduleIdA));
		/* store AST for Module ID */
		executorService.executeStoreAst(PROJECT_ID_1, moduleIdB);
		final CodeViewerLinkModel codeviewerLinkModelNotAssembled = codeViewerLinkService.getLinkModel(PROJECT_ID_1, moduleIdB, false);
		final CodeViewerLinkModel codeviewerLinkModelAssembled = codeViewerLinkService.getLinkModel(PROJECT_ID_1, moduleIdB, true);
		final AssembledContent content = contentAssemblingService.getAssembledContent(PROJECT_ID_1, moduleIdB);
		assertEquals(1, content.getInclusions().size(), "The Module has one CopyBook.");
		assertEquals(moduleIdA, content.getInclusions().get(0).getOriginModule().identity(), "The Module Id of the inclusion should be equal to ModuleA.");

		assertEquals(0, codeviewerLinkModelNotAssembled.getLinks().size(), "The Main Module has not links.");
		assertEquals(16, codeviewerLinkModelAssembled.getLinks().size(), "The assembled content links must be correct.");
		for (final CodeViewerLink codeviewerlink : codeviewerLinkModelAssembled.getLinks()) {
			assertEquals(moduleIdB, codeviewerlink.getFromModuleId(), "All links are in the Copybook.");
			assertTrue(codeviewerlink.getFromRange().getStartLineNumber() > 42, "All the links should start where the copybook starts.");
			if (codeviewerlink.getFromModuleLocation().getOffset().equals(1875)) {
				assertEquals("01 WS-NUMD PIC 9(9) VALUE 100.", codeviewerlink.getTargetLabel(),
						"Target label for link with the offset 1875 should be correct.");
				assertNotNull(codeviewerlink.getToModuleLocation(), "The Link should have to moduleLocation");
				assertEquals(1292, innowake.lib.core.lang.Assert.assertNotNull(codeviewerlink.getToModuleLocation()).getOffset(),
						"The Link should have correct offset.");
			}
		}
	}
	
	@Test
	@Order(2)
	void testGetFormatForCopyBooksIncludedInOtherCopyBooks() {
		final EntityId cpyAId = createCobolCopybook(PROJECT_ID_1, "MMRS710A", "MMRS710A.cpy", RESOURCE_PATH2);
		final EntityId cpyHId = createCobolCopybook(PROJECT_ID_1, "MMRS710H", "MMRS710H.cpy", RESOURCE_PATH2);
		final EntityId programId = createCobolProgram(PROJECT_ID_1, "MMRS71012", "MMRS71012.cbl", RESOURCE_PATH2);
		
		moduleService.createRelationship(new ModuleRelationshipPojoPrototype().setRelationship(RelationshipType.INCLUDES)
				.setSrcModule(programId).setDstModule(cpyAId));
		moduleService.createRelationship(new ModuleRelationshipPojoPrototype().setRelationship(RelationshipType.INCLUDES)
				.setSrcModule(cpyAId).setDstModule(cpyHId));
		
		/* store AST for Module ID */
		executorService.executeStoreAst(PROJECT_ID_1, programId);
		final CodeViewerLinkModel codeviewerLinkModelNotAssembled = codeViewerLinkService.getLinkModel(PROJECT_ID_1, programId, false);
		final CodeViewerLinkModel codeviewerLinkModelAssembled = codeViewerLinkService.getLinkModel(PROJECT_ID_1, programId, true);
		final AssembledContent content = contentAssemblingService.getAssembledContent(PROJECT_ID_1, programId);
		assertEquals(1, content.getInclusions().size(), 1, "The program has one inclusion.");
		assertEquals(63, codeviewerLinkModelNotAssembled.getLinks().size(), "The program has one inclusion.");
		assertEquals(64, codeviewerLinkModelAssembled.getLinks().size(), "The program has one inclusion.");
		
	}
	
	@Test
	@Order(3)
	void testCodeViewerDataLineageService() {
		final EntityId moduleIdA = createCobolProgram(PROJECT_ID_1, "WMIN11406", "WMIN11406.cbl", RESOURCE_PATH);
		/* store AST for Module ID */
		executorService.executeStoreAst(PROJECT_ID_1, moduleIdA);
		
		/* Checks that this method does not crash due to unhandled errors */
		final CodeViewerLinkModel cvl = codeViewerDataLineageService.getDataFlowLinksForField(PROJECT_ID_1, moduleIdA, Integer.valueOf(173), true);
		
		assertEquals(Integer.valueOf(2), cvl.getLinks().size());
	}
	
	@Test
	@Order(4)
	void testCodeViewerDataLineageServiceWithGroupRedefines() {
		final var moduleIdA = createCobolProgram(PROJECT_ID_1, "WMIN10724", "WMIN10724.cbl", RESOURCE_PATH_DL);
		/* store AST for Module ID */
		executorService.executeStoreAst(PROJECT_ID_1, moduleIdA);
		
		final DataFlowGraph dfg = codeViewerDataLineageService.getDataFlowGraphForField(PROJECT_ID_1, moduleIdA, Integer.valueOf(539), false, null);
		
		final Set<StatementNode> statementNodes = dfg.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.STATEMENT))
				.map(StatementNode.class::cast).collect(Collectors.toSet());
		assertEquals(2, statementNodes.size());
		final Set<FieldNode> fieldNodes = dfg.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.FIELD))
				.map(FieldNode.class::cast).collect(Collectors.toSet());
		assertEquals(Integer.valueOf(4), fieldNodes.size());
	}
	
	@Test
	@Order(5)
	void testCodeViewerDataLineageServiceWithMultipleDepth() {
		final var moduleIdA = createCobolProgram(PROJECT_ID_1, "WMIN10724MAIN", "WMIN10724MAIN.cbl", RESOURCE_PATH_DL);
		final var moduleIdB = createCobolProgram(PROJECT_ID_1, "WMIN10724UTIL", "WMIN10724UTIL.cbl", RESOURCE_PATH_DL);
		final var moduleIdC = createCobolProgram(PROJECT_ID_1, "WMIN10724UTILITY", "WMIN10724UTILITY.cbl", RESOURCE_PATH_DL);
		makeCallReference(moduleIdA, moduleIdB, new ModuleLocation(299, 10));
		makeCallReference(moduleIdB, moduleIdC, new ModuleLocation(338, 10));
		/* store AST for Module ID */
		executorService.executeStoreAst(PROJECT_ID_1, moduleIdA);
		executorService.executeStoreAst(PROJECT_ID_1, moduleIdB);
		executorService.executeStoreAst(PROJECT_ID_1, moduleIdC);
		
		/* Trace from 1st module - Ensure the entire data flow for the specific field is traced in all the modules*/
		DataFlowGraph dfg = codeViewerDataLineageService.getDataFlowGraphForField(PROJECT_ID_1, moduleIdA, Integer.valueOf(392), false, null);
		assertDataFlowGraphNodes(dfg, 19, 5, 5, 3);
		
		/* Trace from 2nd module - Should fetch the same results as previous module but currently call does not have
		the information about the associated field */
		dfg = codeViewerDataLineageService.getDataFlowGraphForField(PROJECT_ID_1, moduleIdB, Integer.valueOf(368), false, null);
		assertDataFlowGraphNodes(dfg, 14, 3, 4, 3);
		
		/* Trace from 3rd module - Should fetch the same results as previous modules but currently call does not have
		the information about the associated field */
		dfg = codeViewerDataLineageService.getDataFlowGraphForField(PROJECT_ID_1, moduleIdC, Integer.valueOf(258), false, null);
		assertDataFlowGraphNodes(dfg, 14, 3, 4, 3);

		/* Trace different filed */
		dfg = codeViewerDataLineageService.getDataFlowGraphForField(PROJECT_ID_1, moduleIdC, Integer.valueOf(338), false, null);
		assertDataFlowGraphNodes(dfg, 4, 2, 1, 1);
	}

	@Test
	@Order(6)
	void testCopybookFieldtracing() {
		final var moduleIdA = createCobolProgram(PROJECT_ID_1, "WMIN10724PROGRAM1", "WMIN10724PROGRAM1.cbl", RESOURCE_PATH_DL);
		final var moduleIdB = createCobolProgram(PROJECT_ID_1, "WMIN10724PROGRAM2", "WMIN10724PROGRAM2.cbl", RESOURCE_PATH_DL);
		final var copybookModuleId = createCobolCopybook(PROJECT_ID_1, "WMIN10724COPYBOOK", "WMIN10724COPYBOOK.cpy", RESOURCE_PATH_DL);
		makeIncludesReference(moduleIdA, copybookModuleId);
		makeIncludesReference(moduleIdB, copybookModuleId);
		/* store AST for Module ID */
		executorService.executeStoreAst(PROJECT_ID_1, moduleIdA);
		executorService.executeStoreAst(PROJECT_ID_1, moduleIdB);
		
		/* Trace copybook field from program */
		DataFlowGraph dfg = codeViewerDataLineageService.getDataFlowGraphForField(PROJECT_ID_1, moduleIdA, Integer.valueOf(262), false, null);
		assertDataFlowGraphNodes(dfg, 3, 1, 1, 1);
		
		/* Trace copybook field from copybook */
		dfg = codeViewerDataLineageService.getDataFlowGraphForField(PROJECT_ID_1, copybookModuleId, Integer.valueOf(13), false, null);
		assertDataFlowGraphNodes(dfg, 6, 2, 2, 2);
				
		/* Trace copybook field from program in assembled mode */
		dfg = codeViewerDataLineageService.getDataFlowGraphForField(PROJECT_ID_1, moduleIdB, Integer.valueOf(395), true, null);
		assertDataFlowGraphNodes(dfg, 3, 1, 1, 1);		
	}
	
	@Test
	@Order(7)
	void testExecFieldtracing() {
		final var moduleIdA = createCobolProgram(PROJECT_ID_1, "WMIN10724EXECPROGRAM", "WMIN10724EXECPROGRAM.cbl", RESOURCE_PATH_DL);
		final var execSqlTable = createFileSection(PROJECT_ID_1, "", "IW_SQL_TEST", "", Technology.SQL, Type.TABLE);
		final var fieldInfoList = Arrays.asList(new FieldInfoPojoPrototype().setOrdinal(1).setName("ALPHA_SHORT"), new FieldInfoPojoPrototype().setOrdinal(2).setName("DATUM"));
		insertFieldInfo(execSqlTable, fieldInfoList);
		makeReadsWritesReference(moduleIdA, execSqlTable, new ModuleLocation(429, 303), "STORE");
		/* store AST for Module ID */
		executorService.executeStoreAst(PROJECT_ID_1, moduleIdA);
		
		/* Trace exec sql field from program */
		final DataFlowGraph dfg = codeViewerDataLineageService.getDataFlowGraphForField(PROJECT_ID_1, moduleIdA, Integer.valueOf(634), true, null);
		assertDataFlowGraphNodes(dfg, 12, 3, 3, 2);
	}
	
	protected void makeCallReference(final EntityId fromId, final EntityId toId, @Nullable final ModuleLocation fromModuleLocation) {
		final var reference = new ModuleRelationshipPojoPrototype()
									.setRelationship(RelationshipType.CALLS)
									.setSrcModule(fromId)
									.setDstModule(toId);
		if (fromModuleLocation != null) {
			reference.setSrcLocation(fromModuleLocation);
		}
		moduleService.createRelationship(reference);
	}
	
	protected void makeIncludesReference(final EntityId fromId, final EntityId toId) {
		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.INCLUDES)
				.setSrcModule(fromId)
				.setDstModule(toId));
	}
	
	protected void makeReadsWritesReference(final EntityId fromId, final EntityId toId, final ModuleLocation fromModuleLocation, final String dbAccessType) {
		final var ref = new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.ACCESSES)
				.setSrcModule(fromId)
				.setDstModule(toId);
		
		/* Set the module location from where the exec sql statement is set */
		ref.setSrcLocation(fromModuleLocation);
		
		/* Put the correct DB_ACCESS_TYPE into properties */
		final Map<String, Object> properties = new HashMap<>();
		properties.put(ModelAttributeKey.DB_ACCESS_TYPE.toString(), List.of(dbAccessType));
		ref.setProperties(properties);
		
		moduleService.createRelationship(ref);
	}
	
	private void assertDataFlowGraphNodes(final DataFlowGraph dfg, final int dfgNodeSize, final int statementNodesSize, final int fieldNodesSize,
			final int moduleNodesSize) {
		assertEquals(dfgNodeSize, dfg.getNodes().size());
		final Set<StatementNode> statementNodes = dfg.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.STATEMENT))
				.map(StatementNode.class::cast).collect(Collectors.toSet());
		/* Statement nodes 4 for Student ID and 2 for Student Name (Related field) */
		assertEquals(statementNodesSize, statementNodes.size());
		final Set<FieldNode> fieldNodes = dfg.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.FIELD))
				.map(FieldNode.class::cast).collect(Collectors.toSet());
		assertEquals(fieldNodesSize, fieldNodes.size());
		final Set<ModuleNode> moduleNode = dfg.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.MODULE))
				.map(ModuleNode.class::cast).collect(Collectors.toSet());
		assertEquals(moduleNodesSize, moduleNode.size());
	}
	
	private <T> void insertFieldInfo(final EntityId moduleId, final List<FieldInfoPojoPrototype> fieldInfo) {
		for (int i = 0; i < fieldInfo.size(); i++) {
			dataSchemaService.create(fieldInfo.get(i)
										.setModule(moduleId));
		}
	}
	
}

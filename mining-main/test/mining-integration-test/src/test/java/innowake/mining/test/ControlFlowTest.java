/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;

import innowake.mining.shared.model.job.ResultStatus;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.hamcrest.CoreMatchers;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.google.common.collect.Lists;

import innowake.lib.common.operation.AssertionException;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.controlflow.ControlFlowServiceProvider;
import innowake.mining.client.service.job.JobServiceProvider;
import innowake.mining.client.service.module.ModuleServiceProvider;
import innowake.mining.client.service.reference.ReferenceServiceProvider;
import innowake.mining.data.access.postgres.AstPgDao;
import innowake.mining.data.access.postgres.ModulePgDao;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.ast.AstModuleRelationshipType;
import innowake.mining.shared.entities.ast.AstRelationshipType;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.controlflow.ControlFlowGraph;
import innowake.mining.shared.model.controlflow.ControlFlowNode;
import innowake.mining.shared.model.controlflow.ControlFlowEntity;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.JobStatus;
import innowake.mining.test.util.JobStatusUtil;
import innowake.ndt.core.parsing.ast.model.statement.CallInternalStatement;

/**
 * Integration tests for the AST node control flow service.
 */
class ControlFlowTest extends IntegrationTest {

	private static final String[] NODELABELS = {
			"PARAG-PARA.", "DISPLAY :'HELLO WORLD'", "DISPLAY:'MORE TEXT TO CHECK ELLIPSIS WORKING AS EXPECTED'", "STOP RUN"
	};
	private static final String[] LIMITED_NODELABELS = {
			"PARAG-PARA.", "DISPLAY :'HELLO W...", "DISPLAY:'MORE TEX...", "STOP RUN"
	};
	private static final Integer[] OFFSETS = {
			Integer.valueOf(158), Integer.valueOf(184), Integer.valueOf(221), Integer.valueOf(293)
	};
	private static final Integer[] LENGTH = {
			Integer.valueOf(11), Integer.valueOf(22), Integer.valueOf(57), Integer.valueOf(8)
	};
	private static final EntityId ONE = EntityId.of(1l);
	private static final EntityId TWO = EntityId.of(2l);
	private final ControlFlowServiceProvider controlFlowServiceProvider = MiningApiClient.controlFlowService(getConnectionInfo());
	private final ModuleServiceProvider moduleServiceProvider = MiningApiClient.moduleService(getConnectionInfo());
	private final JobServiceProvider jobServiceProvider = MiningApiClient.jobService(getConnectionInfo());
	private final ReferenceServiceProvider referenceServiceProvider = MiningApiClient.referenceService(getConnectionInfo());
	private EntityId moduleId = EntityId.of(0l);
	private EntityId moduleIdEdgeLabel = EntityId.of(0l);
	private EntityId moduleIdJclJob = EntityId.of(0l);
	private EntityId moduleIdWithAnnotations = EntityId.of(0l);

	@BeforeEach
	void getModuleIdForTest() throws IOException {
		final ModulePgDao moduleDao = new ModulePgDao(getDataSource());

		createTestDataWithMetadata();

		final List<String> paths = moduleDao.findModulesLightweight(q -> q.filterHasSource(true)
																			.notWithNameLike("MMRS7102")).stream()
													.map(m -> m.getPath())
													.collect(Collectors.toList());

		final Result<String> resultProjectOne = controlFlowServiceProvider.calculateControlFlowGraphs()
				.setProjectId(ONE)
				.setModulePaths(paths)
				.execute();

		JobStatusUtil.checkJobStatus(resultProjectOne.getValue().get(), jobServiceProvider);

		final Map<String, ModuleLightweightPojo> modules = moduleDao.findModulesLightweight(q -> q.withNames(List.of("DISPLAYPGM", "IO001A", "MMRS710J", "EXECSQL")))
																	.stream()
																	.collect(Collectors.toMap(m -> m.getName(), Function.identity()));
		var module = modules.get("DISPLAYPGM");
		if (module != null) {
			this.moduleId = module.identity();
		} else {
			throw new IllegalStateException("Module 'DISPLAYPGM' must exists in test data");
		}

		module = modules.get("IO001A");
		if (module != null) {
			this.moduleIdEdgeLabel = module.identity();
		} else {
			throw new IllegalStateException("Module 'IO001A' must exists in test data");
		}

		module = modules.get("MMRS710J");
		if (module != null) {
			this.moduleIdJclJob = module.identity();
		} else {
			throw new IllegalStateException("Module 'MMRS710J' must exists in test data");
		}

		module = modules.get("EXECSQL");
		if (module != null) {
			this.moduleIdWithAnnotations = module.identity();
		} else {
			throw new IllegalStateException("Module 'EXECSQL' must exists in test data");
		}
	}
	
	@Test
	void testGetControlFlowForModuleAndNodeLabel() throws IOException {
		final Result<ControlFlowGraph> resultGetControlFlow = controlFlowServiceProvider.getControlFlowGraphForModule()
				.setProjectId(ONE)
				.setModuleId(moduleId)
				.setCharacterLimit(null)
				.execute();
		assertEquals(200, resultGetControlFlow.getStatusCode());
		final ControlFlowGraph graph = resultGetControlFlow.getValue().get();
		assertEquals(6, graph.nodes.size());
		var astNodes = graph.nodes.stream().filter(node -> ControlFlowEntity.AST_NODE.equals(node.entity)).collect(Collectors.toList());
		for (int i = 0; i < OFFSETS.length; i++) {
			assertEquals(OFFSETS[i], astNodes.get(i).offset);
			assertEquals(NODELABELS[i], astNodes.get(i).label);
			assertEquals(LENGTH[i], astNodes.get(i).length);
		}
	}
	
	@Test
	void testGetControlFlowForModuleWithAnnotations() throws IOException {
		final Result<ControlFlowGraph> resultGetControlFlow = controlFlowServiceProvider.getControlFlowGraphForModule()
				.setProjectId(ONE)
				.setModuleId(moduleIdWithAnnotations)
				.setCharacterLimit(null)
				.execute();
		assertEquals(200, resultGetControlFlow.getStatusCode());
		final ControlFlowGraph graph = resultGetControlFlow.getValue().get();
		/* AST has just a single GOBACK node, 1 entry and 1 exit node and 3 ast nodes */
		assertEquals(6, graph.nodes.size());
		assertEquals(2, graph.nodes.stream().filter(n -> ControlFlowEntity.TERMINAL == n.entity).count());
		assertEquals(3, graph.nodes.stream().filter(n -> ControlFlowEntity.ANNOTATION == n.entity).count());
		/* Module has three Annotations */
		assertEquals(3, graph.annotations.size());
		/* AST is related to only one Module */
		assertEquals(1, graph.relatedModules.size());
		assertTrue(graph.nodes.stream().map(n -> n.type).collect(Collectors.toList())
				.containsAll(Arrays.asList(AstModuleRelationshipType.ENTRY.name(), AstModuleRelationshipType.RETURN.name())));
	}
	
	@Test
	void testGetControlFlowNodeLabelWithLimit() throws IOException {
		final Result<ControlFlowGraph> resultGetControlFlow = controlFlowServiceProvider.getControlFlowGraphForModule()
				.setProjectId(ONE)
				.setModuleId(moduleId)
				.setCharacterLimit(Integer.valueOf(20))
				.execute();
		assertEquals(200, resultGetControlFlow.getStatusCode());
		final ControlFlowGraph graph = resultGetControlFlow.getValue().get();
		assertEquals(6, graph.nodes.size());
		for (int i = 0; i < LIMITED_NODELABELS.length; i++) {
			final String label = graph.nodes.get(i).label;
			assertEquals(LIMITED_NODELABELS[i], label);
			if (label != null) {
				assertTrue(String.format("Label length should be atmost 21 (label : %s)", label), label.length() <= 20);
			}
		}
	}
	
	@Test
	void testCalculateControlFlowJob() throws Exception {
		/* Asserting control flow is not available for test module MMRS7102 */
		final Result<ModulePojo> resultModuleBefore = moduleServiceProvider.findModuleById().setProjectId(TWO).setModuleId(EntityId.of(2003l)).execute();
		assertEquals(200, resultModuleBefore.getStatusCode());
		final var moduleBefore = resultModuleBefore.getValue().get();
		final boolean isCFGCalculatedBefore = isControlFlowCalculated(moduleBefore);
		assertFalse("ControlFlowCalculated should be false before calculation", isCFGCalculatedBefore);
		
		/* Calculating Control Flow request call for test module MMRS7102 */
		final Optional<String> path = moduleBefore.getPath();
		if (path.isEmpty()) {
			fail("Path of module with id '2003' in project '2' must be present.");
		}
		
		final Result<String> resultCalculateCFG = controlFlowServiceProvider.calculateControlFlowGraphs()
				.setProjectId(TWO)
				.setModulePaths(Lists.newArrayList(path.get())) /** using MMRS7102 module of projectId 2*/
				.execute();
		assertEquals(202, resultCalculateCFG.getStatusCode());
		final String jobId = resultCalculateCFG.getValue().get();
		assertNotNull(jobId);
		
		final JobInformation jobInformation = JobStatusUtil.checkJobStatus(jobId, jobServiceProvider);
		
		/* Asserting control flow is available for test module MMRS7102 */
		if (jobInformation.getStatus() == JobStatus.SUCCESS) {
			final Result<ModulePojo> resultModuleAfter = moduleServiceProvider.findModuleById().setProjectId(TWO).setModuleId(EntityId.of(2003l)).execute();
			assertEquals(200, resultModuleAfter.getStatusCode());
			final boolean isCFGCalculatedAfter = isControlFlowCalculated(resultModuleAfter.getValue().get());
			assertTrue("ControlFlowCalculated should be true after calculation", isCFGCalculatedAfter);
		} else {
			throw new AssertionException("Calculate Control Flow Job Failed in ControlFlowTest.java");
		}
	}
	
	@Test
	void testCalculateControlFlowJobForWrongModule() throws Exception {
		/* Calculating Control Flow request call for test module MMRS7102 */
		final Result<ModulePojo> resultModuleBefore = moduleServiceProvider.findModuleById().setProjectId(TWO).setModuleId(EntityId.of(2003l)).execute();
		final var moduleBefore = resultModuleBefore.getValue().get();
		final boolean isCFGCalculatedBefore = isControlFlowCalculated(moduleBefore);
		assertFalse("ControlFlowCalculated should be false before calculation", isCFGCalculatedBefore);
		final Optional<String> path = moduleBefore.getPath();
		if (path.isEmpty()) {
			fail("Path of module with id '2003' in project '2' must be present.");
		}
		final Result<String> resultCalculateCFG = controlFlowServiceProvider.calculateControlFlowGraphs()
				.setProjectId(ONE)
				.setModulePaths(Lists.newArrayList(path.get())) /** using MMRS7102 module*/
				.execute();
		assertEquals(202, resultCalculateCFG.getStatusCode());
		final String jobId = resultCalculateCFG.getValue().get();
		assertNotNull(jobId);
		
		final JobInformation jobInformation = JobStatusUtil.checkJobStatus(jobId, jobServiceProvider);
		
		/* Assert that no control flow is available for test module MMRS7102 */
		if (jobInformation.getStatus() == JobStatus.SUCCESS) {
			final Result<ModulePojo> resultModuleAfter = moduleServiceProvider.findModuleById().setProjectId(TWO).setModuleId(EntityId.of(2003l)).execute();
			assertEquals(200, resultModuleAfter.getStatusCode());
			final boolean isCFGCalculatedAfter = isControlFlowCalculated(resultModuleAfter.getValue().get());
			assertFalse("ControlFlowCalculated should be false before calculation", isCFGCalculatedAfter);
		} else {
			throw new AssertionException("No control flow must be available for an incorrect module");
		}
	}
	
	@Test
	void testCalculateControlFlowJobWithId() throws Exception {
		/* Asserting control flow is not available for test module MMRS7102 */
		final Result<ModulePojo> resultModuleBefore = moduleServiceProvider.findModuleById().setProjectId(TWO).setModuleId(EntityId.of(2003l)).execute();
		assertEquals(200, resultModuleBefore.getStatusCode());
		final var moduleBefore = resultModuleBefore.getValue().get();
		final boolean isCFGCalculatedBefore = isControlFlowCalculated(moduleBefore);
		assertFalse("ControlFlowCalculated should be false before calculation", isCFGCalculatedBefore);
		
		/* Calculating Control Flow request call for test module MMRS7102 */
		final String jobId = calculateControlFlowGraph(TWO, EntityId.of(2003l), false);
		assertNotNull(jobId);
		JobStatusUtil.checkJobStatus(jobId, jobServiceProvider);
		
		/* Asserting control flow is available for test module MMRS7102 */
		final Result<ModulePojo> resultModuleAfter = moduleServiceProvider.findModuleById().setProjectId(TWO).setModuleId(EntityId.of(2003l)).execute();
		assertEquals(200, resultModuleAfter.getStatusCode());
		final boolean isCFGCalculatedAfter = isControlFlowCalculated(resultModuleAfter.getValue().get());
		assertTrue("ControlFlowCalculated should be true after calculation", isCFGCalculatedAfter);
	}
	
	/**
	 * Verifies the recompute AST flag
	 *
	 * @throws Exception error while performing mvc perform
	 */
	@Test
	void testCalculateControlFlowJobResetAst() throws Exception {	
		final var moduleId = EntityId.of(2016l);
		final var projectId = ONE;
	
		final String jobId = calculateControlFlowGraph(projectId, moduleId, false);
		assertNotNull(jobId);
		JobStatusUtil.checkJobStatus(jobId, jobServiceProvider);
		final List<UUID> astNodesInitialRun = getAstNodesForModule(projectId, moduleId);
	
		final String jobId1 = calculateControlFlowGraph(projectId, moduleId, false);
		assertNotNull(jobId1);
		JobStatusUtil.checkJobStatus(jobId1, jobServiceProvider);
		final List<UUID> astNodesReRun = getAstNodesForModule(projectId, moduleId);
	
		final String jobId2 = calculateControlFlowGraph(projectId, moduleId, true);
		assertNotNull(jobId2);
		JobStatusUtil.checkJobStatus(jobId2, jobServiceProvider);
		final List<UUID> astNodesReRunReComputeAst = getAstNodesForModule(projectId, moduleId);
		
		assertThat(astNodesReRun, CoreMatchers.is(astNodesInitialRun));
		assertThat(astNodesReRunReComputeAst, CoreMatchers.not(astNodesInitialRun));
	}

	@Test
	void testControlFlowEdgeLabels() throws IOException {
		final Result<ControlFlowGraph> resultCFG = controlFlowServiceProvider.getControlFlowGraphForModule().setProjectId(ONE).setModuleId(moduleIdEdgeLabel)
				.execute();
		assertEquals(200, resultCFG.getStatusCode());
		assertTrue("ControlFlowGraph should be present", resultCFG.getValue().isPresent());

		final ControlFlowGraph cfg = resultCFG.getValue().get();
		assertEquals(9, cfg.nodes.size());
		final Map<String, UUID> nodeTypeToNodeIDMap = new HashMap<>();
		final Map<UUID, Set<String>> edgeFromIDToLabelMap = new HashMap<>();
		cfg.edges.stream()
			.filter(edge ->  edge.label != null)
			.forEach(edge -> edgeFromIDToLabelMap.computeIfAbsent(edge.fromId, key -> new HashSet<>()).add(edge.label));
		cfg.nodes.stream().forEach(node -> {
			nodeTypeToNodeIDMap.put(node.type, node.id);
		});
		assertEquals("Expected only two nodes to have edges with labels", 2, edgeFromIDToLabelMap.keySet().size());
		Set<String> performNodeLabels = edgeFromIDToLabelMap.get(nodeTypeToNodeIDMap.get("CobolPerformStmt"));
		assertEquals("Expected Perform Statement to have only two edges with label", 2, performNodeLabels.size());
		assertTrue("Expected the Perform statement to have edge with 'EXECUTE' label", performNodeLabels.contains("EXECUTE"));
		assertTrue("Expected the Perform statement to have edge with 'EXIT LOOP' label", performNodeLabels.contains("EXIT LOOP"));
		Set<String> readNodeLabels = edgeFromIDToLabelMap.get(nodeTypeToNodeIDMap.get("CobolReadStmt"));
		assertEquals("Expected Perform Statement to have only two edges with label", 2, readNodeLabels.size());
		assertTrue("Expected the Perform statement to have edge with 'NOT AT END' label", readNodeLabels.contains("NOT AT END"));
		assertTrue("Expected the Perform statement to have edge with 'AT END' label", readNodeLabels.contains("AT END"));
	}
	
	@Test
	void testGetControlFlowForModuleWithSuperTypes() throws IOException {
		final Result<ControlFlowGraph> resultGetControlFlow = controlFlowServiceProvider.getControlFlowGraphForModule()
				.setProjectId(ONE)
				.setModuleId(moduleId)
				.setCharacterLimit(null)
				.execute();
		assertEquals(200, resultGetControlFlow.getStatusCode());
		final ControlFlowGraph graph = resultGetControlFlow.getValue().get();
		assertEquals(6, graph.nodes.size());
		for (int i = 0; i < graph.nodes.size(); i++) {
			final var node = graph.nodes.get(i);
			final String nodeType = node.type; 
			if (nodeType != null && nodeType.equalsIgnoreCase("CobolDisplayStmt")) {
				final List<String> superTypes = new ArrayList<>(node.superTypes);
				assertEquals(1, superTypes.size());
				final String superType = superTypes.get(0);
				assertEquals("statement", superType.toLowerCase());
			}
		}
	}
	
	@Test
	void testGetControlFlowForCfgCollapsiblleNode() throws IOException {
		final Result<ControlFlowGraph> resultGetControlFlow = controlFlowServiceProvider.getControlFlowGraphForModule()
				.setProjectId(ONE)
				.setModuleId(moduleId)
				.setCharacterLimit(null)
				.execute();
		assertEquals(200, resultGetControlFlow.getStatusCode());
		final ControlFlowGraph graph = resultGetControlFlow.getValue().get();
		assertEquals(6, graph.nodes.size());
		boolean hasCobolLabelStmt = false; 
		for (int i = 0; i < graph.nodes.size(); i++) {
			final var node = graph.nodes.get(i);
			final String nodeType = node.type;
			if(nodeType != null && nodeType.equalsIgnoreCase("CobolLabelStmt")) {
				hasCobolLabelStmt = true;
				assertEquals(3, node.superTypes.size());
				assertTrue("CobolLabelStmt must have CfgCollapsibleNode as Supertype.", node.superTypes.contains("CfgCollapsibleNode"));
			}
		}
		assertTrue("There should be a CobolLabelStmt type of AstNode.", hasCobolLabelStmt);
	}
	
	@Test
	void testNaturalCallInternalStatement() throws Exception {
		final String path = "/innowake/mining/test/control-flow-graph/source-extraction/MIN1882A.nsp";
		final EntityId module = createModule(path, Technology.NATURAL);
		final String jobId = calculateControlFlowGraph(ONE, module, false);
		assertNotNull(jobId);
		JobStatusUtil.checkJobStatus(jobId, jobServiceProvider);
		
		final Result<ControlFlowGraph> resultGetControlFlow = controlFlowServiceProvider.getControlFlowGraphForModule()
				.setProjectId(ONE)
				.setModuleId(module)
				.setCharacterLimit(null)
				.execute();
		assertEquals(200, resultGetControlFlow.getStatusCode());
		final ControlFlowGraph graph = resultGetControlFlow.getValue().get();
		assertEquals(11, graph.nodes.size());
		final List<ControlFlowNode> performStatement = graph.nodes.stream().filter(node -> "PerformStmt".equals(node.type)).collect(Collectors.toList());
		assertEquals(1, performStatement.size());
		assertTrue("SuperTypes should contain CALL_INTERNAL_STATEMENT",
				performStatement.get(0).superTypes.contains(CallInternalStatement.class.getSimpleName()));
	}

	@Test
	void controlFlowEdgesOnlyContainsFlowsControlEdges() throws Exception {
		final String path = "/innowake/mining/test/control-flow-graph/source-extraction/Loop.nsp";
		final EntityId module = createModule(path, Technology.NATURAL);
		final String jobId = calculateControlFlowGraph(ONE, module, false);
		assertNotNull(jobId);
		JobStatusUtil.checkJobStatus(jobId, jobServiceProvider);

		final Result<ControlFlowGraph> resultGetControlFlow = controlFlowServiceProvider.getControlFlowGraphForModule()
				.setProjectId(ONE)
				.setModuleId(module)
				.setCharacterLimit(null)
				.execute();
		assertEquals(200, resultGetControlFlow.getStatusCode());
		final ControlFlowGraph graph = resultGetControlFlow.getValue().orElseThrow(() -> new IllegalStateException("Control flow graph must be present."));

		final var astNodes = graph.nodes.stream().filter(node -> ControlFlowEntity.AST_NODE.equals(node.entity)).map(n -> n.id).collect(Collectors.toSet());
		for (final var edge : graph.edges) {
			if (astNodes.contains(edge.fromId) && astNodes.contains(edge.toId)) {
				final UUID from = edge.fromId;
				final UUID to = edge.toId;
				assertTrue("FlowsControl edge between " + from + " and " + to + " must exist. " +
						"Inspect the edges with the following query: 'SELECT FROM E WHERE out=" + from + " AND in=" + to + "'",
						flowsControlEdgeExistsBetween(from, to));
			}
		}
	}

	private boolean flowsControlEdgeExistsBetween(final UUID from, final UUID to) {
		final var astDao = new AstPgDao(getDataSource());
		final var flow = astDao.findRelationships(q -> q.ofSource(from).ofDestination(to).withType(AstRelationshipType.FLOW));
		return ! flow.isEmpty();
	}
	
	@Test
	void testFailingPl1ControlFlowGraph() throws Exception {
		final String path = "/innowake/mining/test/control-flow-graph/source-extraction/P4030H.pl1";
		final EntityId module = createModule(path, Technology.PL1);
		final String jobId = calculateControlFlowGraph(ONE, module, false);
		assertNotNull(jobId);

		final JobInformation jobInformation = JobStatusUtil.checkFailingJobStatus(jobId, jobServiceProvider);
		assertEquals(JobStatus.FAILURE, jobInformation.getStatus());

		final ResultStatus result = jobInformation.getResultStatus();
		assertNotNull(result);
		assertEquals(ResultStatus.Severity.ERROR, result.getSeverity());
	}
	
	/**
	 * Tests whether metadata of JCL control-flow-steps in Module MMRS710J is set correctly.
	 *
	 * @throws IOException on control-flow-job execution
	 */
	@Test
	void testGetControlFlowWithMetadata() throws IOException {
		final Result<ControlFlowGraph> resultGetControlFlow = controlFlowServiceProvider.getControlFlowGraphForModule()
				.setProjectId(ONE)
				.setModuleId(moduleIdJclJob)
				.setCharacterLimit(null)
				.execute();
		
		assertEquals(200, resultGetControlFlow.getStatusCode());
		final ControlFlowGraph graph = resultGetControlFlow.getValue().get();
		final var step1 = obtainStep(graph.nodes, "STEP01 EXEC PGM=MMRS7101");
		final var step2 = obtainStep(graph.nodes, "STEP02 EXEC PGM=MMRS7101");
		final var step3 = obtainStep(graph.nodes, "STEP03 EXEC PGM=MMRS7101");
		final var step4 = obtainStep(graph.nodes, "STEP04 EXEC PGM=MMRS7101");
		final var step5 = obtainStep(graph.nodes, "STEP05 EXEC PGM=MMRS7101");

		final Map<UUID, List<ModuleLightweightPojo>> datasetsIn = new HashMap<>() ;
		final Map<UUID, List<ModuleLightweightPojo>> datasetsOut = new HashMap<>() ;
		for (final var node : graph.nodes) {
			AstService.Properties.FILE_IDS_IN.optionalFrom(node.properties).map(ids -> ids.stream().map(Number::longValue).collect(Collectors.toSet()))
				.ifPresent(inIDs -> datasetsIn.put(node.id, graph.relatedModules.stream().filter(m -> inIDs.contains(m.getId())).collect(Collectors.toList())));
			AstService.Properties.FILE_IDS_OUT.optionalFrom(node.properties).map(ids -> ids.stream().map(Number::longValue).collect(Collectors.toSet()))
				.ifPresent(outIDs -> datasetsOut.put(node.id, graph.relatedModules.stream().filter(m -> outIDs.contains(m.getId())).collect(Collectors.toList())));
		}
		
		checkMetadata(datasetsIn.get(step1.id), Arrays.asList("MMRS00C.A.LOADLIB", "MMRS00C.C.LOADLIB"));
		assertTrue(datasetsOut.get(step1.id).isEmpty());
		
		assertTrue(datasetsIn.get(step2.id).isEmpty());
		checkMetadata(datasetsOut.get(step2.id), Arrays.asList("MMRS00C.A.LOADLIB", "MMRS00C.C.LOADLIB"));
		
		checkMetadata(datasetsIn.get(step3.id), Arrays.asList("MMRS00C.A.LOADLIB", "MMRS00C.C.LOADLIB"));
		checkMetadata(datasetsOut.get(step3.id), Arrays.asList("MMRS00C.A.LOADLIB", "MMRS00C.C.LOADLIB"));
		
		assertNull(datasetsIn.get(step4.id));
		assertNull(datasetsOut.get(step4.id));
		
		checkMetadata(datasetsIn.get(step5.id), Arrays.asList("MMRS00C.A.LOADLIB", "MMRS00C.C.LOADLIB"));
		checkMetadata(datasetsOut.get(step5.id), Arrays.asList("MMRS00C.A.LOADLIB", "MMRS00C.C.LOADLIB"));
	}

	/**
	 * Obtains step with specified stepName from a list of AstNodes. Asserts that there is only one step with the specified name.
	 *
	 * @param astNodes list of AstNodes containing the step
	 * @param stepName name of the step
	 * @return the found step
	 */
	private ControlFlowNode obtainStep(final List<ControlFlowNode> astNodes, final String stepName) {
		final List<ControlFlowNode> potentialSteps = astNodes.stream().filter(n -> stepName.equals(n.label)).collect(Collectors.toList());
		assertEquals(1, potentialSteps.size());
		return potentialSteps.get(0);
	}

	/**
	 * Checks whether metadata is as expected.
	 *
	 * @param metadata metadata of an AstNode
	 * @param expectedInputFiles whether it is expected that the metadata contains one input file
	 * @param expectedOutputFiles whether it is expected that the metadata contains one output file
	 */
	private void checkMetadata(final List<ModuleLightweightPojo> fileModules, final List<String> expectedFiles) {
		final List<String> inputFiles = fileModules.stream().map(ModuleLightweightPojo::getName).collect(Collectors.toList());
		assertThat(inputFiles, Matchers.containsInAnyOrder(expectedFiles.toArray()));
		assertEquals(expectedFiles.size(), fileModules.size());
	}

	private String calculateControlFlowGraph(final EntityId projectId, final EntityId moduleId, final boolean isRecompute) throws Exception {
		final Result<String> resultCalculateCFG = controlFlowServiceProvider.calculateControlFlowGraphForModule()
				.setProjectId(projectId)
				.setModuleId(moduleId) 
				.setRecalculateAst(isRecompute)
				.execute();
		assertEquals(202, resultCalculateCFG.getStatusCode());
		final String jobId = resultCalculateCFG.getValue().get();
		return jobId;
	}
	
	private List<UUID> getAstNodesForModule(final EntityId projectId, final EntityId moduleId) throws IOException {
		final Result<ControlFlowGraph> resultCFG = controlFlowServiceProvider.getControlFlowGraphForModule()
				.setProjectId(projectId)
				.setModuleId(moduleId)
				.execute();
		assertEquals(200, resultCFG.getStatusCode());
		assertTrue("ControlFlowGraph should be present", resultCFG.getValue().isPresent());

		final ControlFlowGraph cfg = resultCFG.getValue().get();
		return cfg.nodes.stream()
				.filter(node -> ControlFlowEntity.AST_NODE.equals(node.entity))
				.map(node -> node.id)
				.collect(Collectors.toList());
	}

	/**
	 * Creates some test-data for testing control-flow-generation with metadata.
	 *
	 * @throws IOException on edge creation
	 */
	private void createTestDataWithMetadata() throws IOException {
		final String jobPath = "/innowake/mining/test/control-flow-graph/source-extraction/MMRS710J.job";
		final EntityId jobId = createModule(Optional.of(jobPath), Technology.JCL, Type.JOB, FilenameUtils.getBaseName(jobPath), null, Storage.FILE);

		final EntityId step1Id = createModule(Optional.empty(), Technology.JCL, Type.EXEC_PGM, "MMRS710J.STEP01.EXEC_PGM", jobId, Storage.FILE_SECTION);
		final EntityId step2Id = createModule(Optional.empty(), Technology.JCL, Type.EXEC_PGM, "MMRS710J.STEP02.EXEC_PGM", jobId, Storage.FILE_SECTION);
		final EntityId step3Id = createModule(Optional.empty(), Technology.JCL, Type.EXEC_PGM, "MMRS710J.STEP03.EXEC_PGM", jobId, Storage.FILE_SECTION);
		final EntityId step4Id = createModule(Optional.empty(), Technology.JCL, Type.EXEC_PGM, "MMRS710J.STEP04.EXEC_PGM", jobId, Storage.FILE_SECTION);
		final EntityId step5Id = createModule(Optional.empty(), Technology.JCL, Type.EXEC_PGM, "MMRS710J.STEP05.EXEC_PGM", jobId, Storage.FILE_SECTION);

		createEdge(jobId, step1Id, RelationshipType.CONTAINS, Collections.emptyMap());
		createEdge(jobId, step2Id, RelationshipType.CONTAINS, Collections.emptyMap());
		createEdge(jobId, step3Id, RelationshipType.CONTAINS, Collections.emptyMap());
		createEdge(jobId, step4Id, RelationshipType.CONTAINS, Collections.emptyMap());
		createEdge(jobId, step5Id, RelationshipType.CONTAINS, Collections.emptyMap());

		/* While fetching the ControlFlowGraph object, the RESOURCE/LIB file will be filtered out and only RESOURCE/FILE, RESOURCE/GDG_FILE file will
		 * be returned */
		final EntityId fileId = createModule(Optional.empty(), Technology.RESOURCE, Type.FILE, "MMRS00C.A.LOADLIB", null, Storage.FILE);
		final EntityId fileId1 = createModule(Optional.empty(), Technology.RESOURCE, Type.LIB, "MMRS00C.B.LOADLIB", null, Storage.FILE);
		final EntityId fileId2 = createModule(Optional.empty(), Technology.RESOURCE, Type.GDG_FILE, "MMRS00C.C.LOADLIB", null, Storage.FILE);
		
		createEdge(step1Id, fileId, RelationshipType.ACCESSES, Collections.singletonMap("FILE_ACCESS_TYPE", "READ"));
		createEdge(step2Id, fileId, RelationshipType.ACCESSES, Collections.singletonMap("FILE_ACCESS_TYPE", "WRITE"));
		createEdge(step3Id, fileId, RelationshipType.ACCESSES, Collections.singletonMap("FILE_ACCESS_TYPE", "READ"));
		createEdge(step3Id, fileId, RelationshipType.ACCESSES, Collections.singletonMap("FILE_ACCESS_TYPE", "WRITE"));
		createEdge(step5Id, fileId, RelationshipType.ACCESSES, Collections.singletonMap("FILE_ACCESS_TYPE", "WRITE,READ"));
		createEdge(step1Id, fileId1, RelationshipType.ACCESSES, Collections.singletonMap("FILE_ACCESS_TYPE", "WRITE"));
		createEdge(step2Id, fileId1, RelationshipType.ACCESSES, Collections.singletonMap("FILE_ACCESS_TYPE", "WRITE,READ"));
		createEdge(step1Id, fileId2, RelationshipType.ACCESSES, Collections.singletonMap("FILE_ACCESS_TYPE", "READ"));
		createEdge(step2Id, fileId2, RelationshipType.ACCESSES, Collections.singletonMap("FILE_ACCESS_TYPE", "WRITE"));
		createEdge(step3Id, fileId2, RelationshipType.ACCESSES, Collections.singletonMap("FILE_ACCESS_TYPE", "WRITE,READ"));
		createEdge(step5Id, fileId2, RelationshipType.ACCESSES, Collections.singletonMap("FILE_ACCESS_TYPE", "WRITE,READ"));
	}

	/**
	 * Uses {@link ReferenceServiceProvider#createReference()} to create a reference between two vertices.
	 *
	 * @param fromId id of the from-vertex
	 * @param toId id of the to-vertex
	 * @param relationship type of relationship
	 * @param properties relationship properties
	 * @throws IOException on reference creation
	 */
	private void createEdge(final EntityId fromId, final EntityId toId, final RelationshipType relationship, final Map<String, Object> properties) throws IOException {
		final ModuleRelationshipPojoPrototype reference = new ModuleRelationshipPojoPrototype();
		reference.setRelationship(relationship);
		reference.setSrcModule(fromId);
		reference.setDstModule(toId);
		reference.setProperties(properties);
		referenceServiceProvider.createReference().setReference(reference).setModuleId(fromId).setProjectId(ONE).execute();
	}

	private EntityId createModule(final String path, final Technology technology) {
		return createModule(Optional.of(path), technology, Type.PROGRAM, FilenameUtils.getBaseName(path), null, Storage.FILE);
	}
	
	private EntityId createModule(final Optional<String> path, final Technology technology, final Type type, final String name, @Nullable EntityId containingModule,
			final Storage storage) {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		if (path.isPresent()) {
			final String actualPath = path.get();
			try (final InputStream inputStream = Files.newInputStream(Paths.get("./src/test/resources/" + actualPath))) {
				final String content = IOUtils.toString(inputStream, StandardCharsets.UTF_8);
				module.setContent(content);
				module.setPath(actualPath);
			} catch (final IOException e) {
				fail(e.getLocalizedMessage());
				return EntityId.of(-1l);
			}
		}
		module.setName(name);
		module.setProject(ONE);
		module.setTechnology(technology);
		module.setType(type);
		module.setStorage(storage);
		module.setIdentification(Identification.IDENTIFIED);
		module.setOrigin(Origin.CUSTOM);
		if (containingModule != null) {
			module.setParent(containingModule);
		}
		module.setCreator(Creator.DISCOVERY);

		final Result<ModulePojo> resultCreate;
		try {
			resultCreate = moduleServiceProvider.createModule().setModule(module).setProjectId(ONE).execute();
		} catch (final IOException e) {
			throw new AssertionError(e);
		}
		assertEquals(201, resultCreate.getStatusCode());
		assertTrue("Created Module should be present", resultCreate.getValue().isPresent());
		return resultCreate.getValue().get().identity();
	}

	private boolean isControlFlowCalculated(final ModulePojo module) {
		final var astDao = new AstPgDao(getDataSource());
		final var entryPoints = astDao.findModuleRelationships(q -> q.ofModule(module.identity()).withType(AstModuleRelationshipType.ENTRY));
		return ! entryPoints.isEmpty();
	}
}

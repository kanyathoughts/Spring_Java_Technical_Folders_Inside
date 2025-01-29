/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */

package innowake.mining.server.datalineage;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.collection.IsEmptyCollection.empty;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import org.apache.commons.collections4.set.ListOrderedSet;
import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Iterables;

import brave.Span;
import brave.Tracer;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.JobExecutionCallback;
import innowake.lib.job.api.NullProgressMonitor;
import innowake.lib.job.api.management.JobManager;
import innowake.lib.job.api.management.JobMonitor;
import innowake.mining.server.datalineage.query.DetailLevel;
import innowake.mining.server.datalineage.query.Parameters;
import innowake.mining.server.service.MiningJobService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.dataflow.DataFlowNodePojo;
import innowake.mining.shared.entities.dataflow.DataFlowNodeRelationshipType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraph;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode;
import innowake.mining.shared.model.datalineage.graph.ModuleNode;
import innowake.mining.shared.model.job.ResultContainer;

@WithMockUser
class DataFlowGraphQueryTest extends BaseDataLineageTest {

	@Autowired
	private JobManager jobManager;
	
	@Autowired
	private Tracer tracer;

	@Autowired
	protected MiningJobService miningJobService;

	@Autowired
	private ObjectMapper objectMapper;
	
	protected EntityId MoveAndComputeStatementModule;
	protected EntityId test7aModule;
	protected EntityId test7bModule;
	protected EntityId test8aModule;
	protected EntityId test8bModule;
	protected EntityId test9aModule;
	protected EntityId test9bModule;
	protected EntityId test10aModule;
	protected EntityId test10bModule;
	protected EntityId test11Module;
	protected EntityId test12aModule;
	protected EntityId test12bModule;
	protected EntityId test13aModule;
	protected EntityId test13bModule;
	protected EntityId test16aModule;
	protected EntityId test16bModule;
	
	/* EXECSQLTESTS */
	protected EntityId test25aModule;
	protected EntityId test25bModule;
	
	protected EntityId deleteAllProjectId;
	protected EntityId test25aInOtherProject;
	protected EntityId test25bInOtherProject;
	
	protected EntityId test33Module;
	
	protected EntityId testMMRS71AModule;
	protected EntityId testMMRS71BModule;
	protected EntityId testMMRS71CModule;
	protected EntityId testMMRS710AModule;
	protected EntityId testMMRS710DModule;
	protected EntityId testMMRS710EModule;
	protected EntityId testDFHAIDModule;
	protected EntityId testMMRS7101Module;
	protected EntityId testMMRS7102Module;
	protected EntityId testMMRS7111Module;
	protected EntityId testMMRS7112Module;
	protected EntityId testMMRS71B1Module;
	protected EntityId testMMRS71C1Module;
	protected EntityId testMMRS71D1Module;
	protected EntityId testMMRS71Z1Module;
	protected EntityId testMMRS71Z2Module;
	protected EntityId testMMRS71Z3Module;
	protected EntityId test39Module;
	protected EntityId test42Module;

	protected String runDataFlowGraphQuery(final EntityId startModule, final Optional<ModuleLocation> location, final DetailLevel dLevel)
			throws IllegalArgumentException, IOException {
				
		final Map<String, List<String>> parameterMap = new HashMap<>();

		final ArrayList<String> detailLevel = new ArrayList<>();
		detailLevel.add(dLevel.toString());
		parameterMap.put("detailLevel", detailLevel);
		
		final ArrayList<String> moduleIds = new ArrayList<>();
		moduleIds.add(startModule.getNid().toString());
		parameterMap.put("moduleId", moduleIds);
		parameterMap.put("strictTracing", Collections.singletonList("true"));
		
		if (location.isPresent()) {			
			final ArrayList<String> offset = new ArrayList<>();
			offset.add(location.get().getOffset().toString());
			parameterMap.put("fieldOffset", offset);
		}

		final EntityId projectId = moduleService.getModule(startModule).getProject();
		final Parameters parameters = new Parameters.Builder().fromMap(parameterMap).setProjectId(projectId).build();
		final DataFlowGraph resultGraph =  dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(), parameters);
		return sortAndSerialize(resultGraph);
	}

	protected void compareJsonResults(final String value, final String expectedPath) throws IOException {
		if(writeExpected) {
			final String prettyString = objectMapper.writerWithDefaultPrettyPrinter().writeValueAsString(objectMapper.readValue(value, JsonNode.class));
			FileUtils.writeStringToFile(BASE_EXPECTED_FOLDER.resolve(expectedPath).toFile(), prettyString, StandardCharsets.UTF_8);
		}

		final String expectedNodes = new String(Files.readAllBytes(BASE_EXPECTED_FOLDER.resolve(expectedPath)), StandardCharsets.UTF_8);

		/* pretty-printing the JSON to make comparing differences much easier - by default everything is on one line */
		final String valuePrettified = objectMapper.writerWithDefaultPrettyPrinter().writeValueAsString(objectMapper.readValue(value, JsonNode.class));
		final String expectedPrettified = objectMapper.writerWithDefaultPrettyPrinter().writeValueAsString(objectMapper.readValue(expectedNodes, JsonNode.class));

		assertEquals(expectedPrettified, valuePrettified);
	}

	/**
	 * Submits the job, waits until the job is finished and returns the job ID.
	 * <p>
	 * There is a timeout of 10 minutes for the job wait.
	 * 
	 * @param job the job to run
	 * @return the ID of the job
	 */
	protected String submitJob(final Job<?> job) {
		final Span rootSpan = tracer.newTrace();
		try (final Tracer.SpanInScope scope = tracer.withSpanInScope(rootSpan)) {
			final CountDownLatch latch = new CountDownLatch(1);
			final Throwable[] error = new Throwable[1];
			final JobMonitor monitor = jobManager.submit(job, new JobExecutionCallback() {
	
				@Override
				public void onCompletion() {
					latch.countDown();
				}
	
				@Override
				public void onFailure(@Nullable final Throwable throwable) {
					error[0] = throwable;
					latch.countDown();
				}
			});
	
			try {
				final boolean countReachedZero = latch.await(10, TimeUnit.MINUTES);
				if ( ! countReachedZero) {
					throw new IllegalStateException(
							"CountDownLatch timed out in DataFlowGraphTest.submitJob(), possible deadlock! (" + latch.toString() + ")");
				}
			} catch (final InterruptedException e) {
				throw new IllegalStateException(e);
			}
	
			if (error[0] != null) {
				throw new IllegalStateException(error[0]);
			}
			return monitor.getJobId();
		} finally {
			rootSpan.finish();
		}
	}

	private void testDataFlowGraphQuery(final EntityId startModule, final ModuleLocation moduleLocation, final String expectedPath, final DetailLevel dLevel)
			throws IllegalArgumentException, IOException {
		compareJsonResults(runDataFlowGraphQuery(startModule, Optional.of(moduleLocation), dLevel), expectedPath);
	}

	private void testDataFlowGraphQuery(final EntityId startModule, final ModuleLocation moduleLocation, final String expectedPath)
			throws IllegalArgumentException, IOException {
		compareJsonResults(runDataFlowGraphQuery(startModule, Optional.of(moduleLocation), DetailLevel.STATEMENT), expectedPath);
	}

	private void testDataFlowGraphQuery(final EntityId startModule, final String expectedPath) throws IllegalArgumentException, IOException {
		compareJsonResults(runDataFlowGraphQuery(startModule, Optional.empty(), DetailLevel.STATEMENT), expectedPath);
	}

	private String sortAndSerialize(final DataFlowGraph resultGraph) throws IOException {
		final List<DataFlowGraphNode> sortedGraph = new ArrayList<>(resultGraph.getNodes());
		sortedGraph.sort(Comparator.comparing(DataFlowGraphNode::getId));
		sortedGraph.forEach(node -> {
			final ArrayList<String> childrenSorted = new ArrayList<>(node.getChildren());
			childrenSorted.sort(Comparator.naturalOrder());
			node.setChildren(ListOrderedSet.listOrderedSet(childrenSorted));

			final ArrayList<String> incomingSorted = new ArrayList<>(node.getIncomings());
			incomingSorted.sort(Comparator.naturalOrder());
			node.setIncomings(ListOrderedSet.listOrderedSet(incomingSorted));

			final ArrayList<String> outgoingSorted = new ArrayList<>(node.getOutgoings());
			outgoingSorted.sort(Comparator.naturalOrder());
			node.setOutgoings(ListOrderedSet.listOrderedSet(outgoingSorted));

			if (node instanceof ModuleNode) {
				final ModuleNode moduleNode = (ModuleNode) node;
				final ArrayList<String> dataInterfacesSorted = new ArrayList<>(moduleNode.getDataInterfaces());
				dataInterfacesSorted.sort(Comparator.naturalOrder());
				moduleNode.setDataInterfaces(ListOrderedSet.listOrderedSet(dataInterfacesSorted));
			}
		});

		return objectMapper.writeValueAsString(new ResultContainer(new DataFlowGraph(sortedGraph)));
	}

	@BeforeAll
	void createTestModules() throws IOException {
		resetTestData();
		sourceService.resetCaches();
		/* create all test modules in fixed order so they have fixed module ids */
		MoveAndComputeStatementModule = createModule("intramodule/MoveAndComputeStatementModule.cbl", "MoveAndComputeStatementModule", Technology.COBOL, Type.PROGRAM);
		test7aModule = createModule("dataflowgraphquerytest/TEST7a.cbl", "TEST7a", Technology.COBOL, Type.PROGRAM);
		test7bModule = createModule("dataflowgraphquerytest/TEST7b.cbl", "TEST7b", Technology.COBOL, Type.PROGRAM);
		makeCallReference(test7aModule, test7bModule, new ModuleLocation(777, 55));
		test8aModule = createModule("dataflowgraphquerytest/TEST8a.cbl", "TEST8a", Technology.COBOL, Type.PROGRAM);
		test8bModule = createModule("dataflowgraphquerytest/TEST8b.cbl", "TEST8b", Technology.COBOL, Type.PROGRAM);
		makeCallReference(test8bModule, test8aModule, new ModuleLocation(455, 33));
		makeCallReference(test8bModule, test8aModule, new ModuleLocation(498, 33));
		makeCallReference(test8bModule, test8aModule, new ModuleLocation(541, 33));
		makeCallReference(test8bModule, test8aModule, new ModuleLocation(584, 33));
		makeCallReference(test8bModule, test8aModule, new ModuleLocation(627, 40));
		makeCallReference(test8bModule, test8aModule, new ModuleLocation(677, 40));
		makeCallReference(test8bModule, test8aModule, new ModuleLocation(727, 40));
		makeCallReference(test8bModule, test8aModule, new ModuleLocation(777, 40));
		test9aModule = createModule("dataflowgraphquerytest/TEST9a.cbl", "TEST9a", Technology.COBOL, Type.PROGRAM);
		test9bModule = createModule("dataflowgraphquerytest/TEST9b.cbl", "TEST9b", Technology.COBOL, Type.PROGRAM);
		makeCallReference(test9bModule, test9aModule, new ModuleLocation(970, 39));
		makeCallReference(test9bModule, test9aModule, new ModuleLocation(1113, 39));
		makeCallReference(test9bModule, test9aModule, new ModuleLocation(1194, 39));
		makeCallReference(test9bModule, test9aModule, new ModuleLocation(1337, 39));
		makeCallReference(test9bModule, test9aModule, new ModuleLocation(1395, 39));
		makeCallReference(test9bModule, test9aModule, new ModuleLocation(1051, 39));
		makeCallReference(test9bModule, test9aModule, new ModuleLocation(1256, 39));
		makeCallReference(test9bModule, test9aModule, new ModuleLocation(1476, 39));
		test10aModule = createModule("dataflowgraphquerytest/TEST10a.cbl", "TEST10a", Technology.COBOL, Type.PROGRAM);
		test10bModule = createModule("dataflowgraphquerytest/TEST10b.cpy", "TEST10b", Technology.COBOL, Type.COPYBOOK);
		makeIncludesReference(test10aModule, test10bModule);
		test11Module = createModule("dataflowgraphquerytest/TEST11.cbl", "TEST11", Technology.COBOL, Type.PROGRAM);
		test12aModule = createModule("dataflowgraphquerytest/TEST12a.cbl", "TEST12a", Technology.COBOL, Type.PROGRAM);
		test12bModule = createModule("dataflowgraphquerytest/TEST12b.cbl", "TEST12b", Technology.COBOL, Type.PROGRAM);
		makeCallReference(test12bModule, test12aModule, new ModuleLocation(275, 40));
		makeCallReference(test12bModule, test12aModule, new ModuleLocation(320, 40));
		test13aModule = createModule("dataflowgraphquerytest/TEST13a.cbl", "TEST13a", Technology.COBOL, Type.PROGRAM);
		test13bModule = createModule("dataflowgraphquerytest/TEST13b.cbl", "TEST13b", Technology.COBOL, Type.PROGRAM);
		makeCallReference(test13bModule, test13aModule, new ModuleLocation(220, 35));
		test16aModule = createModule("dataflowgraphquerytest/TEST16a.cbl", "TEST16a", Technology.COBOL, Type.PROGRAM);
		test16bModule = createModule("dataflowgraphquerytest/TEST16b.cpy", "TEST16b", Technology.COBOL, Type.COPYBOOK);
		makeIncludesReference(test16aModule, test16bModule);
		/* EXECSQLTESTS */
		test25aModule = createModule("dataflowgraphquerytest/TEST25a.cbl", "TEST25a", Technology.COBOL, Type.PROGRAM);
		test25bModule = createModule("dataflowgraphquerytest/TEST25b.cbl", "TEST25b", Technology.COBOL, Type.PROGRAM);
		makeCallReference(test25aModule, test25bModule, new ModuleLocation(478, 44));
		/* We create a new project so we can safely run ModuleDao#deleteAll */
		deleteAllProjectId = projectService.create(new ProjectPojoPrototype()
				.setClient(EntityId.of(1L))
				.setName("Delete-all-modules-data-flow-nodes-test-project")
				.setNatures(Collections.emptySet())
			).identity();
		test25aInOtherProject = createModule("dataflowgraphquerytest/TEST25a.cbl", "TEST25a", Technology.COBOL, Type.PROGRAM, deleteAllProjectId);
		test25bInOtherProject = createModule("dataflowgraphquerytest/TEST25b.cbl", "TEST25b", Technology.COBOL, Type.PROGRAM, deleteAllProjectId);
		test33Module = createModule("dataflowgraphquerytest/TEST33.cbl", "TEST33", Technology.COBOL, Type.PROGRAM);
		testMMRS71AModule = createModule("realmodule/MMRS71A.cpy", "MMRS71A", Technology.COBOL, Type.COPYBOOK);
		testMMRS71BModule = createModule("realmodule/MMRS71B.cpy", "MMRS71B", Technology.COBOL, Type.COPYBOOK);
		testMMRS71CModule = createModule("realmodule/MMRS71C.cpy", "MMRS71C", Technology.COBOL, Type.COPYBOOK);
		testMMRS710AModule = createModule("realmodule/MMRS710A.cpy", "MMRS710A", Technology.COBOL, Type.COPYBOOK);
		testMMRS710DModule = createModule("realmodule/MMRS710D.cpy", "MMRS710D", Technology.COBOL, Type.COPYBOOK);
		testMMRS710EModule = createModule("realmodule/MMRS710E.cpy", "MMRS710E", Technology.COBOL, Type.COPYBOOK);
		testDFHAIDModule = createModule("realmodule/DFHAID.cpy", "DFHAID", Technology.COBOL, Type.COPYBOOK);
		testMMRS7101Module = createModule("realmodule/MMRS7101.cbl", "MMRS7101", Technology.COBOL, Type.PROGRAM);
		testMMRS7102Module = createModule("realmodule/MMRS7102.cbl", "MMRS7102", Technology.COBOL, Type.PROGRAM);
		testMMRS71Z2Module = createModule("realmodule/MMRS71Z2.cbl", "MMRS71Z2", Technology.COBOL, Type.PROGRAM);
		testMMRS71Z3Module = createModule("realmodule/MMRS71Z3.cbl", "MMRS71Z3", Technology.COBOL, Type.PROGRAM);
		testMMRS7111Module = createModule("realmodule/MMRS7111.cbl", "MMRS7111", Technology.COBOL, Type.PROGRAM);
		testMMRS7112Module = createModule("realmodule/MMRS7112.cbl", "MMRS7112", Technology.COBOL, Type.PROGRAM);
		testMMRS71B1Module = createModule("realmodule/MMRS71B1.cbl", "MMRS71B1", Technology.COBOL, Type.PROGRAM);
		testMMRS71C1Module = createModule("realmodule/MMRS71C1.cbl", "MMRS71C1", Technology.COBOL, Type.PROGRAM);
		testMMRS71D1Module = createModule("realmodule/MMRS71D1.cbl", "MMRS71D1", Technology.COBOL, Type.PROGRAM);
		testMMRS71Z1Module = createModule("realmodule/MMRS71Z1.cbl", "MMRS71Z1", Technology.COBOL, Type.PROGRAM);
		test39Module = createModule("dataflowgraphquerytest/TEST39.cbl", "TEST39", Technology.COBOL, Type.PROGRAM);
		makeIncludesReference(testMMRS7101Module, testMMRS710AModule);
		makeCallReference(testMMRS7101Module, testMMRS71Z1Module, new ModuleLocation(3249, 112));
		makeCallReference(testMMRS7101Module, testMMRS71Z1Module, new ModuleLocation(3678, 112));
		makeIncludesReference(testMMRS7102Module, testMMRS710AModule);
		makeCallReference(testMMRS7102Module, testMMRS71Z2Module, new ModuleLocation(4999, 31));
		makeIncludesReference(testMMRS71Z2Module, testMMRS710AModule);
		makeIncludesReference(testMMRS71Z3Module, testMMRS710AModule);
		makeIncludesReference(testMMRS71Z3Module, testMMRS710DModule);
		makeIncludesReference(testMMRS7111Module, testMMRS710AModule);
		makeIncludesReference(testMMRS7112Module, testMMRS710AModule);
		makeIncludesReference(testMMRS71B1Module, testMMRS71AModule);
		makeIncludesReference(testMMRS71B1Module, testMMRS71BModule);
		makeIncludesReference(testMMRS71B1Module, testMMRS710DModule);
		makeIncludesReference(testMMRS71B1Module, testMMRS710EModule);
		makeCallReference(testMMRS71B1Module, testMMRS71D1Module, new ModuleLocation(6347, 66));
		makeCallReference(testMMRS71B1Module, testMMRS71Z1Module, new ModuleLocation(9270, 50));
		makeIncludesReference(testMMRS71C1Module, testMMRS71AModule);
		makeIncludesReference(testMMRS71C1Module, testMMRS71CModule);
		makeIncludesReference(testMMRS71C1Module, testMMRS710DModule);
		makeIncludesReference(testMMRS71C1Module, testMMRS710EModule);
		makeCallReference(testMMRS71C1Module, testMMRS71Z1Module, new ModuleLocation(3358, 80));
		makeIncludesReference(testMMRS71D1Module, testMMRS710AModule);
		makeIncludesReference(testMMRS71D1Module, testMMRS710DModule);
		makeCallReference(testMMRS71D1Module, testMMRS71Z3Module, new ModuleLocation(1864, 33));
		makeCallReference(testMMRS71D1Module, testMMRS71Z3Module, new ModuleLocation(1947, 33));
		makeCallReference(testMMRS71D1Module, testMMRS71Z3Module, new ModuleLocation(2030, 33));
		makeCallReference(testMMRS71D1Module, testMMRS71Z3Module, new ModuleLocation(2113, 33));
		makeCallReference(testMMRS71D1Module, testMMRS71Z3Module, new ModuleLocation(2196, 33));
		test42Module = createModule("performance/TEST42.cbl", "TEST40", Technology.COBOL, Type.PROGRAM);
	}

	@Test
	void test7() throws IOException {
		testDataFlowGraphQuery(test7aModule, new ModuleLocation(253, 13), "dataflowgraphquerytest/TEST7.txt");
		testDataFlowGraphQuery(test7aModule, new ModuleLocation(253, 13), "dataflowgraphquerytest/TEST7_FIELD.txt", DetailLevel.FIELD);
		/* DetailLevel.MODULE seems to have some issues, see WMIN-7545 */
//		testDataFlowGraphQuery(test7aModule, new ModuleLocation(265, 13), "dataflowgraphquerytest/TEST7_MODULE.txt", DetailLevel.MODULE);
		
	}
	
	@Test
	@Disabled("WMIN-9544")
	void test8() throws IOException {
		testDataFlowGraphQuery(test8bModule, new ModuleLocation(132, 5), "dataflowgraphquerytest/TEST8.txt");
	}

	@Test
	void test9() throws IOException {
		testDataFlowGraphQuery(test9bModule, new ModuleLocation(124, 5), "dataflowgraphquerytest/TEST9.txt");
	}

	@Test
	void test10() throws IOException {
		testDataFlowGraphQuery(test10aModule, new ModuleLocation(183, 1), "dataflowgraphquerytest/TEST10.txt");
		testDataFlowGraphQuery(test10aModule, new ModuleLocation(183, 1), "dataflowgraphquerytest/TEST10_FIELD.txt", DetailLevel.FIELD);
	}
	
	/* Seems like the fieldtracer can not actually handle a missing copybook */
	@Test
	void test11() {
		final ModuleLocation location = new ModuleLocation(183, 1);
		assertThrows(Exception.class, () -> testDataFlowGraphQuery(test11Module, location, "dataflowgraphquerytest/TEST11.txt"));
	}
	
	@Test
	void test12() throws IOException {
		/* G2 */
		testDataFlowGraphQuery(test12aModule, new ModuleLocation(198, 2), "dataflowgraphquerytest/TEST12a.txt");
		testDataFlowGraphQuery(test12aModule, new ModuleLocation(198, 2), "dataflowgraphquerytest/TEST12a_FIELD.txt", DetailLevel.FIELD);
		testDataFlowGraphQuery(test12aModule, new ModuleLocation(198, 2), "dataflowgraphquerytest/TEST12a_MODULE.txt", DetailLevel.MODULE);
		
		/* G1 */
		testDataFlowGraphQuery(test12aModule, new ModuleLocation(177, 2), "dataflowgraphquerytest/TEST12b.txt");
		
		/* DUMMY */
		testDataFlowGraphQuery(test12bModule, new ModuleLocation(124, 5), "dataflowgraphquerytest/TEST12c.txt");
		
		/* FIELD1 */
		testDataFlowGraphQuery(test12bModule, new ModuleLocation(184, 6), "dataflowgraphquerytest/TEST12d.txt");

	}
	
	@Test
	void test13() throws IOException {
		/* G2 */
		testDataFlowGraphQuery(test13aModule, new ModuleLocation(198, 2), "dataflowgraphquerytest/TEST13a.txt");
		testDataFlowGraphQuery(test13aModule, new ModuleLocation(198, 2), "dataflowgraphquerytest/TEST13a_FIELD.txt", DetailLevel.FIELD);
		testDataFlowGraphQuery(test13aModule, new ModuleLocation(198, 2), "dataflowgraphquerytest/TEST13a_MODULE.txt", DetailLevel.MODULE);
		
		/* G1 */
		testDataFlowGraphQuery(test13aModule, new ModuleLocation(177, 2), "dataflowgraphquerytest/TEST32b.txt");
		
		/* DUMMY */
		testDataFlowGraphQuery(test13bModule, new ModuleLocation(124, 5), "dataflowgraphquerytest/TEST32c.txt");
		
		/* FIELD1 */
		testDataFlowGraphQuery(test13bModule, new ModuleLocation(160, 6), "dataflowgraphquerytest/TEST13d.txt");
	}
	
	@Test
	void test14() throws IOException {
		testDataFlowGraphQuery(MoveAndComputeStatementModule, new ModuleLocation(251, 8), "dataflowgraphquerytest/TEST14.txt");
		testDataFlowGraphQuery(MoveAndComputeStatementModule, new ModuleLocation(251, 8), "dataflowgraphquerytest/TEST14_FIELD.txt", DetailLevel.FIELD);
	}

	@Test
	void test16() throws IOException {
		testDataFlowGraphQuery(test16aModule, new ModuleLocation(267, 1), "dataflowgraphquerytest/TEST16.txt");
		testDataFlowGraphQuery(test16aModule, new ModuleLocation(267, 1), "dataflowgraphquerytest/TEST16_FIELD.txt", DetailLevel.FIELD);
		testDataFlowGraphQuery(test16aModule, new ModuleLocation(267, 1), "dataflowgraphquerytest/TEST16_MODULE.txt", DetailLevel.MODULE);
	}
	
	/**
	 * Tests whether DataFlowNodes are deleted when module is deleted. 
	 * Also tests whether:
	 * <li>the traced-attribute is set to false on ProxyField-DataFlowNodes related to test17aModule.</li>
	 * <li>the relatedField-edges from ProxyField-DataFlowNodes to nodes of test17aModule are removed.</li>
	 *
	 * @throws IOException on running data-flow-graph-query
	 */
	@Test
	void test25() throws IOException {
		runDataFlowGraphQuery(test25aModule, Optional.of(new ModuleLocation(230, 7)), DetailLevel.STATEMENT);
		final List<DataFlowNodePojo> nodesForAModule = dataFlowService.find(q -> q.ofModule(test25aModule));
		assertThat(nodesForAModule, is(not(empty())));
		final List<DataFlowNodePojo> nodesForBModule = dataFlowService.find(q -> q.ofModule(test25bModule));
		assertThat(nodesForBModule, is(not(empty())));
		assertThat(Iterables.all(nodesForBModule,
				n -> Iterables.all(dataFlowService.find(q -> q.withRelationshipFrom(n.getId(), DataFlowNodeRelationshipType.RELATED_FIELD)), f -> ! f.getModuleId().equals(test25aModule))), is(Boolean.FALSE));

		moduleService.deleteModule(test25aModule, true);
		final List<DataFlowNodePojo> nodesForAModule2 = dataFlowService.find(q -> q.ofModule(test25aModule));
		assertThat(nodesForAModule2, is(empty()));
		final List<DataFlowNodePojo> nodesForBModule2 = dataFlowService.find(q -> q.ofModule(test25bModule));
		assertThat(Iterables.all(nodesForBModule2, n -> ! n.isTraced()), is(true));
		assertThat(Iterables.all(nodesForBModule2,
				n -> Iterables.all(dataFlowService.find(q -> q.withRelationshipFrom(n.getId(), DataFlowNodeRelationshipType.RELATED_FIELD)), f -> ! f.getModuleId().equals(test25aModule))), is(Boolean.TRUE));
	}
	
	@Test
	void test33() throws IOException {
		runDataFlowGraphQuery(test33Module, Optional.of(new ModuleLocation(1896, 20)), DetailLevel.STATEMENT);
		runDataFlowGraphQuery(test33Module, Optional.of(new ModuleLocation(1957, 20)), DetailLevel.STATEMENT);
	}

	@Disabled("WMIN-9348: produces an incorrect result")
	@Test
	void testMMRSSeparately() throws IOException {
		testDataFlowGraphQuery(testMMRS7101Module, "dataflowgraphquerytest/TEST38a.txt");
		testDataFlowGraphQuery(testMMRS7102Module, "dataflowgraphquerytest/TEST38b.txt");
		testDataFlowGraphQuery(testMMRS71Z2Module, "dataflowgraphquerytest/TEST38c.txt");
	}

	@Disabled("WMIN-10550: DataLineage still unstable for these Modules. Will be investigated further.")
	@Test
	void testOtherMMRS() throws IOException {
		testDataFlowGraphQuery(testMMRS71Z3Module, "dataflowgraphquerytest/TEST38d.txt"); // Inconsistent: in ~1 in 4 executions the order of two nodes changes. This has no impact on the correctness of the result. Will be fixed in WMIN-9261.
		testDataFlowGraphQuery(testMMRS7111Module, "dataflowgraphquerytest/TEST38e.txt"); // NOT WORKING due to FileDescriptors producing incorrect results.. Should be fixed by WNDT-3362
		testDataFlowGraphQuery(testMMRS7112Module, "dataflowgraphquerytest/TEST38f.txt"); // NOT WORKING due to execsql -> no RW found -> modelImport crashes -> node not set to traced.. Should be fixed by proper error handling
		testDataFlowGraphQuery(testMMRS71B1Module, "dataflowgraphquerytest/TEST38g.txt");
		testDataFlowGraphQuery(testMMRS71C1Module, "dataflowgraphquerytest/TEST38h.txt");
		testDataFlowGraphQuery(testMMRS71D1Module, "dataflowgraphquerytest/TEST38i.txt");
		testDataFlowGraphQuery(testMMRS71Z1Module, "dataflowgraphquerytest/TEST38j.txt");
	}
	
	@Disabled("WMIN-10550: fails randomly")
	@Test
	void testCOMP() throws IOException {
		testDataFlowGraphQuery(test39Module, "dataflowgraphquerytest/TEST39.txt");
	}
	
	@Disabled("This test is purely for performance testing and should not be used when running all tests")
	@Test
	void testPerformance() throws IOException {
		testDataFlowGraphQuery(test42Module, "dataflowgraphquerytest/TEST42.txt");
	}
}


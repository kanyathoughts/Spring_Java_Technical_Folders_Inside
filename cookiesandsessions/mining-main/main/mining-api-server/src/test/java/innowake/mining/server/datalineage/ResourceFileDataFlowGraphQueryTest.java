/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.datalineage;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.NullProgressMonitor;
import innowake.mining.server.datalineage.operations.file.ResourceFileResolver;
import innowake.mining.server.datalineage.operations.file.ResourceFileTracer;
import innowake.mining.server.datalineage.query.Parameters;
import innowake.mining.server.datalineage.query.QueryDirection;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraph;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode;
import innowake.mining.shared.model.datalineage.graph.ModuleNode;

/**
 * Tests for {@link ResourceFileTracer} and {@link ResourceFileResolver}
 */
class ResourceFileDataFlowGraphQueryTest extends BaseDataLineageTest {

	@Test
	void testReadFileDataFlowGraph() throws IOException {
		final var fileAccessModule = createModule("resourceFile/FileReader.cbl", "FILE_READER", Technology.COBOL, Type.PROGRAM);
		final var readFileModule = createModule("MY.INPUT.READ.DATASET", Technology.RESOURCE, Type.FILE);
		makeReadsWritesFileReference(fileAccessModule, readFileModule, "[\"READ\"]", "\"INPUTFILE\"", "\"INFILE\"");
		
		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder()
						.setProjectId(PROJECT_ID)
						.setStartModuleIds(Collections.singletonList(fileAccessModule))
						.setQueryDirection(QueryDirection.BOTH).setStrictTracing(true).build());
		
		final Map<String, String> expectedIncomingOutgoingMap = Map.of("(file content)", "Read");
		final Map<String, String> expectedIncomingOutgoingMapFileAccess = Map.of("", "(file content)");
		
		assertFileReadWrite(dataFlowGraph, expectedIncomingOutgoingMap, fileAccessModule, readFileModule, expectedIncomingOutgoingMapFileAccess);
	}

	@Test
	void testWriteFileDataFlowGraph() throws IOException {
		final var fileAccessModule = createModule("resourceFile/FileWriter.cbl", "FILE_WRITER", Technology.COBOL, Type.PROGRAM);
		final var writeFileModule = createModule("MY.WRITE.DATASET", Technology.RESOURCE, Type.FILE);
		makeReadsWritesFileReference(fileAccessModule, writeFileModule, "[\"WRITE\"]", "\"OUTPUTFILE\"", "\"OUTFILE\"");
		
		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder()
						.setProjectId(PROJECT_ID)
						.setStartModuleIds(Collections.singletonList(fileAccessModule))
						.setQueryDirection(QueryDirection.BOTH).setStrictTracing(true).build());
		
		final Map<String, String> expectedIncomingOutgoingMap = Map.of("Write", "(file content)");
		final Map<String, String> expectedIncomingOutgoingMapFileAccess = Map.of("(file content)", "");
		
		assertFileReadWrite(dataFlowGraph, expectedIncomingOutgoingMap, fileAccessModule, writeFileModule, expectedIncomingOutgoingMapFileAccess);
	}

	@Test
	void testReadWriteFileDataFlowGraph() throws IOException {
		final var cobolWithReadWrite = createModule("resourceFile/FileReadWrite.cbl", "FILE_READWRITE", Technology.COBOL, Type.PROGRAM);
		final var readFileModule = createModule("MY.INPUT.DATASET", Technology.RESOURCE, Type.FILE);
		final var writeFileModule = createModule("MY.OUTPUT.DATASET", Technology.RESOURCE, Type.FILE);
		makeReadsWritesFileReference(cobolWithReadWrite, readFileModule, "[\"READ\"]", "\"INPUT-FILE\"", "\"INFILE\"");
		makeReadsWritesFileReference(cobolWithReadWrite, writeFileModule, "[\"WRITE\"]", "\"OUTPUT-FILE\"", "\"OUTFILE\"");

		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder()
						.setProjectId(PROJECT_ID)
						.setStartModuleIds(Collections.singletonList(cobolWithReadWrite))
						.setQueryDirection(QueryDirection.BOTH).setStrictTracing(true).build());

		final Map<String, String> expectedIncomingOutgoingMap = Map.of("(file content)", "Read", "Write", "(file content)");
		final Map<String, String> expectedIncomingOutgoingMapFileAccessWrite = Map.of("(file content)", "");
		final Map<String, String> expectedIncomingOutgoingMapFileAccessRead = Map.of("", "(file content)");

		assertFileReadWrite(dataFlowGraph, expectedIncomingOutgoingMap, cobolWithReadWrite, readFileModule, expectedIncomingOutgoingMapFileAccessRead);
		assertFileReadWrite(dataFlowGraph, expectedIncomingOutgoingMap, cobolWithReadWrite, writeFileModule, expectedIncomingOutgoingMapFileAccessWrite);
	}

	@Test
	void testReadWriteFileDataFlowGraphWithMissingFile() throws IOException {
		final var cobolWithReadWrite = createModule("resourceFile/FileReadWrite_NoRef.cbl", "FILE_READWRITE_NoRef", Technology.COBOL, Type.PROGRAM);
		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(), new Parameters.Builder()
				.setProjectId(PROJECT_ID)
				.setStartModuleIds(Collections.singletonList(cobolWithReadWrite))
				.setQueryDirection(QueryDirection.BOTH)
				.setStrictTracing(true)
				.build());
		
		final Map<String, String> expectedIncomingOutgoingMap = Map.of("", "Read", "Write", "");
		assertFileReadWrite(dataFlowGraph, expectedIncomingOutgoingMap, cobolWithReadWrite, null, Collections.emptyMap());
	}

	private void assertFileReadWrite(final DataFlowGraph dataFlowGraph, final Map<String, String> expectedIncomingOutgoingMapFileAccess, 
			final EntityId fileAccessModuleId, @Nullable final EntityId resourceFileModuleId, 
			final Map<String, String> expectedIncomingOutgoingMapResourceFile) {
		final List<ModuleNode> moduleNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.MODULE))
				.map(ModuleNode.class::cast).collect(Collectors.toList());

		final Map<String, DataFlowGraphNode> nodeMap = dataFlowGraph.getNodes().stream()
				.collect(Collectors.toMap(DataFlowGraphNode::getId, Function.identity()));

		Map<String, String> actualIncomingOutgoingMap = getIncomingOutgoingMapForDataInterfaces(fileAccessModuleId, moduleNodes, nodeMap);
		
		assertEquals(expectedIncomingOutgoingMapFileAccess, actualIncomingOutgoingMap);
		
		if (resourceFileModuleId != null) {
			actualIncomingOutgoingMap = getIncomingOutgoingMapForDataInterfaces(resourceFileModuleId, moduleNodes, nodeMap);
			assertEquals(expectedIncomingOutgoingMapResourceFile, actualIncomingOutgoingMap);
		}
	}

	private Map<String, String> getIncomingOutgoingMapForDataInterfaces(final EntityId mdouleId, final List<ModuleNode> moduleNodes,
			final Map<String, DataFlowGraphNode> nodeMap) {
		final ModuleNode module = moduleNodes.stream().filter(n -> n.getModuleId().equals(mdouleId.getNid())).findAny().get();
		final List<DataFlowGraphNode> moduleDataInterfaces = module.getDataInterfaces().stream().map(nodeMap::get)
				.collect(Collectors.toList());

		final Map<String, String> actualIncomingOutgoingMap = new HashMap<>();
		for (DataFlowGraphNode dataFlowGraphNode : moduleDataInterfaces) {
			final Optional<DataFlowGraphNode> incomings = dataFlowGraphNode.getIncomings().stream().map(nodeMap::get).findAny();
			final Optional<DataFlowGraphNode> outgoings = dataFlowGraphNode.getOutgoings().stream().map(nodeMap::get).findAny();
			if (incomings.isPresent() && outgoings.isPresent()) {
				actualIncomingOutgoingMap.put(incomings.get().getName(), outgoings.get().getName());
			} else if (incomings.isPresent()) {
				actualIncomingOutgoingMap.put(incomings.get().getName(), "");
			} else if (outgoings.isPresent()) {
				actualIncomingOutgoingMap.put("", outgoings.get().getName());
			}
		}
		return actualIncomingOutgoingMap;
	}
}


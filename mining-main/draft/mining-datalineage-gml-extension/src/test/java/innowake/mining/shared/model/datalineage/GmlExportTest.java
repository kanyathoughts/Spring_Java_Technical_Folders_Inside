/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.datalineage;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.fasterxml.jackson.databind.ObjectMapper;

import innowake.mining.extensions.datalineage.DataLineageGmlUtilWithSeparateCopybooks;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraph;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode.Type;
import innowake.mining.shared.model.job.ResultContainer;

/**
 * Class for testing DataLineageGmlUtil.
 */
class GmlExportTest {

	private static final Path JSON_PATH = Paths.get("src", "test", "resources");
	
	private static Stream<String> jsonFiles() {
		return Stream.of("TEST21.json", "same-copybook-example.json");
	}
	
	@ParameterizedTest
	@MethodSource("jsonFiles")
	void testNodesFromJsonArePresentIntGml(final String jsonFileName) throws IOException {
		final Path jsonFilePath = JSON_PATH.resolve(jsonFileName);
		final String json = new String(Files.readAllBytes(jsonFilePath));
		final ObjectMapper mapper = new ObjectMapper();
		final ResultContainer container = mapper.readValue(json, ResultContainer.class);
		final DataFlowGraph dataFlowGraph = (DataFlowGraph) container.getObject();
		
		final DataLineageGmlUtilWithSeparateCopybooks dataLineageGmlUtilWithSeparateCopybooks = new DataLineageGmlUtilWithSeparateCopybooks(json);
		final String gml = dataLineageGmlUtilWithSeparateCopybooks.getGml();
		
		/* filter all node IDs from the GML file since we cannot create the graph from GML with jGraphT */
		final Set<String> nodeIDs = new HashSet<>();
		final Pattern pattern = Pattern.compile("node\\s*\\[\\s*id\\s*(-?\\d+)");
		final Matcher matcher = pattern.matcher(gml);
		
		while (matcher.find()) {
			nodeIDs.add(matcher.group(1));
		}
		
		for (final DataFlowGraphNode node : dataFlowGraph.getNodes()) {
			/* modules are not actual nodes in the graph. therefore, they don't exist in the GML file as nodes */
			if (node.getType() != Type.MODULE) {
				assertTrue(nodeIDs.contains(String.valueOf(node.getId().hashCode())), "There's a node missing in the GML file.");
			}
		}
	}
}

/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.extensions.datalineage;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import org.apache.commons.io.FileUtils;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.SimpleGraph;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.systemdir.gml.YedGmlWriter;

import innowake.mining.shared.model.job.ResultContainer;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraph;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode.Type;
import innowake.mining.shared.model.datalineage.util.DataInterface;
import innowake.mining.shared.model.datalineage.util.DataLineageGraphicsProvider;
import innowake.mining.shared.model.datalineage.util.Element;
import innowake.mining.shared.model.datalineage.util.Field;
import innowake.mining.shared.model.datalineage.util.Module;
import innowake.mining.shared.model.datalineage.util.Statement;
import innowake.mining.shared.model.datalineage.util.YedGmlWriterWithNesting;

/**
 * Read input from JSON file and generate a .gml-file to visualize data flow in Yed with nesting of modules.
 */
public class DataLineageGraphMLPrototype {

	private static final Set<String> ALL_IDS = new HashSet<>();
	private static final Logger LOG = LoggerFactory.getLogger(DataLineageGraphMLGenerator.class);

	/**
	 * Given a JSON-file that represents a graph, we create a GML-file representing the same graph for Yed.
	 * @param args Not required.
	 * @throws IOException Invalid paths for input and output or the wrong UTF-format of the JSON-file can cause exceptions. Since this method will be
	 * removed later anyway, we can leave it without handling at this point.
	 */
	public static void main(String[] args) throws IOException {

		final String fileContent = FileUtils.readFileToString(new File("./src/main/resources/newOutStatement.json"), StandardCharsets.UTF_8);
		final ObjectMapper mapper = new ObjectMapper();
		final ResultContainer container = mapper.readValue(fileContent, ResultContainer.class);
		final DataFlowGraph dataFlowGraph = (DataFlowGraph) container.getObject();

		final List<DataFlowGraphNode> allNodes = dataFlowGraph.getNodes();
		final List<Module> moduleList = new ArrayList<>();
		final List<DataInterface> dataInterfaceList = new ArrayList<>();
		final List<Field> fieldList = new ArrayList<>();
		final List<Statement> statementList = new ArrayList<>();

		final Map<String, Element> elementsByIdMap = new HashMap<>();
		final Map<String, Set<Element>> moduleFieldsStatements = new HashMap<>();
		final Map<String, Set<Element>> moduleDataInterfaces = new HashMap<>();
		/*
		 * Map to indicate collapsable groups in Yed.
		 */
		final Map<Module, Set<Element>> moduleGroup = new HashMap<>();

		final SimpleGraph<Element, DefaultEdge> graph = new SimpleGraph<>(DefaultEdge.class);

		for (final DataFlowGraphNode node : allNodes) {
			final String id = node.getId();
			ALL_IDS.add(id);

			final Type type = node.getType();
			if (type != null) {
				switch (type) {
					case MODULE:
						final Module module = new Module(node);
						graph.addVertex(module);
						moduleList.add(module);
						elementsByIdMap.put(node.getId(), module);
						moduleFieldsStatements.put(id, new HashSet<>());
						moduleDataInterfaces.put(id, new HashSet<>());
						break;

					case DATA_INTERFACE:
						final DataInterface dataInterface = new DataInterface(node);
						graph.addVertex(dataInterface);
						dataInterfaceList.add(dataInterface);
						elementsByIdMap.put(id, dataInterface);
						break;

					case FIELD:
						final Field field = new Field(node);
						graph.addVertex(field);
						fieldList.add(field);
						elementsByIdMap.put(id, field);
						break;

					case STATEMENT:
						final Statement statement = new Statement(node);
						graph.addVertex(statement);
						statementList.add(statement);
						elementsByIdMap.put(id, statement);
						break;
				}
			}
		}
		assignElements(dataInterfaceList, moduleDataInterfaces);
		assignElements(statementList, moduleFieldsStatements);
		assignElements(fieldList, moduleFieldsStatements);
		assignElementsToGroups(moduleList, moduleFieldsStatements, moduleDataInterfaces, moduleGroup);
		setEdges(allNodes, graph, elementsByIdMap);
		exportGraphAsGml(graph, moduleGroup);
	}

	private static void assignElements(final List<? extends Element> elements, final Map<String, Set<Element>> toBeAdded) {
		for (final Element element : elements) {
			final String parentModule = element.getFlowGraphNode().getParentModule();
			if (parentModule == null)
				continue;
			final String parentId = parentModule;
			toBeAdded.get(parentId).add(element);
		}
	}

	/**
	 * Maps parent modules to their children as sets regarding the JSON-file. Distinguishes between data-interfaces and fields or statements 
	 * to nest data-interfaces into a group of data-interfaces which itself is added to the parent-module on the same level as fields and statements.
	 * @param moduleList List of all parent-modules.
	 * @param moduleFieldsStatements Maps parent-modules to their field and statement elements.
	 * @param moduleDataInterfaces Groups all data-interfaces of a parent-module and maps a placeholder-module to those data-interfaces.
	 * @param moduleGroup Maps parent-modules to their child-elements, i.e. fields and statements individually and data-interfaces as one group.
	 */
	private static void assignElementsToGroups(final List<Module> moduleList, final Map<String, Set<Element>> moduleFieldsStatements,
			final Map<String, Set<Element>> moduleDataInterfaces, final Map<Module, Set<Element>> moduleGroup) {
		for (final Module module : moduleList) {
			final String id = module.getFlowGraphNode().getId();

			/*
			 * We need to create a new node for the data-interfaces. To group them together, we have to create a pseudo-module which is not part
			 * of the JSON-file. Ultimately, this requires to assign a unique ID to said module. Since this ID is undefined yet, we simply use an
			 * ID-counter. If there aren't any interfaces, we can skip this step.
			 */
			if ( ! moduleDataInterfaces.get(id).isEmpty()) {
				final DataFlowGraphNode tmp = new DataFlowGraphNode();
				tmp.setId(nextID());
				tmp.setName("Data Interfaces");
				final Module dataInterfaces = new Module(tmp);
				moduleGroup.put(dataInterfaces, moduleDataInterfaces.get(id));
				moduleFieldsStatements.get(id).add(dataInterfaces);
			}
			moduleGroup.put(module, moduleFieldsStatements.get(id));
		}
	}

	/**
	 * Generates a unique ID which hasn't been assigned yet.
	 * @return A new and unique ID.
	 */
	private static String nextID() {
		String nextCandidate = UUID.randomUUID().toString();
		while (ALL_IDS.contains(nextCandidate))
			nextCandidate = UUID.randomUUID().toString();
		ALL_IDS.add(nextCandidate);
		return nextCandidate;
	}

	private static void setEdges(final List<DataFlowGraphNode> allNodes, final SimpleGraph<Element, DefaultEdge> graph,
			final Map<String, Element> elementsByIdMap) {
		for (final DataFlowGraphNode node : allNodes) {
			final Set<String> descendants = node.getOutgoings();
			final Element source = elementsByIdMap.get(node.getId());
			for (final String id : descendants) {
				if (id.equals(node.getId()))
					continue;
				final Element target = elementsByIdMap.get(id);
				graph.addEdge(source, target, new DefaultEdge());
			}
		}
	}

	private static void exportGraphAsGml(final SimpleGraph<Element, DefaultEdge> graph, final Map<Module, Set<Element>> moduleGroup) throws IOException {
		final DataLineageGraphicsProvider graphicsProvider = new DataLineageGraphicsProvider();

		final YedGmlWriterWithNesting<Element, DefaultEdge, Module> writer = new YedGmlWriterWithNesting.Builder<>(graphicsProvider, YedGmlWriter.PRINT_LABELS)
				.setGroups(moduleGroup, Element::getName).setEdgeLabelProvider(edge -> "").setVertexLabelProvider(Element::getName).build();
		/*
		 * Create output-path for file
		 */
		/*new File("gml-output").mkdir();
		final File outputFile = new File("gml-output" + File.separator + "graph.gml");

		try (final Writer output = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outputFile), StandardCharsets.UTF_8))) {
			writer.export(output, graph);
		}
		LOG.info("File exported to: " + outputFile.getAbsolutePath());*/
		final StringWriter swriter = new StringWriter();
		writer.export(swriter, graph);
		System.out.println(swriter.toString());
	}
}

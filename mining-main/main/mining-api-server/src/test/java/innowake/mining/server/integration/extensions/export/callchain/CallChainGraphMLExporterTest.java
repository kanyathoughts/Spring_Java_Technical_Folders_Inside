/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.extensions.export.callchain;

import static innowake.mining.extensions.export.callchain.Parameters.PARAMETER_EXPORT_FORMAT;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static java.util.Collections.singletonList;
import static java.util.Arrays.asList;

import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.output.StringBuilderWriter;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.security.test.context.support.WithMockUser;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xmlunit.builder.DiffBuilder;
import org.xmlunit.diff.DefaultNodeMatcher;
import org.xmlunit.diff.Diff;
import org.xmlunit.diff.ElementSelectors;

import innowake.mining.extensions.export.callchain.CallChainExporter;
/**
 * Tests for {@link CallChainExporter}.
 */
@TestInstance(Lifecycle.PER_CLASS) /* required for access to testData */
@WithMockUser
@TestMethodOrder(OrderAnnotation.class)
class CallChainGraphMLExporterTest extends AbstractCallChainExporterTest {
	
	private static final String GRAPHML_EXPORT_FOLDER = "./test-resources/innowake/mining/server/integration/extensions/export/callchain/graphml/";
	
	@DisplayName("GraphML call chain export tests")
	@ParameterizedTest(name = "{0}")
	@MethodSource("testCases")
	@Order(1)
	void test(final String name, final Map<String, List<String>> parameters) throws Exception {
		runTest(name, parameters, testData);
	}

	private void runTest(final String name, final Map<String, List<String>> parameters, final TestData testData) throws Exception {
		parameters.put(PARAMETER_EXPORT_FORMAT, Collections.singletonList("GRAPHML"));
		String actual = callChainJob(parameters);
		
		final List<String> ids = new ArrayList<>(testData.modules.size());
		final List<String> repl = new ArrayList<>(testData.modules.size());
		
		testData.modules.values().forEach(module -> {
			ids.add(module.getId().toString());
			repl.add("ID_" + module.getName());
		});

		/* Replace module id's to its respective module name to make it readable and replacing counter from the output file as counter value may differ for 
		 * each run and cause test failures.
		 */
		actual = StringUtils.replaceEach(actual, ids.toArray(new String[0]), repl.toArray(new String[0]));
		
		/* Replace edge id's to its respective source target relationship format to make it readable.
		 */
		for (final Map.Entry<String, String> entry : createEdgeIdMappingToSourceTargetRel(actual).entrySet()) {
			actual = StringUtils.replace(actual, "\"" + entry.getKey().toString() + "\"", "\"" + entry.getValue() + "\"");
		}
		final File file = new File(GRAPHML_EXPORT_FOLDER + name + ".graphml");
		final Path path = Paths.get(GRAPHML_EXPORT_FOLDER, name + ".graphml");
		if (WRITE_EXPECTED) {
			Files.write(path, getFormattedGraphMlString(actual).getBytes(StandardCharsets.UTF_8));
		} else {
			final String exp = FileUtils.readFileToString(file, StandardCharsets.UTF_8.name());
			final Diff myDiff = DiffBuilder.compare(actual).withTest(exp)
		            .withNodeMatcher(new DefaultNodeMatcher(ElementSelectors.byNameAndAllAttributes))
		            .ignoreWhitespace()
		            .checkForSimilar()
		            .build();
			if (myDiff.hasDifferences()) {
				assertEquals(getFormattedGraphMlString(exp), getFormattedGraphMlString(actual));
			}
		}
	}
	
	@Test
	@Order(2)
	void testInvalidExportFormat() {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put(PARAMETER_EXPORT_FORMAT, Collections.singletonList("ML"));

		assertThrows(IllegalArgumentException.class, () -> callChainJob(parameters));
	}

	@Test
	@Order(5)
	void testExportCallChainForWMIN8413() throws Exception {
		final TestData data = createTestDataForWMIN8413();
		runTest("Both End Modules", createParametersWithEndModuleIds(data, asList(moduleId(data, "EndA"), moduleId(data, "EndB"))), data);
		runTest("End Module with P1", createParametersWithEndModuleIds(data, singletonList(moduleId(data, "EndA"))), data);
		runTest("End Module with P2", createParametersWithEndModuleIds(data, singletonList(moduleId(data, "EndB"))), data);
		cleanupTestData(data);
	}
	
	@Test
	@Order(3)
	void testExportCallChainConditionalDependency() throws Exception {
		final TestData data = createTestDataConditionalDependencyOneModule();
		/* Tests behavior for an outgoing graph with a conditional dependency when condition is met. */
		runTest("exportCallChainConditionalDependencyOut", parameters(DIRECTIONS, asList("OUT"), START_MODULE_IDS,
				asList(moduleId(data, "JOB_A"))), data);
		
		/* Tests behavior for an outgoing graph with a conditional dependency when condition is not met. */
		runTest("exportCallChainConditionalDependencyOutConditionNotMet", parameters(DIRECTIONS, asList("OUT"), START_MODULE_IDS,
				asList(moduleId(data, "JOB_B"))), data);
		
		/* Tests behavior for an incoming graph with a conditional dependency. */
		runTest("exportCallChainConditionalDependencyIn", parameters(DIRECTIONS, asList("IN"), START_MODULE_IDS,
				asList(moduleId(data, "ConditionalFile"))), data);
		cleanupTestData(data);
	}
	
	@Test
	@Order(4)
	void testExportCallChainMultipleConditionalDependencies() throws Exception {
		final TestData data = createTestDataMultipleConditionalDependencies();
		/* Tests behavior for an outgoing graph when condition for both conditional dependencies is met. */
		runTest("exportCallChainMultipleConditionalDependenciesA", parameters(DIRECTIONS, asList("OUT"), START_MODULE_IDS,
				asList(moduleId(data, "JOB_A"))), data);
		
		/* Tests behavior for an outgoing graph when condition for one of the conditional dependencies is met. */
		runTest("exportCallChainMultipleConditionalDependenciesB", parameters(DIRECTIONS, asList("OUT"), START_MODULE_IDS,
				asList(moduleId(data, "JOB_B"))), data);
		
		/* Tests behavior for an outgoing graph when condition for none of the conditional dependencies is met. */
		runTest("exportCallChainMultipleConditionalDependenciesC", parameters(DIRECTIONS, asList("OUT"), START_MODULE_IDS,
				asList(moduleId(data, "JOB_C"))), data);
		cleanupTestData(data);
	}

	@Test
	@Order(6)
	void testExportCallChainWMIN8745WithDirectionIn() throws Exception {
		final TestData data = createTestDataOneForWMIN8745();
		runTest("exportCallChainForWMIN8745DirectionIn", parameters(START_MODULE_IDS, asList(moduleId(data, "Start")), DIRECTIONS, asList("IN"), DEPTH,
				asList("3"), END_MODULE_IDS, asList(moduleId(data, "End"))), data);
		cleanupTestData(data);
	}

	@Test
	@Order(7)
	void testExportCallChainTwoWMIN8745WithDirectionOut() throws Exception {
		final TestData data = createTestDataTwoForWMIN8745();
		runTest("exportCallChainForWMIN8745DirectionOut", parameters(START_MODULE_IDS, asList(moduleId(data, "Start")), DIRECTIONS, asList("OUT"), DEPTH,
				asList("6"), END_MODULE_IDS, asList(moduleId(data, "End"))), data);
		cleanupTestData(data);
	}

	private static String getFormattedGraphMlString(final String unformattedGraphML) throws Exception {
		final DocumentBuilder db = DocumentBuilderFactory.newInstance().newDocumentBuilder();
		final Document doc = db.parse(new InputSource(new StringReader(unformattedGraphML)));
		final Transformer transformer = TransformerFactory.newInstance().newTransformer();

		transformer.setOutputProperty(OutputKeys.INDENT, "yes");

		sortVerticesAndEdges(doc);

		final DOMSource source = new DOMSource(doc);
		final StringBuilderWriter strWriter = new StringBuilderWriter();
		final StreamResult result = new StreamResult(strWriter);
		transformer.transform(source, result);
		
		final String lineSeparator = System.getProperty("line.separator");
		final String graphML = Arrays.stream(strWriter.toString().split(lineSeparator))
				.filter(StringUtils::isNotBlank)
				.collect(Collectors.joining("\n"));
		return graphML;
	}

	private static void sortVerticesAndEdges(final Document doc) {
		final NodeList graphMlChildren = doc.getFirstChild().getChildNodes();
		for (int i = 0; i < graphMlChildren.getLength(); i++) {
			final Node graphNode = graphMlChildren.item(i);
			if ("graph".equals(graphNode.getNodeName())) {
				final NodeList childNodes = graphNode.getChildNodes();
				final List<Node> vertices = new ArrayList<>(64);
				final List<Node> edges = new ArrayList<>(64);
				for (int j = 0; j < childNodes.getLength(); j++) {
					final Node child = childNodes.item(j);
					if ("node".equals(child.getNodeName())) {
						vertices.add(child);
					} else if ("edge".equals(child.getNodeName())) {
						edges.add(child);
					}
				}

				final Function<Node, String> id = n -> {
					if (n.hasAttributes()) {
						final Node attr = n.getAttributes().getNamedItem("id");
						return attr != null ? attr.getTextContent().toLowerCase() : "";
					}
					return "";
				};
				/* vertices have unique ids */
				vertices.stream().sorted((l,r) -> id.apply(l).compareTo(id.apply(r))).forEach(n -> graphNode.appendChild(n));
				/* there can be multiple edges between same source and destination but with different edge properties */
				edges.stream().sorted((l,r) -> {
					final int c = id.apply(l).compareTo(id.apply(r));
					/* in case of a multiple edges additionally compare the whole text  */
					return (c != 0) ? c : l.getTextContent().compareTo(r.getTextContent());
				}).forEach(n -> graphNode.appendChild(n));

				break;
			}
		}
	}
	
	private static Map<String, String> createEdgeIdMappingToSourceTargetRel(final String actual) throws ParserConfigurationException, SAXException, IOException {
		final Map<String, String> edgeMapToSourceTargetRel = new HashMap<>();
		final DocumentBuilder db = DocumentBuilderFactory.newInstance().newDocumentBuilder();
		final Document doc = db.parse(new InputSource(new StringReader(actual)));
		final NodeList edgeList = doc.getElementsByTagName("edge");
		for (int i = 0; i < edgeList.getLength(); i++) {
			final Node edgeNode = edgeList.item(i);
			if (edgeNode.getNodeType() == Node.ELEMENT_NODE) {
				final Element edgeElement = (Element) edgeNode;
				final String id = edgeElement.getAttribute("id");
				final String source = edgeElement.getAttribute("source");
				final String target = edgeElement.getAttribute("target");
				final String relationship = edgeElement.getFirstChild().getFirstChild().getNodeValue();
				final String sourceTargetRel = source + "_" + relationship + "_" + target;
				edgeMapToSourceTargetRel.put(id, sourceTargetRel);
			}
		}
		return edgeMapToSourceTargetRel;
	}
}
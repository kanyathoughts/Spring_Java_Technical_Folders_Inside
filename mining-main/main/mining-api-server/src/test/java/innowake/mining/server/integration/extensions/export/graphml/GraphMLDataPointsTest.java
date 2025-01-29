/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.extensions.export.graphml;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.extensions.export.graphml.GraphMLExportJob.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.extensions.export.graphml.GraphMLExportJob;
import innowake.mining.server.service.MiningJobService.JobResult;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.SourceMetricsPojo;
import innowake.mining.shared.entities.SourceMetricsPojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Test customizable GraphML export.
 */
class GraphMLDataPointsTest extends GraphMLTest {


	private final DocumentBuilderFactory xml = DocumentBuilderFactory.newInstance();
	
	@Nullable
	private EntityId testModule;
	
	@BeforeAll
	public void insertTestData() {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setProject(TEST_PROJECT_ID);
		module.setContent("asdfghjkl");
		module.setDescription("Some description");
		module.setIdentification(Identification.IDENTIFIED);
		module.setMetricsDate(Instant.now());
		module.setName("TESTMOD");
		module.setTechnology(Technology.COBOL);
		module.setType(Type.COPYBOOK);
		module.setStorage(Storage.FILE);
		module.setOrigin(Origin.CUSTOM);
		module.setCreator(Creator.DISCOVERY);
		module.setPath("some/path/name");
		module.setRepresentation(Representation.PHYSICAL);
		module.setRequiresReview(true);
		final SourceMetricsPojoPrototype metrics = new SourceMetricsPojoPrototype();
		metrics.setComplexityMcCabe(9);
		metrics.setCodeLines(123);
		metrics.setCommentLines(45);
		metrics.setDeadCodeLines(21);
		metrics.setPhysicalLines(150);
		module.setSourceMetrics(metrics);
		testModule = moduleService.create(module);
	}
	
	private void expectExport(final List<String> attributes, final Map<String, String> values) throws IOException, SAXException, ParserConfigurationException {
		final GraphMLExportJob job = new GraphMLExportJob(TEST_PROJECT_ID, Collections.singletonMap(GraphMLExportJob.ATTRIBUTES_KEY, attributes), false);
		final Optional<JobResult> jobResult = getJobResult(submitJob(job));
		
		assertTrue(jobResult.isPresent(), "Job Result must exist");

		final Document doc = xml.newDocumentBuilder().parse(jobResult.get().getContent());
		final NodeList docKeys = doc.getElementsByTagName("key");
		final List<String> keys = IntStream.range(0, docKeys.getLength())
				.mapToObj(i -> docKeys.item(i).getAttributes().getNamedItem("id").getNodeValue())
				.collect(Collectors.toList());
		
		values.keySet().forEach(a -> assertTrue(keys.contains(a), a + " missing in result"));
		
		final NodeList docAttr = doc.getElementsByTagName("graph").item(0).getChildNodes().item(0).getChildNodes();
		IntStream.range(0, docAttr.getLength()).mapToObj(docAttr::item).forEach(data -> {
			final String key = data.getAttributes().getNamedItem("key").getNodeValue();
			final String value = values.get(key);
			assertNotNull("Unexpected attribute " + key, value);
			assertEquals(value, data.getTextContent(), () -> "Value mismatch on " + key);
		});
	}
	
	@Test
	void testExportSome() throws IOException, SAXException, ParserConfigurationException {
		final var testModule = moduleService.getModule(assertNotNull(this.testModule));
		final var testMetrics = assertNotNull(testModule.getSourceMetrics());
		
		final List<String> attributes = Arrays.asList("name", "nid", "description", SOURCE_METRICS_CODE_LINES, SOURCE_METRICS_COMMENT_LINES, "uid");
		final Map<String, String> expected = new HashMap<>();
		expected.put("labelV", testModule.getName());
		expected.put("name", testModule.getName());

		expected.put("nid", String.valueOf(testModule.getId()));
		expected.put("description", testModule.getDescription().orElse(null));
		expected.put("codeLines", String.valueOf(testMetrics.map(SourceMetricsPojo::getCodeLines).orElse(null)));
		expected.put("commentLines", String.valueOf(testMetrics.map(SourceMetricsPojo::getCommentLines).orElse(null)));
		expected.put("uid", String.valueOf(testModule.getUid()));
		
		expectExport(attributes, expected);
	}
	
	@Test
	void testExportAll() throws IOException, SAXException, ParserConfigurationException {
		final var testModule = moduleService.getModule(assertNotNull(this.testModule));
		final var testMetrics = testModule.getSourceMetrics();
		
		final List<String> attributes = new ArrayList<>();
		final Map<String, String> expected = new HashMap<>();
		expected.put("labelV", testModule.getName());
		attributes.add(SOURCE_METRICS_COMPLEXITY_MC_CABE);
		expected.put("complexityMcCabe", String.valueOf(testMetrics.map(SourceMetricsPojo::getComplexityMcCabe).orElse(null)));
		attributes.add("contentHash");
		expected.put("contentHash", testModule.getContentHash().get().toString());
		attributes.add("description");
		expected.put("description", testModule.getDescription().orElse(null));
		attributes.add("errors");
		expected.put("errors", String.valueOf(testModule.getErrors()));
		attributes.add("nid");
		expected.put("nid", String.valueOf(testModule.getId()));
		attributes.add("identification");
		expected.put("identification", testModule.getIdentification().name());
		attributes.add(SOURCE_METRICS_CODE_LINES);
		expected.put("codeLines", String.valueOf(testMetrics.map(SourceMetricsPojo::getCodeLines).orElse(null)));
		attributes.add(SOURCE_METRICS_COMMENT_LINES);
		expected.put("commentLines", String.valueOf(testMetrics.map(SourceMetricsPojo::getCommentLines).orElse(null)));
		attributes.add(SOURCE_METRICS_DEAD_CODE_LINES);
		expected.put("deadCodeLines", String.valueOf(testMetrics.map(SourceMetricsPojo::getDeadCodeLines).orElse(null)));
		attributes.add("metricsDate");
		expected.put("metricsDate", testModule.getMetricsDate().get().toString());
		attributes.add("modifiedDate");
		expected.put("modifiedDate", testModule.getModifiedDate().get().toString());
		attributes.add("name");
		expected.put("name", testModule.getName());
		attributes.add("technology");
		expected.put("technology", testModule.getTechnology().name());
		attributes.add("type");
		expected.put("type", testModule.getType().name());
		attributes.add("storage");
		expected.put("storage", testModule.getStorage().name());
		attributes.add("origin");
		expected.put("origin", testModule.getOrigin().name());
		attributes.add("path");
		expected.put("path", testModule.getPath().orElse(null));
		attributes.add("representation");
		expected.put("representation", testModule.getRepresentation().map(Enum::name).orElse(null));
		attributes.add(SOURCE_METRICS_PHYSICAL_LINES);
		expected.put("physicalLines", String.valueOf(testMetrics.map(SourceMetricsPojo::getPhysicalLines).orElse(null)));
		attributes.add("requiresReview");
		expected.put("requiresReview", String.valueOf(testModule.isRequiresReview()));
		attributes.add("sqlStatements");
		expected.put("sqlStatements", String.valueOf(testModule.getSqlStatements()));
		attributes.add("statements");
		expected.put("statements", String.valueOf(testModule.getStatements()));
		attributes.add("uid");
		expected.put("uid", String.valueOf(testModule.getUid()));
		
		expectExport(attributes, expected);
	}

}

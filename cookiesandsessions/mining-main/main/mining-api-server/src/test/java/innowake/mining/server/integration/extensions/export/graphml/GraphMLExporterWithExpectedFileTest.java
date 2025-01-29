/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.extensions.export.graphml;

import static innowake.mining.extensions.export.graphml.GraphMLExportJob.SOURCE_METRICS_COMPLEXITY_MC_CABE;
import static innowake.mining.extensions.export.graphml.GraphMLExportJob.SOURCE_METRICS_DEAD_CODE_LINES;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.StringReader;
import java.nio.charset.Charset;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Instant;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.output.StringBuilderWriter;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import org.xmlunit.builder.DiffBuilder;
import org.xmlunit.diff.DefaultNodeMatcher;
import org.xmlunit.diff.Diff;
import org.xmlunit.diff.ElementSelectors;

import innowake.mining.extensions.export.graphml.GraphMLExportJob;
import innowake.mining.server.service.MiningJobService.JobResult;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Tests for {@link GraphMLExportJob} verifying some expected files.
 */
class GraphMLExporterWithExpectedFileTest extends GraphMLTest {

	private static final Path EXPECTED_FOLDER = Paths.get("./test-resources/innowake/mining/server/integration/export");
	private static final boolean WRITE_EXPECTED = false;

	@BeforeAll
	public void insertTestData() {
		final Instant moduleModifiedDate = Instant.ofEpochMilli(1494487567894L);

		final EntityId rootJob = moduleService.create(new ModulePojoPrototype()
				.setNid(1L)
				.setProject(TEST_PROJECT_ID)
				.setName("ROOTJOB")
				.setTechnology(Technology.COBOL)
				.setType(Type.COPYBOOK)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setModifiedDate(moduleModifiedDate)
				.setCreator(Creator.DISCOVERY)
			);

		
		final EntityId stepA = moduleService.create(new ModulePojoPrototype()
				.setNid(2L)
				.setProject(TEST_PROJECT_ID)
				.setName("ROOTJOB.STEPA")
				.setTechnology(Technology.COBOL)
				.setType(Type.EXEC_PGM)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setModifiedDate(moduleModifiedDate)
				.setCreator(Creator.DISCOVERY)
			);

		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.CALLS)
				.setSrcModule(rootJob)
				.setDstModule(stepA)
			);
	}


	@Test
	void testExportGraphMLWithExpectedGraphMLFile() throws Exception {
		final Map<String, List<String>> parameters = new HashMap<>();
		final String[] selectedAttribute = {
				"contentHash", "creator", SOURCE_METRICS_DEAD_CODE_LINES, "description", "errors", "nid", "uid", "origin", "path",
				SOURCE_METRICS_COMPLEXITY_MC_CABE, "requiresReview", "storage", "technology", "type"
		};
		parameters.put(GraphMLExportJob.ATTRIBUTES_KEY, Arrays.asList(selectedAttribute));

		final GraphMLExportJob job = new GraphMLExportJob(TEST_PROJECT_ID, parameters, false);
		final Optional<JobResult> jobResult = getJobResult(submitJob(job));

		assertTrue(jobResult.isPresent(), "Job Result must exist");

		final String content = toString(jobResult.get().getContent());
		final String actual = getFormattedGraphMlString(content
						.replaceAll("\\w{8}-\\w{4}-\\w{4}-\\w{4}-\\w{12}", "UID")
						.replaceAll("<data key=\"name\">ROOTJOB(\\.STEPA)?</data>", "<data key=\"name\">NAME</data>")
						.replaceAll("<data key=\"nid\">\\d+</data>", "<data key=\"nid\">NID</data>")
						.replaceAll("<data key=\"linkHash\">\\w+</data>", "<data key=\"linkHash\">Hash</data>"));

		if (WRITE_EXPECTED) {
			writeExpectedFile("ExpectedGraphML.graphml", actual);
		} else {
			final String expected = loadExpectedFile("ExpectedGraphML.graphml");
			final Diff myDiff = DiffBuilder
									.compare(actual)
									.withTest(expected)
									.withNodeMatcher(new DefaultNodeMatcher(ElementSelectors.byNameAndAllAttributes))
									.ignoreWhitespace()
									.checkForSimilar()
									.build(); 
			if (myDiff.hasDifferences() && ! assertGraphMLFiles(expected, actual)) {
				assertEquals(expected, actual, "Actual Exported GraphML file is not same as expected graphML files.");
			}
		}
	}

	private static String loadExpectedFile(final String fileName) throws IOException {
		return FileUtils.readFileToString(EXPECTED_FOLDER.resolve(fileName).toFile(), Charset.forName("Cp1252"));
	}

	private static void writeExpectedFile(final String fileName, final String content) throws IOException {
		FileUtils.writeStringToFile(EXPECTED_FOLDER.resolve(fileName).toFile(), content, Charset.forName("Cp1252"));
	}
	
	private static String getFormattedGraphMlString(final String unformattedGraphML) throws Exception {
		final DocumentBuilder db = DocumentBuilderFactory.newInstance().newDocumentBuilder();
		final Document doc = db.parse(new InputSource(new StringReader(unformattedGraphML)));
		final Transformer transformer = TransformerFactory.newInstance().newTransformer();

		transformer.setOutputProperty(OutputKeys.INDENT, "yes");

		final DOMSource source = new DOMSource(doc);
		final StringBuilderWriter strWriter = new StringBuilderWriter();
		final StreamResult result = new StreamResult(strWriter);
		transformer.transform(source, result);

		return strWriter.toString();
	}
}

/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.integration.extensions.export.graphml;

import static org.junit.Assert.fail;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.commons.io.IOUtils;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import brave.Tracer;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.integration.discovery.BaseDiscoveryTest;
import innowake.mining.server.service.MiningJobService;
import innowake.mining.server.service.MiningJobService.JobResult;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.TaxonomyService;

/**
 * Base class for GraphML related tests.
 */
@TestInstance(Lifecycle.PER_CLASS)
@WithMockUser
abstract class GraphMLTest extends DatabaseRelatedTest {

	/** Existing project */
	protected static final EntityId TEST_PROJECT_ID = EntityId.of(4L);

	@Autowired
	protected Tracer tracer;

	@Autowired
	protected JobManager jobManager;

	@Autowired
	protected ModuleService moduleService;

	@Autowired
	protected AnnotationService annotationService;

	@Autowired
	protected MiningJobService miningJobService;

	@Autowired
	protected TaxonomyService taxonomyService;

	/**
	 * Submits the job, waits until the job is finished and returns the job ID.
	 * <p>
	 * There is a timeout of 10 minutes for the job wait.
	 * 
	 * @param job the job to run
	 * @return the ID of the job
	 */
	protected String submitJob(final Job<?> job) {
		return BaseDiscoveryTest.submitJob(jobManager, tracer, job);
	}

	/**
	 * Returns the {@link JobResult} of the given Job ID.
	 *
	 * @param jobId the ID of the job for which the result should be returned
	 * @return the result of the job, if available
	 */
	protected Optional<JobResult> getJobResult(final String jobId) {
		return miningJobService.getJobResult(jobId);
	}

	/**
	 * Converts the given input stream to an UTF-8 String. 
	 * 
	 * @param stream the input stream to convert
	 * @return a String representation of the stream
	 * @throws IOException if an I/O error occurs
	 */
	protected String toString(final InputStream stream) throws IOException {
		final StringWriter writer = new StringWriter();
		IOUtils.copy(stream, writer, "UTF-8");
		return writer.toString();
	}

	protected boolean assertGraphMLFiles(final String graphmlString1, final String graphmlString2) {
		final Map<String, String> graph1Nodes = extractGraphNodes(graphmlString1);
		final Map<String, String> graph2Nodes = extractGraphNodes(graphmlString2);

		return graph1Nodes.equals(graph2Nodes);
	}

	private Map<String, String> extractGraphNodes(final String graphmlString) {
		final Map<String, String> nodes = new HashMap<>();

		try {
			final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
			final DocumentBuilder builder = factory.newDocumentBuilder();
			final Document document = builder.parse(new InputSource(new StringReader(graphmlString)));

			final NodeList nodeList = document.getElementsByTagName("node");

			for (int i = 0; i < nodeList.getLength(); i++) {
				final Node node = nodeList.item(i);
				if (node.getNodeType() == Node.ELEMENT_NODE) {
					final Element element = (Element) node;
					final String nodeId = element.getAttribute("id");
					final String nodeLabel = extractNodeLabel(element);
					nodes.put(nodeId, nodeLabel);
				}
			}
		} catch (final Exception e) {
			fail("Unexpected exception occurred: " + e.getMessage());
		}

		return nodes;
	}

	private @Nullable String extractNodeLabel(final Element nodeElement) {
		final NodeList dataList = nodeElement.getElementsByTagName("data");

		for (int i = 0; i < dataList.getLength(); i++) {
			final Element dataElement = (Element) dataList.item(i);
			if ("label".equals(dataElement.getAttribute("key"))) {
				return dataElement.getTextContent();
			}
		}

		return null;
	}

}

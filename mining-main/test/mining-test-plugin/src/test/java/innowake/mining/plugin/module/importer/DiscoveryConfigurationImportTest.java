/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.importer;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.io.output.StringBuilderWriter;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.junit.Test;
import org.xml.sax.InputSource;

import innowake.mining.client.service.discovery.DiscoveryServiceProvider;
import innowake.mining.plugin.IntegrationBaseTest;
import innowake.mining.shared.io.SecuredZipInputStream;

/**
 * Tests {@link DiscoveryConfigurationsImporter}.
 */
public class DiscoveryConfigurationImportTest extends IntegrationBaseTest {

	private static final Long PROJECT_ID = Long.valueOf(1);
	private static final String ROOT_PATH = "src/configurations/";
	private static final String PATH_1 = ROOT_PATH + "discovery-search-order.xml";
	private static final String SEARCH_ORDER_CONTENT = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><search-orders><search-order><source pattern=\"application/programs/A/*.cbl\"/><target pattern=\"application/copies/A/*.cpy\"/><target pattern=\"application/copies/B/*.cpy\"/></search-order></search-orders>";
	
	private static final String CONFIG_FILE_PATH = "src/test/resources/innowake/mining/plugin/discovery/config/";
	private static final String DISCOVERY_CONFIG = CONFIG_FILE_PATH + "Discovery_Config.xml";
	private static final String DNA_LOUVIAN_RUNNER = CONFIG_FILE_PATH + "DNA_LouvainRunner_Config.xml";
	private static final String DNA_SIMILARITY_PROCESSOR = CONFIG_FILE_PATH + "DNA_SimilarityProcessor_Config.xml";
	private static final String UTILITIES = CONFIG_FILE_PATH + "utilities.xml";
	private static final String DNA_SEQUENCER_CONFIG = CONFIG_FILE_PATH + "DNA_Sequencer_Config.xml";

	/**
	 * Imports configurations and searchOrders.
	 * @throws Exception in case of errors.
	 */
	@Test
	public void testCreateAndUpdate() throws Exception {
		final IFile discoveryConfig = mockFile(DISCOVERY_CONFIG, FileUtils.readFileToString(new File(DISCOVERY_CONFIG), StandardCharsets.UTF_8));
		final IFile dnaLouvianRunner = mockFile(DNA_LOUVIAN_RUNNER, FileUtils.readFileToString(new File(DNA_LOUVIAN_RUNNER), StandardCharsets.UTF_8));
		final IFile dnaSimilarityProcessor = mockFile(DNA_SIMILARITY_PROCESSOR, FileUtils.readFileToString(new File(DNA_SIMILARITY_PROCESSOR), StandardCharsets.UTF_8));
		final IFile utlities = mockFile(UTILITIES, FileUtils.readFileToString(new File(UTILITIES), StandardCharsets.UTF_8));
		final IFile dnaSequencer = mockFile(DNA_SEQUENCER_CONFIG, FileUtils.readFileToString(new File(DNA_SEQUENCER_CONFIG), StandardCharsets.UTF_8));
		final IFile searchOrder = mockFile(PATH_1, SEARCH_ORDER_CONTENT);

		importConfigurations(Arrays.asList(discoveryConfig, dnaLouvianRunner, dnaSimilarityProcessor, utlities, dnaSequencer, searchOrder));

		final Map<String, String> configuration = downloadConfigurations();

		final var expectedConfiguration = toXmlFormatted(FileUtils.readFileToString(new File(DISCOVERY_CONFIG), StandardCharsets.UTF_8));
		final var actualConfiguration = toXmlFormatted(configuration.get("Discovery_Config.xml"));
		assertEquals("Discovery_Config.xml must match", expectedConfiguration, actualConfiguration);

		final var expectedLouvian = toXmlFormatted(FileUtils.readFileToString(new File(DNA_LOUVIAN_RUNNER), StandardCharsets.UTF_8));
		final var actualLouvian = toXmlFormatted(configuration.get("DNA_LouvainRunner_Config.xml"));
		assertEquals("DNA_LouvainRunner_Config.xml must match", expectedLouvian, actualLouvian);

		final var expectedDnaSimilarity = toXmlFormatted(FileUtils.readFileToString(new File(DNA_SIMILARITY_PROCESSOR), StandardCharsets.UTF_8));
		final var actualDnaSimilarity = toXmlFormatted(configuration.get("DNA_SimilarityProcessor_Config.xml"));
		assertEquals("DNA_SimilarityProcessor_Config.xml must match", expectedDnaSimilarity, actualDnaSimilarity);

		final var expectedDnaSequencer = toXmlFormatted(FileUtils.readFileToString(new File(DNA_SEQUENCER_CONFIG), StandardCharsets.UTF_8));
		final var actualDnaSequencer = toXmlFormatted(configuration.get("DNA_Sequencer_Config.xml"));
		assertEquals("DNA_Sequencer_Config.xml must match", expectedDnaSequencer, actualDnaSequencer);

		final var expectedUtilities = toXmlFormatted(FileUtils.readFileToString(new File(UTILITIES), StandardCharsets.UTF_8));
		final var actualUtilities = toXmlFormatted(configuration.get("utilities.xml"));
		assertEquals("utilities.xml must match", expectedUtilities, actualUtilities);

		final var expectedSearchOrder = toXmlFormatted(SEARCH_ORDER_CONTENT);
		final var actualSearchOrder = toXmlFormatted(configuration.get("discovery-search-order.xml"));
		assertEquals("discovery-search-order.xml must match", expectedSearchOrder, actualSearchOrder);
	}

	private static String toXmlFormatted(final String unformatted) throws Exception {
		assertNotNull("Unformatted XML string must not be null", unformatted);

		final var builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
		final var source = new DOMSource(builder.parse(new InputSource(new StringReader(unformatted))));
		final var writer = new StringBuilderWriter();
		final var result = new StreamResult(writer);

		final var transformer = TransformerFactory.newInstance().newTransformer();
		transformer.setOutputProperty(OutputKeys.INDENT, "yes");
		transformer.transform(source, result);

		return Arrays.stream(writer.toString().split(System.lineSeparator()))
				.filter(StringUtils::isNotBlank)
				.collect(Collectors.joining("\n"));
	}

	private IFile mockFile(final String path, final String content) {
		final IFile mock = mock(IFile.class);
		doReturn(Path.fromOSString(path)).when(mock).getProjectRelativePath();
		try {
			doReturn(IOUtils.toInputStream(content, StandardCharsets.UTF_8)).when(mock).getContents();
		} catch (final CoreException e) {
			throw new IllegalStateException(e);
		}
		return mock;
	}

	private void importConfigurations(final List<IFile> files) {
		final var discoveryConfigurationsImporter = new DiscoveryConfigurationsImporter();
		discoveryConfigurationsImporter.setConnectionInfo(getConnectionInfo());
		discoveryConfigurationsImporter.setProjectId(PROJECT_ID);
		discoveryConfigurationsImporter.setFiles(files);
		discoveryConfigurationsImporter.uploadConfigurations();
	}

	private Map<String, String> downloadConfigurations() throws IOException {
		final var result = new DiscoveryServiceProvider(getConnectionInfo())
				.downloadConfiguration()
				.setProjectId(PROJECT_ID)
				.execute();
		if (result.isValid()) {
			final Optional<byte[]> stream = result.getValue();
			if (stream.isPresent()) {
				final Map<String, String> map = new HashMap<>();
				final ZipInputStream zipIn = new SecuredZipInputStream(new ByteArrayInputStream(stream.get()));
				ZipEntry zipEntry;
				while ((zipEntry = zipIn.getNextEntry()) != null) {
					final ByteArrayOutputStream out = new ByteArrayOutputStream();
					IOUtils.copy(zipIn, out);
					map.put(FilenameUtils.getName(zipEntry.getName()), new String(out.toByteArray(), StandardCharsets.UTF_8));
				}

				return map;
			} else {
				throw new IOException("Download Configuration: Byte stream must be present");
			}
		} else {
			throw new IOException("Download Configuration: Result must be valid but was: " + result.getStatusMessage());
		}
	}
}

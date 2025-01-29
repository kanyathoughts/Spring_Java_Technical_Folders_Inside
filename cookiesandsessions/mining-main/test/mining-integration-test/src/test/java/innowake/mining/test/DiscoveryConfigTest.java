/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.project.ProjectServiceProvider;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;


/**
 * Tests the XML representation.
 */
class DiscoveryConfigTest extends IntegrationTest {
	
	public static final Charset WORKSPACE_CHARSET = Charset.forName("cp1252");
	
	private final ProjectServiceProvider projectServiceProvider = MiningApiClient.projectService(getConnectionInfo());
	private static final EntityId CLIENT_ONE = EntityId.of(1L);
	private static final Long PROJECT_ID = Long.valueOf(1);
	private static final String ROOT_PATH = "src/configurations/";
	private static final String PATH_1 = ROOT_PATH + "DNA_Sequencer_Config.xml";
	private static final String DNA_COBOLOBJECTSEQUENCER_CONTENT = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\r\n<properties>\r\n<property key=\"testEmptyDNA\" value=\"true\" title=\"Filter Empty DNA\">\r\n<comment>Set to true to avoid empty DNA strings. Highly recommended.</comment>\r\n</property>\r\n</properties>";
		
	/**
	 * Imports configurations and searchOrders.
	 * 
	 * @throws IOException IOException occurs
	 */
	@Test
	void testCreateAndUpdate() throws IOException {
		final Long projectId = createProject(new ProjectPojoPrototype()
				.setName("TEST PROJECT 1")
				.setClient(CLIENT_ONE)
				.setNatures(Collections.emptySet())
			).getId();
		Assertions.assertNotNull(projectId);

		Assertions.assertDoesNotThrow(() -> {
			final byte[] archive;
			archive = compress();
			importConfigurations(archive);
		});

		final Result<ProjectPojo> selectedProject = projectServiceProvider.findProjectById().setProjectId(PROJECT_ID).execute();
		assertEquals(200, selectedProject.getStatusCode());
		assertTrue(selectedProject.getValue().isPresent());
	}
	
	private ProjectPojo createProject(final ProjectPojoPrototype project) throws IOException {
		final Result<ProjectPojo> result = projectServiceProvider.createProject().setProject(project).execute();
		assertEquals(201, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		return result.getValue().get();
	}
	
	private void importConfigurations(final byte[] archive) throws IOException {
		try (final InputStream inputStream = new ByteArrayInputStream(archive)) {
			final Result<Void> uploadResult = MiningApiClient.discoveryService(assertNotNull(getConnectionInfo()))
					.uploadConfiguration()
					.setProjectId(assertNotNull(PROJECT_ID))
					.setInputStreamId("/")
					.setInputStream(inputStream)
					.execute();
			assertEquals(200, uploadResult.getStatusCode());
		}
	}
	
	private byte[] compress() throws IOException {
		try (final ByteArrayOutputStream byteOut = new ByteArrayOutputStream()) {
			try (final ZipOutputStream zipOut = new ZipOutputStream(byteOut, StandardCharsets.UTF_8)) {
				final File file = new File(PATH_1);
				final byte[] strToBytes = DNA_COBOLOBJECTSEQUENCER_CONTENT.getBytes();
				zipOut.putNextEntry(new ZipEntry(file.getPath().toString()));
				zipOut.write(strToBytes);				
			}
			return byteOut.toByteArray();
		}
	}
}

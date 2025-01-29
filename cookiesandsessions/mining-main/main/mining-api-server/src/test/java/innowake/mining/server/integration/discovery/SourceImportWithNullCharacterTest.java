/* Copyright (c) 2021 Deloitte. All rights reserved. */
package innowake.mining.server.integration.discovery;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.google.gson.Gson;
import innowake.lib.job.api.config.properties.JobConfigurationProperties;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.server.importer.sources.SourceObjectImportJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.io.MiningFileIndex;
import org.junit.jupiter.api.Test;
import org.springframework.security.test.context.support.WithMockUser;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

@WithMockUser
class SourceImportWithNullCharacterTest extends BaseDiscoveryTest {

	/**
	 * Tests that no errors are found in the program with null characters.
	 */
	@Test
	void testNoErrorsExistsForProgram() throws IOException {
		final EntityId projectId = performDiscovery();
		final List<ModulePojo> modules = moduleService.findModules(builder -> builder.ofProject(projectId).withName("PGM"));

		assertEquals(1, modules.size());
		assertEquals(0, modules.get(0).getErrors());
	}

	public EntityId performDiscovery() throws IOException {
		sourceService.resetCaches();
		/* Prepare data for file upload for sourceImport job */
		final var contentBytes = Files.readAllBytes(Paths.get(getSourcePath().toString(), "PGM"));
		final var uploadData = prepareFileUpload(Long.valueOf(21), new byte[][] {
						contentBytes,
				}, "WMIN13315/PGM");

		final EntityId projectId = createProject().identity();
		submitJob(jobManager, tracer, new SourceObjectImportJob(projectId, new ByteArrayInputStream(uploadData),
				new JobConfigurationProperties()));
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		submitJob(jobManager, tracer, new DiscoverMetricsJob(projectId, false));
		return projectId;
	}

	@Override
	public String getTestFolder() {
		return "WMIN13315";
	}

	private byte[] prepareFileUpload(final Long revision, final byte[][] filesBytes,	final String... fileNames) throws IOException {
		assertEquals(filesBytes.length, fileNames.length);
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();

		final List<MiningFileIndex.File> files = Arrays.stream(fileNames)
				.map(fileName -> new MiningFileIndex.File(null, fileName, null, null, revision.longValue(), revision.longValue(), true))
				.collect(Collectors.toList());

		final MiningFileIndex fileIndex = new MiningFileIndex();
		fileIndex.setVersion(1);
		fileIndex.setScope("/");
		fileIndex.setSourceCodeRevision(revision);
		fileIndex.setFiles(files);

		try (final ZipOutputStream zipOut = new ZipOutputStream(baos)) {
			zipOut.putNextEntry(new ZipEntry(MiningFileIndex.NAME));
			zipOut.write(new Gson().toJson(fileIndex).getBytes(StandardCharsets.UTF_8));

			int i = 0;
			for (final String fileName : fileNames) {
				zipOut.putNextEntry(new ZipEntry(fileName));
				zipOut.write(filesBytes[i++]);
			}
		}

		return baos.toByteArray();

	}
}

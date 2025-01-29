/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.importer;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.time.StopWatch;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.client.service.discovery.UploadConfiguration;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.client.ApiClient;
import innowake.mining.shared.entities.ProjectPojo;

/**
 * Uploads configurations and search orders from innoWake plugins to the Mining server.
 */
public class DiscoveryConfigurationsImporter {
	@Nullable
	private ConnectionInfo connectionInfo;

	@Nullable
	private Long projectId;

	@Nullable
	private List<IFile> files;
	
	/**
	 * Gets the {@link ConnectionInfo}.
	 *
	 * @return The Connection Info.
	 */
	@Nullable
	public ConnectionInfo getConnectionInfo() {
		return connectionInfo;
	}

	/**
	 * Sets {@link ConnectionInfo}.
	 *
	 * @param connectionInfo Connection Info.
	 */
	public void setConnectionInfo(final ConnectionInfo connectionInfo) {
		this.connectionInfo = connectionInfo;
	}

	/**
	 * Gets the {@link ProjectPojo} id.
	 *
	 * @return The Project Id.
	 */
	@Nullable
	public Long getProjectId() {
		return projectId;
	}

	/**
	 * Sets the {@link ProjectPojo} id.
	 *
	 * @param projectId The Project Id.
	 */
	public void setProjectId(final Long projectId) {
		this.projectId = projectId;
	}

	/**
	 * Gets the list of {@link IFile}.
	 *
	 * @return List of {@link IFile}.
	 */
	@Nullable
	public List<IFile> getFiles() {
		return files;
	}

	/**
	 * Sets the list of {@link IFile}.
	 *
	 * @param files List of {@link IFile}.
	 */
	public void setFiles(final List<IFile> files) {
		this.files = files;
	}
	
	/**
	 * Invokes the {@link UploadConfiguration} end point and also zips the files.
	 */
	public void uploadConfigurations() {
		final StopWatch watch = new StopWatch();
		watch.start();
		Logging.info("Uploading configuration");
		
		final byte[] archive;
		try {
			archive = compress();
		} catch (final IOException | CoreException e) {
			Logging.error("Error while compressing files", e);
			return;
		}
		try (final InputStream inputStream = new ByteArrayInputStream(archive)) {
			MiningServiceExecutor.create(() -> createUploadDiscoveryConfigurationService(inputStream))
					.setExceptionConsumer(e -> Logging.error("Error while executing upload configuration service", e))
					.setInvalidResultConsumer(result -> Logging.error(result.getExtendedStatusMessage()))
					.execute();
		} catch (final IOException e) {
			Logging.error("Error while accessing compressed files", e);
			return;
		}
		watch.stop();
		Logging.info(String.format("Overall uploading configuration of took %s (H:mm:ss.SSS)", watch.toString()));
	}

	private byte[] compress() throws IOException, CoreException {
		try (final ByteArrayOutputStream byteOut = new ByteArrayOutputStream()) {
			try (final ZipOutputStream zipOut = new ZipOutputStream(byteOut, StandardCharsets.UTF_8)) {
				for (final IFile file : assertNotNull(files)) {
					zipOut.putNextEntry(new ZipEntry(file.getProjectRelativePath().toString()));
					try (final InputStream inputStream = file.getContents()) {
						zipOut.write(IOUtils.toByteArray(inputStream));
					}
				}
			}
			return byteOut.toByteArray();
		}
	}

	private UploadConfiguration createUploadDiscoveryConfigurationService(final InputStream inputStream) {
		return ApiClient.discoveryService(assertNotNull(connectionInfo))
				.uploadConfiguration()
				.setProjectId(assertNotNull(projectId))
				.setInputStreamId("/")
				.setInputStream(inputStream);
	}
}

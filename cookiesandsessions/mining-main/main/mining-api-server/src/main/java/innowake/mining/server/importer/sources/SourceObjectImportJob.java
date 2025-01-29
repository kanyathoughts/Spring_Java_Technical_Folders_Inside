/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.importer.sources;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.apache.commons.io.IOUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import com.google.gson.Gson;
import com.google.gson.stream.JsonWriter;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.lib.job.api.config.properties.JobConfigurationProperties;
import innowake.mining.data.io.sourceobject.SourceObjectImportService;
import innowake.mining.server.discovery.monitor.ProgressMonitorText;
import innowake.mining.server.job.MiningFileJob;
import innowake.mining.server.util.ProgressMonitorThrottle.ThrottlingProgressMonitor;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.io.MiningFileIndex;

/**
 * Job for executing source import
 */
public class SourceObjectImportJob extends MiningFileJob<FileSystemResult> {
	
	/** The name of this job's result file */
	private static final String RESULT_FILE_NAME = "MiningFileIndex_%s.zip";
	private static final Logger LOG = LoggerFactory.getLogger(innowake.mining.data.Logging.IO);
	private static final String FILE_NAME = "%s_sourceImport.zip";

	@Autowired
	private transient SourceObjectImportService sourceObjectImportService;
	@Autowired
	private transient Gson gson;

	private final EntityId projectId;
	private final String importFile;

	/**
	 * Public constructor.
	 *
	 * <p>Creates a file on the local machine and copies all bytes from the given {@code inputStream} into it. Does not close the {@code inputStream}.</p>
	 *
	 * @param projectId The ID of the project to execute the source import
	 * @param inputStream The {@link InputStream} for the to be imported source objects
	 * @param jobConfigurationProperties The {@link JobConfigurationProperties}
	 * @throws IllegalStateException if the export of the {@code inputStream} into the file system of the local machine failed
	 */
	public SourceObjectImportJob(final EntityId projectId, final InputStream inputStream, final JobConfigurationProperties jobConfigurationProperties) {
		super(jobConfigurationProperties);
		this.projectId = projectId;
		importFile = String.format(FILE_NAME, getJobId());

		try (final OutputStream outputStream = this.createFile(importFile)) {
			IOUtils.copy(inputStream, outputStream);
		} catch (final IOException exception) {
			throw new IllegalStateException("Failed to create file for SourceObjectImport", exception);
		}
	}

	@Override
	protected Result<FileSystemResult> run(final ProgressMonitor progressMonitor) {
		LOG.info(() -> "Starting Source Objects Import for project ID " + projectId);
		progressMonitor.setJobDescription(ProgressMonitorText.SOURCE_IMPORT_TASK);
		progressMonitor.setStepDescription(ProgressMonitorText.SOURCE_IMPORT_TASK_DESCRIPTION);

		Result<FileSystemResult> result;
		try (
			/* The source import zip containing all source contents */
			final InputStream inputStream = openFile(importFile);
			/* Zip the JSON of the MiningFileIndex and write it as job result into the local file system */
			final ZipOutputStream zipOut = new ZipOutputStream(createResultFile(), StandardCharsets.UTF_8);
			/* MiningFileIndex to JSON */
			final JsonWriter writer = new JsonWriter(new OutputStreamWriter(zipOut, StandardCharsets.UTF_8))) {
			final MiningFileIndex fileIndex = sourceObjectImportService.importSourceObjects(new ThrottlingProgressMonitor(progressMonitor),
					projectId, getJobId(), inputStream);
			zipOut.putNextEntry(new ZipEntry(MiningFileIndex.NAME));
			gson.toJson(fileIndex, MiningFileIndex.class, writer);

			result = new Result<>(new FileSystemResult(MediaType.APPLICATION_OCTET_STREAM_VALUE, String.format(RESULT_FILE_NAME, getJobId()), true));
		} catch (final IOException e) {
			LOG.error(() -> "SourcePojo import failed for project ID " + projectId, e);
			result = new Result<>(new Status(e));
		}

		/* Clear the step description as we are done now. */
		progressMonitor.setStepDescription("");
		LOG.info(() -> "Finished Source Objects Import successfully for project ID " + projectId);
		return result;
	}

	@Override
	public String getJobName() {
		return "Source Code Import";
	}
}

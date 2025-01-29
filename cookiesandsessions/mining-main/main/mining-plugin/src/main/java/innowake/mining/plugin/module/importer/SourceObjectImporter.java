/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.importer;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.plugin.base.JobUtil.isJobNotRunning;
import static java.nio.charset.StandardCharsets.UTF_8;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiFunction;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.io.input.CountingInputStream;
import org.apache.commons.lang.time.StopWatch;
import org.eclipse.core.filesystem.IFileInfo;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ICoreRunnable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.fasterxml.jackson.databind.ObjectMapper;

import innowake.base.eclipse.core.util.ProgressUtil;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.io.ImportSourceObjects;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.MiningPlugin;
import innowake.mining.plugin.base.JobUtil;
import innowake.mining.plugin.base.ResourceUtil;
import innowake.mining.plugin.client.ApiClient;
import innowake.mining.plugin.module.ui.handler.PluginWorkingStorageHelper;
import innowake.mining.plugin.module.ui.handler.PluginWorkingStorageHelper.SourceObjectTimestampHelper;
import innowake.mining.plugin.ui.SourceObjectErrorDialog;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.io.MiningFileIndex;
import innowake.mining.shared.io.MiningFileIndex.File;
import innowake.mining.shared.io.SourceObjectReferences;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.ResultContainer;

/**
 * Uploads {@link SourcePojo SourcePojos} and {@link SourceObjectReferences} from innoWake plugins object caches to the Mining server.
 */
public class SourceObjectImporter implements ICoreRunnable {

	private final ConnectionInfo connectionInfo;
	private final Long projectId;
	private final String rootPath;
	private final List<IFile> files;
	private final IProject project;
	private final SourceObjectTimestampHelper sourceObjectTimestampHelper;
	private final boolean forceUpload;
	private static final long FILE_SIZE_LIMIT = SourceService.CONTENT_SIZE_LIMIT / (long) Math.pow(10, 6);  
	private final Shell parentShell;
	private final SourceImportResultHandler sourceImportResultHandler;
	
	private int inclusionCount = 0;

	/**
	 * Constructor.
	 * 
	 * @param connectionInfo the {@link ConnectionInfo}
	 * @param project the local workspace {@link IProject}
	 * @param projectId  the remote project ID
	 * @param parentShell the parent shell
	 * @param rootPath the root path containing the files to upload
	 * @param files the files to upload
	 * @param forceUpload Setting it to {@code true} will skip the check if the Source code was modified after the previous successful upload.
	 */
	public SourceObjectImporter(
			final ConnectionInfo connectionInfo,
			final IProject project,
			final Long projectId, 
			final Shell parentShell,
			final String rootPath, 
			final List<IFile> files, 
			final boolean forceUpload) {
		
		this.connectionInfo = connectionInfo;
		this.projectId = projectId;
		this.rootPath = rootPath;
		this.files = files;
		this.project = project;
		this.sourceObjectTimestampHelper = new SourceObjectTimestampHelper(project, projectId, connectionInfo);
		this.sourceImportResultHandler = new SourceImportResultHandler();
		this.forceUpload = forceUpload;
		this.parentShell = parentShell;
	}
	
	@Override
	public void run(@Nullable final IProgressMonitor monitor) throws CoreException {
		validate();
		final StopWatch watch = new StopWatch();
		watch.start();
		Logging.info(String.format("Uploading %s source code files", Integer.valueOf(files.size())));

		final SubMonitor subMonitor = SubMonitor.convert(monitor, "Compressing files", 100);
		final MiningFileIndex existingFileIndex;
		@Nullable
		Path zipPath = null;
		try {
			try {
				existingFileIndex = PluginWorkingStorageHelper.loadMiningFileIndex(project);
				logDuration("Loading file index took %s (H:mm:ss.SSS)", watch);
				zipPath = compress(existingFileIndex, subMonitor);
				logDuration("Creating file archive took %s (H:mm:ss.SSS)", watch);
			} catch (final IOException | CoreException exception) {
				Logging.error("Error while compressing files", exception);
				throw new CoreException(new Status(IStatus.ERROR, MiningPlugin.ID, "Error while compressing files.", exception));
			}

			ProgressUtil.checkCanceled(subMonitor);

			subMonitor.split(50);
			subMonitor.setTaskName("Uploading files");

			try (final BufferedInputStream bufferedInput = new BufferedInputStream(Files.newInputStream(zipPath));
				 final CountingInputStream inputStream = new ZipCountingInputStream(bufferedInput, subMonitor, Files.size(zipPath))) {
				final Optional<String> jobId = MiningServiceExecutor.create(() -> createUploadSourceObjectService(inputStream))
						.setExceptionConsumer(exception -> Logging.error("Error while executing upload source object service", exception))
						.setInvalidResultConsumer(result -> Logging.error(result.getExtendedStatusMessage()))
						.execute();
	
				if ( ! jobId.isPresent()) {
					return;
				}
				JobUtil.submittedRemoteJob(project, jobId.get());
				
				while ( ! subMonitor.isCanceled()) {
					JobUtil.waitForNextPoll();
	
					final Optional<JobInformation> jobInfo = MiningServiceExecutor.create(() -> ApiClient.jobService(project).getJobInfo()
							.setJobId(jobId.get()))
							.setInvalidResultConsumer(invalidResult -> logError("Failed to retrieve SourceObjectImport job information. Response was: ", invalidResult))
							.setExceptionConsumer(exception -> Logging.error(exception.getLocalizedMessage(), exception))
							.execute();
	
					if ( ! jobInfo.isPresent()) {
						return;
					}
					/* when job is no longer running ... */
					if (isJobNotRunning(jobInfo.get().getStatus())) {
						final Optional<ResultContainer> maybeUpdatedFileIndex = MiningServiceExecutor.create(() -> ApiClient.jobService(project).getJobResult()
								.setJobId(jobId.get())
								.setCustomResponseHandler(sourceImportResultHandler))
								.setValidResultConsumer(result -> {
									/* clear source object time stamp in case where src folder is empty and metrics has been performed on it */
									if (files.isEmpty()) {
										sourceObjectTimestampHelper.clearProjectData();
									} else {
										sourceObjectTimestampHelper.removeFiles(files, existingFileIndex);
									}
									sourceObjectTimestampHelper.commit();
								})
								.setInvalidResultConsumer(result -> logError("Failed to retrieve SourceObjectImport result. Response was:", result))
								.setExceptionConsumer(exception -> Logging.error(exception.getLocalizedMessage(), exception))
								.execute();
	
						final MiningFileIndex updatedFileIndex = maybeUpdatedFileIndex.map(result -> (MiningFileIndex) result.getObject()).orElse(new MiningFileIndex());
						updatedFileIndex.setScope(rootPath);
						final MiningFileIndex mergedFileIndex = PluginWorkingStorageHelper.mergeFileIndex(existingFileIndex, updatedFileIndex);
						PluginWorkingStorageHelper.storeMiningFileIndex(project, mergedFileIndex);		
	
						break;
					}
				}
			} catch (final IOException exception) {
				Logging.error("Error while accessing compressed files", exception);
				throw new CoreException(new Status(IStatus.ERROR, MiningPlugin.ID, "Error while accessing compressed files.", exception));
			}
	
			logDuration("Uploading files took %s (H:mm:ss.SSS)", watch);
			subMonitor.split(50);
			watch.stop();
			if (inclusionCount < files.size()) {
				Logging.info(String.format(
						"%s of %s source code files were not modified since last upload and hence contents of those files were excluded during this upload.",
						Integer.valueOf(files.size() - inclusionCount), Integer.valueOf(files.size())));
			}
			Logging.info(String.format("Overall uploading of %s source code files took %s (H:mm:ss.SSS)", Integer.valueOf(files.size()), watch.toString()));
		} finally {
			if (zipPath != null) {
				FileUtils.deleteQuietly(zipPath.toFile());
				Logging.info("Deleted temp file of source upload: " + zipPath);
			}			
		}
	}

	private static void logError(final String msg, final Result<?> invalidResult) {
		Logging.error(msg + invalidResult.getStatusCode() + " " + invalidResult.getStatusMessage() + "\n" + invalidResult.getStatusBody().orElse(""));
	}

	private void validate() throws CoreException {
		final List<IFile> invalidFiles = new ArrayList<>();
		for (final IFile file : files) {
			final IFileInfo fileInfo = ResourceUtil.getFileInfo(file.getLocation());
			if (fileInfo != null) {
				if (fileInfo.getLength() > SourceService.CONTENT_SIZE_THRESHOLD_FOR_LIMIT_CHECK) {
					/* don't read-in anything bigger than 32 MB */
					if (fileInfo.getLength() > SourceService.CONTENT_SIZE_LIMIT * 2) {
						invalidFiles.add(file);
						Logging.info(String.format("File: %s cannot be uploaded. File content exceeds the allowable limit. File size: %d",
													file.getLocation(), Long.valueOf(fileInfo.getLength())));
					} else {
						try {
							/* This creates the string by using the default charset of the VM which is configurable and when not set can also be
							 * different between different Java Versions. Then we get the bytes of the string using UTF8 for length check. */
							final String content = new String(IOUtils.toByteArray(file.getContents()));
							final int contentLengthAfterEncoding = content.getBytes(StandardCharsets.UTF_8).length;
							if (contentLengthAfterEncoding > SourceService.CONTENT_SIZE_LIMIT) {
								invalidFiles.add(file);
								Logging.info(String.format("File: %s cannot be uploaded. File content after encoding exceeds the allowable limit."
										+ "File content size after encoding: %d bytes", file.getLocation(), contentLengthAfterEncoding));
							}
						} catch (final IOException e) {
							throw new CoreException(new Status(IStatus.ERROR, MiningPlugin.ID, "Error while reading the file."));
						}
					}
				}
			} else {
				throw new CoreException(new Status(IStatus.ERROR, MiningPlugin.ID, "Error while accessing file info."));
			}
		}
		if ( ! invalidFiles.isEmpty()) {
			if (getUserResponseForInvalidFiles(parentShell)
					.apply(invalidFiles, "Some file(s) are larger than " + FILE_SIZE_LIMIT + " MB after encoding and cannot be uploaded.")
					.intValue() != IDialogConstants.SKIP_ID) {
				throw new CoreException(new Status(IStatus.ERROR, MiningPlugin.ID, "User aborted the SourceObject import process"));
			}
			files.removeAll(invalidFiles);
		}
	}

	private Path compress(final MiningFileIndex existingFileIndex, final IProgressMonitor monitor) throws IOException, CoreException {
		final Map<String, Long> existingFileIds = existingFileIndex.getFiles().stream().collect(Collectors.toMap(file -> {
				/* IFILE.getProjectRelativePath() never starts with "/" so we need to remove it for finding matches if present */
				final String path = file.getPath();
				return path.charAt(0) == '/' ? path.substring(1) : path;
			},
			indexFile -> assertNotNull(indexFile.getId())));

		final Path zipPath = Files.createTempFile("source-upload-", ".zip");
		Logging.info("Created temp file for source upload: " + zipPath);
		setPermissions(zipPath);

		try (final OutputStream fileOut = Files.newOutputStream(zipPath);
			 final ZipOutputStream zipOut = new ZipOutputStream(new BufferedOutputStream(fileOut), UTF_8)) {
				final List<File> mIndexFiles = new ArrayList<>();
				final boolean isModified = sourceObjectTimestampHelper.isModified();
				for (int i = 0; i < files.size(); i++) {
					ProgressUtil.checkCanceled(monitor);

					monitor.setTaskName(String.format("Determining files to upload from file index (%d/%d).", Integer.valueOf(i), Integer.valueOf(files.size())));
					final IFile file = files.get(i);
					final File miningFile = new File();
					final String projectRelativePathString = file.getProjectRelativePath().toString();
					@Nullable
					final Long fileId = existingFileIds.get(projectRelativePathString);
					if (fileId != null) {
						miningFile.setId(fileId);
					}
					if (forceUpload || isModified || sourceObjectTimestampHelper.isModified(file)) {
						miningFile.setContentIncluded(true);
					}
					miningFile.setPath(projectRelativePathString);
					mIndexFiles.add(miningFile);
				}
				final MiningFileIndex index = new MiningFileIndex();
				index.setFiles(mIndexFiles);
				index.setScope(rootPath);
				index.setVersion(1);
				final ObjectMapper mapper = new ObjectMapper();
				zipOut.putNextEntry(new ZipEntry(MiningFileIndex.NAME));
				zipOut.write(mapper.writeValueAsBytes(index));
				for (int i = 0; i < files.size(); i++) {
					ProgressUtil.checkCanceled(monitor);
					monitor.setTaskName(String.format("Compressing files (%d/%d).", Integer.valueOf(i), Integer.valueOf(files.size())));
					final IFile file = files.get(i);

					if (forceUpload || isModified || sourceObjectTimestampHelper.isModified(file)) {
						compress(zipOut, file);
					}
					sourceObjectTimestampHelper.addFile(file);
				}

			return zipPath;
		}
	}

	private static void setPermissions(final Path zipPath) throws CoreException {
		final java.io.File zipFile = zipPath.toFile();
		if ( ! zipFile.setReadable(true)) {
			throw new CoreException(new Status(IStatus.ERROR, MiningPlugin.ID, "Faild to set read permission for current user on " + zipPath));
		}
		if ( ! zipFile.setWritable(true)) {
			throw new CoreException(new Status(IStatus.ERROR, MiningPlugin.ID, "Faild to set write permission for current user on " + zipPath));
		}
		if ( ! zipFile.setExecutable(true)) {
			throw new CoreException(new Status(IStatus.ERROR, MiningPlugin.ID, "Faild to set execute permission for current user on " + zipPath));
		}
	}
	
	private void compress(final ZipOutputStream zipOut, final IFile file) throws IOException, CoreException {
		inclusionCount++;
		zipOut.putNextEntry(new ZipEntry(file.getProjectRelativePath().toString()));
		try (final InputStream inputStream = file.getContents()) {
			IOUtils.copy(inputStream, zipOut);
		}
	}
	
	private ImportSourceObjects createUploadSourceObjectService(final InputStream inputStream) {
		return ApiClient.ioService(connectionInfo)
				.importSourceObjects()
				.setProjectId(projectId)
				.setInputStreamId(rootPath)
				.setInputStream(inputStream);
	}

	private static void logDuration(final String message, final StopWatch watch) {
		watch.stop();
		Logging.info(String.format(message, watch));
		watch.reset();
		watch.start();
	}
	
	private static BiFunction<List<IFile>, String, Integer> getUserResponseForInvalidFiles(final Shell shell) {
		return (invalidFiles, message) -> {
			final AtomicInteger integer = new AtomicInteger();
			Display.getDefault().syncExec(() -> integer.set(SourceObjectErrorDialog.open(shell, invalidFiles, message)));
			return Integer.valueOf(integer.get());
		};
	}

	private static class ZipCountingInputStream extends CountingInputStream {
		
		private final SubMonitor monitor;
		private final long zipSize;
		private final String zipSizeDisplay;
		
		private ZipCountingInputStream(final InputStream inputStream, final SubMonitor monitor, final long zipSize) {
			super(inputStream);

			this.monitor = monitor;
			this.zipSize = zipSize;
			zipSizeDisplay = FileUtils.byteCountToDisplaySize(zipSize);
		}
		
		@Override
		protected synchronized void afterRead(final int n) {
			super.afterRead(n);
			monitor.setTaskName(String.format("Uploaded %s/%s (%d%%)",
					FileUtils.byteCountToDisplaySize(getByteCount()),
					zipSizeDisplay,
					Integer.valueOf((int) ((double) getByteCount() / zipSize * 100))));
		}
	}
}
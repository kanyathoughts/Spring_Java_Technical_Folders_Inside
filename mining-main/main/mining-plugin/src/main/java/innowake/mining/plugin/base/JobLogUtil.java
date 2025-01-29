/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.plugin.base;

import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Map;
import java.util.Optional;

import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;

import org.eclipse.core.resources.IFile;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.core.resources.IProject;

import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.client.ApiClient;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.client.service.Result;
import innowake.mining.plugin.base.ui.WorkbenchUtil;


/**
 * This utility class can be used to generate log files for remote job.
 */
public class JobLogUtil {
	
	private JobLogUtil() {}

	/**
	 * Generates the job log(s) content for the provided jobId.
	 * (THIS METHOD NEEDS TO BE REMOVED ONCE THE SAVING OF ZIP FILES WORK WMIN-12947)
	 *
	 * @param jobId the job id for which log needs to be downloaded.
	 * @param project the IProject.
	 * @return logContent the contents of log file.
	 */
	public static String generateLogContent(final String jobId, final IProject project) {
		final Optional<Map<String, String>> jobLog =
				MiningServiceExecutor.create(() -> ApiClient.jobService(project).getJobLog().setJobId(jobId))
						.setInvalidResultConsumer(invalidResult -> Logging.error("Failed to retrieve Job log output. Response was:"
								+ invalidResult.getStatusCode() + " " + invalidResult.getStatusMessage() + "\n" + invalidResult.getStatusBody().orElse("")))
						.execute();

		if ( ! jobLog.isPresent()) {
			return StringUtils.EMPTY;
		}

		final Map<String, String> logs = jobLog.get();
		final boolean includeNodeId = logs.size() > 1;
		final StringBuilder logContent = new StringBuilder(1000);
		final String lineSeparator = System.lineSeparator();
		for (final Map.Entry<String, String> entry : logs.entrySet()) {
			if (includeNodeId) {
				logContent.append(entry.getKey()).append(lineSeparator).append("--------------------").append(lineSeparator);
			}
			logContent.append(entry.getValue()).append(lineSeparator).append(lineSeparator);
		}
		return logContent.toString();
	}
	
	/**
	 * Saves the ZIP data as either a ZIP file or a log file depending on the content.
	 * 
	 * @param jobId The id of the job to retrieve.
	 * @param project The current project.
	 * @param fileLocation Location of the file to save to.
	 * @param tryAsZip Flag if this file should be saved as ZIP or not
	 * @throws IOException If an I/O error occurs.
	 */
	public static void saveJobLogAsZipOrAsLog(final String jobId, final IProject project, final String fileLocation, final boolean tryAsZip) throws IOException {

		final byte[] log = getJobLogResult(jobId, project);

		//TODO: The 'false' needs to be removed once the saving of the zip files works correctly
		if (hasMultipleFiles(log) && tryAsZip && false) {
			saveAsZip(log, project, fileLocation);
		} else {
			saveAsLog(log, fileLocation, project);
		}
		
		try {
			project.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
		} catch (final CoreException e) {
			throw new IllegalStateException(e);
		}
	}

	//TODO: Fix in WMIN-12947
	private static void saveAsZip(final byte[] log, final IProject project, final String fileLocation) {

		final ByteArrayInputStream bis = new ByteArrayInputStream(log);
		FileOutputStream fos = null;
		
		try {
			final File folder = createLogFolder(project);
			final File actualFile = new File(folder, fileLocation + ".zip");
			
			fos = new FileOutputStream(actualFile);
			IOUtils.copy(bis, fos);
		} catch (final IOException e) {
			e.printStackTrace();
		} finally {
			try {
				bis.close();
				
				if (fos != null) {
					fos.close();
				}
			} catch (final IOException e) {
				e.printStackTrace();
			}
		}
	}

	private static File createLogFolder(final IProject project) {
		
		final File folder = new File(project.getLocation().toFile() + "/logs"); 
		if ( ! folder.exists()) {
			folder.mkdir();
		}
		return folder;
	}

	private static byte[] getJobLogResult(final String jobId, final IProject project) {
		
		final MiningServiceExecutor<byte[]> executor = MiningServiceExecutor.create(() -> 
		ApiClient.jobService(project).getJobLogStreamed().setJobId(jobId));

		executor.setInvalidResultConsumer(invalidResult -> {
			throw new IllegalStateException("Failed to retrieve Job log output. Response was:"
					+ invalidResult.getStatusCode() + " " + invalidResult.getStatusMessage() + "\n" + invalidResult.getStatusBody().orElse("") + "\n");
		});


		final Result<byte[]> result = executor.getResult();
		if (result == null) {
			throw new IllegalStateException("ERROR IN getJobLogResult(jobId = " + jobId + " project =" + project + " result = " + result + " executor = " + executor + ")");
		}
		final byte[] value = result.getValue()
				.orElseThrow(() -> new IllegalStateException("ERROR IN getJobLogResult(jobId = " + jobId + " project =" + project + " result = " + result + ")"));

		return value;
	}

	private static boolean hasMultipleFiles(final byte[] stream) {
		try {
			final InputStream inputStream = new ByteArrayInputStream(stream);
			final ZipInputStream zipInputStream = new ZipInputStream(inputStream);

			int fileCount = 0;
			ZipEntry entry;
			while ((entry = zipInputStream.getNextEntry()) != null) {
				if (!entry.isDirectory()) {
					fileCount++;
				}
			}

			zipInputStream.close();
			inputStream.close();


			return fileCount > 1;
		} catch (final IOException e) {
			e.printStackTrace();

			return false;
		}
	}	

	private static void saveAsLog(final byte[] zipData, final String logFilePath, final IProject project) throws IOException {
		try (ByteArrayInputStream bais = new ByteArrayInputStream(zipData);
				ZipInputStream zipInputStream = new ZipInputStream(bais)) {
			final File folder = createLogFolder(project);
			ZipEntry entry = null;
			while((entry = zipInputStream.getNextEntry()) != null) {
				final File file = new File(folder, entry.getName() + "-" +  logFilePath + ".log");
				try (final FileOutputStream fis = new FileOutputStream(file)) {
					IOUtils.copy(zipInputStream, fis);
				} catch (final IOException e) {
					e.printStackTrace(); 
				}
			}
		}
	}

}

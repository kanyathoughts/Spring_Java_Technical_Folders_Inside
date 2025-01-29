/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.importer;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.function.Function;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringEscapeUtils;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.commons.lang.time.StopWatch;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;

import innowake.lib.core.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.client.exceptions.ExecutionException;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.base.ValidationException;
import innowake.mining.plugin.client.ApiClient;

/**
 * Importer for the Discovery CSV file running on Mining server.
 */
public final class DiscoveryCsvImporterServerside {
	
	private DiscoveryCsvImporterServerside() {}
	
	/**
	 * Import the CSV file into the given project of the mining backend.
	 *
	 * @param filePath the path of the Excel file
	 * @param projectId the ID of the project the import should be associated with
	 * @param connectionInfo the connection info of the mining backend
	 * @param existingModulesHandler function which controls the overwriting of existing modules in the backend
	 * @param monitor the progress monitor
	 * @throws ValidationException if something goes wrong while importing
	 */
	public static void doImport(
			final String filePath,
			final Long projectId, 
			final ConnectionInfo connectionInfo, 
			final Function<Long, Boolean> existingModulesHandler,
			final @Nullable IProgressMonitor monitor) throws ValidationException {
		
		final StopWatch watch = new StopWatch();
		watch.start();
		Logging.info(String.format("Importing server-side %s", filePath));
		final SubMonitor subMonitor = SubMonitor.convert(monitor, "Importing Discovery CSV", 100);

		try {
			if (Utils.checkForCancelOnExistingModules(existingModulesHandler, connectionInfo, projectId, subMonitor.split(10))) {
				Logging.info("Import was cancelled due to user interaction");
				return;
			}
			subMonitor.setTaskName("Call import service");
			importCSV(filePath, projectId, connectionInfo);
		} catch (final IOException e) {
			final String rootCauseMessage = ExceptionUtils.getRootCauseMessage(e);
			final Throwable rootCause = ExceptionUtils.getRootCause(e);
			throw new ValidationException(String.format("Error while importing CSV file %s", filePath), rootCauseMessage, rootCause);
		} finally {
			watch.stop();
			Logging.info(String.format("Overall import took %s (H:mm:ss.SSS)", watch.toString()));
		}
	}
	
	private static void importCSV(final String filePath, final Long projectId, final ConnectionInfo connectionInfo) throws IOException {
		try (final FileInputStream inputStream = FileUtils.openInputStream(new File(filePath))) {
			MiningServiceExecutor
					.create(() ->
							ApiClient.ioService(connectionInfo)
							.importCSV()
							.setProjectId(projectId)
							.setInputStreamId(filePath)
							.setInputStream(inputStream))
					.setInvalidResultConsumer(invalidResult -> {
							final String statusMessage = StringEscapeUtils.unescapeJava(invalidResult.getExtendedStatusMessage());
							Logging.error(String.format("Error while importing CSV file %s:%n%s", filePath, statusMessage));
					})
					.execute();
		} catch (final ExecutionException executionException) {
			/* In case exception consumer is specified for handling some other exception with the above MiningServiceExecutor in future,
			 * then executionException must be thrown from the consumer to prevent IOException from being consumed. */
			if (executionException.getCause() instanceof IOException) {
				throw (IOException) executionException.getCause();
			}
			throw executionException;
		}
	}
}

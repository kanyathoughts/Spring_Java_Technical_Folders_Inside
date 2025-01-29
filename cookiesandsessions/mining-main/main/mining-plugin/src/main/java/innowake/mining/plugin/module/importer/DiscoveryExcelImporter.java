/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.importer;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringEscapeUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.commons.lang.time.StopWatch;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.swt.widgets.Shell;
import innowake.lib.core.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.client.exceptions.ExecutionException;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.base.ValidationException;
import innowake.mining.plugin.client.ApiClient;

/**
 * Importer for the Discovery Excel file. 
 */
public final class DiscoveryExcelImporter {
	
	private DiscoveryExcelImporter() {}
	
	/**
	 * Import the Excel file into the given project.
	 *
	 * @param filePath the path of the Excel file
	 * @param projectId the ID of the project the import should be associated with
	 * @param connectionInfo the connection info of the mining backend
	 * @param existingModulesHandler function which controls the overwriting of existing modules
	 * @param monitor the progress monitor
	 * @param project the {@link Optional} project in which the actual source code resides, this is used for the upload if active
	 * @param shell the parent shell of the dialog 
	 * @throws ValidationException if something goes wrong while importing
	 */
	public static void doImport(
			final String filePath,
			final Long projectId,
			final ConnectionInfo connectionInfo,
			final Function<Long, Boolean> existingModulesHandler,
			final @Nullable IProgressMonitor monitor,
			final Optional<IProject> project,
			final Shell shell) throws ValidationException {

		final StopWatch watch = new StopWatch();
		watch.start();
		Logging.info(String.format("Importing %s", filePath));
		final SubMonitor subMonitor = SubMonitor.convert(monitor, "Importing Discovery Excel", 100);

		try {
			subMonitor.setTaskName("Validating Excel file");
			final List<String> modulePathsList = DiscoveryExcelValidator.validateAndReturnModulePaths(filePath);
			subMonitor.split(5);
			watch.split();
			Logging.info(String.format("Validating Excel file took %s (H:mm:ss.SSS)", watch.toSplitString()));

			if (Utils.checkForCancelOnExistingModules(existingModulesHandler, connectionInfo, projectId, subMonitor.split(10))) {
				Logging.info("Import was cancelled due to user interaction");
				return;
			}
			subMonitor.setTaskName("Call import service");
			importCSV(filePath, projectId, connectionInfo);

			final List<IFile> iFiles = modulePathsList.stream()
					.map(modulePath -> getFileByModulePath(modulePath, project))
					.filter(Objects::nonNull)
					.collect(Collectors.toList());

			if ( ! iFiles.isEmpty() && project.isPresent()) {
				new SourceObjectImporter(connectionInfo, project.get(), projectId, shell, StringUtils.EMPTY, iFiles, true).run(subMonitor);
			}

		} catch (final IOException e) {
			final String rootCauseMessage = ExceptionUtils.getRootCauseMessage(e);
			final Throwable rootCause = ExceptionUtils.getRootCause(e);
			throw new ValidationException(String.format("Error while importing Excel file %s", filePath), rootCauseMessage, rootCause);
		} catch (final CoreException e) {
			throw new IllegalStateException("Error while uploading source code with modules path ", e);
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
							.importExcel()
							.setProjectId(projectId)
							.setInputStreamId(filePath)
							.setInputStream(inputStream))
					.setInvalidResultConsumer(invalidResult -> {
						final String statusMessage = StringEscapeUtils.unescapeJava(invalidResult.getExtendedStatusMessage());
						Logging.error(String.format("Error while importing Excel file %s:%n%s", filePath, statusMessage));
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
	
	@Nullable
	private static IFile getFileByModulePath(final String path, final Optional<IProject> project) {
		IFile file = null;
		if (project.isPresent() && StringUtils.isNotBlank(path)) {
			file = project.get().getFile(path);
			if ( ! file.exists()) {
				Logging.error(String.format("The file with the path '%s' in the project '%s' is not available", path, project.get()));
				return null;
			}
		}
		return file;
	}
}

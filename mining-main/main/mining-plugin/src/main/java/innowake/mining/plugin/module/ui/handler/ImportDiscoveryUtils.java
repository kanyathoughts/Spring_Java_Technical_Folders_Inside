package innowake.mining.plugin.module.ui.handler;

import java.util.Optional;
import java.util.function.Function;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.base.ValidationException;
import innowake.mining.plugin.module.importer.DiscoveryCsvImporterServerside;
import innowake.mining.plugin.module.importer.DiscoveryExcelImporter;
import innowake.mining.plugin.preferences.MiningPreferences;
import innowake.mining.plugin.preferences.ProjectData;

/**
 * Utilities for importing Discovery files.
 */
final class ImportDiscoveryUtils {
	
	private ImportDiscoveryUtils() {
		throw new IllegalStateException("Can't instantiate this class");
	}
	
	/**
	 * Creates a {@link Job} to import the Discovery Excel workbook.
	 *
	 * @param filePath the file path of the Discovery Excel workbook
	 * @param project {@link Optional} object containing the {@link IProject} used to obtain the mining preferences
	 * @param projectData {@link Optional} object containing the {@link ProjectData}
	 * @param shell the parent shell of the dialog
	 * @return the {@code Job} that will be scheduled to import data
	 */
	static Job createExcelImportJob(
			final String filePath, 
			final Optional<IProject> project, 
			final Optional<ProjectData> projectData,
			final Shell shell) {

		return Job.create("Import Discovery Excel", (@Nullable final IProgressMonitor monitor) -> {
			final Tuple2<ConnectionInfo, Long> connectionInfoAndProjectId = getConnectionInfoAndProjectId(project, projectData);
			final Function<Long, Boolean> existingModulesHandler = value -> createExistingModulesHandler(shell, value);
			
			try {
				DiscoveryExcelImporter.doImport(filePath, connectionInfoAndProjectId.b, connectionInfoAndProjectId.a, existingModulesHandler,
						monitor, project, shell);

			} catch (final ValidationException e) {
				Display.getDefault().syncExec(() -> MessageDialog.openError(shell, e.getTitle(), e.getMessage()));
				Logging.error(e.getLocalizedMessage(), e);
				return;
			}
		});
	}
	
	/**
	 * Creates a {@link Job} to import the Discovery CSV file.
	 *
	 * @param filePath the file path of the Discovery CSV file
	 * @param project {@link Optional} object containing the {@link IProject} used to obtain the mining preferences
	 * @param projectData {@link Optional} object containing the {@link ProjectData}
	 * @param shell the parent shell of the dialog
	 * @return the {@code Job} that will be scheduled to import data
	 */
	static Job createCsvImportJob(
			final String filePath, 
			final Optional<IProject> project, 
			final Optional<ProjectData> projectData,
			final Shell shell) {
		
		return Job.create("Import Discovery CSV", (@Nullable final IProgressMonitor monitor) -> {
			final Tuple2<ConnectionInfo, Long> connectionInfoAndProjectId = getConnectionInfoAndProjectId(project, projectData);
			final Function<Long, Boolean> existingModulesHandler = value -> createExistingModulesHandler(shell, value);
			
			try {
				DiscoveryCsvImporterServerside.doImport(filePath, connectionInfoAndProjectId.b, connectionInfoAndProjectId.a, existingModulesHandler, 
						monitor);
			} catch (final ValidationException e) {
				Display.getDefault().syncExec(() -> MessageDialog.openError(shell, e.getTitle(), e.getMessage()));
				Logging.error(e.getLocalizedMessage(), e);
				return;
			}
		});
	}
	
	private static Tuple2<ConnectionInfo, Long> getConnectionInfoAndProjectId(final Optional<IProject> project, final Optional<ProjectData> projectData) {
		final ConnectionInfo connectionInfo;
		final Long projectId;
		
		if (projectData.isPresent()) {
			connectionInfo = MiningPreferences.getConnectionInfo()
						.orElseThrow(() -> new IllegalStateException("Connection information is not present"));
			projectId = projectData.get().getProjectId();
		} else if (project.isPresent()) {
			connectionInfo = MiningPreferences.getConnectionInfo(project.get()).orElseThrow(IllegalStateException::new);
			projectId = MiningPreferences.getApiProject(project.get()).orElseThrow(IllegalStateException::new).getProjectId();
		} else {
			throw new IllegalStateException("There was no mining project associated, please ensure that a mining project has been configured.");
		}
		
		return new Tuple2<>(connectionInfo, projectId);
	}
	
	private static Boolean createExistingModulesHandler(final Shell shell, final Long value) {
		return UIHandlerUtils.getUserResponseFunction(shell)
				.apply(String.format("There are already %d modules on the server for this project."
									+ "Do you want to overwrite the modules on the server?", value), "Overwrite?");
	}
}

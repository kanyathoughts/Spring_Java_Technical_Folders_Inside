package innowake.mining.plugin.module.ui.handler;

import java.util.Optional;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;
import innowake.base.eclipse.common.ui.util.SelectionUtil;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.base.ValidationException;
import innowake.mining.plugin.module.importer.DiscoveryImportFileExtensions;
import innowake.mining.plugin.preferences.ProjectValidator;

/**
 * The handler for import process of a Discovery.
 */
public class ImportDiscoveryHandler extends AbstractHandler {
	
	@Nullable
	@Override
	public Object execute(@Nullable final ExecutionEvent event) throws ExecutionException {
		final Shell shell = HandlerUtil.getActiveShell(event);
		final ISelection currentSelection = HandlerUtil.getCurrentSelection(event);
		
		final IProject project;
		final String filePath;
		try {
			project = ProjectValidator.validate(currentSelection);
			final IResource resource = SelectionUtil.getResource(currentSelection);
			filePath = resource.getLocation().toFile().toString();
		} catch (final ValidationException e) {
			Logging.error(e.getLocalizedMessage(), e);
			MessageDialog.openError(shell, e.getTitle(), e.getMessage());
			return null;
		}
		
		final Job job;
		if (filePath.endsWith(DiscoveryImportFileExtensions.CSV.getFileExtension())) {
			job = ImportDiscoveryUtils.createCsvImportJob(
					filePath, 
					Optional.of(project), 
					Optional.empty(),
					shell);
		} else {
			job = ImportDiscoveryUtils.createExcelImportJob(
					filePath, 
					Optional.of(project), 
					Optional.empty(),
					shell);
		}
		
		job.schedule();
		return null;
	}
}

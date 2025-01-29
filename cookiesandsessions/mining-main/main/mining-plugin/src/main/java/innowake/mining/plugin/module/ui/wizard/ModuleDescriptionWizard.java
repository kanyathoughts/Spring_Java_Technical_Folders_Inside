/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.ui.wizard;

import java.util.Optional;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.client.service.module.FindModuleByPath;
import innowake.mining.plugin.MiningPlugin;
import innowake.mining.plugin.base.ResourceUtil;
import innowake.mining.plugin.base.ui.SelectionUtil;
import innowake.mining.plugin.client.ApiClient;
import innowake.mining.shared.entities.ModulePojo;


/**
 * Wizard for maintaining the module description.
 */
public class ModuleDescriptionWizard extends Wizard implements INewWizard {
	
	private ModuleDescriptionWizardPage page;
	
	private Optional<IProject> project = Optional.empty();
	private Optional<IFile> file = Optional.empty();
	private Optional<ModulePojo> module = Optional.empty();
	 
	public ModuleDescriptionWizard() {
		this.page = new ModuleDescriptionWizardPage();
	}
	
	@Override
	public void init(@Nullable final IWorkbench workbench, @Nullable final IStructuredSelection resourceSelection) {
		setWindowTitle("Mining");
		setHelpAvailable(false);
		setForcePreviousAndNextButtons(false);

		project = resourceSelection != null ? SelectionUtil.getProject(resourceSelection) : Optional.empty();
		page.setProject(project);
		page.setConnected(ApiClient.checkConnection(project.orElse(null)));
		file = resourceSelection != null ? SelectionUtil.getFile(resourceSelection) : Optional.empty();
		page.setFile(file);
		module = MiningServiceExecutor.create(this::findModule).executeWithDefaultErrorHandling();
		page.setModule(module);
		
		addPage(page);
	}

	@Override
	public boolean performFinish() {
		return Assert.assertNotNull(page).performFinish();
	}

	private FindModuleByPath findModule() throws CoreException, StorageException {
		final IFile presentFile = file.orElseThrow(() -> new CoreException(new Status(IStatus.ERROR, MiningPlugin.ID,
				"Could not determine the file of the active editor.")));
		final IProject presentProject = project.orElseThrow(() -> new CoreException(new Status(IStatus.ERROR, MiningPlugin.ID,
				"No project present for " + presentFile.getFullPath().toPortableString())));
		return ApiClient.moduleService(presentProject)
				.findModuleByPath()
				.setPath(ResourceUtil.getProjectRelativePath(presentFile));
	}
	
}

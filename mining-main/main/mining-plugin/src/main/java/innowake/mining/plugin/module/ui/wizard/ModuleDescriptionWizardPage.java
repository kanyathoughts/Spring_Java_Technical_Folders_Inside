/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.ui.wizard;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.Optional;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.client.service.module.UpdateModule;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.MiningJobGroup;
import innowake.mining.plugin.MiningPlugin;
import innowake.mining.plugin.base.ui.ConnectedWizardPage;
import innowake.mining.plugin.client.ApiClient;
import innowake.mining.plugin.preferences.MiningPreferences;
import innowake.mining.plugin.preferences.ProjectData;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;


/**
 * Wizard page for maintaining the module description.
 */
public class ModuleDescriptionWizardPage extends ConnectedWizardPage {

	@Nullable private Label lblRemoteClient;
	@Nullable private Label lblRemoteProject;
	@Nullable private Label lblLocalResource;
	@Nullable private Text txtDescription;
	
	private boolean initializationError;
	
	private Optional<IFile> file = Optional.empty();
	private Optional<ModulePojo> module = Optional.empty();
	
	ModuleDescriptionWizardPage() {
		super("ModuleDescriptionWizardPage");
		setTitle("Add description to Module");
	}

	@SuppressWarnings("unused") /* unused labels for layout */
	@Override
	public void createControl(@Nullable final Composite parent) {
		final Composite container = new Composite(parent, SWT.NONE);
		setControl(container);
		
		final GridLayout gridLayout = new GridLayout(2, false);
		gridLayout.verticalSpacing = 10;
		gridLayout.horizontalSpacing = 20;
		gridLayout.marginLeft = 20;
		gridLayout.marginRight = 20;
		container.setLayout(gridLayout);
		
		final Label lblClient = new Label(container, SWT.NONE);
		lblClient.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
		lblClient.setText("Client:");
		
		lblRemoteClient = new Label(container, SWT.NONE);
		
		final Label lblProject = new Label(container, SWT.NONE);
		lblProject.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
		lblProject.setText("Project:");
		
		lblRemoteProject = new Label(container, SWT.NONE);
		
		final Label separator = new Label(container, SWT.SEPARATOR | SWT.HORIZONTAL);
		separator.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 2, 1));
		
		final Label lblDescription = new Label(container, SWT.NONE);
		lblDescription.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
		lblDescription.setText("Description:");
		
		final Text localText = new Text(container, SWT.MULTI | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL);
		localText.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 4));
		localText.addModifyListener(listener -> validatePage());
		localText.setFont(JFaceResources.getFont(JFaceResources.TEXT_FONT));
		txtDescription = localText;
		new Label(container, SWT.NONE);
		new Label(container, SWT.NONE);
		new Label(container, SWT.NONE);
		
		final Label secondSeparator = new Label(container, SWT.SEPARATOR | SWT.HORIZONTAL);
		secondSeparator.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 2, 1));
		
		final Label lblResource = new Label(container, SWT.NONE);
		lblResource.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
		lblResource.setText("Resource:");
		
		lblLocalResource = new Label(container, SWT.NONE);
	
		initControls();
		validatePageAfterCreation();
	}
	
	/**
	 * Set the module {@link IFile}.
	 *
	 * @param file The file instance.
	 */
	public void setFile(final Optional<IFile> file) {
		this.file = file;
	}

	/**
	 * Set the module instance. This class contain the description member which this wizard should edit.
	 *
	 * @param module The module instance.
	 */
	public void setModule(final Optional<ModulePojo> module) {
		this.module = module;
	}

	boolean performFinish() {
		if ( ! validatePage()) {
			return false;
		}

		final String moduleDescription = getModuleDescription();
		final Job job = Job.create("Updating Module Description", (@Nullable final IProgressMonitor monitor) ->
			MiningServiceExecutor
				.create(() -> updateDescription(moduleDescription))
				.setInvalidResultConsumer(r -> {
					Logging.error(r.getExtendedStatusMessage());
					Display.getDefault().asyncExec(() ->
						MessageDialog.openError(getShell(), "Server Error while updating module description.", r.getStatusMessage())
					);
				})
				.setExceptionConsumer(e -> {
					Logging.error("Error while updating module description.", e);
					Display.getDefault().asyncExec(() ->
						MessageDialog.openError(getShell(), "Error while updating module description.", e.getLocalizedMessage())
					);
				})
				.execute()
		);
		job.setSystem(false);
		job.setJobGroup(MiningJobGroup.INSTANCE);
		job.schedule();
		
		return true;
	}

	private void initControls() {
		if ( ! validateIsConnected()) {
			initializationError = true;
			return;
		}
		
		if ( ! project.isPresent() ||  ! file.isPresent()) {
			setErrorMessage("Close wizard and select a file.");
			initializationError = true;
			return;
		}
		if ( ! module.isPresent()) {
			setErrorMessage("Mining module not found for selected file. Please check if the server is running.");
			initializationError = true;
			return;
		}
		assertNotNull(lblLocalResource).setText(file.get().getFullPath().toString());
		final Optional<ProjectData> miningProject = MiningPreferences.getApiProject(project.get());
		if (miningProject.isPresent()) {
			assertNotNull(lblRemoteProject).setText(miningProject.get().getProjectName());
			assertNotNull(lblRemoteClient).setText(miningProject.get().getClientName());
		} else {
			setErrorMessage("Mining project in properties not set.");
			initializationError = true;
			return;
		}
	
		module.ifPresent(m -> {
			if (m.getDescription().isPresent()) {
				assertNotNull(txtDescription).setText(m.getDescription().get());
			}
		});
	}

	private String getModuleDescription() {
		return assertNotNull(txtDescription).getText();
	}
	
	private UpdateModule updateDescription(final String description) throws CoreException, StorageException {
		final ModulePojo module2 = module.get();
		final IProject presentProject = project.orElseThrow(() -> new CoreException(new Status(IStatus.ERROR, MiningPlugin.ID,
				"Cannot determine project from current selection.")));
		return ApiClient.moduleService(presentProject)
				.updateModule()
				.setProjectId(module2.getProject())
				.setModule(new ModulePojoPrototype()
								.withId(module2.identity())
								.setDescription(description));
	}

	private boolean validatePage() {
		if ( ! validateInitializationError()) {
			return false;
		}
	
		resetValidation();
		
		setPageComplete(true);
		return true;
	}

	private void validatePageAfterCreation() {
		if ( ! validateInitializationError()) {
			return;
		}
	
		resetValidation();
		setPageComplete(true);
	}

	private boolean validateInitializationError() {
		if (initializationError) {
			assertNotNull(txtDescription).setEnabled(false);
			setPageComplete(false);
		}
		return ! initializationError;
	}
	
	private void resetValidation() {
		assertNotNull(txtDescription).setEnabled(true);
		setMessage(null);
		setErrorMessage(null);
	}

}

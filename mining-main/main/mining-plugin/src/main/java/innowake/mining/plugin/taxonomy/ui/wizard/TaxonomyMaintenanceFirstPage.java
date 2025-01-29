/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.taxonomy.ui.wizard;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.plugin.client.ApiClient.taxonomyTypeService;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.plugin.base.ui.ConnectedWizardPage;
import innowake.mining.plugin.preferences.MiningPreferences;
import innowake.mining.plugin.preferences.ProjectData;
import innowake.mining.shared.entities.TaxonomyTypePojo;

/**
 * Wizard page to select the {@link TaxonomyTypePojo} in {@link TaxonomyMaintenanceWizard}.
 */
class TaxonomyMaintenanceFirstPage extends ConnectedWizardPage {

	@Nullable
	private Label lbRemoteProject;
	private final List<Button> taxonomyTypes = new ArrayList<>();

	private Optional<ProjectData> projectData = Optional.empty();
	private boolean initializationError;

	/**
	 * Create a new instance.
	 */
	TaxonomyMaintenanceFirstPage() {
		super("TaxonomyMaintenanceFirstPage");
		setTitle("Maintain Taxonomies");
	}
	
	@Override
	public void createControl(@Nullable final Composite parent) {
		final Composite container = new Composite(parent, SWT.NONE);
		setControl(container);
		
		final GridLayout containerGridLayout = new GridLayout(2, false);
		containerGridLayout.marginRight = 20;
		containerGridLayout.horizontalSpacing = 20;
		containerGridLayout.verticalSpacing = 20;
		containerGridLayout.marginWidth = 20;
		containerGridLayout.marginTop = 20;
		containerGridLayout.marginLeft = 20;
		container.setLayout(containerGridLayout);
		
		final Label lblRemoteProject = new Label(container, SWT.NONE);
		lblRemoteProject.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, false, 1, 1));
		lblRemoteProject.setText("Project:");
		lbRemoteProject = new Label(container, SWT.NONE);
		lbRemoteProject.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));

		new Label(container, SWT.SEPARATOR | SWT.HORIZONTAL | SWT.CENTER).setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1));

		final Label lblcbxType = new Label(container, SWT.NONE);
		lblcbxType.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, false, 1, 1));
		lblcbxType.setText("Type:");
		
		final Composite buttonContainer = new Composite(container, SWT.NONE);
	    buttonContainer.setLayout(new GridLayout());
	    buttonContainer.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
	    
		final Optional<TaxonomyTypePojo[]> maybeTaxonomyTypes = MiningServiceExecutor
				.create(() -> taxonomyTypeService(project.get()).findAllTaxonomyTypes())
				.executeWithDefaultErrorHandling();
		
		if (maybeTaxonomyTypes.isPresent()) {
			final TaxonomyTypePojo[] taxonomytypes = maybeTaxonomyTypes.get();
		    for (int i = 0; i < taxonomytypes.length; i++) {
		    	final TaxonomyTypePojo type = taxonomytypes[i];
		    	final Button button = new Button(buttonContainer, SWT.RADIO);
				button.setText(type.getName());
		    	button.addSelectionListener(new SelectionAdapter() {
		    		
		    		@Override
		    		public void widgetSelected(@Nullable final SelectionEvent event) {
		    			getNextPage().setTaxonomyType(type);
		    		}
		    		
		    	});
		    	taxonomyTypes.add(button);
		    	if (i == 0) {
		    		getNextPage().setTaxonomyType(type);
		    		button.setSelection(true);
		    	}
		    }
		}
	    
		initControls();
		getNextPage().setInitializationError(initializationError);
		validatePage();
	}

	@Override
	public boolean canFlipToNextPage() {
		return ! initializationError;
	}
	
	@Override
	public TaxonomyMaintenanceSecondPage getNextPage() {
		return (TaxonomyMaintenanceSecondPage) super.getNextPage();
	}
	
	private void initControls() {
		if ( ! validateIsConnected()) {
			initializationError = true;
			return;
		}
		
		if (project.isPresent()) {
			getNextPage().setProject(project);
		} else {
			setErrorMessage("Close wizard and select a file.");
			initializationError = true;
			return;
		}

		projectData = MiningPreferences.getApiProject(project.get());
		if (projectData.isPresent()) {
			assertNotNull(lbRemoteProject).setText(projectData.get().getProjectName());
			getNextPage().setProjectData(projectData.get());
		} else {
			setErrorMessage("Mining project in properties not set.");
			initializationError = true;
			return;
		}
	}

	private boolean validatePage() {
		taxonomyTypes.forEach(button -> button.setEnabled( ! initializationError));
		setPageComplete( ! initializationError);
		
		if (initializationError) {
			return false;
		}
		
		setMessage(null);
		setErrorMessage(null);
		return true;
	}
}

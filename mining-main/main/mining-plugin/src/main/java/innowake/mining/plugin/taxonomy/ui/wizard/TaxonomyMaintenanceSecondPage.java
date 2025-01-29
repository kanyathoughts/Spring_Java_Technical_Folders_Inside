/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.taxonomy.ui.wizard;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.plugin.client.ApiClient.taxonomyService;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.plugin.preferences.ProjectData;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojo;
import innowake.mining.shared.model.TaxonomyFieldName;
import innowake.mining.shared.model.aggregations.AggregationOperator;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;

/**
 * Wizard page to add, edit and delete {@linkplain TaxonomyPojo taxonomies}.
 */
class TaxonomyMaintenanceSecondPage extends WizardPage {

	@Nullable
	private Label lbRemoteProject;
	@Nullable
	private Text txtName;
	@Nullable
	private org.eclipse.swt.widgets.List listExisting;
	@Nullable
	private Label lbUsage;
	@Nullable
	private Button addButton;
	@Nullable
	private Button editButton;
	@Nullable
	private Button deleteButton;

	private boolean initializationError;

	private Optional<IProject> project = Optional.empty();
	@Nullable
	private ProjectData projectData;
	private long miningProjectId;
	
	@Nullable
	private TaxonomyTypePojo taxonomyType;
	private final Map<String, TaxonomyPojo> taxonomies = new HashMap<>();
	@Nullable
	private Map<Object, Object> taxonomyToModuleCount = null;
	
	/**
	 * Create a new instance.
	 */
	TaxonomyMaintenanceSecondPage() {
		super("TaxonomyMaintenanceSecondPage");
	}
	
	@Override
	public void createControl(@Nullable final Composite parent) {
		final Composite container = new Composite(parent, SWT.NONE);
		setControl(container);
		
		final GridLayout containerGridLayout = new GridLayout(3, false);
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
		lbRemoteProject.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 2, 1));

		new Label(container, SWT.SEPARATOR | SWT.HORIZONTAL | SWT.CENTER).setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 3, 1));

		final Composite labelContainer1 = new Composite(container, SWT.NONE);
		labelContainer1.setLayout(new GridLayout());
		labelContainer1.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, false, 1, 1));

		final Label lblName = new Label(labelContainer1, SWT.NONE);
		lblName.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, false, 1, 1));
		lblName.setText("Name:");
		final Text txtNameLocal = new Text(container, SWT.BORDER);
		txtNameLocal.addModifyListener(listener -> nameModified());
		final GridData txtNameGridData = new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1);
		txtNameGridData.widthHint = 400;
		txtNameLocal.setLayoutData(txtNameGridData);
		txtName = txtNameLocal; 
		
		final Composite buttonContainer1 = new Composite(container, SWT.NONE);
		buttonContainer1.setLayout(new GridLayout());
		buttonContainer1.setLayoutData(new GridData(SWT.FILL, SWT.TOP, false, false, 1, 1));
		
		final GridData buttonGridData = new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1);
		buttonGridData.widthHint = 100;
		
		final Button addButtonLocal = new Button(buttonContainer1, SWT.NONE);
		addButtonLocal.setText("Add");
		addButtonLocal.setLayoutData(buttonGridData);
		addButtonLocal.setEnabled(false);
		addButtonLocal.addSelectionListener(new SelectionAdapter() {
			
			@Override
			public void widgetSelected(@Nullable final SelectionEvent event) {
				addTaxonomy();
			}
		});
		addButton = addButtonLocal;
		
		final Label lblExisting = new Label(container, SWT.NONE);
		lblExisting.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, false, 1, 1));
		lblExisting.setText("Existing:");
		final org.eclipse.swt.widgets.List listExistingLocal = new org.eclipse.swt.widgets.List(container, SWT.BORDER | SWT.V_SCROLL);
		final GridData listExistingGridData = new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1);
		listExistingGridData.heightHint = 200;
		listExistingLocal.setLayoutData(listExistingGridData);
		listExistingLocal.addSelectionListener(new SelectionAdapter() {
			
			@Override
			public void widgetSelected(@Nullable final SelectionEvent event) {
				existingSelected();
			}
		});
		listExistingLocal.addMouseListener(new MouseAdapter() {
			
			@Override
			public void mouseDoubleClick(@Nullable final MouseEvent e) {
				editTaxonomy();
			}
		});
		listExisting = listExistingLocal; 
		
		final Composite buttonContainer2 = new Composite(container, SWT.NONE);
	    buttonContainer2.setLayout(new GridLayout());
	    buttonContainer2.setLayoutData(new GridData(SWT.FILL, SWT.TOP, false, false, 1, 1));
	    
	    final Button editButtonLocal = new Button(buttonContainer2, SWT.NONE);
	    editButtonLocal.setText("Edit");
	    editButtonLocal.setLayoutData(buttonGridData);
	    editButtonLocal.setEnabled(false);
	    editButtonLocal.addSelectionListener(new SelectionAdapter() {
	    	
	    	@Override
	    	public void widgetSelected(@Nullable final SelectionEvent event) {
	    		editTaxonomy();
	    	}
	    });
	    editButton = editButtonLocal;
	    
	    final Button deleteButtonLocal = new Button(buttonContainer2, SWT.NONE);
	    deleteButtonLocal.setText("Delete");
	    deleteButtonLocal.setLayoutData(buttonGridData);
	    deleteButtonLocal.setEnabled(false);
	    deleteButtonLocal.addSelectionListener(new SelectionAdapter() {
	    	
	    	@Override
	    	public void widgetSelected(@Nullable final SelectionEvent event) {
	    		deleteTaxonomy();
	    	}
	    });
	    deleteButton = deleteButtonLocal;
		
		final Label lblUsage = new Label(container, SWT.NONE);
		lblUsage.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, true, false, 1, 1));
		lblUsage.setText("Used in:");
		lbUsage = new Label(container, SWT.NONE);
		lbUsage.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 2, 1));

		initControls();
		validatePage();
	}

	@Override
	public boolean canFlipToNextPage() {
		return false;
	}
	
	@Override
	public void setVisible(final boolean visible) {
		super.setVisible(visible);
		setTitle("Maintain " + assertNotNull(taxonomyType).getName());
		reloadExisting();
		assertNotNull(editButton).setEnabled(false);
		assertNotNull(deleteButton).setEnabled(false);
	}
	
	void setProject(final Optional<IProject> project) {
		this.project = project;
	}
	
	void setProjectData(final ProjectData projectData) {
		this.projectData = projectData;
	}

	void setTaxonomyType(final TaxonomyTypePojo taxonomyType) {
		this.taxonomyType = taxonomyType;
	}
	
	void setInitializationError(boolean initializationError) {
		this.initializationError = initializationError;
	}
	
	private void initControls() {
		if (initializationError) {
			return;
		}
		
		assertNotNull(lbRemoteProject).setText(assertNotNull(projectData).getProjectName());
		miningProjectId = assertNotNull(projectData).getProjectId().longValue();
	}
	
	private boolean validatePage() {
		assertNotNull(txtName).setEnabled( ! initializationError);
		setPageComplete( ! initializationError);
		
		if (initializationError) {
			return false;
		}
		
		setMessage(null);
		setErrorMessage(null);
		return true;
	}
	
	private void nameModified() {
		final boolean enabled =  ! assertNotNull(txtName).getText().trim().isEmpty();
		assertNotNull(addButton).setEnabled(enabled);
	}

	private void existingSelected() {
		assertNotNull(editButton).setEnabled(true);
		assertNotNull(deleteButton).setEnabled(true);
		
		final int selectionIndex = assertNotNull(listExisting).getSelectionIndex();
		if (selectionIndex != -1) {
			final String taxonomy = assertNotNull(listExisting).getItem(selectionIndex);
			final EntityId taxId = taxonomies.get(taxonomy).identity();
			if (taxonomyToModuleCount == null) {
				taxonomyToModuleCount = fetchTaxonomyToModuleCount();
			}
			
			final long number = taxonomyToModuleCount != null ? (Integer) taxonomyToModuleCount.getOrDefault((Integer)taxId.getNid().intValue(), 0) : -1;
			assertNotNull(lbUsage).setText(number + (number == 1 ? " Module" : " Modules"));
		}
	}

	@Nullable
	private Map<Object, Object> fetchTaxonomyToModuleCount() {
		final AggregationRequest<TaxonomyFieldName> request = new AggregationRequest<>();
		final Map<TaxonomyFieldName, AggregationOperator> map = new HashMap<>();
		map.put(TaxonomyFieldName.MODULE_ID, AggregationOperator.COUNT);
		request.setFields(map);
		final Set<TaxonomyFieldName> set = new HashSet<>();
		set.add(TaxonomyFieldName.ID);
		request.setGroupBy(set);
		final Optional<List<AggregationResult<TaxonomyFieldName>>> response = MiningServiceExecutor
				.create(() -> taxonomyService(project.get())
								.getTaxonomyAggregations()
								.setRequest(request))
				.executeWithDefaultErrorHandling();
		
		if (response.isPresent()) {
			return response.get().stream()
					.collect(Collectors.toMap(ar -> ar.getGroup().get(TaxonomyFieldName.ID), 
							ar -> ar.getFields().get(TaxonomyFieldName.MODULE_ID)));
		} else {
			setErrorMessage("Could not fetch taxonomy assignments for project: " + project.get().getName());
			return null;
		}
	}

	private void addTaxonomy() {
		setMessage(null);
		setErrorMessage(null);
		
		final TaxonomyPojoPrototype taxonomy = new TaxonomyPojoPrototype()
				.setProject(EntityId.of(miningProjectId))
				.setName(assertNotNull(txtName).getText().trim())
				.setType(assertNotNull(taxonomyType).getId());
		
		MiningServiceExecutor
				.create(() -> taxonomyService(project.get()).createTaxonomy().setTaxonomy(taxonomy))
				.setAdditionalErrorHandler(() -> setErrorMessage("Taxonomy cannot be created."))
				.executeWithDefaultErrorHandling();
		
		assertNotNull(editButton).setEnabled(false);
		assertNotNull(deleteButton).setEnabled(false);
		assertNotNull(txtName).setText("");
		reloadExisting();
	}
	
	private void editTaxonomy() {
		setMessage(null);
		setErrorMessage(null);
		
		final int selectionIndex = assertNotNull(listExisting).getSelectionIndex();
		if (selectionIndex != -1) {
			final String item = assertNotNull(listExisting).getItem(selectionIndex);

			final InputDialog dialog = new InputDialog(getShell(), 				
				"Edit " + assertNotNull(taxonomyType).getName(), 
				"Insert new name:", 
				item, 
				newText -> assertNotNull(newText).trim().isEmpty() ? "New name is invalid." : null );
			
			if (dialog.open() == Window.OK) {
				final TaxonomyPojoPrototype taxonomy = new TaxonomyPojoPrototype();
				taxonomy.setProject(EntityId.of(miningProjectId));
				final String taxonomyName = dialog.getValue().trim();
				taxonomy.setName(taxonomyName);
				final EntityId taxonomyId = taxonomies.get(item).identity();
				taxonomy.withId(taxonomyId);
				taxonomy.setType(assertNotNull(taxonomyType).getId());
				
				MiningServiceExecutor
					.create(() -> taxonomyService(project.get()).updateTaxonomy().setId(taxonomyId).setTaxonomy(taxonomy))
					.setAdditionalErrorHandler(() -> setErrorMessage("Taxonomies cannot be updated."))
					.executeWithDefaultErrorHandling();
				
				assertNotNull(editButton).setEnabled(false);
				assertNotNull(deleteButton).setEnabled(false);
				reloadExisting();
			}
		}
	}
	
	private void deleteTaxonomy() {
		setMessage(null);
		setErrorMessage(null);
		
		final int selectionIndex = assertNotNull(listExisting).getSelectionIndex();
		if (selectionIndex != -1) {
			final String item = assertNotNull(listExisting).getItem(selectionIndex);
			
			MiningServiceExecutor
				.create(() -> taxonomyService(project.get()).deleteTaxonomy().setId(taxonomies.get(item).identity()))
				.setAdditionalErrorHandler(() -> setErrorMessage("Taxonomies cannot be deleted."))
				.executeWithDefaultErrorHandling();
			
			assertNotNull(editButton).setEnabled(false);
			assertNotNull(deleteButton).setEnabled(false);
			reloadExisting();
		}
	}
	
	private void reloadExisting() {
		assertNotNull(listExisting).removeAll();
		assertNotNull(lbUsage).setText("");
		taxonomies.clear();

		final Optional<TaxonomyPojo[]> allTaxonomies = MiningServiceExecutor
				.create(() -> taxonomyService(project.get()).findAllTaxonomies().setTaxonomyType(assertNotNull(taxonomyType).getName()))
				.setAdditionalErrorHandler(() -> setErrorMessage("Taxonomies cannot be read."))
				.executeWithDefaultErrorHandling();

		if (allTaxonomies.isPresent()) {
			for (final TaxonomyPojo taxonomy : allTaxonomies.get()) {
				taxonomies.put(taxonomy.getName(), taxonomy);
				assertNotNull(listExisting).add(taxonomy.getName());
			}
		}
	}
}

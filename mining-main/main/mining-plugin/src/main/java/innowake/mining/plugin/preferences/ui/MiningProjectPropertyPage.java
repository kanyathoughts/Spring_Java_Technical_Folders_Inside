package innowake.mining.plugin.preferences.ui;

import java.awt.Desktop;
import java.io.IOException;
import java.net.URI;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import innowake.lib.core.api.lang.NonNullByDefault;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.plugin.Features;
import innowake.mining.plugin.base.JobUtil;
import innowake.mining.plugin.client.ui.ConnectionTester;
import innowake.mining.plugin.preferences.MiningPreferences;
import innowake.mining.plugin.preferences.ProjectData;
import innowake.mining.plugin.preferences.ProjectDataUtils;
import innowake.mining.plugin.preferences.PropertyUtils;
import innowake.mining.plugin.preferences.SecureScopedPreferenceStore;

/**
 * Property page to define project local mining properties.
 */
@NonNullByDefault(value = false) /* Eclipse lifecycle guarantees UI components are initialized */
public class MiningProjectPropertyPage extends SecurePropertyPage implements FocusListener {

	private Button cbxProjectSpecificSettings;
	private Text txtApiServerUrl;
	private Text txtAccessToken;
	private ComboViewer cbxProjects;
	private String initialApiServerUrl;
	
	private ConnectionInfo connectionInfo;

	@Override
	public boolean performOk() {
		checkVersion(getProject());
		if ( ! isValid()) {
			return false;
		}
		save();
		MiningPreferences.resetConnectionInfo(getProject());
		refreshFeatureCache();
		return true;
	}
	
	@Override
	protected void performApply() {
		super.performApply();
		connectionInfo = null;
		MiningPreferences.resetConnectionInfo(getProject());
	}
	
	private void refreshFeatureCache() {
		MiningPreferences.getConnectionInfo(getProject()).ifPresent(Features.INSTANCE::refreshCache); 
	}

	@Override
	public void focusGained(@Nullable final FocusEvent e) {
		/* No action */
	}

	@Override
	public void focusLost(@Nullable final FocusEvent e) {
		txtApiServerUrl.setText(txtApiServerUrl.getText().trim());
		txtAccessToken.setText(txtAccessToken.getText().trim());
		updateApplyButton();
	}

	@Override
	public boolean isValid() {
		final boolean projectSpecific = cbxProjectSpecificSettings.getSelection();
		if (projectSpecific && getApiServerUrl().isEmpty()) {
			setErrorMessage("Enter the mining API server URL.");
			return false;
		}
		
		return true;
	}
	
	@SuppressWarnings("unused")
	@Override
	protected Control createContents(@Nullable final Composite parent) {
		final Composite composite = new Composite(parent, SWT.NONE);
		final GridLayout layout = new GridLayout(2, false);
		composite.setLayout(layout);
		final GridData data = new GridData(GridData.FILL);
		data.grabExcessHorizontalSpace = true;
		composite.setLayoutData(data);

		cbxProjectSpecificSettings = new Button(composite, SWT.CHECK);
		cbxProjectSpecificSettings.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 2, 1));
		cbxProjectSpecificSettings.setText("Enable project specific settings");
		cbxProjectSpecificSettings.addListener(SWT.Selection, listener -> setProjectSpecificSettings(cbxProjectSpecificSettings.getSelection()));
		
		final Label lblServerUrl = new Label(composite, SWT.NONE);
		lblServerUrl.setText("API Server URL:");
		
		txtApiServerUrl = new Text(composite, SWT.BORDER);
		txtApiServerUrl.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		txtApiServerUrl.addFocusListener(this);
		txtApiServerUrl.addModifyListener(listener -> resetConnection());
		
		final Label lblAccessToken = new Label(composite, SWT.NONE);
		lblAccessToken.setText("Access token:");
		
		txtAccessToken = new Text(composite, SWT.BORDER | SWT.PASSWORD);
		txtAccessToken.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		txtAccessToken.setTextLimit(ConnectionInfo.OFFLINE_TOKEN_LENGTH);
		txtAccessToken.addFocusListener(this);
		txtAccessToken.addModifyListener(listener -> resetConnection());
		
		new Label(composite, SWT.NONE);
		
		final Composite compCmd = new Composite(composite, SWT.None);
		compCmd.setLayout(new RowLayout());
		compCmd.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
		
		final Button btnManageTokens = new Button(compCmd, SWT.NONE);
		btnManageTokens.addListener(SWT.Selection, listener -> {
			resetMessage();
			try {
				Desktop.getDesktop().browse(URI.create(getApiServerUrl() + "#/tokens"));
			} catch (Exception e) {
				setMessage(e.getMessage(), ERROR);
			}
		});
		btnManageTokens.setText("Manage Tokens");
		
		final Button btnTestConnection = new Button(compCmd, SWT.NONE);
		btnTestConnection.addListener(SWT.Selection, listener -> testConnection());
		btnTestConnection.setText("Test API Connection");
		
		new Label(composite, SWT.SEPARATOR | SWT.HORIZONTAL).setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1));
		
		final Composite compPrj = new Composite(composite, SWT.NONE);
		compPrj.setLayout(new GridLayout(3, false));
		compPrj.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1));
		
		final Label lblProject = new Label(compPrj, SWT.NONE);
		lblProject.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 1, 1));
		lblProject.setText("Project:");
		
		cbxProjects = new ComboViewer(compPrj, SWT.READ_ONLY);
		cbxProjects.getCombo().setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		cbxProjects.setContentProvider(ArrayContentProvider.getInstance());
		cbxProjects.setLabelProvider(new LabelProvider() {
			@Override
			public String getText(@Nullable final Object element) {
				if (element instanceof ProjectData) {
					final ProjectData client = (ProjectData) element;
					return client.getViewName();
				} else {
					return super.getText(element);
				}
			}
		});
		cbxProjects.getCombo().addFocusListener(new FocusAdapter() {
			@Override
			public void focusGained(@Nullable final FocusEvent e) {
				initProjects();
			}
		});
		
		final Button btnLoad = new Button(compPrj, SWT.NONE);
		btnLoad.addListener(SWT.Selection, listener -> initProjects());
		btnLoad.setText("Load");

		load();

		return composite;
	}

	@Override
	protected void performDefaults() {
		super.performDefaults();
		if (cbxProjectSpecificSettings.getSelection()) {
			txtApiServerUrl.setText(MiningPreferences.DEFAULT_API_SERVER);
			txtAccessToken.setText(MiningPreferences.DEFAULT_ACCESS_TOKEN);
		} else {
			try {
				loadWorkbenchDefaults();
			} catch (final StorageException e) {
				setErrorMessage("Error while loading the properties: " + e.getMessage());
				throw new IllegalStateException(e);
			}
		}
		cbxProjects.getCombo().deselectAll();
		cbxProjects.setInput(null);
		updateApplyButton();
	}
	
	private void setProjectSpecificSettings(final boolean enabled) {
		resetMessage();
		
		try {
			if (enabled) {
				loadProjectDefaults();
			} else {
				loadWorkbenchDefaults();
			}
		} catch (final StorageException e) {
			setErrorMessage("Error while loading the properties: " + e.getMessage());
			throw new IllegalStateException(e);
		}

		txtApiServerUrl.setEnabled(enabled);
		txtAccessToken.setEnabled(enabled);
	}
	
	private void load() {
		try {
			moveToProperty(MiningPreferences.KEY_QN_API_SERVER);
			moveToSecureProperty(MiningPreferences.KEY_QN_ACCESS_TOKEN);
			moveToProperty(MiningPreferences.KEY_QN_PROJECT_SETTINGS);
			moveToProperty(MiningPreferences.KEY_QN_PROJECT);
			
			final boolean projectSpecific = Boolean.parseBoolean(getProperty(MiningPreferences.KEY_PROJECT_SETTINGS));
			cbxProjectSpecificSettings.setSelection(projectSpecific);
			setProjectSpecificSettings(projectSpecific);
			
			final Optional<ProjectData> project = MiningPreferences.getApiProject(getProject());
			if (project.isPresent()) {
				cbxProjects.setInput(new ProjectData[] { project.get() });
				cbxProjects.setSelection(new StructuredSelection(project.get()));
			}
		} catch (final CoreException | StorageException e) {
			setErrorMessage("Error while saving properties: " + e.getMessage());
			throw new IllegalStateException(e);
		}
	}
	
	private void loadProjectDefaults() throws StorageException {
		final String apiServerUrl = getProperty(MiningPreferences.KEY_API_SERVER);
		txtApiServerUrl.setText(apiServerUrl);
		initialApiServerUrl = apiServerUrl;
		final String accessToken = getSecureProperty(MiningPreferences.KEY_ACCESS_TOKEN);
		if (accessToken.length() <= ConnectionInfo.OFFLINE_TOKEN_LENGTH) {
			txtAccessToken.setText(accessToken);
		}
	}
	
	private void loadWorkbenchDefaults() throws StorageException {
		final SecureScopedPreferenceStore store = MiningPreferences.getWorkbenchStore();
		final String apiServerUrl = store.getString(MiningPreferences.KEY_API_SERVER);
		txtApiServerUrl.setText(apiServerUrl);
		initialApiServerUrl = apiServerUrl;
		final String accessToken = store.getSecureString(MiningPreferences.KEY_ACCESS_TOKEN);
		if (accessToken.length() <= ConnectionInfo.OFFLINE_TOKEN_LENGTH) {
			txtAccessToken.setText(accessToken);
		}
	}
	
	@Override
	protected SecureScopedPreferenceStore getSecurestore() {
		return MiningPreferences.getProjectStore(getProject());
	}
	
	private IProject getProject() {
		return getResource().getProject();
	}
	
	private void save() {
		try {
			if (isValid()) {
				/* Show confirmation dialog if the API-Server URL has been changed and there are still managed jobs for the previous one. */
				if ( ! MiningWorkbenchPreferencePage.saveWhenUrlChangedAndHasManagedJobs(initialApiServerUrl, getApiServerUrl())) {
					/* Don't save if the user did not confirm. */
					return;
				}
				
				final boolean projectSpecific = cbxProjectSpecificSettings.getSelection();
				setProperty(MiningPreferences.KEY_PROJECT_SETTINGS, Boolean.toString(projectSpecific));
				if (projectSpecific) {
					setProperty(MiningPreferences.KEY_API_SERVER, getApiServerUrl());
					setSecureProperty(MiningPreferences.KEY_ACCESS_TOKEN, txtAccessToken.getText().trim());
				} else {
					setProperty(MiningPreferences.KEY_API_SERVER, MiningPreferences.DEFAULT_API_SERVER);
					setSecureProperty(MiningPreferences.KEY_ACCESS_TOKEN, MiningPreferences.DEFAULT_ACCESS_TOKEN);
				}
				setProperty(MiningPreferences.KEY_PROJECT, PropertyUtils.toString((ProjectData) ((StructuredSelection) cbxProjects.getSelection()).getFirstElement()));
				getSecurestore().save();
				
				/* In case the URL has been changed, this removes all now orphaned jobs from the workspace. */
				JobUtil.refreshManagedRemoteJobs();
			}
		} catch (final StorageException | IOException e) {
			setErrorMessage("Error while saving properties:" + e.getMessage());
			throw new IllegalStateException(e);
		}
	}
	
	private ConnectionInfo getConnectionInfo() {
		if (this.connectionInfo == null) {
			this.connectionInfo = new ConnectionInfo(getApiServerUrl(), txtAccessToken.getText().trim());
		}
		return this.connectionInfo;
	}
	
	private void resetConnection() {
		resetMessage();
		connectionInfo = null;
	}
	
	private void testConnection() {
		resetMessage();
		ConnectionTester.test(getConnectionInfo(), getShell());
		checkVersion(getProject());
	}
	
	private void initProjects() {
		final Object oldSelection = ((StructuredSelection) cbxProjects.getSelection()).getFirstElement();
		cbxProjects.setInput(null);
		final List<ProjectData> projectViews = ProjectDataUtils.getProjectData(getConnectionInfo(), this);
		Collections.sort(projectViews, Comparator.comparing(ProjectData::getClientName).thenComparing(ProjectData::getProjectName));
		if ( ! projectViews.isEmpty()) {
			cbxProjects.setInput(projectViews);
			if (oldSelection != null) {
				cbxProjects.setSelection(new StructuredSelection(oldSelection));
			}
		}
	}

	@Override
	protected String getApiServerUrl() {
		return txtApiServerUrl.getText().trim();
	}
}
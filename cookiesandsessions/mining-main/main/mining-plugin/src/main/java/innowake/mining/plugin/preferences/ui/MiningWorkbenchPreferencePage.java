/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.preferences.ui;

import java.awt.Desktop;
import java.net.URI;

import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
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
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import innowake.base.eclipse.common.ui.util.WorkbenchUtil;
import innowake.lib.core.api.lang.NonNullByDefault;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.plugin.Features;
import innowake.mining.plugin.base.JobUtil;
import innowake.mining.plugin.client.ui.ConnectionTester;
import innowake.mining.plugin.preferences.MiningPreferences;
import innowake.mining.plugin.preferences.SecureScopedPreferenceStore;
import innowake.mining.shared.model.FeatureId;

/**
 * Workbench preference page to set Mining backend server/port and access token.
 * Could be overridden by project preferences.
 */
@NonNullByDefault(value = false) /* Eclipse lifecycle guarantees UI components are initialized */
public class MiningWorkbenchPreferencePage extends SecurePropertyPage implements IWorkbenchPreferencePage, FocusListener {

	private static final String MANAGED_JOBS_TITLE = "Change API-Server URL";
	private static final String MANAGED_JOBS_MESSAGE = "There is at least one managed remote job for the previous API-Server URL '%s'." + System.lineSeparator()
			+ "All local job information bound to the previous URL will be lost from this workspace when changing the URL." + System.lineSeparator()
			+ "Do you still want to change the URL?";

	private Text txtApiServerUrl;
	private Text txtAccessToken;
	private Label lblDeep;
	private String initialApiServerUrl;
	
	private ConnectionInfo connectionInfo;

	@Override
	public boolean performOk() {
		checkVersion();

		/* Show confirmation dialog if the API-Server URL has been changed and there are still managed jobs for the previous one. */
		if ( ! saveWhenUrlChangedAndHasManagedJobs(initialApiServerUrl, getApiServerUrl())) {
			/* Don't save if the user did not confirm. */
			return false;
		}
		
		if (super.performOk()) {
			if ( ! isValid()) {
				return false;
			}
			save();
			MiningPreferences.resetConnectionInfo(null);
			refreshFeatureCache();
			/* In case the URL has been changed, this removes all now orphaned jobs from the workspace. */
			JobUtil.refreshManagedRemoteJobs();
			return true;
		}
		
		return false;
	}
	
	@Override
	protected void performApply() {
		super.performApply();
		connectionInfo = null;
		MiningPreferences.resetConnectionInfo(null);
	}

	private void refreshFeatureCache() {
		MiningPreferences.getConnectionInfo().ifPresent(Features.INSTANCE::refreshCache);
	}

	@Override
	public void init(final IWorkbench workbench) {
		setPreferenceStore(MiningPreferences.getWorkbenchStore());
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
		if (getApiServerUrl().isEmpty()) {
			setErrorMessage("Enter the mining API server URL.");
			return false;
		}
		return true;
	}
	
	/**
	 * Shows a confirmation dialog if the API-server URL has been changed and if there is at least one managed remote job
	 * connected to the previous URL.
	 * 
	 * @param initialApiServerUrl the original URL
	 * @param currentApiServerUrl the current URL
	 * @return {@code true} if URL has changed and there is at least one managed job for the previous one; {@code false} otherwise
	 */
	public static boolean saveWhenUrlChangedAndHasManagedJobs(final String initialApiServerUrl, final String currentApiServerUrl) {
			/* Don't save if the user did not confirm. */
			return !(! currentApiServerUrl.equals(initialApiServerUrl) && JobUtil.hasManagedJobsForUrl(initialApiServerUrl)
					&& ! MessageDialog.openConfirm(WorkbenchUtil.getActiveShell(), MANAGED_JOBS_TITLE, String.format(MANAGED_JOBS_MESSAGE, initialApiServerUrl)));
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

		final Label lblServerUrl = new Label(composite, SWT.LEFT);
		lblServerUrl.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 1, 1));
		lblServerUrl.setText("API Server URL:");

		txtApiServerUrl = new Text(composite, SWT.BORDER);
		txtApiServerUrl.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		txtApiServerUrl.addModifyListener(listener -> resetConnection());
		txtApiServerUrl.addFocusListener(this);
		initialApiServerUrl = getApiServerUrl();

		final Label lblAccessToken = new Label(composite, SWT.NONE);
		lblAccessToken.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 1, 1));
		lblAccessToken.setText("Access token:");

		txtAccessToken = new Text(composite, SWT.BORDER | SWT.PASSWORD);
		txtAccessToken.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		txtAccessToken.setTextLimit(ConnectionInfo.OFFLINE_TOKEN_LENGTH);
		txtAccessToken.addFocusListener(this);
		txtAccessToken.addModifyListener(listener -> resetConnection());
		
		resetMessage();
		loadWorkbenchDefaults();
		
		new Label(composite, SWT.NONE);

		composite.layout();

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
		
		final Button btnTestApiConnection = new Button(compCmd, SWT.NONE);
		btnTestApiConnection.addListener(SWT.Selection, listener -> testConnection());
		btnTestApiConnection.setText("Test API Connection");
		new Label(composite, SWT.NONE);

		final boolean deepLinksIsActive = Features.INSTANCE.isEnabled(FeatureId.ECLIPSE_DEEP_LINK);
		if (deepLinksIsActive) {
			lblDeep = new Label(composite, SWT.NONE);
			lblDeep.setLayoutData(new GridData(SWT.LEFT, SWT.LEFT, false, false, 1, 1));
			doLoad();
		}
		
		checkVersion();

		return composite;
	}

	protected void doLoad() {
		final int port = getPreferenceStore().getInt(MiningPreferences.KEY_DEEP_LINKS_PORT);
		setPort(port);
	}

	protected void doLoadDefault() {
		final int port = getPreferenceStore().getDefaultInt(MiningPreferences.KEY_DEEP_LINKS_PORT);
		setPort(port);
	}

	@Override
	protected void performDefaults() {
		super.performDefaults();
		loadWorkbenchDefaults();
		doLoadDefault();
		updateApplyButton();
	}

	@Override
	protected SecureScopedPreferenceStore getSecurestore() {
		return MiningPreferences.getWorkbenchStore();
	}

	private void setPort(final int port) {
		if (lblDeep == null) {
			return;
		}
		if (port > 0) {
			lblDeep.setText("The Deep Links server (\"Open in Eclipse\") is listening on port " + port);
		} else {
			lblDeep.setText("The Deep Links server is not running.");
		}
	}

	private void loadWorkbenchDefaults() {
		final SecureScopedPreferenceStore store = MiningPreferences.getWorkbenchStore();
		txtApiServerUrl.setText(store.getString(MiningPreferences.KEY_API_SERVER));
		String accessToken;
		try {
			accessToken = getSecureProperty(MiningPreferences.KEY_ACCESS_TOKEN);
		} catch (final StorageException e) {
			throw new IllegalStateException(e);
		}
		if (accessToken.length() <= ConnectionInfo.OFFLINE_TOKEN_LENGTH) {
			txtAccessToken.setText(accessToken);
		}
	}

	private void save() {
		if (isValid()) {
			setProperty(MiningPreferences.KEY_API_SERVER, getApiServerUrl());
			try {
				setSecureProperty(MiningPreferences.KEY_ACCESS_TOKEN, txtAccessToken.getText());
			} catch (StorageException e) {
				throw new IllegalStateException(e);
			}	
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
		checkVersion();
	}

	@Override
	protected String getApiServerUrl() {
		return txtApiServerUrl.getText().trim();
	}

}

/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.preferences.ui;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.dialogs.PropertyPage;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.plugin.client.VersionChecker;
import innowake.mining.plugin.client.VersionChecker.VersionResult;
import innowake.mining.plugin.client.VersionResultHandler;
import innowake.mining.plugin.preferences.SecureScopedPreferenceStore;

/**
 * Extension of PropertyPage. Adds functionality to manage secure properties.
 */
public abstract class SecurePropertyPage extends PropertyPage {
	protected IResource getResource() {
		return getElement().getAdapter(IResource.class);
	}
	
	protected abstract SecureScopedPreferenceStore getSecurestore();
	
	protected void setSecureProperty(final String key, final String value) throws StorageException {
		getSecurestore().setSecureValue(key, value);
	}
	
	protected String getSecureProperty(final String key) throws StorageException {
		return getSecurestore().getSecureString(key);
	}
	
	protected void moveToProperty(final QualifiedName key) throws CoreException {
		final String value = getResource().getPersistentProperty(key);
		if (value != null) {
			setProperty(key.getLocalName(), value);
			getResource().setPersistentProperty(key, null);
		}
	}
	
	protected void moveToSecureProperty(final QualifiedName key) throws StorageException, CoreException {
		moveToProperty(key);
		final String value = getProperty(key.getLocalName());
		if ( ! IPreferenceStore.STRING_DEFAULT_DEFAULT.equals(value)) {
			setSecureProperty(key.getLocalName(), value);
			getResource().setPersistentProperty(key, null);
		}
	}
	
	protected void setProperty(final String key, @Nullable final String value) {
		getSecurestore().setValue(key, value);
	}
	
	protected String getProperty(final String key) {
		return getSecurestore().getString(key);
	}

	/**
	 * Resets the message area of the page.
	 */
	protected void resetMessage() {
		setMessage(null);
		setErrorMessage(null);
		updateApplyButton();
	}

	/**
	 * Checks the version of the server connected to the workspace.
	 */
	protected void checkVersion() {
		checkVersion(null);
	}

	/**
	 * Checks the version of the server connected to the given project.
	 * 
	 * @param project the project for which the connected server version is determined
	 */
	protected void checkVersion(@Nullable final IProject project) {
		/* Directly use the server URL provided in case an initial connect is done.
		 * In that case there is no connection info available but we still want to be able to do a version check in those cases. */
		final String serverUrl = getApiServerUrl();
		if (serverUrl.isEmpty()) {
			return;
		}
		final VersionResult result = VersionChecker.checkVersion(project, new ConnectionInfo(serverUrl, ""), /* force check */ true);
		final String message = VersionResultHandler.handle(project, serverUrl, result);
		
		if ( ! result.match) {
			setErrorMessage(message);
		} else {
			resetMessage();
		}
	}

	/**
	 * Returns the URL of the API server.
	 * <p>
	 * The value must not be {@code null}.
	 *
	 * @return the URL of the API server
	 */
	protected abstract String getApiServerUrl();
}

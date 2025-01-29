/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.base.ui;

import java.util.Optional;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.wizard.WizardPage;

import innowake.mining.plugin.preferences.MiningPreferences;

/**
 * Super class for wizard pages which need a connection.
 */
public abstract class ConnectedWizardPage extends WizardPage {

	private boolean isConnected;
	protected Optional<IProject> project = Optional.empty();
	
	/**
	 * Constructor for WizardPages requiring a connection.
	 * 
	 * @param pageName title of the page.
	 */
	protected ConnectedWizardPage(final String pageName) {
		super(pageName);
	}
	
	/**
	 * Marks this page as connected.
	 *
	 * @param isConnected boolean value, true iff connected.
	 */
	public void setConnected(final boolean isConnected) {
		this.isConnected = isConnected;
	}
	
	/**
	 * Sets the project of this page.
	 *
	 * @param project to be set.
	 */
	public void setProject(final Optional<IProject> project) {
		this.project = project;
	}
	
	/**
	 * Checks whether a connection could be established. Sets the error message of the page if not.
	 *
	 * @return true iff connection could be established.
	 */
	protected boolean validateIsConnected() {
		if ( ! isConnected) {
			final Optional<String> serverUrl = project.isPresent() ? MiningPreferences.getApiServerUrl(project.get()) : MiningPreferences.getApiServerUrl();
			if (serverUrl.isPresent()) {
				setErrorMessage("No connection to API-server: " + serverUrl.get());
			} else {
				setErrorMessage("Server URL in properties not set.");
			}
			return false;
		}
		return true;
	}
	
}

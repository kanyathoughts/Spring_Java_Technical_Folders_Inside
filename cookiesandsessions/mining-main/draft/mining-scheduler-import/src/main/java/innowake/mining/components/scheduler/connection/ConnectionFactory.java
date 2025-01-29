/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.components.scheduler.connection;

import java.util.Optional;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.equinox.security.storage.StorageException;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.plugin.preferences.MiningPreferences;

/**
 * Factory method for mining server connections
 */
public final class ConnectionFactory {

	private static final String KEY_API_SERVER = "WorkbenchServer";
	private static final String KEY_ACCESS_TOKEN = "WorkbenchAccessToken";
	
	private ConnectionFactory() {}
	
	/**
	 * Get the connection info stored in the selected project or in the workbench.
	 * @param project The eclipse project
	 * @return The connection info or {@link Optional#empty()} if not available.
	 * @throws StorageException If the eclipse storage access failed.
	 */
	public static final Optional<ConnectionInfo> connectionFor(final IProject project) throws StorageException {
		if ( hasProjectSettings(project) ) {
			final SecureScopedPreferenceStore store = getProjectStore(project);
			final String server = store.getString(KEY_API_SERVER);
			final String accessToken = store.getSecureString(KEY_ACCESS_TOKEN);
			return Optional.of(new ConnectionInfo(server, accessToken));
		} else {
			return getConnectionInfo();
		}
	}
	
	/**
	 * Get the mining project data stored in the mining project.
	 * Requires a eclipse project mining setup - otherwise the optional is empty.
	 *
	 * @param project The eclipse project reference.
	 * @return The project data or {@link Optional#empty()}
	 */
	public static Optional<ProjectData> getApiProject(final IProject project) {
		Optional<ProjectData> result;
		if (project == null) {
			result = Optional.empty();
		} else {
			final String projectData = getProjectStore(project).getString(MiningPreferences.KEY_PROJECT);
			try {
				result = Optional.ofNullable(PropertyUtils.fromString(projectData, ProjectData.class));
			} catch (final CoreException e) {
				return Optional.empty();
			}
		}
		return result;
	}
	
	private static final boolean hasProjectSettings(final IProject project) {
		return Boolean.parseBoolean(getProjectStore(project).getString("ProjectSettings"));
	}
	
	private static final SecureScopedPreferenceStore getProjectStore(final IProject project) {
		return new SecureScopedPreferenceStore(new ProjectScope(project), "innowake.mining.plugin");
	}
	
	private static final Optional<ConnectionInfo> getConnectionInfo() throws StorageException {
		if (getWorkbenchStore().contains(KEY_API_SERVER)) {
			final SecureScopedPreferenceStore store = getWorkbenchStore();
			final ConnectionInfo connectionInfo = new ConnectionInfo(store.getString(KEY_API_SERVER), store.getSecureString(KEY_ACCESS_TOKEN));
			return Optional.of(connectionInfo);
		}
		return Optional.empty();
	}
	
	private static final SecureScopedPreferenceStore getWorkbenchStore() {
		return new SecureScopedPreferenceStore(InstanceScope.INSTANCE, "innowake.mining.plugin");
	}
}

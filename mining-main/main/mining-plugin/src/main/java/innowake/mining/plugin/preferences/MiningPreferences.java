/* Copyright (c) 2019 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.plugin.preferences;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.equinox.security.storage.StorageException;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.MiningPlugin;

/**
 * Class to read & write the mining related preferences.
 */
public class MiningPreferences {

	/**
	 * Default API server URL set if the user restore the default settings.
	 */
	public static final String DEFAULT_API_SERVER = "http://localhost:8080/";

	/**
	 * Default access token (empty) set if the user restore the default settings.
	 */
	public static final String DEFAULT_ACCESS_TOKEN = "";

	/**
	 * Default deep links server port.
	 */
	public static final int DEFAULT_DEEP_LINKS_PORT = 8083;

	/**
	 * Key used to flag if the project define its own settings.
	 */
	public static final String KEY_PROJECT_SETTINGS = "ProjectSettings";

	/**
	 * Key for the workbench setting of the API server URL.
	 */
	public static final String KEY_API_SERVER = "WorkbenchServer";

	/**
	 * Key for the workbench setting of the access token.
	 */
	public static final String KEY_ACCESS_TOKEN = "WorkbenchAccessToken";

	/**
	 * Property key for the project.
	 */
	public static final String KEY_PROJECT = "project";

	/**
	 * Property key for the snapshot.
	 */
	public static final String KEY_SNAPSHOT = "snapshot";

	/**
	 * Property key for the deep links server port.
	 */
	public static final String KEY_DEEP_LINKS_PORT = "deepLinksServerPort";
	
	/**
	 * Property key for the list of server URLs and versions to ignore when trying to show an error dialog on version mismatches
	 * between the Eclipse plugin and the API server.
	 */
	public static final String KEY_DO_NOT_SHOW_VERSION_MISMATCH_DIALOG = "doNotShowVersionMismatchDialogFor";

	/**
	 * Qualified name used to flag if the project define its own settings.
	 */
	public static final QualifiedName KEY_QN_PROJECT_SETTINGS = new QualifiedName(MiningPlugin.ID, KEY_PROJECT_SETTINGS);

	/**
	 * Qualified name to define the property key for the API server URL.
	 */
	public static final QualifiedName KEY_QN_API_SERVER = new QualifiedName(MiningPlugin.ID, KEY_API_SERVER);

	/**
	 * Qualified name to define the property key for the access token.
	 */
	public static final QualifiedName KEY_QN_ACCESS_TOKEN = new QualifiedName(MiningPlugin.ID, KEY_ACCESS_TOKEN);

	/**
	 * Qualified name to define the property key for the project.
	 */
	public static final QualifiedName KEY_QN_PROJECT = new QualifiedName(MiningPlugin.ID, KEY_PROJECT);

	/**
	 * Qualified name to define the property key for the snapshot.
	 */
	public static final QualifiedName KEY_QN_SNAPSHOT = new QualifiedName(MiningPlugin.ID, KEY_SNAPSHOT);

	private static ConcurrentMap<Optional<IProject>, ConnectionInfo> connectionInfoMap = new ConcurrentHashMap<>();
	
	private MiningPreferences() {}
	
	/**
	 * Get access to the workbench preference store.
	 *
	 * @return The instance scope preference store for the mining plugin {@link MiningPlugin#ID}.
	 */
	public static final SecureScopedPreferenceStore getWorkbenchStore() {
		return new SecureScopedPreferenceStore(InstanceScope.INSTANCE, MiningPlugin.ID);
	}

	/**
	 * Get access to the project preference store.
	 * 
	 * @param project The project whose preference store is to be returned.
	 *
	 * @return The instance scope preference store for project.
	 */
	public static final SecureScopedPreferenceStore getProjectStore(final IProject project) {
		return new SecureScopedPreferenceStore(new ProjectScope(project), MiningPlugin.ID);
	}
	
	public static final void resetConnectionInfo(final @Nullable IProject project) {
		connectionInfoMap.remove(Optional.ofNullable(project));
	}

	/**
	 * Get the {@link ConnectionInfo} URL, port and access token wrapped in this class.
	 * Order is
	 * <br>1. Project
	 * <br>2. Workbench
	 * <br>3. {@link Optional#empty()} 
	 *
	 * @param project The project instance to check if connection infos are set
	 * @return The connection infos if available.
	 */
	public static final Optional<ConnectionInfo> getConnectionInfo(final IProject project) {
		if (hasProjectSettings(project)) {
			return Optional.of(connectionInfoMap.computeIfAbsent(Optional.of(project), p -> {
				final SecureScopedPreferenceStore store = getProjectStore(project);
				final String server = store.getString(KEY_API_SERVER);
				try {
					return new ConnectionInfo(server, store.getSecureString(KEY_ACCESS_TOKEN));
				} catch (StorageException e) {
					throw new IllegalStateException(e);
				}
			}));
		}
		return getConnectionInfo();
	}

	/**
	 * Get the {@link ConnectionInfo} (URL, port and access token) from workbench preferences or an empty {@link Optional} if not configured.
	 *
	 * @return The connection info if available.
	 */
	public static final Optional<ConnectionInfo> getConnectionInfo() {
		if (getWorkbenchStore().contains(KEY_API_SERVER)) {
			return Optional.of(connectionInfoMap.computeIfAbsent(Optional.empty(), x -> {
				final SecureScopedPreferenceStore store = getWorkbenchStore();
				final String server = store.getString(KEY_API_SERVER);
				try {
					return new ConnectionInfo(server, store.getSecureString(KEY_ACCESS_TOKEN));
				} catch (StorageException e) {
					throw new IllegalStateException(e);
				}
			}));
		}
		return Optional.empty();
	}

	/**
	 * Get the API server URL.
	 * Order is
	 * <br>1. Project
	 * <br>2. Workbench
	 * <br>3. {@link Optional#empty()} 
	 *
	 * @param project The project instance to check if API server URL is set
	 * @return The API server URL if available.
	 */
	public static final Optional<String> getApiServerUrl(final IProject project) {
		if (hasProjectSettings(project)) {
			return Optional.of(getProjectStore(project).getString(KEY_API_SERVER));
		} else {
			return getApiServerUrl();
		}
	}

	/**
	 * Get the API server URL from workbench preferences or an empty {@link Optional} if not configured.
	 *
	 * @return The API server URL if available.
	 */
	public static final Optional<String> getApiServerUrl() {
		if (getWorkbenchStore().contains(KEY_API_SERVER)) {
			return Optional.of(getWorkbenchStore().getString(KEY_API_SERVER));
		}
		return Optional.empty();
	}

	/**
	 * Used to set default but invalid (access token is missing) values.
	 * Should show the expected value layout to the user to update accordingly.
	 *
	 */
	public static final void setStoreDefaults() {
		final SecureScopedPreferenceStore store = getWorkbenchStore();
		store.setDefault(KEY_API_SERVER, DEFAULT_API_SERVER);
		store.setDefault(KEY_DEEP_LINKS_PORT, DEFAULT_DEEP_LINKS_PORT);
		try {
			store.setSecureDefault(KEY_ACCESS_TOKEN, DEFAULT_ACCESS_TOKEN);
		} catch (StorageException e) {
			throw new IllegalStateException("Error while setting secure default properties: " + e.getMessage(), e);
		}
	}

	/**
	 * Returns the mining API project which a given local project is linked to.
	 *
	 * @param project the local project
	 * @return the mining API project for a given local project, if it's bound to an API project
	 */
	public static Optional<ProjectData> getApiProject(@Nullable final IProject project) {
		Optional<ProjectData> result;
		if (project == null) {
			result = Optional.empty();
		} else {
			final String projectData = getProjectStore(project).getString(MiningPreferences.KEY_PROJECT);
			try {
				result = Optional.ofNullable(PropertyUtils.fromString(projectData, ProjectData.class));
			} catch (final CoreException e) {
				Logging.error(e.getLocalizedMessage(), e);
				return Optional.empty();
			}
		}
		return result;
	}
	
	/**
	 * Checks if the given URL and version should be ignored in case of version mismatch.
	 * <p>
	 * This checks the given version against the already persisted version (if any). If those version do not match
	 * this will return {@code true} effectively resetting the check.
	 *
	 * @param url the URL of the server
	 * @param newVersion the version of the server
	 * @return {@code true} if the URL with the given version should be ignored, {@code false} otherwise
	 */
	public static final boolean shouldIgnoreUrl(final String url, final String newVersion) {
		final String serverUrlWithoutTrailingSlash = StringUtils.removeEnd(url, "/");
		final Map<String, String> serverUrlVersions = getIgnoredServerUrlVersions();
		final String oldVersion = serverUrlVersions.get(serverUrlWithoutTrailingSlash);
		
		if (oldVersion == null) {
			/* No value saved for this URL so we show the error dialog */
			return true;
		}
		
		/* If the versions do not match we also show the dialog */
		if ( ! oldVersion.equals(newVersion)) {
			/* First remove the persisted value, effectively resetting the do not show again checkbox for future calls */
			serverUrlVersions.remove(serverUrlWithoutTrailingSlash);
			storeIgnoredServerUrls(serverUrlVersions);
			return true;
		} else {
			return false;
		}
	}

	/**
	 * Adds the given URL with the given version to the list of URLs to ignore in case of version mismatches.
	 *
	 * @param url the URL of the server
	 * @param version the version of the server
	 */
	public static final void addIgnoredUrl(final String url, final String version) {
		final String serverUrlWithoutTrailingSlash = StringUtils.removeEnd(url, "/");
		final Map<String, String> serverUrlVersions = getIgnoredServerUrlVersions();
		if (serverUrlVersions.containsKey(serverUrlWithoutTrailingSlash)) {
			return;
		}
		serverUrlVersions.put(serverUrlWithoutTrailingSlash, version);
		storeIgnoredServerUrls(serverUrlVersions);
	}
	
	/**
	 * Removes the given URL from the list of URLs for which no error dialog should be shown on version mismatch.
	 *
	 * @param url the URL to remove
	 */
	public static void removeIgnoredUrl(final String url) {
		final String serverUrlWithoutTrailingSlash = StringUtils.removeEnd(url, "/");
		final Map<String, String> serverUrlVersions = getIgnoredServerUrlVersions();
		if (serverUrlVersions.containsKey(serverUrlWithoutTrailingSlash)) {
			serverUrlVersions.remove(serverUrlWithoutTrailingSlash);
			storeIgnoredServerUrls(serverUrlVersions);
		}
	}
	
	private static Map<String, String> getIgnoredServerUrlVersions() {
		return convertToMap(getWorkbenchStore().getString(KEY_DO_NOT_SHOW_VERSION_MISMATCH_DIALOG));
	}

	private static void storeIgnoredServerUrls(final Map<String, String> serverUrlVersions) {
		final String mapAfterUpdate = convertToString(serverUrlVersions);
		getWorkbenchStore().setValue(KEY_DO_NOT_SHOW_VERSION_MISMATCH_DIALOG, mapAfterUpdate);
	}

	private static String convertToString(final Map<String, String> map) {
	    return map.keySet().stream()
	      .map(key -> key + "=" + map.get(key))
	      .collect(Collectors.joining(","));
	}

	private static Map<String, String> convertToMap(final String mapAsString) {
		if (mapAsString.isEmpty()) {
			return new HashMap<>();
		}
		return Arrays.stream(mapAsString.split(","))
				.map(entry -> entry.split("="))
				.collect(Collectors.toMap(entry -> entry[0], entry -> entry[1]));
	}
	
	private static final boolean hasProjectSettings(final IProject project) {
		return Boolean.parseBoolean(getProjectStore(project).getString(KEY_PROJECT_SETTINGS));
	}
}

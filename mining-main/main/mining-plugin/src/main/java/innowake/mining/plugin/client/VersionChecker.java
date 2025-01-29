/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.plugin.client;

import static innowake.mining.client.MiningServiceExecutor.create;
import static innowake.mining.plugin.client.ApiClient.versionService;
import static java.lang.String.format;

import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;

import org.eclipse.core.resources.IProject;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.MiningPlugin;
import innowake.mining.plugin.preferences.MiningPreferences;
import innowake.mining.shared.Versions;

/**
 * Checks the versions of the Mining plugin and the corresponding API server.
 */
public class VersionChecker {
	
	/**
	 * Signifies if there is already an error dialog shown.
	 */
	public static final AtomicBoolean DIALOG_ALREADY_SHOWING = new AtomicBoolean(false); 
	
	/**
	 * The title which can be shown in an error dialog.
	 */
	public static final String VERSION_MISMATCH_TITLE = "Version Mismatch";

	/**
	 * The message pattern which can be shown in an error dialog.
	 * <p>
	 * The following string parameters must be provided:
	 * <ol>
	 * <li>the target, i.e. the project name or workspace
	 * <li>the server version
	 * <li>the server URL
	 * <li>the plugin version
	 */
	public static final String VERSION_MISMATCH_MESSAGE_PATTERN = 
			"The versions of Eclipse Plugin and the API server for %s do not match. Please ensure using a matching Eclipse version to avoid unforeseen issues."
			+ "\r\n"
			+ "\r\n"
			+ "API Server Version: %s (%s)\r\n"
			+ "\r\n"
			+ "Plugin Version: %s\r\n"
			+ "\r\n"
			+ "Contact innowakesupport@deloitte.com for further assistance.";
	
	private static final String SERVER_VERSION_NOT_RETRIEVABLE_MESSAGE_PATTERN =
			"The version of the API Server (%s) could not be determined for %s.\r\n"
			+ "\r\n" 
			+ "Please ensure that the server you are trying to connect to matches the version of the Mining Plugin.\r\n" 
			+ "\r\n" 
			+ "Contact innowakesupport@deloitte.com for further assistance.";

	private static final Map<String, String> CCHECKS = new ConcurrentHashMap<>();
	private static final Logger LOG = LoggerFactory.getLogger(VersionChecker.class);

	private VersionChecker() {}

	/**
	 * Checks the version of the given project.
	 * <p>
	 * In case the project was already checked and the force check is not {@code true}, the result will have the match attribute set to {@code true}.
	 *
	 * @param project the project for which the version on the server should be checked. If {@code null}, will check for the workspace server.
	 * @param forceCheck {@code true} if the check should be done regardless if it was already done within the current Eclipse session. 
	 *                   A session begins when Eclipse is started and ends when Eclipse is quit.
	 * @return the version check result
	 */
	public static VersionResult checkVersion(@Nullable final IProject project, final boolean forceCheck) {
		return checkVersion(project, null, forceCheck);
	}

	/**
	 * Checks the version of the given project and connection information.
	 * <p>
	 * <ul>
	 *  <li>If the connection information is not null, the URL of the server is taken from the connection information rather than the project.
	 *  <li>If the connection information is null, the URL is retrieved from the project.
	 *  <li>If no URL can be determined, the check is aborted
	 * <p>
	 * In case the project was already checked and the force check is not {@code true}, the result will have the match attribute set to {@code true}.
	 *
	 * @param project the project for which the version on the server should be checked. If {@code null}, will check for the workspace server.
	 * @param connectionInfo the connection information to use for determining the server version. 
	 *                   If {@code null} the project will be used for determining the connection information.
	 * @param forceCheck {@code true} if the check should be done regardless if it was already done within the current Eclipse session. 
	 *                   A session begins when Eclipse is started and ends when Eclipse is quit.
	 * @return the version check result
	 */
	public static VersionResult checkVersion(@Nullable final IProject project, @Nullable final ConnectionInfo connectionInfo, final boolean forceCheck) {
		final String name = project != null ? "project '" + project.getName() + "'" : "workspace";
		final Optional<String> optionalServerUrl = getServerUrl(project, connectionInfo);
		
		if ( ! optionalServerUrl.isPresent()) {
			LOG.error(String.format("Could not determine server URL for %s", name));
			return VersionResult.alreadyChecked();
		}
		
		/* Check if the version was already checked, if so return */
		final String serverUrl = optionalServerUrl.get();
		final boolean alreadyChecked = CCHECKS.get(serverUrl) != null;

		if (forceCheck) {
			LOG.debug(() -> format("Force checking version for %s", name));
		} else if (alreadyChecked) {
			LOG.debug(() -> format("Already did a version check for %s", name));
			return VersionResult.alreadyChecked();
		} else {
			LOG.debug(() -> format("Checking version for %s", name));
		}

		/* Retrieve server version */
		final Optional<Map<String, String>> serverVersionResult;
		final AtomicBoolean serverExceptionOccurred = new AtomicBoolean();
		if (connectionInfo != null) {
			serverVersionResult = create(() -> versionService(connectionInfo).version())
					.setInvalidResultConsumer(result -> LOG.debug(result::getExtendedStatusMessage))
					.setExceptionConsumer(exception -> {
						serverExceptionOccurred.set(true);
						LOG.error(exception::getLocalizedMessage, exception);
					}).execute();
		} else {
			serverVersionResult = create(() -> versionService(project).version())
					.setInvalidResultConsumer(result -> LOG.debug(result::getExtendedStatusMessage))
					.setExceptionConsumer(exception -> {
						serverExceptionOccurred.set(true);
						LOG.error(exception::getLocalizedMessage, exception);
					}).execute();
		}
		
		if (serverExceptionOccurred.get()) {
			/* Abort if the connection to the server was unsuccessful. */
			return VersionResult.alreadyChecked();
		}

		final String serverVersion;
		if ( ! serverVersionResult.isPresent()) {
			serverVersion = "version information not retrievable";
			Logging.error(format(SERVER_VERSION_NOT_RETRIEVABLE_MESSAGE_PATTERN, serverUrl, name));
		} else {
			serverVersion = serverVersionResult.get().get("version");
		}
		LOG.debug(() -> format("Server version: %s", serverVersion));
		CCHECKS.put(serverUrl, serverVersion);

		/* Retrieve plugin version */
		final String pluginVersion = MiningPlugin.getDefaultNonNull().getBundleInfo().getBundleVersion();
		LOG.debug(() -> format("Plugin version: %s", pluginVersion));

		/* Determine version equality */
		final boolean versionsEqual = Versions.equals(pluginVersion, serverVersion);

		if (versionsEqual) {
			LOG.debug(() -> format("Versions match."));
		} else {
			LOG.warn(() -> format("Version mismatch (Server version: '%s' | Plugin version: '%s')", serverVersion, pluginVersion));
		}

		return VersionResult.of(versionsEqual, pluginVersion, serverVersion, serverUrl);
	}

	private static Optional<String> getServerUrl(@Nullable final IProject project, @Nullable ConnectionInfo connectionInfo2) {
		Optional<ConnectionInfo> connectionInfo = Optional.empty();
		if (project != null) {
			connectionInfo = MiningPreferences.getConnectionInfo(project);
		}
		if (connectionInfo.isPresent()) {
			return Optional.of(connectionInfo.get().getUrl());
		} else if (connectionInfo2 != null) {
			return Optional.of(connectionInfo2.getUrl());
		} else {
			return Optional.empty();
		}
	}

	/**
	 * Holds the result of the version equality check. 
	 */
	public static class VersionResult {
		/** Signifies if the versions matched. If the check was already done then this is {@code true} and the other attributes are blank. */
		public final boolean match;
		/** The version of the plugin. */
		public final String pluginVersion;
		/** The version of the server. */
		public final String serverVersion;
		/** The server URL. */
		public final String serverUrl;

		private VersionResult(final boolean match, final String pluginVersion, final String serverVersion, final String serverUrl) {
			this.match = match;
			this.pluginVersion = pluginVersion;
			this.serverVersion = serverVersion;
			this.serverUrl = serverUrl;
		}

		private static VersionResult of(final boolean match, final String pluginVersion, final String serverVersion, final String serverUrl) {
			return new VersionResult(match, pluginVersion, serverVersion, serverUrl);
		}

		private static VersionResult alreadyChecked() {
			return new VersionResult(true, "", "", "");
		}
	}
}

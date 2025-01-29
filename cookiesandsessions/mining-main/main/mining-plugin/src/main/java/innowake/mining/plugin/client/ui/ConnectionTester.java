/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.client.ui;

import static innowake.mining.plugin.client.ApiClient.infoService;

import java.util.Map;
import java.util.Optional;

import org.apache.commons.lang.exception.ExceptionUtils;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.client.service.info.Info;
import innowake.mining.client.service.info.InfoServiceProvider;
import innowake.mining.plugin.Logging;

/**
 * Testing the connection to the mining API backend.
 */
public final class ConnectionTester {
	
	private ConnectionTester() {}

	/**
	 * Tests the connection to the mining API backend and shows information to the user.
	 *
	 * @param connectionInfo the {@link ConnectionInfo} instance
	 * @param shell the shell to use for showing information to the user
	 * @return {@code true} if the connection was successful; {@code false} otherwise
	 */
	public static boolean test(final ConnectionInfo connectionInfo, final Shell shell) {
		final InfoServiceProvider infoServiceProvider = infoService(connectionInfo);
		final Info infoService = infoServiceProvider.info();
		
		final long startNanos = System.nanoTime();
		final Optional<Map<String, String>> info = MiningServiceExecutor
				.create(() -> infoService)
				.setInvalidResultConsumer(invalidResult -> {
					MessageDialog.openError(shell, "Server Error", invalidResult.getStatusMessage());
					Logging.error(invalidResult.getExtendedStatusMessage());
				})
				.setExceptionConsumer(exception -> {
					final String message = ExceptionUtils.getRootCauseMessage(exception);
					final Throwable rootException = ExceptionUtils.getRootCause(exception);
					MessageDialog.openError(shell, "Server Error", message);
					Logging.error(message, rootException);
				})
				.execute();
		final long endNanos = System.nanoTime();
		
		if ( ! info.isPresent()) {
			return false;
		}

		/* 1000000 factor to convert ns to ms */
		final long elapsedMillis = (endNanos - startNanos) / 1000000;
		
		final StringBuilder sb = new StringBuilder("Server connection successful.\n\n");
		
		final String userId = info.get().get("userId");
		if (userId == null) {
			sb.append("Could not fetch connected user.\n");
		} else {
			sb.append("Connected user: ").append(userId).append("\n");
		}
		
		String quality = "normal";
		if (elapsedMillis <= 40) {
			quality = "fast";
		} else if (elapsedMillis > 90){
			quality = "slow";
		}
		sb.append("Response time: ").append(elapsedMillis).append(" ms (").append(quality).append(")\n");
		
		final String apiVersion = info.get().get("api-version");
		if (apiVersion == null) {
			sb.append("Could not fetch API version.\n");
		} else {
			sb.append("Server API version: ").append(apiVersion).append("\n");
		}
		
		MessageDialog.openInformation(shell, "Server Info", sb.toString());
		return true;
	}
}

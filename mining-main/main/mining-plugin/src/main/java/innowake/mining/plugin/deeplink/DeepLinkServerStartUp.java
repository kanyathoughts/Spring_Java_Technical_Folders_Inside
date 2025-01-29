/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.plugin.deeplink;

import innowake.mining.plugin.Logging;
import innowake.mining.plugin.deeplink.server.DeepLinkServerException;
import innowake.mining.plugin.deeplink.server.EclipseServer;
import innowake.mining.plugin.preferences.MiningPreferences;

/**
 * This provides methods for starting the Deep Links Server.
 * 
 */
public class DeepLinkServerStartUp {
	private DeepLinkServerStartUp() {}
	
	private static final int DEFAULT_PORT = 8083;
	
	/**
	 * This method can be called to start a new deep links server.
	 * It first attempts to use the default port. If the default port is not available, any open port will be used.
	 *
	 */
	public static void startUp() {
		new Thread(() -> {
			final int port = EclipseServer.createServer(DEFAULT_PORT);
			MiningPreferences.getWorkbenchStore().setValue(MiningPreferences.KEY_DEEP_LINKS_PORT, port);
			if (port > 0) {
				Logging.info("Deep links server listening on port: " + port);
			} else {
				throw new IllegalStateException(new DeepLinkServerException("Failed to find avilable port"));
			}
		}).start();
	}
}

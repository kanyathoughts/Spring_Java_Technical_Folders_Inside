/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.deeplink.server;

import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.servlet.FilterMapping;
import org.eclipse.jetty.servlet.ServletHandler;

import innowake.mining.plugin.Logging;

/**
 * This class manages the creation of the Eclipse DeepLinks server
 */
public class EclipseServer {
	private EclipseServer() {
	}
	
	/**
	 * Attempts to open a server at the given port.
	 * If the port is 0, any port is used.
	 * 
	 * @param port The port to attempt to open the server.
	 * @return The server instance if the attempt was successful.
	 */
	public static Server tryOpenServerAtPort(final int port) {
		final ServletHandler handler = new ServletHandler();
		handler.addFilterWithMapping(CorsFilter.class, "/*", FilterMapping.ALL);
		handler.addServletWithMapping(RequestHandler.class, "/");
		
		final Server server = new Server(port);
		server.setHandler(handler);
		
		try {
			server.start();
		} catch (final Exception e) {
			Logging.error("Unable to start server", e);
			try {
				server.stop();
			} catch (final Exception e1) {
				Logging.error("Error while stopping server", e1);
			}
			handler.destroy();
			return null;
		}
		return server;
	}
	
	/**
	 * Attempts to create a server only on the given port. 
	 *
	 * @param defaultPort The port to use
	 * @return The port, if starting the server was successful; -1 otherwise
	 */
	public static int createServer(int defaultPort) {
		Logging.info("Attempting port " + defaultPort + " for Deep Links server.");
		Server server = tryOpenServerAtPort(defaultPort);
		if (server != null) {
			return server.getURI().getPort();
		}
		Logging.info("Port " + defaultPort + " was unavailable. Using next available port.");
		server = tryOpenServerAtPort(0);
		if (server != null) {
			return server.getURI().getPort();
		}
		return -1;
	}
}

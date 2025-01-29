/* Copyright (c) 2021 Deloitte. All rights reserved. */
package innowake.lsp;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

import org.eclipse.lsp4j.jsonrpc.Launcher;
import org.eclipse.lsp4j.launch.LSPLauncher;
import org.eclipse.lsp4j.services.LanguageClient;

import innowake.lsp.cobol.CobolLanguageServer;

/**
 * FIXME: description
 */
public class SocketLauncher {

	public static void main(final String[] args) {
		launch(55555);
	}

	public static void launch(final int port) {
		try (final ServerSocket ss = new ServerSocket(port)) {
			System.out.println("server listening on " + ss.getLocalSocketAddress().toString());
			while (true) {
				final Socket connected = ss.accept();
				System.out.println("client connected from " + connected.getRemoteSocketAddress().toString());
				final CobolLanguageServer ls = new CobolLanguageServer();
				final Launcher<LanguageClient> launcher = LSPLauncher.createServerLauncher(ls, connected.getInputStream(), connected.getOutputStream());
				final LanguageClient client = launcher.getRemoteProxy();
				ls.connect(client);
				launcher.startListening();
			}
		} catch (final IOException e) {
			e.printStackTrace();
		}
	}

}

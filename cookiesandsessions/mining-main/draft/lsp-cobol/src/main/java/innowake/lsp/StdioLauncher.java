/* Copyright (c) 2021 Deloitte. All rights reserved. */
package innowake.lsp;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import org.eclipse.lsp4j.jsonrpc.Launcher;
import org.eclipse.lsp4j.launch.LSPLauncher;
import org.eclipse.lsp4j.services.LanguageClient;

import innowake.lsp.cobol.CobolLanguageServer;

/**
 * FIXME: description
 */
public class StdioLauncher {

	public static void main(final String[] args) {
		final InputStream stdin = System.in;
		final PrintStream stdout = System.out;
		final PrintStream stderr = System.err;
		launch(stdin, stdout, stderr);
	}

	public static void launch(final InputStream stdin, final OutputStream stdout, final PrintStream stderr) {
		try {
			final CobolLanguageServer ls = new CobolLanguageServer();
			final Launcher<LanguageClient> launcher = LSPLauncher.createServerLauncher(ls, stdin, stdout);
			final LanguageClient client = launcher.getRemoteProxy();
			ls.connect(client);
			final Future<Void> result = launcher.startListening();
			result.get();
		} catch (final InterruptedException | ExecutionException e) {
			e.printStackTrace(stderr);
		}
	}

}

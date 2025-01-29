/* Copyright (c) 2021 Deloitte. All rights reserved. */
package innowake.lsp.cobol;

import java.util.concurrent.CompletableFuture;

import org.eclipse.lsp4j.InitializeParams;
import org.eclipse.lsp4j.InitializeResult;
import org.eclipse.lsp4j.MessageParams;
import org.eclipse.lsp4j.MessageType;
import org.eclipse.lsp4j.ServerCapabilities;
import org.eclipse.lsp4j.services.LanguageClient;
import org.eclipse.lsp4j.services.LanguageClientAware;
import org.eclipse.lsp4j.services.LanguageServer;
import org.eclipse.lsp4j.services.TextDocumentService;
import org.eclipse.lsp4j.services.WorkspaceService;

/**
 * FIXME: description
 */
public class CobolLanguageServer implements LanguageServer, LanguageClientAware {

	private final CobolModuleService moduleService = new CobolModuleService();

	@Override
	public CompletableFuture<InitializeResult> initialize(final InitializeParams params) {
		return CompletableFuture.supplyAsync(() -> {
			final ServerCapabilities capabilities = new ServerCapabilities();
			capabilities.setHoverProvider(Boolean.TRUE);
			capabilities.setReferencesProvider(Boolean.TRUE);
			capabilities.setFoldingRangeProvider(Boolean.TRUE);
			final InitializeResult result = new InitializeResult(capabilities);
			return result;
		});
	}

	@Override
	public CompletableFuture<Object> shutdown() {
		return CompletableFuture.supplyAsync(() -> null);
	}

	@Override
	public void exit() {
	}

	@Override
	public TextDocumentService getTextDocumentService() {
		return moduleService;
	}
	
	@Override
	public WorkspaceService getWorkspaceService() {
		return null;
	}

	@Override
	public void connect(final LanguageClient client) {
		client.showMessage(new MessageParams(MessageType.Info, "Connected to COBOL Lsp"));
		moduleService.connect(client);
	}

}

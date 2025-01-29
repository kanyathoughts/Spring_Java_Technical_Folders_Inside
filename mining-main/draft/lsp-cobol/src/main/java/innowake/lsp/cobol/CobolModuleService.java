/* Copyright (c) 2021 Deloitte. All rights reserved. */
package innowake.lsp.cobol;

import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.eclipse.lsp4j.DidChangeTextDocumentParams;
import org.eclipse.lsp4j.DidCloseTextDocumentParams;
import org.eclipse.lsp4j.DidOpenTextDocumentParams;
import org.eclipse.lsp4j.DidSaveTextDocumentParams;
import org.eclipse.lsp4j.FoldingRange;
import org.eclipse.lsp4j.FoldingRangeRequestParams;
import org.eclipse.lsp4j.Hover;
import org.eclipse.lsp4j.HoverParams;
import org.eclipse.lsp4j.Location;
import org.eclipse.lsp4j.MarkupContent;
import org.eclipse.lsp4j.MessageParams;
import org.eclipse.lsp4j.MessageType;
import org.eclipse.lsp4j.Position;
import org.eclipse.lsp4j.Range;
import org.eclipse.lsp4j.ReferenceParams;
import org.eclipse.lsp4j.services.LanguageClient;
import org.eclipse.lsp4j.services.LanguageClientAware;
import org.eclipse.lsp4j.services.TextDocumentService;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.ndt.cobol.parser.ast.CobolAstUtil;
import innowake.ndt.cobol.parser.ast.CobolParseConfiguration;
import innowake.ndt.cobol.parser.ast.CobolParserAst;
import innowake.ndt.cobol.parser.ast.model.CobolDataField;
import innowake.ndt.cobol.parser.ast.model.CobolModel;
import innowake.ndt.cobol.parser.ast.model.CobolReference;
import innowake.ndt.cobol.parser.ast.model.WorkingStorageSection;
import innowake.ndt.core.assembling.AssemblingException;
import innowake.ndt.core.assembling.IAssemblingDataProvider;
import innowake.ndt.core.assembling.IAssemblingObjectType;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.spi.Document;

public class CobolModuleService implements TextDocumentService, LanguageClientAware {

	class StringAssemblingDataProvider implements IAssemblingDataProvider<String> {

		@Override
		public String find(String root, String name, IAssemblingObjectType expectedType) {
			return null;
		}

		@Override
		public String getPath(String object) {
			return null;
		}

		@Override
		public String getSource(String object) throws AssemblingException {
			return object;
		}

		@Override
		public IAssemblingObjectType getType(String object) {
			return null;
		}

		@Override
		public String getName(String object) {
			return null;
		}

		@Override
		public Object getHashable(String object) {
			return object;
		}

		@Override
		public boolean isObjectProxy(String object) {
			return false;
		}

	}

	private static final Logger LOG = LoggerFactory.getLogger(CobolModuleService.class);

	@Nullable
	private LanguageClient client;

	private final Map<String, CobolModel> asts = Collections.synchronizedMap(new HashMap<>());
	private final Map<String, Document> openDocuments = Collections.synchronizedMap(new HashMap<>());

	private AstNode nodeAtPosition(final String uri, final Position pos) {
		ensureOpen(uri);

		final CobolModel cm = asts.get(uri); // must be loaded because the document must be open
		final int offset = openDocuments.get(uri).getOffset(pos.getLine()) + pos.getCharacter();
		final List<AstNode> found = cm.getNode(offset);
		final AstNode node = found.get(0);
		return node;
	}

	private void ensureOpen(final String uri) {
		if (asts.containsKey(uri) && openDocuments.containsKey(uri)) {
			return;
		}

		byte[] fileData;
		try {
			fileData = Files.readAllBytes(Paths.get(URI.create(uri)));
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}
		final String text = new String(fileData, StandardCharsets.UTF_8);

		final CobolParserAst cobolParser = new CobolParserAst(LOG);
		final CobolModel cm = cobolParser
				.parse(new CobolParseConfiguration.Builder<String>(text, text, new StringAssemblingDataProvider()).enableNodeLookup(true).build());
		asts.put(uri, cm);
		openDocuments.put(uri, new Document(cm.getSource()));
	}

	@Override
	public void didOpen(final DidOpenTextDocumentParams params) {
		final String uri = params.getTextDocument().getUri();
		final String text = params.getTextDocument().getText();

		final CobolParserAst cobolParser = new CobolParserAst(LOG);
		final CobolModel cm = cobolParser
				.parse(new CobolParseConfiguration.Builder<String>(text, text, new StringAssemblingDataProvider()).enableNodeLookup(true).build());
		asts.put(uri, cm);
		openDocuments.put(uri, new Document(cm.getSource()));
	}

	@Override
	public void didChange(final DidChangeTextDocumentParams params) {
		System.out.println("change " + params.getTextDocument().getUri());
		// read-only, so ignore
	}

	@Override
	public void didClose(final DidCloseTextDocumentParams params) {
		asts.remove(params.getTextDocument().getUri());
		openDocuments.remove(params.getTextDocument().getUri());
	}

	@Override
	public void didSave(final DidSaveTextDocumentParams params) {
		// read-only, so ignore
	}

	@Override
	public void connect(final LanguageClient client) {
		this.client = client;
	}

	@Override
	public CompletableFuture<Hover> hover(final HoverParams params) {
		return CompletableFuture.supplyAsync(() -> {
			final String uri = params.getTextDocument().getUri();
			final Position pos = params.getPosition();

			final Hover hover = new Hover();
			final AstNode node = nodeAtPosition(uri, pos);
			final String name = node.getClass().getName();

			hover.setContents(new MarkupContent("markdown", String.format("[%s](https://google.com)", name)));
			return hover;
		});
	}

	@Override
	public CompletableFuture<List<? extends Location>> references(final ReferenceParams params) {
		return CompletableFuture.supplyAsync(() -> {
			final List<Location> locations = new ArrayList<>();

			final String uri = params.getTextDocument().getUri();
			final Position pos = params.getPosition();
			final CobolModel cm = asts.get(uri); // must be loaded because the document must be open
			final AstNode node = nodeAtPosition(uri, pos);
			final String desiredName;
			if (node instanceof CobolReference) {
				desiredName = ((CobolReference) node).getToken().getText();
			} else if (node instanceof CobolDataField) {
				desiredName = ((CobolDataField) node).getName();
			} else {
				if (client != null) {
					client.showMessage(new MessageParams(MessageType.Info, "Can't find reference for a " + node.getClass().getSimpleName()));
				}
				return Collections.emptyList();
			}

			/* find all usages of that reference */
			final List<CobolReference> references = CobolAstUtil.findChildren(cm.getRoot().get(), CobolReference.class);
			references.forEach(ref -> {
				if (ref.getToken() == null) {
					return;
				}
				if (!desiredName.equals(ref.getToken().getText())) {
					return;
				}
				final Position start = new Position(ref.getToken().getLine() - 1, ref.getToken().getColumn());
				final Position end = new Position(ref.getToken().getLine() - 1, ref.getToken().getColumn() + ref.getToken().getLength());
				final Range range = new Range(start, end);
				final Location location = new Location(uri, range);
				locations.add(location);
			});

			/* find the reference in the data definitions */
			final List<CobolDataField> definitions = CobolAstUtil.findChildren(cm.getRoot().get(), CobolDataField.class);
			definitions.forEach(def -> {
				if (!desiredName.equals(def.getName())) {
					return;
				}
				final Position start = new Position(def.getStartToken().getLine() - 1, def.getStartToken().getColumn());
				final Position end = new Position(def.getEndToken().getLine() - 1, def.getEndToken().getColumn() + def.getEndToken().getLength());
				final Range range = new Range(start, end);
				final Location location = new Location(uri, range);
				locations.add(location);
			});
			return locations;
		});

	}

	@Override
	public CompletableFuture<List<FoldingRange>> foldingRange(final FoldingRangeRequestParams params) {
		return CompletableFuture.supplyAsync(() -> {
			final List<FoldingRange> foldings = new ArrayList<>();

			final String uri = params.getTextDocument().getUri();
			ensureOpen(uri);
			final CobolModel cm = asts.get(uri);

			/* working storage folding */
			final WorkingStorageSection ws = CobolAstUtil.findChildren(cm.getRoot().get(), WorkingStorageSection.class).get(0);
			final FoldingRange wsFolding = new FoldingRange(ws.getStartToken().getLine() - 1, ws.getEndToken().getLine() - 1);
			foldings.add(wsFolding);

			return foldings;
		});
	}
}

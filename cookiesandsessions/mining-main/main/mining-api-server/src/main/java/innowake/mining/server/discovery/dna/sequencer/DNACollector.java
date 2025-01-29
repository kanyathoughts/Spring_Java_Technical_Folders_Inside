/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dna.sequencer;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerCancellationException;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.parser.ParseResultProvider;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.SourcePojo;
import innowake.ndt.cobol.parser.ArtificialToken;
import innowake.ndt.core.parsing.IDocument;
import innowake.ndt.core.parsing.ILocationMultiline;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.spi.Document;
import innowake.ndt.core.parsing.spi.LocationMultiline;

/**
 * Container class to collect dna entries for one module.
 *
 * @param <T> the type of parse result this collector holds
 */
public class DNACollector<T> {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.DNA);

	private final List<DNAItem> matches = new ArrayList<>();
	private final T parseResult;
	private final SourcePojo sourceObject;

	@Nullable
	private IDocument document;

	public DNACollector(final SourcePojo sourceObject, final ParseResultProvider<T> parser) throws DiscoveryException {
		this.sourceObject = sourceObject;
		try {
			this.parseResult = assertNotNull(parser.getParseResult(sourceObject));
		} catch (final DiscoveryException | WorkerCancellationException e) {
			throw new DiscoveryException("Unable to create the model for " + sourceObject.getName() + " in path" + sourceObject.getPath(), e);
		}
	}

	/**
	 * Add a new dna entry for a given ast node.
	 *
	 * @param primitiveCallback The callback to generate the dna value.
	 * @param node The ast node to access the location information.
	 */
	public void add(final DNAValueProvider primitiveCallback, @Nullable final AstNode node) {
		add(primitiveCallback, node, node);
	}

	/**
	 * Add a new dna entry for a given ast node.
	 *
	 * @param primitiveCallback The callback to generate the dna value.
	 * @param node The ast node to access the location information.
	 */
	public void add(final DNAValueProvider primitiveCallback, final innowake.lib.parsing.AstNode node) {
		final int startOffset = node.hasLocationInfo() ? node.getLeft() : 0;
		final int length = node.hasLocationInfo() ? node.getRight() - node.getLeft() : 0;
		if (length < 0) {
			LOG.trace(() -> "Tokens for DNA string of " + sourceObject.getPath() + " contain invalid offset information: offset=" + startOffset + " length="
					+ length);
		}
		add(primitiveCallback, startOffset, Math.max(0, length));
	}

	/**
	 * Add a new dna entry for a range between two ast nodes.
	 * <b>Important:</b> The start ast node must not be of type {@link ArtificialToken}.
	 *
	 * @param primitiveCallback The callback to generate the dna value.
	 * @param nodeStart The start node of the dna.
	 * @param nodeEnd The end node of the dna.
	 */
	private void add(final DNAValueProvider primitiveCallback, @Nullable final AstNode nodeStart, @Nullable final AstNode nodeEnd) {
		final int startOffset = nodeStart == null ? 0 : nodeStart.getStartOffset();
		final int length = nodeEnd == null ? 0 : nodeEnd.getEndOffset() - startOffset;
		if (length < 0) {
			LOG.trace(() -> "Tokens for DNA string of " + sourceObject.getPath() + " contain invalid offset information: offset=" + startOffset+ " length=" + length);
		}
		add(primitiveCallback, startOffset, Math.max(0, length));
	}

	/**
	 * Get the parse result for the current module.
	 *
	 * @return The parse result.
	 */
	public final T getParseResult() {
		return parseResult;
	}

	/**
	 * Get the list of {@link DNAItem}s for the current module.
	 *
	 * @return list of {@link DNAItem}s
	 */
	public List<DNAItem> getDNAItems() {
		return matches;
	}

	/**
	 * Reset all dna entries for this module.
	 * <b>Important:</b>This method is required to prepare the collector to process a new dna sequencer rule.
	 * It will keep the ast parse result, because this will not change for the module.
	 *
	 */
	public void reset() {
		matches.clear();
	}

	private void add(final DNAValueProvider primitiveCallback, final int offset, final int length) {
		final int endOffset = offset + length;
		@Nullable ILocationMultiline location = null;
		final IDocument doc = getDocument();
		final int line = doc.getLineNumber(offset);
		final int endLine = doc.getLineNumber(endOffset);
		final int column = doc.getColumnNumber(offset);
		final int endColumn = doc.getColumnNumber(endOffset);
		location = new LocationMultiline(offset, length, line, column, endLine, endColumn);

		final DNAItem match = new DNAItem(primitiveCallback, Optional.ofNullable(location));
		add(match);
	}

	private void add(final DNAItem match) {
		matches.add(match);
	}

	private final IDocument getDocument() {
		if (document == null) {
			document = new Document(sourceObject.getContent().toString());
		}
		return assertNotNull(document);
	}
}

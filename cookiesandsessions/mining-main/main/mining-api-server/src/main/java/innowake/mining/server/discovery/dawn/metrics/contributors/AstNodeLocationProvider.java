/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.contributors;

import innowake.mining.shared.model.AstNodeLocation;
import innowake.mining.shared.discovery.Tuple2;
import innowake.ndt.core.assembling.IAssembling;
import innowake.ndt.core.parsing.IDocument;
import innowake.ndt.core.parsing.ast.AdvancedLocationProvider;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.spi.Document;

/**
 * This class provides advanced location information for an AST node.
 * @param <T>
 */
public class AstNodeLocationProvider<T> {

	private final AdvancedLocationProvider<T> locationProvider;
	private final IDocument document;

	/**
	 * Constructor.
	 * @param assembling the assembling
	 * @param content the content
	 */
	public AstNodeLocationProvider(final IAssembling<T> assembling, final String content) {
		locationProvider = new AdvancedLocationProvider<>(assembling);
		document = new Document(content);
	}

	/**
	 * Returns the advanced module location for the given AST node.
	 * @param node the AST node
	 * @return the advanced module location
	 */
	public AstNodeLocation getAstNodeLocation(final AstNode node) {
		final var retracedLocation = locationProvider.getRetracedLocation(node);
		final int retracedOffset;
		final int retracedLength;
		if (retracedLocation == null) {
			retracedOffset = -1;
			retracedLength = -1;
		} else {
			retracedOffset = retracedLocation.getOffset();
			retracedLength = retracedLocation.getLength();
		}
		final var rootRelativeLocation = locationProvider.getRootRelativeLocation(node);
		final int rootRelativeOffset;
		final int rootRelativeLength;
		if (rootRelativeLocation == null) {
			rootRelativeOffset = -1;
			rootRelativeLength = -1;
		} else {
			rootRelativeOffset = rootRelativeLocation.getOffset();
			rootRelativeLength = rootRelativeLocation.getLength();
		}
		final var assembledLocation = locationProvider.getAssembledLocation(node);
		final int assembledOffset;
		final int assembledLength;
		if (assembledLocation == null) {
			assembledOffset = -1;
			assembledLength = -1;
		} else {
			assembledOffset = assembledLocation.getOffset();
			assembledLength = assembledLocation.getLength();
		}
		final var rootRelativeStartLineNumber = Integer.valueOf(document.getLineNumber(rootRelativeOffset) + 1);
		final var rootRelativeEndOffset = rootRelativeOffset + rootRelativeLength;
		final var rootRelativeEndLineNumber = Integer.valueOf(document.getLineNumber(rootRelativeEndOffset) + 1);
		return new AstNodeLocation(retracedOffset, retracedLength, assembledOffset, assembledLength, rootRelativeOffset, rootRelativeLength,
				rootRelativeStartLineNumber, rootRelativeEndLineNumber);
	}

	public static Tuple2<Integer, Integer> getLineNumbers(final Document document, final int offset, final int length) {
		final var startLineNumber = document.getLineNumber(offset) + 1;
		final var endOffset = offset + length;
		final var endLineNumber = document.getLineNumber(endOffset) + 1;
		return new Tuple2<>(startLineNumber, endLineNumber);
	}
}

/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import innowake.mining.data.core.moduledescription.api.LexerExtractor;
import innowake.ndt.core.parsing.ILexScanner;

/**
 * Abstract base class for lexer based comment extraction. 
 */
public abstract class AbstractCommentExtractor implements LexerExtractor {

	@Override
	public Stream<String> extract(final ILexScanner scanner, final String content) {
		final List<String> result = new ArrayList<>();
		while (scanner.next()) {
			if (isComment(scanner)) {
				result.add(scanner.getText().toString());
			}
		}
		
		return result.stream();
	}
	
	@Override
	public Stream<String> extract(final String content) {
		throw new UnsupportedOperationException();
	}
	
	/**
	 * @param scanner the language specific {@link ILexScanner}
	 * @return {@code true} if the current lexer region is a comment; {@code false} otherwise
	 */
	protected abstract boolean isComment(final ILexScanner scanner);

}

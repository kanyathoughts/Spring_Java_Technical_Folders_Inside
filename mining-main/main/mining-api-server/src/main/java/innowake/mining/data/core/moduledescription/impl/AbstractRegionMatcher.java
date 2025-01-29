/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.impl;

import innowake.lib.core.lang.Assert;
import innowake.mining.data.core.moduledescription.api.LexerMatcher;
import innowake.ndt.core.parsing.ILexScanner;

/**
 * Abstract base class for lexer based content matching.
 */
public abstract class AbstractRegionMatcher implements LexerMatcher {

	@Override
	public String match(final ILexScanner scanner, final String content) {
		final ILexScanner cobolScanner = Assert.assertNotNull(scanner);
		while (cobolScanner.next()) {
			if ( ! regionMatched(scanner)) {
				continue;
			}
			if (isRegionEnd(scanner)) {
				break;
			}
		}
		
		return content.substring(0, cobolScanner.getOffset());
	}
	
	@Override
	public String match(String content) {
		throw new UnsupportedOperationException();
	}
	
	/**
	 * Can be used to do additional token checks or handling.
	 * 
	 * @param scanner the language specific {@link ILexScanner}
	 * @return {@code true} if the current region matched; {@code false} otherwise to skip to the next token
	 */
	protected boolean regionMatched(final ILexScanner scanner) {
		return true;
	}
	
	/**
	 * @param scanner the language specific {@link ILexScanner}
	 * @return {@code true} if the end of the region to be matched has been reached and processing should stop; {@code false} otherwise
	 */
	protected abstract boolean isRegionEnd(final ILexScanner scanner);

}

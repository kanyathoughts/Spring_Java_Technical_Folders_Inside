/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.api;

import innowake.ndt.core.parsing.ILexScanner;

/**
 * This interface is used to reduce the given module source code content to the areas of interest for the module extraction.
 * This makes use of an {@link ILexScanner} to process the content.
 */
public interface LexerMatcher extends Matcher {

	/**
	 * Used to limit the source code to process for the next step.
	 * Needs an implementation per language.
	 *
	 * @param scanner language specific {@link ILexScanner}
	 * @param content the original source code content.
	 * @return the extracted source code or block comments passed to the {@link Extractor}.
	 */
	String match(ILexScanner scanner, String content);
	
}

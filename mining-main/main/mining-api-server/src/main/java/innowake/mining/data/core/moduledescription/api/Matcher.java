/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.api;

/**
 * This interface is used to reduce the given module source code content to the areas of interest for the module extraction.
 * The {@link Extractor} as next step gets only the limited view on the source code. 
 */
@FunctionalInterface
public interface Matcher {

	/**
	 * Used to limit the source code to process for the next step.
	 * Needs an implementation per language.
	 *
	 * @param content the original source code content.
	 * @return the extracted source code or block comments passed to the {@link Extractor}.
	 */
	String match(String content);
	
}

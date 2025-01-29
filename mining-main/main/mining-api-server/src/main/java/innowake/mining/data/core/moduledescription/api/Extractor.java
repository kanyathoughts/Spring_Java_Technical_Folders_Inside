/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.data.core.moduledescription.api;

import java.util.stream.Stream;

/**
 * Used to extract specific content from the given content.
 * The received content is passed from the {@link Matcher}, so it is probably not the original module content.
 * The extractor should build the final (unformatted) module description.
 */
@FunctionalInterface
public interface Extractor {

	/**
	 * Extract the final module description.
	 * Content is given by the {@link Matcher} and the result is forwarded to the {@link PrettyPrinter} 
	 *
	 * @param content the source code content as result of the previous {@link Matcher}
	 * @return Stream of string with the final (unformatted) module description. This is passed to the {@link PrettyPrinter}
	 */
	Stream<String> extract(String content);

}

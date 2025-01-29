/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.data.core.moduledescription.impl;

import java.util.stream.Stream;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.moduledescription.api.Extractor;
import innowake.mining.data.core.moduledescription.api.LexerExtractor;
import innowake.mining.data.core.moduledescription.api.LexerMatcher;
import innowake.mining.data.core.moduledescription.api.Matcher;
import innowake.mining.data.core.moduledescription.api.PrettyPrinter;
import innowake.ndt.core.parsing.ILexScanner;

/**
 * Container class for rules. They are applied in the order
 * <ol>
 * <li> {@link Matcher}: Used to limit the region where to find the module description.
 * <li> {@link Extractor}: Used to extract the module description.
 * <li> {@link PrettyPrinter}: Apply pretty printing to improve readability.
 * </ol>
 */
public class RuleSet {

	private final Matcher matcher;
	private final Extractor extractor;
	private final PrettyPrinter printer;

	private RuleSet(final Matcher matcher, final Extractor extractor, final PrettyPrinter printer) {
		this.matcher = matcher;
		this.extractor = extractor;
		this.printer = printer;
	}

	/**
	 * Create a new rule set instance.
	 *
	 * @param matcher The matcher instance.
	 * @param extractor The extractor instance.
	 * @param printer The printer instance.
	 * @return The rule set instance.
	 */
	public static final RuleSet create(final Matcher matcher, final Extractor extractor, final PrettyPrinter printer) {
		return new RuleSet(matcher, extractor, printer);
	}

	/**
	 * Apply the matcher.
	 *
	 * @param scanner The lighweight parser or {@code null} if not available
	 * @param content The original content.
	 * @return The matched subset of the content.
	 */
	public String applyMatcher(@Nullable final ILexScanner scanner, final String content) {
		if (scanner != null && matcher instanceof LexerMatcher) {
			return ((LexerMatcher) matcher).match(scanner, content);
		}
		return matcher.match(content);
	}

	/**
	 * Apply the extractor on the reduced matched content. 
	 *
	 * @param scanner The lighweight parser or {@code null} if not available
	 * @param content The subset of the original content
	 * @return A stream of descriptive strings. This is the extract for the module description.
	 */
	public Stream<String> applyExtractor(@Nullable final ILexScanner scanner, final String content) {
		if (scanner != null && extractor instanceof LexerExtractor) {
			return ((LexerExtractor) extractor).extract(scanner, content);
		}
		return extractor.extract(content);
	}

	/**
	 * Pretty print the descriptive strings to improve the human readability.
	 *  
	 * @param lines The stream of strings to prettify.
	 * @return The final module description.
	 */
	public String applyPrettyPrinter(final Stream<String> lines) {
		return printer.print(lines);
	}

}

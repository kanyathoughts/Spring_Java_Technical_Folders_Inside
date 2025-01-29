/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.impl;

import java.util.stream.Stream;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.moduledescription.impl.basic.BasicDescriptionExtractor;
import innowake.mining.data.core.moduledescription.impl.bms.BMSDescriptionExtractor;
import innowake.mining.data.core.moduledescription.impl.cobol.CobolDescriptionExtractor;
import innowake.mining.data.core.moduledescription.impl.jcl.JclDescriptionExtractor;
import innowake.mining.data.core.moduledescription.impl.natural.NaturalDescriptionExtractor;
import innowake.ndt.core.parsing.ILexer;

/**
 * Base class for Module Description Extraction.
 * This should be implemented per supported language. 
 */
public abstract class DescriptionExtractor {

	/**
	 * Predefined extractor for Cobol 
	 */
	public static final DescriptionExtractor FOR_COBOL = new CobolDescriptionExtractor();
	/**
	 * Predefined extractor for BMS.
	 */
	public static final DescriptionExtractor FOR_BMS   = new BMSDescriptionExtractor();
	
	/**
	 * Predefined extractor for BASIC.
	 */
	public static final DescriptionExtractor FOR_BASIC = new BasicDescriptionExtractor();
	
	/**
	 * Predefined extractor for Natural.
	 */
	public static final DescriptionExtractor FOR_NATURAL = new NaturalDescriptionExtractor();
	
	/**
	 * Predefined extractor for JCL.
	 */
	public static final DescriptionExtractor FOR_JCL = new JclDescriptionExtractor();
	
	private final RuleSet ruleSet;
	
	/**
	 * Create a new instance with a given rule set.
	 * 
	 * @param ruleSet The rule set to apply during extraction.
	 */
	protected DescriptionExtractor(final RuleSet ruleSet) {
		this.ruleSet = ruleSet;
	}
	
	/**
	 * Get the Module Description for a given source code.
	 * Keep in mind that this method must be called for the correct language.
	 * Example: For Cobol use {@link #FOR_COBOL}.
	 * 
	 *
	 * @param source The module source code to extract the description on.
	 * @return The extracted description. May be empy if none is found.
	 */
	public String getDescription(final String source) {
		final ILexer lexer = createLexer();
		final String matchedContent = ruleSet.applyMatcher(lexer != null ? lexer.createScanner(source) : null, source);
		final Stream<String> lines = ruleSet.applyExtractor(lexer != null ? lexer.createScanner(matchedContent) : null, matchedContent);
		return ruleSet.applyPrettyPrinter(lines);
	}
	
	/**
	 * Create the language specific ndt-core lexer instance.
	 *
	 * @return The lexer instance.
	 */
	@Nullable
	protected ILexer createLexer() {
		return null;
	}
	
}

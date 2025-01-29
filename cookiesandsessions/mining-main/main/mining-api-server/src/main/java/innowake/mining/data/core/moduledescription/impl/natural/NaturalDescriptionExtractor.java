/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.impl.natural;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.moduledescription.impl.DescriptionExtractor;
import innowake.mining.data.core.moduledescription.impl.RuleSet;
import innowake.ndt.core.parsing.ILexer;
import innowake.ndt.core.parsing.LexerConfiguration;
import innowake.ndt.core.parsing.natural.NaturalLexerFactory;

/**
 * Module description extractor for Natural. 
 */
public class NaturalDescriptionExtractor extends DescriptionExtractor {

	/**
	 * Create a new instance for Natural.
	 * The rule set is
	 * <ol>
	 * <li> {@link NaturalTopRegionMatcher} as matcher
	 * <li> {@link NaturalCommentExtractor} as extractor
	 * <li> {@link NaturalCommentPrettyPrinter} as pretty printer
	 * </ol>
	 */
	public NaturalDescriptionExtractor() {
		super(RuleSet.create(NaturalTopRegionMatcher.INSTANCE, NaturalCommentExtractor.INSTANCE, NaturalCommentPrettyPrinter.INSTANCE));
	}

	@Override
	@Nullable
	protected ILexer createLexer() {
		return new NaturalLexerFactory(true).createLexer(LexerConfiguration.DEFAULT);
	}

}

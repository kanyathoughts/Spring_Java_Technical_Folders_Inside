/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.impl.cobol;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.moduledescription.api.CommentPrettyPrinter;
import innowake.mining.data.core.moduledescription.impl.AbstractCommentExtractor;
import innowake.mining.data.core.moduledescription.impl.AbstractRegionMatcher;
import innowake.mining.data.core.moduledescription.impl.DescriptionExtractor;
import innowake.mining.data.core.moduledescription.impl.RuleSet;
import innowake.ndt.core.parsing.ILexer;
import innowake.ndt.core.parsing.LexerConfiguration;
import innowake.ndt.core.parsing.cobol.CobolLexerFactory;

/**
 * Module description extractor for Cobol. 
 */
public class CobolDescriptionExtractor extends DescriptionExtractor {

	/**
	 * Create a new instance for Cobol.
	 * The rule set is
	 * <ol>
	 * <li> {@link AbstractRegionMatcher} as matcher
	 * <li> {@link AbstractCommentExtractor} as extractor
	 * <li> {@link CommentPrettyPrinter} as pretty printer
	 * </ol>
	 */
	public CobolDescriptionExtractor() {
		super(RuleSet.create(CobolTopRegionMatcher.INSTANCE, CobolBlockCommentExtractor.INSTANCE, CommentPrettyPrinter.INSTANCE));
	}

	@Override
	@Nullable
	protected ILexer createLexer() {
		return CobolLexerFactory.get().createLexer(LexerConfiguration.DEFAULT);
	}

}

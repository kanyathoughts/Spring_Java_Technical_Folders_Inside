/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.impl.jcl;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.moduledescription.impl.DescriptionExtractor;
import innowake.mining.data.core.moduledescription.impl.RuleSet;
import innowake.ndt.core.parsing.ILexer;
import innowake.ndt.core.parsing.jcl.JclLexer;

/**
 * Module description extractor for JCL. 
 */
public class JclDescriptionExtractor extends DescriptionExtractor {

	/**
	 * Create a new instance for Natural.
	 * The rule set is
	 * <ol>
	 * <li> {@link JclTopRegionMatcher} as matcher
	 * <li> {@link JclCommentExtractor} as extractor
	 * <li> {@link JclCommentPrettyPrinter} as pretty printer
	 * </ol>
	 */
	public JclDescriptionExtractor() {
		super(RuleSet.create(JclTopRegionMatcher.INSTANCE, JclCommentExtractor.INSTANCE, JclCommentPrettyPrinter.INSTANCE));
	}

	@Override
	@Nullable
	protected ILexer createLexer() {
		return new JclLexer();
	}

}

/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.impl.basic;

import innowake.mining.data.core.moduledescription.api.AllMatcher;
import innowake.mining.data.core.moduledescription.impl.DescriptionExtractor;
import innowake.mining.data.core.moduledescription.impl.RuleSet;

/**
 * {@link DescriptionExtractor} implementation for BASIC Modules based on block comments.
 */
public class BasicDescriptionExtractor extends DescriptionExtractor {

	/**
	 * Creates a new description extractor instance for BASIC Modules.
	 */
	public BasicDescriptionExtractor() {
		/* There is no specific "marker" in order to constrain the source code to a particular region, so we 
		 * are using the AllMatcher here. The identification of the relevant block comments will be completely
		 * done in the Extractor implementation. */
		super(RuleSet.create(AllMatcher.INSTANCE, BasicBlockCommentDescriptionExtractor.INSTANCE, BasicDescriptionPrettyPrinter.INSTANCE));
	}

}

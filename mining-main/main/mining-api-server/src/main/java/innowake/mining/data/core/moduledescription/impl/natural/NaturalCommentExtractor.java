/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.impl.natural;

import innowake.mining.data.core.moduledescription.api.LexerExtractor;
import innowake.mining.data.core.moduledescription.impl.AbstractCommentExtractor;
import innowake.ndt.core.parsing.ILexScanner;
import innowake.ndt.core.parsing.IRegionType;
import innowake.ndt.core.parsing.natural.NaturalRegionType;

/**
 * Used to extract the Natural comments for a given content. 
 */
public class NaturalCommentExtractor extends AbstractCommentExtractor {

	/**
	 * The {@link NaturalCommentExtractor} instance.
	 */
	public static final LexerExtractor INSTANCE = new NaturalCommentExtractor();
	
	@Override
	public boolean isComment(final ILexScanner scanner) {
		final IRegionType type = scanner.getRegionType();
		return type == NaturalRegionType.TYPE_COMMENT_BEGIN_OF_LINE || type == NaturalRegionType.TYPE_COMMENT_END_OF_LINE
				|| type == NaturalRegionType.TYPE_COMMENT_MULTILINE;
	}

}

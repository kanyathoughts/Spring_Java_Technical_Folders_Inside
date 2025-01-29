/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.impl.jcl;

import innowake.mining.data.core.moduledescription.api.LexerExtractor;
import innowake.mining.data.core.moduledescription.impl.AbstractCommentExtractor;
import innowake.ndt.core.parsing.ILexScanner;
import innowake.ndt.core.parsing.jcl.JclRegionType;

/**
 * Used to extract the JCL comments for a given content. 
 */
public class JclCommentExtractor extends AbstractCommentExtractor {

	/**
	 * The {@link JclCommentExtractor} instance.
	 */
	public static final LexerExtractor INSTANCE = new JclCommentExtractor();
	
	@Override
	public boolean isComment(final ILexScanner scanner) {
		return scanner.getRegionType() == JclRegionType.TYPE_COMMENT;
	}

}

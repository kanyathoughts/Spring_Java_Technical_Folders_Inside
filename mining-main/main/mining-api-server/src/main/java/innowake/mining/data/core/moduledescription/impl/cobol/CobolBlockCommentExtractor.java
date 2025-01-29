/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.impl.cobol;

import innowake.mining.data.core.moduledescription.api.LexerExtractor;
import innowake.mining.data.core.moduledescription.impl.AbstractCommentExtractor;
import innowake.ndt.core.parsing.ILexScanner;
import innowake.ndt.core.parsing.cobol.CobolRegionType;

/**
 * Used to extract the Cobol comments for a given content. 
 */
public class CobolBlockCommentExtractor extends AbstractCommentExtractor {

	/**
	 * The {@link CobolBlockCommentExtractor} instance.
	 */
	public static final LexerExtractor INSTANCE = new CobolBlockCommentExtractor();
	
	@Override
	public boolean isComment(final ILexScanner scanner) {
		return scanner.getRegionType() == CobolRegionType.TYPE_COMMENT;
	}

}

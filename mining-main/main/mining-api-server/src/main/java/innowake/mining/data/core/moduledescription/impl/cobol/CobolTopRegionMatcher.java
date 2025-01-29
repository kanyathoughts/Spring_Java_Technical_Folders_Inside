/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.impl.cobol;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.moduledescription.api.LexerMatcher;
import innowake.mining.data.core.moduledescription.impl.AbstractRegionMatcher;
import innowake.ndt.core.parsing.ILexScanner;
import innowake.ndt.core.parsing.cobol.CobolRegionType;

/**
 * Matcher to extract the Cobol top region between the program start and PROCEDURE DIVISION.
 */
public class CobolTopRegionMatcher extends AbstractRegionMatcher {
	
	/**
	 * The {@link CobolTopRegionMatcher} instance.
	 */
	public static final LexerMatcher INSTANCE = new CobolTopRegionMatcher();
	
	@Nullable
	private CharSequence lastToken;
	
	@Override
	public String match(final ILexScanner scanner, final String content) {
		lastToken = null;
		return super.match(scanner, content);
	}
	
	@Override
	protected boolean isRegionEnd(final ILexScanner scanner) {
		if (scanner.getRegionType() == CobolRegionType.TYPE_CODE_STANDARD
				&& ("ENVIRONMENT".equals(lastToken) || "DATA".equals(lastToken) || "PROCEDURE".equals(lastToken))
				&& "DIVISION".equals(scanner.getText())) {
			return true;
		}
		lastToken = scanner.getText();
		return false;
	}
}
